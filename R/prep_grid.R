#' Ajout des régions admins aux données GBIF
#'
#' @description
#' Utiliser les polygones des régions administratifs (e.g. municipalités)
#' pour établir une grille hexagonale H3 à une échelle donnée échelle
#' (e.g., résolution 10L) qui couvre tous les polygones.
#' Essentiellement, c'est un tableau des polygones de régions avec les indices
#' H3 correspondant.
#'
#' L'ajout des indices H3, aux données administratives, crée une grille
#' standardisée avec tous les noms de régions associés à H3.
#' Cette grille
#' utile pour en faire référence avec des données GBIF.
#' Pour faire une carte en ligne, .pmtiles serait mieux.
#'
#' L'utilisation de duckdb permet de créer un fichier de 43.86 millions de
#' lignes puis d'exporter directement en parquet et sa compression.
#' L'exportation en parquet permet d'avoir un fichier de petite taille
#' (compression), et rapidement (plus que .csv)
#'
#' @section Utilisation potentielle :
#' Attribue un nom de région à une grille standardisée H3.
#' Pour faire de la cartographie statique de points qui
#' se présenterait en unité spatiale standardisée.
#' Cela permet de garder séparer la colonne des informations de région des
#' données de GBIF. Avant, le code fait tout en même temps. Mais la grille
#' n'a besoin d'être créé qu'une seule fois. Donc, cette étape a été séparée.
#'
#' @md
#' @param con Connection à un pilote `duckdb()`
#' @param config Liste avec les chemins d'accès au minimum :
#' - `admin_shp` : mis en mémoire avec `ST_Read`.
#' - `out_admin_pq` : qui exporte un fichier ".parquet".
#' @param res "integer": nombre déterminant la résolution H3 e.g., `9L`
#' @param colsAdmin (Character) est un vecteur des noms de colonnes à
#' sélectionner pour les données administrative (rend le jeu de données
#' plus petit).
#'
#'
#' @details
#' Connexion à `duckdb` et trouver les indices H3 à une résolution `res`.
#' Utiliser `transmute` pour garder que les noms '`MUS_NM_*`' et
#' l'indice H3. Exportation (`COPY`) du fichier selon une préférence
#' choisie dans le `config`.
#'
#' Pour donner un ordre de grandeur :
#' Resolution: 4,
#' "842baa5ffffffff" est un polygone qui
#' engloble aisément Montréal et Laval (aire ~1930 km2),
#'
#' Resolution: 7,
#' "872baa441ffffff" englobe le Mont-Royal (aire ~5.6 km2)
#'
#' Resolution: 9,
#' "872baa441ffffff" est plus petit que le Stade-Olympic (aire ~114490 m2)
#'
#' Resolution: 10, "8a2baa46a50ffff" englobe la salle Wilfrid-Pelletier et
#' la maison symphonique (aire ~16345 m2)
#'
#' @returns
#' Exportation de données sous forme d'un tableau des colonnes `colsAdmin`
#' et h3_cell qui est l'indice de l'hexagone du système H3. À résolution
#' 10L pour la province de Québec,
#' `out_admin_pq`.
#'
#' @export
#'
#' @importFrom glue glue
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # --- Configuration ---
#' con = setup_duckdb()
#' folder = "test_path" # Dossier avec les données
#' paths <- list(
#'   # Importation
#'   admin_shp  = file.path(folder, "munic_s.shp"),
#'   # Exportation
#'   out_admin_pq  = file.path(folder, "admin_mun.parquet")
#' )
#' res = 10L
#' # --- Calcul---
#'
#' prep_h3_admin(con, config = paths, res = res)
#' }
prep_h3_admin <- function(
  con = NULL,
  config,
  res = 10L,
  colsAdmin = c(
    "MUS_NM_MUN",
    "MUS_NM_MRC",
    "MUS_NM_REG"
  )
) {
  if (is.null(con)) {
    con <- setup_duckdb()
    on.exit(expr = DBI::dbDisconnect(con, shutdown = TRUE))
  }

  # Définition de la colonne de géométrie
  geom_col_name <- "geom"
  geom_sym <- rlang::sym(geom_col_name)
  # NOTE :
  # Colonnes spécifique au jeu de données administratif du Québec
  # Prépare les données de régions administratives
  admin_qc_tbl <- dplyr::tbl(
    src = con,
    from = dbplyr::sql(
      glue::glue_sql(
        "SELECT * FROM ST_Read({config$admin_shp})",
        .con = con
      )
    )
  ) |>
    dplyr::select(
      dplyr::all_of(colsAdmin),
      !!geom_sym
    ) |>
    dplyr::mutate(
      # ST_Transform a besoin de extension spatiale de duckdb
      # la quasiquation (i.e., !!) demande le walrus operator!!
      !geom_sym := dbplyr::sql(
        # Remplace le nom de la colonne de géométrie de manière dynamique
        glue::glue_sql(
          "ST_Transform({`geom_sym`}, 'EPSG:4269', 'EPSG:4326')",
          .con = con
        )
      )
    )

  # Préparation de la table région admin en trouvant les IDs des polygones
  # H3 cells (Résolution XX)
  admin_h3_indexed <- admin_qc_tbl |>
    dplyr::mutate(
      # Création d'une 'liste' de cells (liste d'hexagones couvrant le polygone)
      # cells = h3_polygon_wkt_to_cells(ST_AsText("geom"), as.integer(res))
      cells = dbplyr::sql(
        glue::glue_sql(
          "h3_polygon_wkt_to_cells(ST_AsText({`geom_sym`}), {as.integer(res)})",
          .con = con
        )
      )
    ) |>
    # Utilisation d'une subquery/transmute pour faire une opération (un peu
    # comme mutate), mais qui ne garde que les colonnes désirées. Avec unnest
    # DuckDB's unnest() on prend les listes d'hexagone et on les reporte sur des
    # nouvelles lignes. Donc chaque MUS_NM_* se retrouve avec 1 ID de
    # la grille H3
    dplyr::mutate(
      h3_cell = dbplyr::sql("unnest(cells)"),
      # Garde colonnes d'indicateur de région (pour la jointure basée sur H3)
      dplyr::across(
        dplyr::all_of(colsAdmin)
      ),
      # Comportement similaire à 'Transmute'
      .keep = "none"
    )

  # Requête SQL
  raw_dbplyr_query <- DBI::SQL(
    dbplyr::remote_query(
      x = admin_h3_indexed
    )
  )

  export_sql <- glue::glue_sql(
    "COPY ({raw_dbplyr_query})
     TO {config$out_admin_pq}
     (FORMAT PARQUET);",
    .con = con
  )

  # Execute l'exportation
  message(
    glue(
      "Grille : jointure spatiale et exporte en Parquet...
  Sortie : {config$out_admin_pq}"
    )
  )

  tictoc::tic("Calcul de la grille selon les régions administratives")
  DBI::dbExecute(
    conn = con,
    statement = export_sql
  )
  tictoc::toc()

  message(glue("Réussi: Données ici --> {config$out_admin_pq}"))
}
