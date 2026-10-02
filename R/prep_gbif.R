#' Prépare et jointure des données GBIF et
#' des régions administratives via la grille H3
#'
#' @md
#' @description
#' Filtre les données brutes GBIF, calcule l'indexation
#' spatiale H3 à la résolution spécifiée, puis effectue une jointure
#' interne avec une table administrative précalculée.
#' Le résultat est exporté directement en Parquet compressé.
#'
#' @param con Connexion DuckDB valide. Si `NULL`, une
#' connexion temporaire est initialisée.
#' @param config Liste avec les chemins d'accès au minimum :
#' - `gbif_raw` : fichier GBIF d'entrée en .parquet préalablement
#'   transformé après le téléchargement avec [evolovr::transforme_gbif()].
#' - `out_admin_pq` : tableau des noms administrative et leur index
#'      H3 à une résolution.
#' - `out_gbif_pq` : chemin d'accès de sortie (`./path/nom.parquet`).
#' @param res Integer. Résolution de la grille H3 (ex: `10L`).
#' @param basisRec Vecteur avec basisOfRecord
#' @param occStat Vecteur avec occurrenceStatus
#' @param taxRank Vecteur avec taxonRank
#' @param kingdm Vecteur avec kingdom
#' @param coordUncertainM Vecteur avec coordinateUncertaintyInMeters
#'
#' @details
#' L'identifiant H3 retourné est un entier DuckDB de type `uint64`.
#' Comme R ne gère pas nativement le 64-bit de manière précise,
#' cet ID est traité sous forme de chaîne de
#' caractères ou via le type natif de DuckDB lors des requêtes.
#'
#' @return NULL. La fonction exporte directement le
#' fichier traité sur le disque.
#'
#' @importFrom dplyr tbl filter mutate inner_join
#' @importFrom dbplyr remote_query sql
#' @importFrom glue glue
#' @importFrom DBI dbExecute dbQuoteLiteral
#' @importFrom tictoc tic toc
#' @importFrom cli cli_abort cli_alert_info cli_alert_success
#'   cli_progress_message
#'
#' @export
#'
#' @examples
#' \dontrun{
#' con <- setup_duckdb()
#' paths <- list(
#'   gbif_raw     = "data/gbif_prep_all.parquet",
#'   out_admin_pq = "data/admin_mun.parquet",
#'   out_gbif_pq  = "data/gbif_prep_h3_res_10.parquet"
#' )
#' join_gbif_admin(con, config = paths, res = 10L)
#' }
join_gbif_admin <- function(
  con = NULL,
  config,
  res = 10L,
  basisRec = c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION"),
  occStat = c("PRESENT"),
  taxRank = c("SPECIES", "SUBSPECIES", "VARIETY"),
  kingdm = c("Chromista", "Fungi", "Plantae", "Animalia"),
  coordUncertainM = 200
) {
  # Gestion de la connexion
  is_local_con <- is.null(con)
  if (is_local_con) {
    con <- evolovr::setup_duckdb()
    on.exit(
      {
        evolovr::discon_duckdb(con)
      },
      add = TRUE
    )
  }
  # Chemin d'accès temporaire pour duckdb
  # Bug : https://github.com/duckdb/duckdb-r/pull/2562
  # Si personnes sur ancienne version de duckdb, cela va fonctionner
  # avec ce code défensif.
  duckdb_temp <- file.path(tempdir(), "duckdb", "temp")
  dir.create(duckdb_temp, recursive = TRUE, showWarnings = FALSE)
  safe_path <- DBI::dbQuoteLiteral(con, duckdb_temp)
  set_temp_sql <- glue::glue("SET temp_directory = {safe_path};")

  # Validations des arguments de configuration
  required_paths <- c("out_admin_pq", "gbif_raw", "out_gbif_pq")
  message(
    "Config paths:\n",
    paste(
      sprintf(" %s :\t%s", required_paths, config[required_paths]),
      collapse = "\n"
    )
  )
  missing_paths <- setdiff(required_paths, names(config))
  if (length(missing_paths) > 0) {
    cli::cli_abort(
      "Le paramètre {.arg config} requiert les
        champs manquants suivants : {.val {missing_paths}}"
    )
  }

  # Création du dossier de sortie si manquant
  dir.create(
    path = dirname(path = config$out_gbif_pq),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Connexion aux tables distantes via DuckDB
  # NOTE : finalement, ne PAS faire la jointure à ce stade
  # message("Lecture de la grille admin-H3")
  # admin_tbl <- dplyr::tbl(
  #   src = con,
  #   from = dbplyr::sql(
  #     glue::glue(
  #       "SELECT * FROM read_parquet('{config$out_admin_pq}')"
  #     )
  #   )
  # ) |>
  #       dplyr::select(
  #         "MUS_NM_MUN",
  #         "MUS_NM_MRC",
  #         "MUS_NM_REG"
  #       )
  # Lire les données GBIF transformées du fichier original vers parquet
  message("Lecture données GBIF")
  gb_tbl <- dplyr::tbl(
    src = con,
    from = dbplyr::sql(
      glue::glue(
        "SELECT * FROM read_parquet('{config$gbif_raw}')"
      )
    )
  )

  # Pipeline de transformation et filtration des données GBIF
  message("Pipeline de transformation")
  # options
  pipeline_filt <- gb_tbl |>
    # Quelque filtre des données GBIF
    dplyr::filter(
      # Retirer les espèces avec NA
      # NOTE : après exploration, ce champ GBIF n'est pas obligatoire
      # alors que scientificName oui. Donc, beaucoup de données sont manquantes
      # si on met ce filtre.
      # !is.na(species),
      basisOfRecord %in% basisRec,
      # Filtre administratif (pas vraiment besoin puisque nous utilisation
      # une jointure avec les données spatiales des régions administratives)
      # countryCode == "CA",
      # stateProvince %in% c("Quebec", "Québec", "Qc") |
      # is.na(stateProvince),
      occurrenceStatus %in% occStat,
      # Filtre taxonomique
      taxonRank %in% taxRank,
      kingdom %in% kingdm,
      # Filtre géographique
      coordinateUncertaintyInMeters <= coordUncertainM |
        is.na(coordinateUncertaintyInMeters)
    ) |>
    # Création colonne H3
    dplyr::mutate(
      # Utilisation de dbplyr::sql pour forcer l'évaluation par DuckDB
      h3_cell = dbplyr::sql(
        glue::glue(
          "h3_latlng_to_cell(
             ST_Y(geometry), ST_X(geometry),
             {as.integer(res)}
          )"
        )
      )
    )
  # Spatial join to keep only points inside municipality polygons
  #

  colsAdmin <- c(
    "MUS_NM_MUN",
    "MUS_NM_MRC",
    "MUS_NM_REG"
  )

  # Préparer pour SELECT de colonnes dans SQL
  cols_sql <- paste(colsAdmin, collapse = ", ")

  # NOTE : jointure (inner join) des données admin
  # filtre les données spatialement tout en ajoutant le nom des colonnes
  # administratives.
  pipeline <- dplyr::tbl(
    src = con,
    from = dbplyr::sql(
      glue::glue(
        "
      SELECT 
        g.*, 
        {cols_sql}
      FROM ({dbplyr::remote_query(pipeline_filt)}) AS g
      INNER JOIN (
        SELECT 
          {cols_sql}, 
          ST_MakeValid(ST_Transform(geometry, 'EPSG:4269', 'EPSG:4326')) AS geom_admin
        FROM read_parquet('{config$out_admin_pq}')
      ) AS q
      ON ST_Intersects(g.geometry, q.geom_admin)
    "
      )
    )
  )
  # Ajout de l'information administrative
  # --> trouver l'intersection entre X et Y
  # dplyr::inner_join(
  #   admin_h3_idx_precalc,
  #   by = "h3_cell"
  # )

  # Préparation de la requête d'exportation native
  query_raw <- dbplyr::remote_query(pipeline)

  export_sql <- glue::glue(
    "COPY (
    {query_raw}
    )
    TO '{config$out_gbif_pq}' (
    FORMAT parquet,
    COMPRESSION 'zstd',
    COMPRESSION_LEVEL 4
    );"
  )

  # Exécution et Chronométrage
  cli::cli_alert_info(
    "Exécution de la jointure H3 et exportation vers Parquet"
  )
  # p <- cli::cli_progress_message(
  #   msg = "Exécution de la jointure H3 et exportation vers Parquet\n"
  #   )
  # Désactiver la barre de progrès de duckdb pour
  # prendre le contrôle de ce qui s'affiche dans la console
  # DBI::dbExecute(con, "SET enable_progress_bar = false;")

  tictoc::tic("Exécution pipeline de filtre, H3 et jointure admin-H3")
  DBI::dbExecute(con, export_sql)
  tictoc::toc()

  cli::cli_alert_success(
    "Données exportées avec succès à l'emplacement :
    {.path {config$out_gbif_pq}}"
  )

  return(invisible(NULL))
}
