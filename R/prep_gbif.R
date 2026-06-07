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
#' \itemize{
#'   \item \code{gbif_raw} : entrée.
#'   \item \code{out_admin_pq} : référence administrative.
#'   \item \code{out_gbif_pq} : sortie.
#' }
#' @param res Integer. Résolution de la grille H3 (ex: `10L`).
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
#' @importFrom DBI dbExecute
#' @importFrom tictoc tic toc
#' @importFrom cli cli_abort cli_alert_info cli_alert_success
#' cli_process_start cli_process_done
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
join_gbif_admin <- function(con = NULL, config, res = 10L) {

  # Gestion de la connexion
  is_local_con <- is.null(con)
  if (is_local_con) {
    con <- evolovr::setup_duckdb()
    on.exit({
      evolovr::discon_duckdb(con)
    },
    add = TRUE)
  }

  # Validations des arguments de configuration
  required_paths <- c("out_admin_pq", "gbif_raw", "out_gbif_pq")
  missing_paths <- setdiff(required_paths, names(config))
  if (length(missing_paths) > 0) {
    cli::cli_abort(
        "Le paramètre {.arg config} requiert les
        champs manquants suivants : {.val {missing_paths}}")
  }

  # Création du dossier de sortie si manquant
  dir.create(
    path = dirname(path = config$out_gbif_pq),
    showWarnings = FALSE,
    recursive = TRUE)

  # Connexion aux tables distantes via DuckDB
  admin_h3_idx_precalc <- dplyr::tbl(
    src = con,
    from = dbplyr::sql(
      glue::glue(
        "SELECT * FROM read_parquet('{config$out_admin_pq}')"
      )
    )
  ) # |>
    # mutate(
    #   h3_cell_4 = dbplyr::sql(
    #     glue::glue("h3_cell_to_parent(\"h3_cell\", {as.integer(res)})")
    #   )
    # )

# Lire les données GBIF transformées du fichier original vers parquet
  gb_tbl <- dplyr::tbl(
    src = con,
    from = dbplyr::sql(
      glue::glue(
        "SELECT * FROM read_parquet('{config$gbif_raw}')"
      )
    )
  )

  # Pipeline de transformation et filtration des données GBIF
  pipeline <- gb_tbl |>
    # Quelque filtre des données GBIF
    dplyr::filter(
      !is.na(species),
      basisOfRecord == "HUMAN_OBSERVATION",
      countryCode == "CA",
      stateProvince %in% c("Quebec", "Québec", "Qc") |
        is.na(stateProvince),
      taxonRank %in% c("SPECIES", "SUBSPECIES", "VARIETY"),
      kingdom %in% c("Chromista", "Fungi", "Plantae", "Animalia"),
      coordinateUncertaintyInMeters <= 200 |
        is.na(coordinateUncertaintyInMeters)
    ) |>
    # Création colon  ne H3
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
    ) |>
    # Ajout de l'information administrative
    dplyr::inner_join(
      admin_h3_idx_precalc,
      by = "h3_cell"
    )

  # 5. Préparation de la requête d'exportation native
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
   # p <- cli::cli_process_start(
   #   msg = "Exécution de la jointure H3 et exportation vers Parquet\n"
   #   )
  # Désactiver la barre de progrès de duckdb pour
  # prendre le contrôle de ce qui s'affiche dans la console
  # DBI::dbExecute(con, "SET enable_progress_bar = false;")

  tictoc::tic()
  DBI::dbExecute(con, export_sql)
  tictoc::toc()

  # cli::cli_process_done(p)

  cli::cli_alert_success(
    "Données exportées avec succès à l'emplacement :
    {.path {config$out_gbif_pq}}"
    )

  return(invisible(NULL))
}
