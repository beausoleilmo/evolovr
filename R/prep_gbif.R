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
#' @param config Liste contenant les chemins d'accès : `gbif_raw` (entrée),
#'   `out_admin_pq` (référence administrative) et `out_gbif_pq` (sortie).
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

  # 1. Gestion stricte de la connexion
  is_local_con <- is.null(con)
  if (is_local_con) {
    con <- evolovr::setup_duckdb()
    on.exit({
      evolovr::discon_duckdb(con)
    },
    add = TRUE)
  }

  # 2. Validations des arguments de configuration
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

  # 3. Connexion aux tables distantes via DuckDB
  admin_h3_idx_precalc <- dplyr::tbl(
    src = con,
    from = dbplyr::sql(
      glue::glue(
        "SELECT * FROM read_parquet('{config$out_admin_pq}')"
      )
    )
  )

  gb_tbl <- dplyr::tbl(
    src = con,
    from = dbplyr::sql(
      glue::glue(
        "SELECT * FROM read_parquet('{config$gbif_raw}')"
      )
    )
  )

  # 4. Pipeline de transformation et filtrage
  pipeline <- gb_tbl |>
    dplyr::filter(
      !is.na(species),
      basisofrecord == "HUMAN_OBSERVATION",
      countrycode == "CA",
      stateprovince %in% c("Quebec", "Québec", "Qc") | is.na(stateprovince),
      taxonrank %in% c("SPECIES", "SUBSPECIES", "VARIETY"),
      kingdom %in% c("Chromista", "Fungi", "Plantae", "Animalia"),
      coordinateuncertaintyinmeters <= 200 |
        is.na(coordinateuncertaintyinmeters)
    ) |>
    dplyr::mutate(
      # Utilisation de dbplyr::sql pour forcer l'évaluation par DuckDB
      h3_cell = dbplyr::sql(
        glue::glue(
          "h3_latlng_to_cell(ST_Y(geometry), ST_X(geometry), {as.integer(res)})"
        )
      )
    ) |>
    dplyr::inner_join(
      admin_h3_idx_precalc,
      by = "h3_cell"
    )

  # 5. Préparation de la requête d'exportation native
  query_raw <- dbplyr::remote_query(pipeline)

  export_sql <- glue::glue(
    "COPY ({query_raw}) ",
    "TO '{config$out_gbif_pq}' ",
    "(FORMAT parquet, COMPRESSION 'zstd', COMPRESSION_LEVEL 4);"
  )

  # 6. Exécution et Chronométrage
  p <- cli::cli_process_start(
    "Exécution de la jointure H3 et exportation vers Parquet"
    )

  tictoc::tic()
  DBI::dbExecute(con, export_sql)
  tictoc::toc()

  cli::cli_process_done(p)
  cli::cli_alert_success(
    "Données exportées avec succès à l'emplacement :
    {.path {config$out_gbif_pq}}"
    )

  return(invisible(NULL))
}
