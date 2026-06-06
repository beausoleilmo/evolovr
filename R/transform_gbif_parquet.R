#' Transforme données GBIF brutes en fichier spatial PARQUET
#' @md
#'
#' @description
#' Transforme un répertoire de données brutes GBIF au format `SIMPLE_PARQUET`
#' en un unique fichier Parquet compressé et non partitionné contenant une
#' colonne géométrique géospatiale (`geometry`).
#'
#' @param entree Character. Chemin d'accès vers le répertoire contenant les
#'   fichiers \code{occurrence.parquet} du GBIF.
#' @param sortie Character. Chemin d'accès du nouveau fichier Parquet
#' e.g., "\code{donnees/gbif_raw_new.parquet}"
#'
#'
#' @details
#' L'utilisation de la compression ZSTD ("\code{COMPRESSION 'zstd'}") et
#' le niveau 4  ("\code{COMPRESSION_LEVEL 4}") permet d'optimiser
#' l'espace disque tout en conservant la performances de lecture.
#'
#' @source Voir le ticket GitHub du portail GBIF
#'   \url{https://github.com/gbif/portal-feedback/issues/6570}.
#'
#' @note
#' Les téléchargements de type `SIMPLE_PARQUET` de GBIF génèrent un dossier
#' contenant des sous-fichiers fragmentés par Apache Spark (e.g., `000000`, `000001`).
#' Si l'un de ces fichiers est vide (0 octet, souvent le fichier `000000`), DuckDB
#' échoue lors de la lecture globale. Cette fonction contourne explicitement ce bug
#' en filtrant les fichiers vides et les fichiers système cachés (commençant par `._`).
#'
#' @return NULL. La fonction exporte directement le fichier
#' traité sur le disque.
#'
#' @importFrom cli cli_abort cli_warn cli_alert_info
#' cli_process_start cli_process_done cli_ul cli_li cli_end cli_alert_success
#' @importFrom DBI dbConnect dbIsValid dbDisconnect dbExecute dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom tictoc tic toc
#'
#' @export
#'
#' @examples
#' \dontrun{
#' transforme_gbif(
#'   entree = "occurrence.parquet",
#'   sortie = "gbif_raw_new.parquet"
#' )
#' }
transforme_gbif <- function(entree, sortie) {

  # Vérifications
  if (!dir.exists(entree)) {
    cli::cli_abort("Le répertoire d'entrée {.path {entree}} n'existe pas.")
  }

  if (!grepl("\\.parquet$", sortie, ignore.case = TRUE)) {
    cli::cli_warn(
      message =
        "Le fichier de sortie {.path {sortie}} n'a pas l'extension '.parquet'.")
  }

  # Création du dossier du fichier de sortie
  dir.create(
    path = dirname(sortie),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Initialise la connexion DuckDB (en mémoire)
  con <- DBI::dbConnect(duckdb::duckdb())

  # Assurer la déconnexion et la fermeture propre de la DB en cas d'erreur
  on.exit({
    if (DBI::dbIsValid(con)) {
      # Fermer la connexion duckdb
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  },
  add = TRUE)

  # Installation et chargement d'extension spatiale
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")

  # Définir des variables DuckDB
  # Fichier d'entrée
  DBI::dbExecute(con, sprintf("SET VARIABLE gb_path_pq = '%s';", entree))
  # Fichier de sortie
  DBI::dbExecute(con, sprintf("SET VARIABLE outpath = '%s';", sortie))

  # Filtrer les fichiers dans 'gb_files' pour exclure 000000 (vide = 0 byte) et
  # fichier commançants par '._'
  DBI::dbExecute(con, "
  SET variable gb_files = (
    SELECT list(file)
    FROM glob(getvariable('gb_path_pq') || '/*')
    WHERE file NOT LIKE '%/000000'
    AND file NOT LIKE '%/._%'
  );
")

  # Récupération du nombre de fichiers (length de la liste)
  # Note : 'length()' sur une liste DuckDB retourne le nombre d'éléments.
  nb_fichiers <- DBI::dbGetQuery(
    conn = con,
    "SELECT length(getvariable('gb_files')) AS total;")

  cli::cli_alert_info(
    text = "Nombre de fichiers fragmentés à traiter : {nb_fichiers$total}"
  )

  if (is.na(nb_fichiers$total) || nb_fichiers$total == 0) {
    cli::cli_abort("Aucun fichier valide à traiter dans le répertoire source.")
  }

  # Préparation et exécution de la copie Spatiale
  DBI::dbExecute(con, "
  PREPARE copy_spatial_data AS
  COPY (
    SELECT
      *,
      ST_Point(decimalLongitude, decimalLatitude) AS geometry
    FROM
      read_parquet(getvariable('gb_files'))
  ) TO ? (FORMAT parquet, COMPRESSION 'zstd', COMPRESSION_LEVEL 4);
")

  on.exit({
    try(DBI::dbExecute(con, "DEALLOCATE copy_spatial_data;"),
        silent = TRUE)
  },
  add = TRUE)

  cli::cli_process_start("Transformation des données GBIF en cours...")
  cli::cli_ul()
  cli::cli_li("Source : {.path {entree}}")
  cli::cli_li("Destination : {.path {sortie}}")
  cli::cli_end()

  message(sprintf(
    fmt = "
    Exécution de la copie de données spatiales
    Fichier entrée : %s
    Fichier sortie : %s
    ",
    entree,
    sortie))

  tictoc::tic() # 70 s
  DBI::dbExecute(con, "EXECUTE copy_spatial_data(getvariable('outpath'));")
  tictoc::toc()
  cli_process_done()

  # Nettoyage de la requête préparée
  DBI::dbExecute(con, "DEALLOCATE copy_spatial_data;")

  cli::cli_alert_success("Le fichier spatial Parquet a été généré avec succès !")
}
