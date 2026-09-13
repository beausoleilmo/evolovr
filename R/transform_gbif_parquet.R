#' Transforme données GBIF brutes en fichier spatial PARQUET
#' @md
#'
#' @description
#' À partir du téléchargement des données de GBIF en au
#' format `SIMPLE_PARQUET` (dossier avec fichiers nommé d'une série de
#' chiffre comme "003997"),
#' transforme un répertoire de données brutes GBIF en un unique fichier :
#' - Parquet compressé et non partitionné
#' - colonne camelCase.
#' - colonne géométrique géospatiale (`geometry`).
#'
#' @param entree Chemin d'accès (Character) vers le répertoire contenant les
#'   fichiers `occurrence.parquet` du GBIF.
#' @param sortie Chemin d'accès (Character) du nouveau fichier Parquet
#' e.g., `donnees/gbif_raw_new.parquet`
#' @param compression Type de compression pour `COPY() TO (FORMAT PARQUET)`.
#' (Défaut : "zstd")
#' @param compression_level Niveau de compression (Défaut : 4)
#'
#' @details
#' L'utilisation de la compression ZSTD (`COMPRESSION 'zstd'`) et
#' le niveau 4  (`COMPRESSION_LEVEL 4`) permet d'optimiser
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
#'   cli_progress_message cli_ul cli_li cli_end cli_alert_success
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
transforme_gbif <- function(
  entree,
  sortie,
  compression = "zstd",
  compression_level = 4
) {
  # Vérifications
  if (!dir.exists(entree)) {
    cli::cli_abort("Le répertoire d'entrée {.path {entree}} n'existe pas.")
  }

  if (!grepl("\\.parquet$", sortie, ignore.case = TRUE)) {
    cli::cli_warn(
      message = "Le fichier de sortie {.path {sortie}} n'a pas l'extension '.parquet'."
    )
  }

  # Création du dossier du fichier de sortie
  dir.create(
    path = dirname(sortie),
    showWarnings = FALSE,
    recursive = TRUE
  )

  # Initialise la connexion DuckDB (en mémoire)
  con <- DBI::dbConnect(
    drv = duckdb::duckdb()
  )

  # Assurer la déconnexion et la fermeture propre de la DB en cas d'erreur
  on.exit(
    {
      if (DBI::dbIsValid(con)) {
        # Fermer la connexion duckdb
        DBI::dbDisconnect(con, shutdown = TRUE)
      }
    },
    add = TRUE
  )

  # Installation et chargement d'extension spatiale
  DBI::dbExecute(con, "INSTALL spatial; LOAD spatial;")

  # Définir des variables DuckDB
  # Fichier d'entrée
  DBI::dbExecute(
    conn = con,
    statement = glue::glue_sql(
      "SET VARIABLE gb_path_pq = {entree};",
      .con = con
    )
  )

  # Fichier de sortie
  DBI::dbExecute(
    conn = con,
    statement = glue::glue_sql("SET VARIABLE outpath = {sortie};", .con = con)
  )

  # Filtrer les fichiers dans 'gb_files' pour exclure 000000 (vide = 0 byte) et
  # fichier commançants par '._'
  DBI::dbExecute(
    con,
    statement = "
  SET variable gb_files = (
    SELECT list(file)
    FROM glob(getvariable('gb_path_pq') || '/*')
    WHERE file NOT LIKE '%/000000'
    AND file NOT LIKE '%/._%'
  );
"
  )

  # Récupération du nombre de fichiers (length de la liste)
  # Note : 'length()' sur une liste DuckDB retourne le nombre d'éléments.
  nb_fichiers <- DBI::dbGetQuery(
    conn = con,
    "SELECT length(getvariable('gb_files')) AS total;"
  )

  cli::cli_alert_info(
    text = "Nombre de fichiers à traiter du dossier .parquet : {nb_fichiers$total}"
  )

  if (is.na(nb_fichiers$total) || nb_fichiers$total == 0) {
    cli::cli_abort(
      "Aucun fichier valide à traiter dans le répertoire source."
    )
  }
  # En duckdb, si on regarde un fichier au hasard, on voit les noms en
  # minuscule seulement ce qui ne respecte pas le DarwinCore.
  # from read_parquet('0010290-260519110011954/occurrence.parquet/003975')
  # limit 10;
  # Dictionnaire de traduction (Format : "nom_origine" = "NomCamelCase")
  # pour respecter le DarwinCore
  # Mettez-y uniquement les colonnes qui changent de nom.
  dwc_mapping <- c(
    gbifid = "gbifID",
    datasetkey = "datasetKey",
    occurrenceid = "occurrenceID",
    publishingorgkey = "publishingOrgKey",
    rightsholder = "rightsHolder",
    lastinterpreted = "lastInterpreted",
    "\"order\"" = "order", # Échappement propre pour le mot-clé SQL
    infraspecificepithet = "infraspecificEpithet",
    taxonrank = "taxonRank",
    scientificname = "scientificName",
    verbatimscientificname = "verbatimScientificName",
    verbatimscientificnameauthorship = "verbatimScientificNameAuthorship",
    taxonkey = "taxonKey",
    specieskey = "speciesKey",
    countrycode = "countryCode",
    stateprovince = "stateProvince",
    decimallatitude = "decimalLatitude",
    decimallongitude = "decimalLongitude",
    coordinateuncertaintyinmeters = "coordinateUncertaintyInMeters",
    coordinateprecision = "coordinatePrecision",
    elevationaccuracy = "elevationAccuracy",
    depthaccuracy = "depthAccuracy",
    occurrencestatus = "occurrenceStatus",
    individualcount = "individualCount",
    basisofrecord = "basisOfRecord",
    establishmentmeans = "establishmentMeans",
    eventdate = "eventDate",
    institutioncode = "institutionCode",
    collectioncode = "collectionCode",
    catalognumber = "catalogNumber",
    recordnumber = "recordNumber",
    recordedby = "recordedBy",
    identifiedby = "identifiedBy",
    dateidentified = "dateIdentified",
    typestatus = "typeStatus",
    mediatype = "mediaType"
  )

  # Les colonnes qui n'ont pas besoin d'alias 'AS'
  unchanged_cols <- c(
    "license",
    "issue",
    "kingdom",
    "phylum",
    "class",
    "family",
    "genus",
    "species",
    "locality",
    "elevation",
    "depth",
    "day",
    "month",
    "year"
  )

  # Construire dynamiquement les morceaux du SELECT
  aliased_fields <- paste(
    names(dwc_mapping),
    "AS",
    dwc_mapping,
    collapse = ",\n    "
  )
  simple_fields <- paste(unchanged_cols, collapse = ",\n    ")

  # PREPARE : prépare une requête SQL qui sera exécuté
  # en remplaçant le ? par un paramètre qu'on passe (e.g., le nom d'un
  # fichier de sortie!).
  # Voir la commande "EXECUTE" plus bas.
  # Requête SQL avec colonnes renomées
  sql_query <- glue_sql(
    "
  PREPARE copy_spatial_data AS
  COPY (
    SELECT
      {aliased_fields_sql},
      {simple_fields_sql},
      ST_Point(decimallongitude, decimallatitude) AS geometry
    FROM
      read_parquet(getvariable('gb_files'))
  ) TO ? (FORMAT parquet, COMPRESSION {compression}, COMPRESSION_LEVEL {compression_level});
",
    .con = con
  )

  # Préparation et exécution de la copie Spatiale
  DBI::dbExecute(
    conn = con,
    statement = sql_query
  )

  on.exit(
    {
      try(DBI::dbExecute(con, "DEALLOCATE copy_spatial_data;"), silent = TRUE)
    },
    add = TRUE
  )

  cli::cli_progress_message("Transformation des données GBIF en cours...")
  cli::cli_bullets(
    c(
      "*" = "Source : {.path {entree}}",
      "*" = "Destination : {.path {sortie}}"
    )
  )

  tictoc::tic("Pipeline de transformation") # 70 s
  DBI::dbExecute(con, "EXECUTE copy_spatial_data(getvariable('outpath'));")
  tictoc::toc()

  # Nettoyage de la requête préparée
  DBI::dbExecute(con, "DEALLOCATE copy_spatial_data;")

  cli::cli_alert_success(
    "Le fichier spatial Parquet a été généré avec succès !"
  )
}
