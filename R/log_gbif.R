#' Exécuter une requête SQL et imprimer directement dans la sortie active
#'
#' @description
#' Fonction interne ou utilitaire permettant d'exécuter une requête SQL via
#' une connexion DuckDB et d'en afficher le résultat. Si le résultat ne contient
#' qu'une seule valeur unique (1 ligne, 1 colonne), elle est extraite et affichée
#' directement sous forme de texte brut.
#'
#' @param con Connexion DuckDB valide (`DBIConnection`). Si `NULL`, une
#'   connexion temporaire est initialisée via [setup_duckdb()].
#' @param title Caractères. Titre de la section dans la sortie textuelle.
#' @param sql_query Requête SQL sous forme de chaîne de caractères.
#'
#' @return Retourne `NULL` invisiblement, imprime le résultat dans la console ou le `sink()`.
#' @export
#'
#' @seealso Cette fonction est principalement appelée à l'intérieur de [generate_gb_log()]
#'   pour structurer le fichier de rapport.
#'
#' @importFrom DBI dbDisconnect dbGetQuery
#'
#' @examples
#' \dontrun{
#' db_pq <- system.file("extdata", "exemple.parquet", package = "votrePaquet")
#' run_and_log(
#'   con = NULL,
#'   title = "Total rangées :",
#'   sql_query = paste0("SELECT count(*) as n FROM read_parquet('", db_pq, "');")
#' )
#' }
run_and_log <- function(con = NULL, title, sql_query) {
  if (is.null(con)) {
    con <- setup_duckdb()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  base::cat("# ", title, "\n", sep = "")
  res <- DBI::dbGetQuery(con, sql_query)

  # Vérifier si le résultat est une valeur unique (1 ligne, 1 colonne)
  if (base::nrow(res) == 1 && base::ncol(res) == 1) {
    # Extraction propre de la valeur unique [[1]]
    base::cat(res[[1]], "\n")
  } else {
    # Affichage standard pour les structures plus complexes (ex: GROUP BY)
    base::print.data.frame(res, row.names = FALSE)
  }

  base::cat("\n")
  base::invisible(NULL)
}

#' Générer un fichier sommaire de statistiques sur les données GBIF
#'
#' @description
#' Génère un fichier texte (.log) contenant des statistiques descriptives et des
#' décomptes de lignes basés sur des filtres spécifiques appliqués à un fichier Parquet GBIF.
#'
#' @param con Connexion DuckDB (duckdb_connection) valide (`DBIConnection`). Si `NULL`, une
#'   connexion temporaire est initialisée via [setup_duckdb()].
#' @param path_in Chemin d'accès (Character) vers le fichier Parquet.
#' @param path_log Chaîne de caractères. Chemin d'accès et nom du fichier de sortie (ex: `summary.log`).
#'
#' @return Chaîne de caractères (`character`) contenant le chemin du fichier log, de manière invisible.
#' @export
#'
#' @importFrom DBI dbDisconnect dbExecute
#' @importFrom dplyr tbl
#' @importFrom dbplyr sql remote_query
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' paths <- file.path(
#'   "data/raw/biodiv/gbif_data/gbif_raw_new.parquet")
#' # Fichier temporaire
#' tmp_log_file = tempfile(fileext = ".log")
#' # Génère le fichier ".log"
#' generate_gbif_log(path_in = paths, path_log = tmp_log_file)
#' # Lire le fichier dans la console R
#' cat(readLines(tmp_log_file), sep = "\n")
#' }
generate_gbif_log <- function(
  con = NULL,
  path_in,
  path_log = "gb_file_summary.log"
) {
  if (is.null(con)) {
    con <- setup_duckdb()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  # Ouvrir le sink de manière sécuritaire avec on.exit
  base::sink(path_log)

  on.exit(
    expr = base::sink(),
    add = TRUE
  )

  base::cat("=========================================================\n")
  base::cat(
    "Sommaire des données GBIF avec quelques filtres\n",
    "Date : ",
    base::as.character(base::Sys.time()),
    "\n",
    glue::glue("Fichier entrée : '{path_in}'"),
    "\n",
    sep = ""
  )
  base::cat("=========================================================\n\n")

  # Désactiver la barre de progression de DuckDB
  DBI::dbExecute(
    conn = con,
    statement = "SET enable_progress_bar = false;"
  )

  # Établir le pointeur de table de base
  gb_tbl <- dplyr::tbl(
    src = con,
    from = dbplyr::sql(
      glue::glue(
        "SELECT * FROM read_parquet('{path_in}')"
      )
    )
  )

  # Extraire la requête source compilée
  db_source <- dbplyr::remote_query(
    x = gb_tbl
  )

  # Exécution des requêtes métriques
  run_and_log(
    con = con,
    title = "Total des rangées dans le fichier :",
    sql_query = glue::glue(
      "SELECT count(*) as n
      FROM ({db_source});"
    )
  )

  run_and_log(
    con = con,
    title = "countryCode = 'CA':",
    sql_query = glue::glue(
      "SELECT count(*) as n
      FROM ({db_source})
      WHERE countrycode = 'CA';"
    )
  )

  run_and_log(
    con = con,
    title = "stateProvince == Quebec, Québec, Qc & Canada:",
    sql_query = glue::glue(
      "SELECT count(*) as n
      FROM ({db_source})
      WHERE
       stateprovince IN ('Quebec', 'Québec', 'Qc')
       AND countrycode = 'CA';"
    )
  )

  run_and_log(
    con = con,
    title = "stateProvince == Quebec, Québec, Qc or stateProvince IS NULL:",
    sql_query = glue::glue(
      "SELECT count(*) as n
      FROM ({db_source})
      WHERE
       stateprovince IN ('Quebec', 'Québec', 'Qc')
       OR stateprovince IS NULL;"
    )
  )

  run_and_log(
    con = con,
    title = "Sommaire basisOfRecord:",
    sql_query = glue::glue(
      "SELECT basisofrecord, count(basisofrecord) as n
       FROM ({db_source})
       GROUP BY basisofrecord;"
    )
  )

  # CORRECTION : L'appel original manquait l'argument 'con' ici
  run_and_log(
    con = con,
    title = "Sommaire taxonRank:",
    sql_query = glue::glue(
      "
      SELECT taxonrank, count(taxonrank) as n
      FROM ({db_source})
      GROUP BY all ORDER BY n DESC;"
    )
  )

  run_and_log(
    con = con,
    title = "Sommaire kingdom:",
    sql_query = glue::glue(
      "SELECT kingdom, count(kingdom) as n
       FROM ({db_source})
       GROUP BY all
       ORDER BY n DESC;"
    )
  )

  run_and_log(
    con = con,
    title = "Compte de Species non-null ou non-NA:",
    sql_query = glue::glue(
      "SELECT count(species) as n
       FROM ({db_source});"
    )
  )

  run_and_log(
    con = con,
    title = "Filtres taxonrank, kingdom, coordinateUncertaintyInMeters (<= 200):",
    sql_query = glue::glue(
      "SELECT count(*) as n
       FROM ({db_source})
       WHERE
         taxonrank IN ('SPECIES', 'SUBSPECIES', 'VARIETY')
         AND
         kingdom IN ('Chromista', 'Fungi', 'Plantae', 'Animalia')
         AND
         (coordinateUncertaintyInMeters <= 200
         OR
         coordinateUncertaintyInMeters IS NULL);"
    )
  )

  base::invisible(path_log)
}
