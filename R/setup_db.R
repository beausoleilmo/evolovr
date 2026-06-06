#' Initialisation du 'pilote' ou 'moteur' duckdb
#'
#' @description
#' Démarrer une session duckdb en mémoire avec un environnement avec
#' des extensions spatiales (spatial + h3)
#' @param drv 'Pilote' pour gestionnaire de BD \code{duckdb::duckdb()}
#' @param dbdir Localisation de la BD \code{":memory:"} ou \code{XXX.duckdb}
#' @param extensions Vecteur de caractères. Liste des extensions
#' officielles à installer/charger.
#' @param community_extensions Vecteur de caractères. Liste des
#' extensions de la communauté à installer/charger.
#'
#' @details
#' Permet de créer un 'environnement' duckdb prêt à l'analyse avec extensions
#' utile pour faire des requêtes spatiales
#' @export
#' @returns Connection duckdb
#' @examples
#' \dontrun{
#' require(DBI)
#' con = setup_duckdb() # Exporte con dbdir=':memory:'
#'
#' # Exemple de requête dans l'environnement duckdb (et voir les extensions)
#' DBI::dbGetQuery(
#'   conn = con,
#'   statement = "
#'   SELECT
#'     extension_name, installed, loaded, description
#'   FROM
#'     duckdb_extensions()
#'   WHERE
#'     installed = true;" # Filtre ce qui est installé
#' )
#' }
setup_duckdb <- function(
    drv = duckdb::duckdb(),
    dbdir = ":memory:",
    extensions = c("spatial"),
    community_extensions = c("h3")
) {
  # Connection avec duckdb (mémoire)
  con <- DBI::dbConnect(drv = drv, dbdir)

  # Install et charge les extensions duckdb
  # Chargement des extensions standards
  if (!is.null(extensions) && length(extensions) > 0) {
    lapply(extensions, function(ext) {
      query <- sprintf("INSTALL %s; LOAD %s;", ext, ext)
      DBI::dbExecute(conn = con, statement = query)
    })
  }
  # Chargement des extensions 'FROM community'
  if (!is.null(community_extensions) && length(community_extensions) > 0) {
    lapply(community_extensions, function(ext) {
      query <- sprintf("INSTALL %s FROM community; LOAD %s;", ext, ext)
      DBI::dbExecute(conn = con, statement = query)
    })
  }
  # Configuration spécifique à l'extension spatiale (si elle est chargée)
  if ("spatial" %in% extensions) {
    DBI::dbExecute(conn = con, statement = "SET geometry_always_xy = true;")
  }

  # Retourne la connection pour réutiliser
  return(con)
}


#' Ferme le 'pilote' ou 'moteur' duckdb
#'
#' @param con Connection
#'
#' @description
#' Ferme une session duckdb en mémoire.
#'
#' @export
#' @returns NULL
#' @examples
#' \dontrun{
#' con = setup_duckdb() # Exporte con dbdir=':memory:'
#' discocon(con)
#' }
discon_duckdb <- function(con) {
  # Ferme duckdb (mémoire)
  DBI::dbDisconnect(
    conn = con
  )
}
