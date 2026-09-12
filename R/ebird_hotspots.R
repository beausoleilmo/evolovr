#' Télécharge les hotspots eBirds du Québec
#'
#' @description
#' Téléchargement de la liste des hotspots eBird avec une clé API de eBird (nécesssite un compte eBird et la clé accessible
#' au https://ebird.org/data/download). La clé API d'eBird doit être mis dans
#' le .Renviron avec le nom `EBIRD_API_KEY="<VOTRE_CLE_API>"`.
#'
#' @param export_path Chemin d'accès (Défaut : ".", emplacement actuel) pour
#' déposer les données téléchargées.
#'
#' @return
#' Données téléchargées
#'
#' @importFrom httr2 request req_headers req_url_query req_perform
#'
#' @examples
#' \dontrun{
#'   download_ebird_hotspots()
#' }
#'
#' @export
download_ebird_hotspots <- function(export_path = ".") {
  readRenviron(normalizePath("~/.Renviron"))
  api_key <- Sys.getenv("EBIRD_API_KEY")

  if (api_key == "") {
    stop("EBIRD_API_KEY environment variable n'est pas dans .Renviron")
  }

  # Ensure target directory exists
  if (!dir.exists(export_path)) {
    dir.create(export_path, recursive = TRUE, showWarnings = FALSE)
  }

  output_file <- file.path(
    export_path,
    sprintf("eBird_hotspots_CA_QC_%s.csv", Sys.Date())
  )

  message("Requête à l'API de eBird")
  httr2::request("https://api.ebird.org/v2/ref/hotspot/CA-QC") |>
    httr2::req_headers(`X-eBirdApiToken` = api_key) |>
    httr2::req_url_query(fmt = "csv") |>
    httr2::req_perform(path = output_file)

  message("Téléchargement des hotspots --> \n", output_file)
}
