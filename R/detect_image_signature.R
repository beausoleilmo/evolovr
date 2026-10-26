#' Obtient les extensions des images en ligne de iNaturalist
#'
#' @description Certaines images provenant d'iNaturalist n'ont pas
#' d'extension. Il faut inférer l'extension à partir de la signature du
#' fichiers directement ('bytes' de signatures).
#'
#' @param image_urls URLs (Character) des images à valider l'extension.
#'
#' @author Marc-Olivier Beausoleil
#'
#'
#' @importFrom httr2 request req_headers req_error req_perform_parallel resp_status resp_body_raw
#'
#' @return Vecteur de caractère qui indique l'extension des fichiers pour un
#' ou des liens donnés. Par exemple, la sortie peut être "jpeg".
#'
#' @export
#' @examples
#' # Exécution pour un lien sans extension (jpeg)
#' detect_image_types_fast(
#' "https://inaturalist-open-data.s3.amazonaws.com/photos/4159605/medium.")
#'
#' # Avec plusieurs liens
#' liens_verif <- c(
#' "https://inaturalist-open-data.s3.amazonaws.com/photos/4135520/square.",
#' "https://inaturalist-open-data.s3.amazonaws.com/photos/4159475/square."
#' )
#' detect_image_types_fast(liens_verif)
detect_image_types_fast <- function(image_urls) {
  # Track which values are NA
  is_na_url <- base::is.na(image_urls)

  # Initialize results vector with NA for those positions
  results <- base::character(base::length(image_urls))
  results[is_na_url] <- NA_character_

  # Extract only the valid, non-NA URLs for parallel processing
  valid_indices <- base::which(!is_na_url)
  valid_urls <- image_urls[valid_indices]

  # If all URLs are NA, return immediately
  if (base::length(valid_urls) == 0) {
    return(results)
  }

  # Requête pour URLs valides seulements
  reqs <- base::lapply(valid_urls, function(u) {
    httr2::request(u) |>
      httr2::req_headers(Range = "bytes=0-11") |>
      httr2::req_error(is_error = function(resp) FALSE)
  })

  #  Exécution en parallèle
  resps <- httr2::req_perform_parallel(reqs, on_error = "continue")

  # 4. Parse the results
  valid_results <- base::sapply(
    resps,
    function(resp) {
      if (base::inherits(resp, "error") || httr2::resp_status(resp) >= 400) {
        return("error")
      }

      header <- httr2::resp_body_raw(resp)
      # Vérification du type d'image
      if (
        length(header) >= 3 &&
          base::all(header[1:3] == base::as.raw(c(0xFF, 0xD8, 0xFF)))
      ) {
        return("jpeg")
      } else if (
        length(header) >= 4 &&
          base::all(header[1:4] == base::as.raw(c(0x89, 0x50, 0x4E, 0x47)))
      ) {
        return("png")
      } else if (
        length(header) >= 4 &&
          base::all(header[1:4] == base::as.raw(c(0x47, 0x49, 0x46, 0x38)))
      ) {
        return("gif")
      } else if (
        length(header) >= 12 &&
          base::all(header[9:12] == base::as.raw(c(0x57, 0x45, 0x42, 0x50)))
      ) {
        return("webp")
      } else {
        return("unknown")
      }
    },
    USE.NAMES = FALSE
  )

  # Put the processed results back into their original positions
  results[valid_indices] <- valid_results
  return(results)
}
