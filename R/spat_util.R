#' Convertir CRS des points (spatiaux) x et y
#'
#' @description Transforme les valeurs longitude et latitude
#' d'un CRS vers un autre CRS.
#'
#' @param x Numérique. Longitude (ou coordonnée X).
#' @param y Numérique. Latitude (ou coordonnée Y).
#' @param crs_to CRS final (code EPSG numérique ou objet CRS).
#' @param crs_from CRS original (code EPSG numérique ou objet CRS). Par défaut `4326` (WGS84).
#'
#' @importFrom sf st_point st_sfc st_transform st_crs st_coordinates
#' @export
#'
#' @returns Une matrice (ou vecteur) contenant les coordonnées converties `X` et `Y`.
#' @examples
#' xy_convert(
#'   x = -73.58705319777413,
#'   y = 45.50351229089908,
#'   crs_to = 32198,
#'   crs_from = 4326
#' )
xy_convert <- function(x, y, crs_to, crs_from = 4326) {

  # 1. Création du point et transformation
  pts <- sf::st_point(c(x, y)) |>
    sf::st_sfc(crs = crs_from) |>
    sf::st_transform(crs = sf::st_crs(crs_to)) |>
    sf::st_coordinates()

  return(pts)
}
