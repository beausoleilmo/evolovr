#' @importFrom utils globalVariables
#' @keywords internal
NULL

# Silence les notes de R CMD check pour les fonctions de requêtes dbplyr/SQL
utils::globalVariables(
  names = c(
    "ST_Transform",
    "h3_polygon_wkt_to_cells",
    "ST_AsText"
  )
)

# Colonnes qui sont passées dans dplyr
utils::globalVariables(
  names = c(
    "species", "basisOfRecord",
    "countryCode", "stateProvince",
    "taxonRank", "kingdom",
    "coordinateUncertaintyInMeters", "geometry"
  )
)
