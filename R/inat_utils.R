## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Définition de fonction supplémentaire pour exécuter les scripts
##
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
# date création: 2025-08-31
# auteur: Marc-Olivier Beausoleil

## ____________####
## Lisez-moi --------
#   --> Fonctions pour extraire les photos de iNaturalist et pour obtenir
#       que les photos de certaines licences

# https://stackoverflow.com/questions/9543343/plot-a-jpg-image-using-base-graphics-in-r
# message("-> get_jpeg() : Utilisation déconseillé")
# #' Download image and plot it
# #'
# #' @param path URL of an iNaturalist image
# #' @param plot logical. TRUE will plot the image
# #' @param ... Other arguments passed to `plot()`
# #'
# #' @description
# #' Downloads an image from a URL (e.g., rinat jpeg URL) and plot it.
# #'
# #' @returns
# #' DEPRECATED
# #' @export
# #'
# #' @examples
# #' DEPRECATED
# get_jpeg = function(path, plot=TRUE, ...)
# {
#   tmp_f = base::tempfile(pattern = 'image_inat', fileext = '.jpg')
#   utils::download.file(url = path, destfile = tmp_f)
#
#   # Add plot if plot==TRUE
#   if (plot) {
#     require('jpeg')
#     jpg = jpeg::readJPEG(tmp_f, native=T) # read the file
#     res = dim(jpg)[2:1] # get the resolution, [x, y]
#     plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),
#          asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',
#          yaxt='n',xlab='',ylab='',bty='n', ...)
#     graphics::rasterImage(jpg,1,1,res[1],res[2])
#   }
# }

#' Tentative de connection à iNaturaliste pour extraire l'information sur
#' les observation d'espèces
#'
#' @description
#' L'utilisation de l'API d'iNaturalist, pour extraire des informations sur
#' les observations peut parfois causer des erreurs. Dans ces cas, cette
#' fonction prévient l'arrêt d'exécution en prenant en charge cette erreur.
#'
#' @param nom_latin Nom latin d'une espèce
#' @param ... Autres arguments de la fonction \code{rinat::get_inat_obs()}
#' @param verbose Logique, pour afficher le nom de l'espèce
#'  traitée (Default TRUE)
#'
#' @returns Sortie de la fonction \code{rinat::get_inat_obs()} provenant
#' d'iNaturalist pour obtenir un tableau des observations.
#'
#' @export
#'
#' @importFrom rinat get_inat_obs
#'
#' @examples
#' \dontrun{
#' sp.out <- iNatTry(
#'   nom_latin = "Anaxyrus americanus",
#'   year = 2025,
#'   photo_license = "CC0",
#'   place_id = 6712,  # Canada
#'   maxresults = 50
#' )
#' head(sp.out)
#' }
iNatTry <- function(nom_latin, ..., verbose = TRUE) {
  # Obtenir l'ID d'une espèce en fonction du nom latin
  sp_id = get_inat_taxon_id(nom_latin)$id

  # Tentative de connection
  tryCatch(
    {
      # Extraction du tableau d'observation iNaturalist pour
      # une espèce à partir de l'ID
      sp_obs_tab_cc0 = rinat::get_inat_obs(
        taxon_name  = nom_latin,
        taxon_id = sp_id,
        ...
      )
    },
    # Message d'erreur
    error = function(cond) {
      message(conditionMessage(cond))
      nom_latin
    },
    # indication finale
    finally = {
      if (verbose) {
        message(sprintf("🔎 Espèce traitée : %s, ID %s", nom_latin, sp_id))
      }
    }
  )
}

#' Extraction de l'ID d'un taxon à partir d'un nom scientifique
#' avec l'API d'iNaturalist
#'
#' @param scientific_name 'Character'. Nom scientifique
#' (genre + épithète spécifique)
#'
#' @description
#' Requête à l'API \code{(v1)} d'iNaturalist pour obtenir l'ID du taxon et
#' l'URL.
#'
#' @details
#' Les valeurs rapportées proviennent de l'API d'iNaturalist. Si une
#' correspondance n'est pas exacte, un avertissement est donnée.
#'
#' @returns
#' Data.frame avec le résultat de la requête
#'
#' @export
#' @importFrom httr2 request req_url_query req_perform resp_body_string
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' get_inat_taxon_id(scientific_name = "Poecile atricapillus")
get_inat_taxon_id <- function(scientific_name) {

  # Nécessite certain progiciels en mémoire
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Package 'httr2' needed.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' needed.")

  # Variables pour l'URL de l'API iNaturalist
  # Construire l'URL pour une
  API_URL = "https://api.inaturalist.org/v1/"
  # Recherche par taxon (espèce)
  API_taxa = paste0(API_URL, "taxa")

  # Niveau taxonomique de la recherche
  niveau_taxonomique = "species"
  # Pages à retourner de l'API
  pagination = "200"
  # Espèce avec nom active (pas renoomé, regroupé dans un autre taxon)
  taxon_active = "true"

  req <- httr2::request(
    base_url = API_taxa
  ) |>
    # Parametères de requête de l'API d'iNaturalist
    httr2::req_url_query(
      q = scientific_name, # httr2::req_url_query corrige les espaces dans nom
      rank = niveau_taxonomique,
      per_page = pagination,
      is_active = taxon_active
    )
  # Obtenir la réponse
  data <- req |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    jsonlite::fromJSON()

  if (length(data$results) > 0) {
    # Match the exact scientific name to avoid synonyms/homonyms
    match <- data$results[data$results$name == scientific_name, ]
    if (nrow(match) > 0) {
      return(
        match
      )
    } else {
      warning("Pas de correspondance exacte!")
      no_exact_match = data$results # Fallback to first result
      return(

        list(
          obs_cnt = no_exact_match$observations_count,
          name_comm = no_exact_match$preferred_common_name,
          name_sci = no_exact_match$name,
          id = no_exact_match$id[1],
          url = sprintf("https://www.inaturalist.org/taxa/%s", no_exact_match$id),
          photo_lic = no_exact_match$default_photo$license_code,
          photo_att = no_exact_match$default_photo$attribution,
          photo_att_nname = no_exact_match$default_photo$attribution_name,
          photo_url = no_exact_match$default_photo$medium_url,
          wiki_url = no_exact_match$wikipedia_url
        )
      )
    }
  }
  if (length(data$results) == 0) {
    warning("Pas de résultat pour la requête...")
  }
  return(NA)
}



#' Obtenir le nom français d'un taxon depuis l'API d'iNaturalist
#'
#' @param taxon_id Integer. ID d'un taxon iNaturalist.
#' @param collapse_char Caractère de séparation quand plusieurs
#' noms sont présents dans la sortie (Default: ";")
#' @param lang_filter Langue pour filtrer les données (Default: "french")
#' @returns Character. Nom français ou NA si absent.
#' Si multiple noms retournés, les mots sont séparés par des "\code{;}"
#' @export
#' @importFrom httr2 request req_url_query resp_header
#' req_headers req_retry req_perform resp_body_json
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as_tibble filter pull
#' @importFrom rlang .data
#' @examples
#' # id_crapaud = get_inat_taxon_id(scientific_name="Anaxyrus americanus")$id
#' # id_orignal = get_inat_taxon_id(scientific_name="Alces alces")$id
#' inat_nom_langue(64968) # Crapaud
#' inat_nom_langue(522193) # Orignal, exemple avec \code{;}
inat_nom_langue <- function(
    taxon_id, collapse_char = ";", lang_filter = "french") {
  if (!requireNamespace("httr2", quietly = TRUE)) stop("'httr2' requis.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("'jsonlite' requis.")

  base_url = "https://www.inaturalist.org/taxon_names.json"

  req <- httr2::request(
    base_url = base_url) |>
    httr2::req_url_query(
      taxon_id = taxon_id,
      per_page = 200
    ) |>
    httr2::req_headers("Accept" = "application/json") |>
    # INDUSTRY STANDARD: Automatically retry on 429, respecting Retry-After!
    httr2::req_retry(max_tries = 3, backoff = function(resp) {
      # If 429, look for Retry-After, otherwise wait a default time
      retry_after <- httr2::resp_header(resp, "Retry-After")
      if (!is.null(retry_after)) as.numeric(retry_after) else 5
    })

  # Faire la requête
  resp <- tryCatch(
    httr2::req_perform(req = req),
    httr2_http_429 = function(cnd) {
      # Explication de l'erreur
      stop("Rate limit exceeded consistently. Please try again later.")
    }
  )

  # Extraire les informations du corps JSON dans un data.frame
  parsed_df <- resp |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    dplyr::as_tibble()

  # Si pas de données
  if (nrow(parsed_df) == 0) return(NA_character_)

  # Filtrer l'information
  nom_lange <- parsed_df |>
    dplyr::filter(tolower("lexicon") == lang_filter) |>
    dplyr::pull("name") |>
    stats::na.omit()

  # Retourne les résultats
  if (length(nom_lange) == 0) {
    return(NA_character_)
  } else {
    return(paste0(nom_lange, collapse = collapse_char))
  }

}


get_try_taxon_name <- function(taxon_id, lang = "french", attend) {
  #' Test une fonction et attendre
  #'
  #' @param taxon_id ID d'un taxon d'iNaturalist
  #' @param lang Langue à filtrer dans \code{inat_nom_langue()}
  #'  (Default "french")
  #' @param attend Nombre de seconde à attendre. Permet de faire une pause
  #' pour l'API afin de ne pas le surcharger. Ainsi, il est préférable de ne
  #' pas dépasser 60 requêtes par minute et 10 000 requête par jour.
  #'
  #' @details
  #' Utilise \code{inat_nom_langue()} pour 'tenter' (\code{TryCatch})
  #' lire les données d'iNaturalist.
  #'
  #' @returns
  #' Vecteur avec noms français (séparé par ";")
  #' @export
  #'
  #' @examples
  #' # taxon_id = get_taxon_info(
  #' #              taxon_id = "Castor canadensis",
  #' #              attend = 1)$id
  #' fr_nm = get_try_taxon_name(
  #'   taxon_id = 43794,
  #'   attend = .1
  #' )
  success <- FALSE
  fr_names <- NULL

  # Test
  while (!success) {
    fr_names <- tryCatch(
      expr = {
        # Utilisation de l'API d'iNaturalist
        # pour prendre les noms français des pages d'espèces
        # e.g., https://www.inaturalist.org/taxa/43794-Castor-canadensis
        res <- inat_nom_langue(
          taxon_id = taxon_id,
          lang_filter = "french"
        )

        success <- TRUE  # Si on arrive ici, c'est qu'on a réussi!
        res # Retourne le résultat
      },
      error = function(cond) {
        # Report the error and wait before the next loop iteration
        message(paste("Error detected:", conditionMessage(cond)))
        message(sprintf("Attendre %s secondes avant de réessayer...", attend))
        Sys.sleep(time = attend)
        return(NULL) # Keeps success as FALSE
      } # fin de 'Error'
    )
  } # end while

  # Montre le résultat trouvé
  message(sprintf("%s", fr_names))

  # Retourne la valeur
  return(fr_names)
}


get_taxon_info <- function(nom_latin, attend) {
  #' Obtenir les informations d'un taxon d'iNaturalist
  #'
  #' @description
  #' Information taxon utilisant \code{get_inat_taxon_id()}
  #' dans un \code{tryCatch}. Permet de faire une pause
  #' pour l'API afin de ne pas le surcharger.
  #'
  #' @param nom_latin Nom latin d'une espèce
  #' @param attend Nombre de seconde à attendre. Permet de faire une pause
  #' pour l'API afin de ne pas le surcharger. Ainsi, il est préférable de ne
  #' pas dépasser 60 requêtes par minute et 10 000 requête par jour
  #'
  #' @returns Sortie de la fonction get_inat_taxon_id s'il n'y a pas d'erreur.
  #' @export
  #' @examples
  #' get_taxon_info(
  #'   nom_latin = "Castor canadensis",
  #'   attend = 1
  #'   )
  success <- FALSE
  sptmp <- NULL

  # Retry if there is an error
  while (!success) {
    sptmp <- tryCatch(
      {
        # Utilisation de l'API d'iNaturalist
        # pour prendre les noms français des pages d'espèces
        # e.g., https://www.inaturalist.org/taxa/43794-Castor-canadensis
        res <- get_inat_taxon_id(
          scientific_name = nom_latin
        )

        success <- TRUE  # Si on arrive ici, c'est qu'on a réussi!
        res # Retourne le résultat
      },
      error = function(cond) {
        # Report the error and wait before the next loop iteration
        message(paste("Error detected:", conditionMessage(cond)))
        message(sprintf("Attendre %s secondes avant de réessayer...", attend))
        Sys.sleep(time = attend)
        return(NULL) # Keeps success as FALSE
      } # fin de 'Error'
    )
  } # end while

  # Retourne la valeur
  return(sptmp)
}
