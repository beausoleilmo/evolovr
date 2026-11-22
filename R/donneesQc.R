# Fonctions pour obtenir des données du portail de Données Québec
# Fait avec l'aide de Gemini.

#' Build a CKAN Action API Resource URL
#'
#' @description
#' Constructs a CKAN Action API endpoint URL (such as `resource_show`) for a
#' given resource identifier on Données Québec or another CKAN-compatible portal.
#'
#' @param resource_id A single non-empty character string identifying the CKAN
#'   resource.
#' @param base_url A single non-empty character string giving the base CKAN
#'   Action API URL. Defaults to
#'   `"https://www.donneesquebec.ca/recherche/api/3/action"`.
#' @param action A single non-empty character string naming the CKAN API action.
#'   Defaults to `"resource_show"`.
#'
#' @return A single character string containing the encoded CKAN API URL.
#'
#' @examples
#' build_ckan_resource_url("3448b141-f236-4c1b-add8-fe59109ac4f7")
#'
#' @export
#' @md
build_ckan_resource_url <- function(
  resource_id,
  base_url = "https://www.donneesquebec.ca/recherche/api/3/action",
  action = "resource_show"
) {
  assert_non_empty_string(resource_id, "resource_id")
  assert_non_empty_string(base_url, "base_url")
  assert_non_empty_string(action, "action")

  base_clean <- sub("/+$", "", trimws(base_url))
  action_clean <- sub("^/+|/+$", "", trimws(action))
  id_encoded <- utils::URLencode(trimws(resource_id), reserved = TRUE)

  paste0(base_clean, "/", action_clean, "?id=", id_encoded)
}

#' Download a CKAN Resource File
#'
#' @description
#' Queries a CKAN resource metadata endpoint, validates the response, downloads
#' the underlying resource file to disk, and optionally extracts `.zip`
#' archives.
#'
#' @param api_url A single non-empty character string pointing to a CKAN
#'   `resource_show` URL (for example, produced by [build_ckan_resource_url()]).
#' @param out_dir A single non-empty character string specifying the output
#'   directory for the downloaded file. Defaults to a temporary directory created
#'   with [base::tempfile()].
#' @param unzip A single logical (`TRUE` or `FALSE`) indicating whether `.zip`
#'   archives should be extracted. Defaults to `TRUE`.
#' @param extract_dir Optional single non-empty character string specifying where
#'   `.zip` archives are extracted. Defaults to a temporary directory via
#'   [base::tempfile()] when `unzip = TRUE`, or `NULL` when `unzip = FALSE`.
#' @param user_agent A single non-empty character string sent as the HTTP
#'   `User-Agent` header. Defaults to `"R-donneesquebec-client"`.
#'
#' @return A list (invisibly) containing:
#'   * `out_dir`: Path to the output directory.
#'   * `file_path`: Path to the downloaded file.
#'   * `extract_dir`: Path to the extraction directory (or `NULL` if not unzipped).
#'   * `extracted_files`: Character vector of extracted file paths (empty if not
#'     unzipped or not a `.zip` file).
#'   * `metadata`: Parsed CKAN resource metadata list.
#'
#' @examples
#' \dontrun{
#' api_url <- build_ckan_resource_url("3448b141-f236-4c1b-add8-fe59109ac4f7")
#' res <- download_ckan_resource(api_url, out_dir = "data/raw")
#' if (!is.null(res$extract_dir) && !identical(res$extract_dir, res$out_dir)) {
#'   on.exit(
#'     unlink(res$extract_dir, recursive = TRUE, force = TRUE),
#'     add = TRUE
#'   )
#' }
#' res$extracted_files
#' }
#'
#' @export
#' @md
download_ckan_resource <- function(
  api_url,
  out_dir = tempfile("dq_admin_"),
  unzip = TRUE,
  extract_dir = if (isTRUE(unzip)) tempfile("dq_extract_") else NULL,
  user_agent = "R-donneesquebec-client"
) {
  assert_non_empty_string(api_url, "api_url")
  assert_non_empty_string(out_dir, "out_dir")
  assert_non_empty_string(user_agent, "user_agent")
  assert_flag(unzip, "unzip")
  if (!is.null(extract_dir)) {
    assert_non_empty_string(extract_dir, "extract_dir")
  }

  meta <- perform_ckan_get(api_url = api_url, user_agent = user_agent)

  if (!isTRUE(meta$success)) {
    cli::cli_abort("CKAN API request failed for {.url {api_url}}.")
  }

  res_info <- meta$result
  download_url <- res_info$url

  if (
    !is.character(download_url) ||
      length(download_url) != 1L ||
      !nzchar(trimws(download_url))
  ) {
    cli::cli_abort(
      "CKAN metadata for {.url {api_url}} did not include a valid download URL."
    )
  }

  resource_label <- res_info$name %||% "unnamed resource"
  resource_fmt <- res_info$format %||% "unknown format"
  cli::cli_inform("Fetching resource:\n{resource_label} ({resource_fmt}).")

  tictoc::tic("Download time: ")
  # Create output directory if it doesn't exist yet (works for temp and user folders)
  dir_created <- dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir_created && !dir.exists(out_dir)) {
    cli::cli_abort("Could not create output directory at {.path {out_dir}}.")
  }

  archive_path <- resolve_download_path(
    download_url = download_url,
    out_dir = out_dir
  )

  perform_ckan_get(
    api_url = download_url,
    user_agent = user_agent,
    path = archive_path
  )
  tictoc::toc()

  extracted_files <- character(0)
  used_extract_dir <- NULL

  if (isTRUE(unzip) && grepl("\\.zip$", archive_path, ignore.case = TRUE)) {
    used_extract_dir <- extract_dir %||% tempfile("dq_extract_")

    ext_created <- dir.create(
      used_extract_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
    if (!ext_created && !dir.exists(used_extract_dir)) {
      cli::cli_abort(
        "Could not create extraction directory at {.path {used_extract_dir}}."
      )
    }

    extracted_files <- utils::unzip(archive_path, exdir = used_extract_dir)
  }

  invisible(list(
    out_dir = out_dir,
    file_path = archive_path,
    extract_dir = used_extract_dir,
    extracted_files = extracted_files,
    metadata = res_info
  ))
}

#' Fetch the Données Québec Dataset Catalog
#'
#' @description
#' Queries a CKAN `package_search` endpoint in pages and returns a data frame
#' containing dataset identifiers, slugs, titles, and resource counts.
#'
#' @param base_url A single non-empty character string giving the base CKAN
#'   Action API URL. Defaults to
#'   `"https://www.donneesquebec.ca/recherche/api/3/action"`.
#' @param rows A single positive integer specifying how many datasets to fetch
#'   per page. Defaults to `500L`.
#' @param user_agent A single non-empty character string sent as the HTTP
#'   `User-Agent` header. Defaults to `"R-donneesquebec-client"`.
#'
#' @return A `data.frame` with columns `id`, `name`, `title`, and
#'   `n_resources`.
#'
#' @examples
#' \dontrun{
#' catalog <- fetch_dq_catalog()
#' head(catalog, 10)
#' }
#'
#' @export
#' @md
fetch_dq_catalog <- function(
  base_url = "https://www.donneesquebec.ca/recherche/api/3/action",
  rows = 500L,
  user_agent = "R-donneesquebec-client"
) {
  assert_non_empty_string(base_url, "base_url")
  assert_non_empty_string(user_agent, "user_agent")
  rows_int <- assert_positive_int(rows, "rows")

  clean_base <- sub("/+$", "", trimws(base_url))
  start <- 0L
  total_count <- NA_integer_
  pages <- list()

  repeat {
    query_url <- paste0(
      clean_base,
      "/package_search?rows=",
      rows_int,
      "&start=",
      start
    )

    resp <- perform_ckan_get(api_url = query_url, user_agent = user_agent)

    if (!isTRUE(resp$success)) {
      cli::cli_abort("CKAN {.code package_search} failed at offset {start}.")
    }

    result <- resp$result
    if (is.na(total_count)) {
      total_count <- as.integer(result$count %||% 0L)
    }

    batch <- result$results %||% list()
    if (length(batch) == 0L) {
      break
    }

    pages[[length(pages) + 1L]] <- parse_catalog_page(batch)
    start <- start + length(batch)

    if (start >= total_count) {
      break
    }
  }

  if (length(pages) == 0L) {
    return(empty_catalog_df())
  }

  do.call(rbind, pages)
}

# --- Internal Helpers --------------------------------------------------------

#' @noRd
assert_non_empty_string <- function(x, arg_name) {
  if (!is.character(x) || length(x) != 1L || !nzchar(trimws(x))) {
    cli::cli_abort(
      "{.arg {arg_name}} must be a single non-empty character string."
    )
  }
  invisible(x)
}

#' @noRd
assert_flag <- function(x, arg_name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort(
      "{.arg {arg_name}} must be a single non-missing logical ({.code TRUE} or {.code FALSE})."
    )
  }
  invisible(x)
}

#' @noRd
assert_positive_int <- function(x, arg_name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 1 || x %% 1 != 0) {
    cli::cli_abort("{.arg {arg_name}} must be a single positive integer.")
  }
  as.integer(x)
}

#' @noRd
perform_ckan_get <- function(api_url, user_agent, path = NULL) {
  req <- httr2::request(api_url) |>
    httr2::req_user_agent(user_agent)

  if (!is.null(path)) {
    httr2::req_perform(req, path = path)
    return(invisible(path))
  }

  httr2::req_perform(req) |>
    httr2::resp_body_json()
}

#' @noRd
resolve_download_path <- function(download_url, out_dir) {
  # Fall back to a generated filename if the download URL has no basename
  clean_url <- sub("[?#].*$", "", download_url)
  file_name <- basename(clean_url)

  if (
    !nzchar(file_name) ||
      identical(file_name, "/") ||
      identical(file_name, ".")
  ) {
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    file_name <- paste0("resource_", timestamp)
  }

  file.path(out_dir, file_name)
}

#' @noRd
parse_catalog_page <- function(batch) {
  data.frame(
    id = vapply(batch, function(x) x$id %||% NA_character_, character(1)),
    name = vapply(batch, function(x) x$name %||% NA_character_, character(1)),
    title = vapply(batch, function(x) x$title %||% NA_character_, character(1)),
    n_resources = vapply(
      batch,
      function(x) length(x$resources %||% list()),
      integer(1)
    ),
    stringsAsFactors = FALSE
  )
}

#' @noRd
empty_catalog_df <- function() {
  data.frame(
    id = character(0),
    name = character(0),
    title = character(0),
    n_resources = integer(0),
    stringsAsFactors = FALSE
  )
}


#' List Resources for a CKAN Dataset
#'
#' @description
#' Queries a CKAN `package_show` endpoint using a dataset identifier or slug and
#' returns a data frame of its associated resources, including resource IDs,
#' names, formats, and URLs.
#'
#' @param dataset_id_or_name A single non-empty character string specifying the
#'   dataset `id` (for example, from [fetch_dq_catalog()]) or dataset slug
#'   (`name`).
#' @param base_url A single non-empty character string giving the base CKAN
#'   Action API URL. Defaults to
#'   `"https://www.donneesquebec.ca/recherche/api/3/action"`.
#' @param user_agent A single non-empty character string sent as the HTTP
#'   `User-Agent` header. Defaults to `"R-donneesquebec-client"`.
#'
#' @return A `data.frame` with columns `resource_id`, `name`, `format`, and
#'   `url`.
#'
#' @examples
#' \dontrun{
#' res_tbl <- fetch_dataset_resources("decoupages-administratifs")
#' head(res_tbl)
#' }
#'
#' @export
#' @md
fetch_dataset_resources <- function(
  dataset_id_or_name,
  base_url = "https://www.donneesquebec.ca/recherche/api/3/action",
  user_agent = "R-donneesquebec-client"
) {
  assert_non_empty_string(dataset_id_or_name, "dataset_id_or_name")
  assert_non_empty_string(base_url, "base_url")
  assert_non_empty_string(user_agent, "user_agent")

  clean_base <- sub("/+$", "", trimws(base_url))
  encoded_id <- utils::URLencode(trimws(dataset_id_or_name), reserved = TRUE)
  query_url <- paste0(clean_base, "/package_show?id=", encoded_id)

  resp <- perform_ckan_get(api_url = query_url, user_agent = user_agent)

  if (!isTRUE(resp$success)) {
    cli::cli_abort(
      "CKAN {.code package_show} failed for {.val {dataset_id_or_name}}."
    )
  }

  resources <- resp$result$resources %||% list()
  if (length(resources) == 0L) {
    return(empty_resources_df())
  }

  parse_resources_list(resources)
}

# --- Internal Helpers --------------------------------------------------------

#' @noRd
parse_resources_list <- function(resources) {
  data.frame(
    resource_id = vapply(
      resources,
      function(r) r$id %||% NA_character_,
      character(1)
    ),
    name = vapply(
      resources,
      function(r) r$name %||% NA_character_,
      character(1)
    ),
    format = vapply(
      resources,
      function(r) r$format %||% NA_character_,
      character(1)
    ),
    url = vapply(
      resources,
      function(r) r$url %||% NA_character_,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

#' @noRd
empty_resources_df <- function() {
  data.frame(
    resource_id = character(0),
    name = character(0),
    format = character(0),
    url = character(0),
    stringsAsFactors = FALSE
  )
}
