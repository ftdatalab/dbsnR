# ============================================================
# DBSN SOURCES
# ============================================================


#' Discover DBSN data sources
#'
#' Reads the official IGM DBSN download page and discovers
#' the currently available provincial ZIP archives.
#'
#' @param verify_ssl Logical. If `TRUE`, verifies the SSL/TLS
#'   certificate of the IGM server. Setting this to `FALSE`
#'   disables certificate verification and should only be used
#'   temporarily when the official server has certificate issues.
#'
#' @return A data frame containing area code, province name,
#'   version, archive name and download URL.
#'
#' @export
dbsn_sources <- function(verify_ssl = TRUE) {

  if (!is.logical(verify_ssl) || length(verify_ssl) != 1L ||
      is.na(verify_ssl)) {
    stop(
      "`verify_ssl` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  page_url <- paste0(
    "https://igmi.esercito.difesa.it/servizi/",
    "database-di-sintesi-nazionale/",
    "database-di-sintesi-nazionale-download/"
  )

  handle <- curl::new_handle()

  if (!verify_ssl) {

    warning(
      paste0(
        "SSL/TLS certificate verification is disabled. ",
        "Use `verify_ssl = FALSE` only temporarily."
      ),
      call. = FALSE
    )

    curl::handle_setopt(
      handle,
      ssl_verifypeer = FALSE,
      ssl_verifyhost = FALSE
    )
  }

  response <- tryCatch(
    curl::curl_fetch_memory(
      page_url,
      handle = handle
    ),
    error = function(e) {

      stop(
        paste0(
          "Unable to access the official IGM DBSN download page.",
          "\n\n",
          "Original error:",
          "\n",
          conditionMessage(e),
          "\n\n",
          "If the IGM server currently has an invalid TLS ",
          "certificate, you can temporarily retry with:",
          "\n\n",
          "dbsn_sources(verify_ssl = FALSE)"
        ),
        call. = FALSE
      )
    }
  )

  page <- xml2::read_html(
    response$content
  )

  links <- xml2::xml_find_all(
    page,
    ".//a[@href]"
  )

  href <- xml2::xml_attr(
    links,
    "href"
  )

  label <- trimws(
    xml2::xml_text(links)
  )

  # ----------------------------------------------------------
  # Keep DBSN ZIP archives
  # ----------------------------------------------------------

  keep <- !is.na(href) &
    grepl(
      "/porta-magna/data/dbsn/.*\\.zip(?:\\?.*)?$",
      href,
      ignore.case = TRUE
    )

  href <- href[keep]
  label <- label[keep]

  if (length(href) == 0L) {
    stop(
      paste0(
        "No DBSN ZIP archives were found on the official ",
        "IGM download page."
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Remove query strings before extracting archive names
  # ----------------------------------------------------------

  href_clean <- sub(
    "\\?.*$",
    "",
    href
  )

  archive_name <- basename(
    href_clean
  )

  # ----------------------------------------------------------
  # Keep provincial archives
  #
  # Expected format:
  # RI_dbsn_2025-09-02.zip
  # ----------------------------------------------------------

  provincial <- grepl(
    "^[A-Za-z]{2}_dbsn_[0-9]{4}-[0-9]{2}-[0-9]{2}\\.zip$",
    archive_name,
    ignore.case = TRUE
  )

  href <- href[provincial]
  label <- label[provincial]
  archive_name <- archive_name[provincial]

  if (length(archive_name) == 0L) {
    stop(
      paste0(
        "No provincial DBSN archives were found on the ",
        "official IGM download page."
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Extract area
  # ----------------------------------------------------------

  area <- toupper(
    sub(
      "_dbsn_.*$",
      "",
      archive_name,
      ignore.case = TRUE
    )
  )

  # ----------------------------------------------------------
  # Extract version
  # ----------------------------------------------------------

  version <- sub(
    "^[A-Za-z]{2}_dbsn_",
    "",
    archive_name,
    ignore.case = TRUE
  )

  version <- sub(
    "\\.zip$",
    "",
    version,
    ignore.case = TRUE
  )

  # ----------------------------------------------------------
  # Extract province label
  # ----------------------------------------------------------

  province <- trimws(
    sub(
      "\\s*\\([A-Za-z]{2}\\)\\s*$",
      "",
      label
    )
  )

  # ----------------------------------------------------------
  # Build result
  # ----------------------------------------------------------

  sources <- data.frame(
    area = area,
    province = province,
    version = version,
    archive_name = archive_name,
    url = href,
    stringsAsFactors = FALSE
  )

  # ----------------------------------------------------------
  # If multiple versions of the same province are listed,
  # keep the newest version
  # ----------------------------------------------------------

  sources <- sources[
    order(
      sources$area,
      sources$version,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]

  sources <- sources[
    !duplicated(sources$area),
    ,
    drop = FALSE
  ]

  sources <- sources[
    order(sources$area),
    ,
    drop = FALSE
  ]

  rownames(sources) <- NULL

  sources
}


# ============================================================
# SINGLE DBSN SOURCE
# ============================================================


#' Get a DBSN data source
#'
#' Returns the current official IGM download source for
#' a provincial DBSN dataset.
#'
#' @param area Provincial area code, for example `"RI"` or `"AQ"`.
#' @param verify_ssl Logical. If `TRUE`, verifies the SSL/TLS
#'   certificate of the IGM server.
#'
#' @return One row describing the current DBSN source.
#'
#' @export
dbsn_source <- function(
    area,
    verify_ssl = TRUE
) {

  if (length(area) != 1L || is.na(area)) {
    stop(
      "`area` must contain exactly one value.",
      call. = FALSE
    )
  }

  area <- toupper(
    trimws(area)
  )

  if (!nzchar(area)) {
    stop(
      "`area` cannot be empty.",
      call. = FALSE
    )
  }

  sources <- dbsn_sources(
    verify_ssl = verify_ssl
  )

  result <- sources[
    sources$area == area,
    ,
    drop = FALSE
  ]

  if (nrow(result) == 0L) {
    stop(
      paste0(
        "No DBSN source found for area '",
        area,
        "'."
      ),
      call. = FALSE
    )
  }

  result
}
