# ============================================================
# DBSN DOWNLOAD
# ============================================================


#' Download a DBSN dataset
#'
#' Downloads the current official IGM DBSN archive for a province,
#' extracts it into the local cache and returns the path to the
#' extracted File Geodatabase.
#'
#' @param area Provincial area code, for example `"RI"` or `"AQ"`.
#' @param verify_ssl Logical. If `TRUE`, verifies the SSL/TLS
#'   certificate of the IGM server.
#' @param overwrite Logical. If `TRUE`, downloads and extracts
#'   the dataset again even when it is already available locally.
#'
#' @return A character string containing the path to the `.gdb`.
#'
#' @export
dbsn_download <- function(
    area,
    verify_ssl = TRUE,
    overwrite = FALSE
) {

  area <- toupper(trimws(area))

  if (!nzchar(area)) {
    stop(
      "`area` cannot be empty.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Return existing dataset unless overwrite is requested
  # ----------------------------------------------------------

  dataset_dir <- dbsn_cache_path(
    area,
    create = TRUE
  )

  existing_gdb <- list.dirs(
    dataset_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  existing_gdb <- existing_gdb[
    grepl(
      "\\.gdb$",
      existing_gdb,
      ignore.case = TRUE
    )
  ]

  if (length(existing_gdb) > 0L && !overwrite) {

    gdb_path <- normalizePath(
      existing_gdb[[1]],
      winslash = "/",
      mustWork = TRUE
    )

    message(
      "DBSN dataset '",
      area,
      "' is already available in the local cache:\n",
      gdb_path
    )

    return(gdb_path)
  }

  # ----------------------------------------------------------
  # Discover current official source
  # ----------------------------------------------------------

  source <- dbsn_source(
    area,
    verify_ssl = verify_ssl
  )

  # ----------------------------------------------------------
  # Downloads directory
  # ----------------------------------------------------------

  downloads_dir <- file.path(
    dbsn_cache_dir(),
    "downloads"
  )

  if (!dir.exists(downloads_dir)) {
    dir.create(
      downloads_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  zip_path <- file.path(
    downloads_dir,
    source$archive_name[[1]]
  )

  # ----------------------------------------------------------
  # CURL handle
  # ----------------------------------------------------------

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

  # ----------------------------------------------------------
  # Download
  # ----------------------------------------------------------

  response <- tryCatch(
    curl::curl_fetch_disk(
      source$url[[1]],
      zip_path,
      handle = handle
    ),
    error = function(e) {

      stop(
        paste0(
          "Unable to download DBSN dataset '",
          area,
          "'.\n\n",
          "Original error:\n",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  # ----------------------------------------------------------
  # Validate downloaded ZIP
  # ----------------------------------------------------------

  if (!file.exists(zip_path)) {
    stop(
      "The DBSN archive was not downloaded.",
      call. = FALSE
    )
  }

  if (file.info(zip_path)$size == 0) {
    stop(
      "The downloaded DBSN archive is empty.",
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # Clean previous extracted dataset when overwriting
  # ----------------------------------------------------------

  if (overwrite && dir.exists(dataset_dir)) {

    unlink(
      dataset_dir,
      recursive = TRUE,
      force = TRUE
    )

    dir.create(
      dataset_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  # ----------------------------------------------------------
  # Extract archive
  # ----------------------------------------------------------

  tryCatch(
    utils::unzip(
      zipfile = zip_path,
      exdir = dataset_dir
    ),
    error = function(e) {

      stop(
        paste0(
          "Unable to extract DBSN dataset '",
          area,
          "'.\n\n",
          "Original error:\n",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  # ----------------------------------------------------------
  # Discover extracted File Geodatabase
  # ----------------------------------------------------------

  gdb <- list.dirs(
    dataset_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  gdb <- gdb[
    grepl(
      "\\.gdb$",
      gdb,
      ignore.case = TRUE
    )
  ]

  if (length(gdb) == 0L) {
    stop(
      paste0(
        "No File Geodatabase (.gdb) was found after extracting ",
        "DBSN dataset '",
        area,
        "'."
      ),
      call. = FALSE
    )
  }

  if (length(gdb) > 1L) {
    stop(
      paste0(
        "Multiple File Geodatabases were found for DBSN dataset '",
        area,
        "'."
      ),
      call. = FALSE
    )
  }

  normalizePath(
    gdb[[1]],
    winslash = "/",
    mustWork = TRUE
  )
}
