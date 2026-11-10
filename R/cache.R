#' DBSN cache directory
#'
#' Returns the path to the local cache directory used by dbsnR.
#' The directory is created automatically if it does not exist.
#'
#' @param create Logical. If `TRUE`, creates the cache directory
#'   when it does not exist.
#'
#' @return A character string containing the cache directory path.
#'
#' @export
dbsn_cache_dir <- function(create = TRUE) {

  path <- file.path(
    Sys.getenv("LOCALAPPDATA"),
    "dbsnR"
  )

  if (create && !dir.exists(path)) {
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
}

#' DBSN dataset cache path
#'
#' Returns the local cache path for a DBSN dataset.
#'
#' @param area Area identifier, for example `"RI"`, `"RM"` or `"AQ"`.
#' @param create Logical. If `TRUE`, creates the directory if necessary.
#'
#' @return A character string containing the dataset cache path.
#'
#' @export
dbsn_cache_path <- function(area, create = FALSE) {

  area <- toupper(trimws(area))

  if (!nzchar(area)) {
    stop("`area` cannot be empty.", call. = FALSE)
  }

  path <- file.path(
    dbsn_cache_dir(),
    "datasets",
    area
  )

  if (create && !dir.exists(path)) {
    dir.create(
      path,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
}

#' Check DBSN dataset availability
#'
#' Checks whether a DBSN File Geodatabase is available
#' in the local cache for the specified area.
#'
#' @param area Area identifier, for example `"RI"`, `"RM"` or `"AQ"`.
#'
#' @return `TRUE` if at least one `.gdb` directory is available,
#'   otherwise `FALSE`.
#'
#' @export
dbsn_available <- function(area) {

  path <- dbsn_cache_path(
    area,
    create = FALSE
  )

  if (!dir.exists(path)) {
    return(FALSE)
  }

  gdb <- list.dirs(
    path,
    full.names = TRUE,
    recursive = TRUE
  )

  gdb <- gdb[
    grepl(
      "\\.gdb$",
      gdb,
      ignore.case = TRUE
    )
  ]

  length(gdb) > 0
}
