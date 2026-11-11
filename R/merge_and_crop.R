#' Merge and crop DBSN spatial data
#'
#' Merges DBSN layers from cached provincial geodatabases, crops the merged
#' data to a spatial area, and optionally stores the cropped result as an
#' RDS file for later reuse.
#'
#' The function combines `dbsn_merge()` and `dbsn_crop()` into a single
#' workflow. Cached cropped results are loaded automatically when the same
#' study-area name, provinces, layers, and options are requested again.
#'
#' @param area Cropping area. It can be:
#' \itemize{
#'   \item an sf object;
#'   \item an sfc object;
#'   \item an sf bbox object.
#' }
#'
#' @param name Character string identifying the study area. The value is used
#'   in the cropped-cache filename. For example, `"orvinio"` or
#'   `"rieti_study_area"`.
#'
#' @param layer Character vector containing the DBSN layer names to merge
#'   and crop. If NULL, all available layers are processed.
#'
#' @param province Character vector containing province codes, for example
#'   `c("RI", "AQ", "TE")`. If NULL, all provinces available in the
#'   datasets cache are used.
#'
#' @param method Cropping method. Either `"intersects"` or `"clip"`.
#'   `"intersects"` retains complete features intersecting `area`.
#'   `"clip"` geometrically cuts features at the boundary of `area`.
#'
#' @param keep_empty Logical. When multiple layers are processed, determines
#'   whether layers containing zero features after cropping are retained.
#'
#' @param cache_dir Optional path to the dbsnR datasets cache.
#'   If NULL, the default dbsnR datasets cache is used.
#'
#' @param add_source Logical. If TRUE, the merged data contain the columns
#'   `province` and `source_gdb`.
#'
#' @param cache Logical. If TRUE, both the merged data and the cropped result
#'   can use persistent RDS caching.
#'
#' @param overwrite Logical. If TRUE, an existing cached cropped result is
#'   ignored and replaced. This does not force reconstruction of the merged
#'   cache.
#'
#' @param overwrite_merge Logical. If TRUE, the cached merge is ignored and
#'   recreated before cropping. Use this only after provincial source data
#'   have changed.
#'
#' @param quiet Logical. If FALSE, progress messages are displayed.
#'
#' @return
#' If a single layer is requested, an sf object cropped to `area`.
#'
#' If multiple layers are requested or `layer = NULL`, a named list of
#' cropped sf objects.
#'
#' @details
#' Cropped RDS files are stored in the `cropped` directory next to the
#' `datasets` and `merged` cache directories.
#'
#' The cache filename contains:
#' \itemize{
#'   \item the study-area name;
#'   \item the selected provinces;
#'   \item the requested layers;
#'   \item the cropping method;
#'   \item whether source columns were retained.
#' }
#'
#' The spatial contents of `area` are not encoded in the filename. If the
#' geometry of an existing named study area changes, use `overwrite = TRUE`
#' to rebuild its cropped cache.
#'
#' @examples
#' \dontrun{
#' orvinio <- dbsn_merge_crop(
#'   area = area_orvinio,
#'   name = "orvinio",
#'   province = c(
#'     "AP", "AQ", "PG", "RI",
#'     "RM", "TE", "TR", "VT"
#'   )
#' )
#'
#' orvinio_loc_sg <- dbsn_merge_crop(
#'   area = area_orvinio,
#'   name = "orvinio",
#'   layer = "loc_sg",
#'   province = c("RI", "RM", "AQ"),
#'   method = "intersects"
#' )
#'
#' orvinio_clipped <- dbsn_merge_crop(
#'   area = area_orvinio,
#'   name = "orvinio",
#'   layer = c("loc_sg", "edifc", "tr_str"),
#'   province = c("RI", "RM", "AQ"),
#'   method = "clip",
#'   overwrite = TRUE
#' )
#' }
#'
#' @export
dbsn_merge_crop <- function(
    area,
    name,
    layer = NULL,
    province = NULL,
    method = c("intersects", "clip"),
    keep_empty = FALSE,
    cache_dir = NULL,
    add_source = TRUE,
    cache = TRUE,
    overwrite = FALSE,
    overwrite_merge = FALSE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate method
  # ---------------------------------------------------------------------------

  method <- match.arg(
    method
  )


  # ---------------------------------------------------------------------------
  # 2. Validate study-area name
  # ---------------------------------------------------------------------------

  if (
    !is.character(name) ||
    length(name) != 1L ||
    is.na(name) ||
    !nzchar(trimws(name))
  ) {
    stop(
      "`name` must be a single non-empty character string.",
      call. = FALSE
    )
  }

  area_name <- trimws(
    name
  )


  # ---------------------------------------------------------------------------
  # 3. Validate cropping area
  # ---------------------------------------------------------------------------

  valid_area <- (
    inherits(area, "sf") ||
      inherits(area, "sfc") ||
      inherits(area, "bbox")
  )

  if (!valid_area) {
    stop(
      "`area` must be an sf object, sfc object, or bbox.",
      call. = FALSE
    )
  }

  if (inherits(area, "sf")) {

    if (nrow(area) == 0L) {
      stop(
        "`area` contains no features.",
        call. = FALSE
      )
    }

    area_crs <- sf::st_crs(
      area
    )

  } else {

    if (length(area) == 0L) {
      stop(
        "`area` contains no geometry.",
        call. = FALSE
      )
    }

    area_crs <- sf::st_crs(
      area
    )
  }

  if (is.na(area_crs)) {
    stop(
      "`area` must have a valid CRS.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Validate layer
  # ---------------------------------------------------------------------------

  if (!is.null(layer)) {

    if (
      !is.character(layer) ||
      length(layer) == 0L ||
      anyNA(layer) ||
      any(!nzchar(layer))
    ) {
      stop(
        "`layer` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    layer <- sort(
      unique(layer)
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Validate province
  # ---------------------------------------------------------------------------

  if (!is.null(province)) {

    if (
      !is.character(province) ||
      length(province) == 0L ||
      anyNA(province) ||
      any(!nzchar(province))
    ) {
      stop(
        "`province` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    province <- sort(
      unique(
        toupper(province)
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Validate logical options
  # ---------------------------------------------------------------------------

  logical_options <- list(
    keep_empty = keep_empty,
    add_source = add_source,
    cache = cache,
    overwrite = overwrite,
    overwrite_merge = overwrite_merge,
    quiet = quiet
  )

  for (option_name in names(logical_options)) {

    option_value <- logical_options[[option_name]]

    if (
      !is.logical(option_value) ||
      length(option_value) != 1L ||
      is.na(option_value)
    ) {
      stop(
        "`",
        option_name,
        "` must be TRUE or FALSE.",
        call. = FALSE
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 7. Identify datasets cache directory
  # ---------------------------------------------------------------------------

  if (is.null(cache_dir)) {

    local_appdata <- Sys.getenv(
      "LOCALAPPDATA"
    )

    if (nzchar(local_appdata)) {

      cache_dir <- file.path(
        local_appdata,
        "dbsnR",
        "datasets"
      )

    } else {

      cache_dir <- file.path(
        tools::R_user_dir(
          "dbsnR",
          which = "cache"
        ),
        "datasets"
      )
    }
  }

  cache_dir <- normalizePath(
    cache_dir,
    winslash = "/",
    mustWork = FALSE
  )

  if (!dir.exists(cache_dir)) {
    stop(
      "The dbsnR datasets cache directory does not exist:\n",
      cache_dir,
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Determine provinces used in the cache key
  # ---------------------------------------------------------------------------

  province_dirs <- list.dirs(
    path = cache_dir,
    recursive = FALSE,
    full.names = TRUE
  )

  available_provinces <- sort(
    unique(
      toupper(
        basename(province_dirs)
      )
    )
  )

  if (length(available_provinces) == 0L) {
    stop(
      "No province directories were found in the dbsnR cache.",
      call. = FALSE
    )
  }

  if (is.null(province)) {

    selected_provinces <- available_provinces

  } else {

    selected_provinces <- intersect(
      province,
      available_provinces
    )

    missing_provinces <- setdiff(
      province,
      available_provinces
    )

    if (length(missing_provinces) > 0L) {
      warning(
        "The following provinces were not found in the cache: ",
        paste(
          missing_provinces,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
  }

  if (length(selected_provinces) == 0L) {
    stop(
      "None of the requested provinces is available in the cache.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 9. Define cropped cache directory
  # ---------------------------------------------------------------------------

  cropped_cache_dir <- file.path(
    dirname(cache_dir),
    "cropped"
  )

  if (cache && !dir.exists(cropped_cache_dir)) {

    created <- dir.create(
      cropped_cache_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (
      !created &&
      !dir.exists(cropped_cache_dir)
    ) {
      stop(
        "Unable to create the cropped cache directory:\n",
        cropped_cache_dir,
        call. = FALSE
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 10. Build cropped-cache filename
  # ---------------------------------------------------------------------------

  clean_name <- gsub(
    "[^A-Za-z0-9_.-]",
    "_",
    area_name
  )

  province_key <- paste(
    selected_provinces,
    collapse = "_"
  )

  if (is.null(layer)) {

    layer_key <- "ALL"

  } else {

    layer_key <- paste(
      layer,
      collapse = "_"
    )
  }

  source_key <- if (add_source) {
    "source"
  } else {
    "nosource"
  }

  empty_key <- if (keep_empty) {
    "keepempty"
  } else {
    "dropempty"
  }

  cache_name <- paste(
    "crop",
    clean_name,
    province_key,
    layer_key,
    method,
    source_key,
    empty_key,
    sep = "__"
  )

  cache_name <- gsub(
    "[^A-Za-z0-9_.-]",
    "_",
    cache_name
  )

  crop_cache_file <- file.path(
    cropped_cache_dir,
    paste0(
      cache_name,
      ".rds"
    )
  )


  # ---------------------------------------------------------------------------
  # 11. Load existing cropped cache
  # ---------------------------------------------------------------------------

  if (
    cache &&
    file.exists(crop_cache_file) &&
    !overwrite
  ) {

    if (!quiet) {
      message(
        "Loading cached crop:\n",
        crop_cache_file
      )
    }

    cached_result <- tryCatch(

      readRDS(
        crop_cache_file
      ),

      error = function(e) {
        stop(
          "Unable to read the cached crop:\n",
          crop_cache_file,
          "\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    return(
      cached_result
    )
  }


  # ---------------------------------------------------------------------------
  # 12. Merge DBSN layers
  # ---------------------------------------------------------------------------

  if (!quiet) {
    message(
      "Preparing merged DBSN data..."
    )
  }

  merged <- dbsn_merge(
    layer = layer,
    province = selected_provinces,
    cache_dir = cache_dir,
    add_source = add_source,
    cache = cache,
    overwrite = overwrite_merge,
    quiet = quiet
  )


  # ---------------------------------------------------------------------------
  # 13. Crop merged data
  # ---------------------------------------------------------------------------

  if (!quiet) {
    message(
      "Cropping merged DBSN data to study area '",
      area_name,
      "'..."
    )
  }

  cropped <- dbsn_crop(
    x = merged,
    area = area,
    method = method,
    keep_empty = keep_empty,
    quiet = quiet
  )


  # ---------------------------------------------------------------------------
  # 14. Validate cropped result
  # ---------------------------------------------------------------------------

  if (inherits(cropped, "sf")) {

    result_features <- nrow(
      cropped
    )

    result_layers <- 1L

  } else if (is.list(cropped)) {

    result_layers <- length(
      cropped
    )

    result_features <- sum(
      vapply(
        cropped,
        nrow,
        integer(1)
      )
    )

  } else {

    stop(
      "Unexpected result returned by `dbsn_crop()`.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 15. Save cropped cache
  # ---------------------------------------------------------------------------

  if (cache) {

    if (!quiet) {
      message(
        "Saving cropped cache:\n",
        crop_cache_file
      )
    }

    tryCatch(

      saveRDS(
        cropped,
        file = crop_cache_file,
        compress = TRUE
      ),

      error = function(e) {
        warning(
          "Unable to save the cropped cache:\n",
          crop_cache_file,
          "\n",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }


  # ---------------------------------------------------------------------------
  # 16. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Study area: ",
      area_name,
      "."
    )

    message(
      "Method: ",
      method,
      "."
    )

    message(
      "Result: ",
      format(
        result_layers,
        big.mark = ","
      ),
      " layer(s), ",
      format(
        result_features,
        big.mark = ","
      ),
      " feature(s)."
    )
  }


  # ---------------------------------------------------------------------------
  # 17. Return
  # ---------------------------------------------------------------------------

  cropped
}
