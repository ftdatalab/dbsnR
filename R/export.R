#' Export DBSN spatial layers
#'
#' Exports a single sf object or a named list of sf objects to a GeoPackage.
#'
#' When a list is supplied, each element is written as a separate layer
#' in the same GeoPackage.
#'
#' @param x An sf object or a named list of sf objects.
#'
#' @param file Output GeoPackage path. The `.gpkg` extension is added
#'   automatically if missing.
#'
#' @param layer Optional layer name when `x` is a single sf object.
#'   If NULL, `"dbsn"` is used.
#'
#' @param overwrite Logical. If TRUE, an existing GeoPackage is deleted
#'   before writing.
#'
#' @param skip_empty Logical. If TRUE, layers containing zero features
#'   are not exported.
#'
#' @param quiet Logical. If FALSE, progress messages are displayed.
#'
#' @return Invisibly returns the normalized path of the exported GeoPackage.
#'
#' @export
dbsn_export <- function(
    x,
    file,
    layer = NULL,
    overwrite = FALSE,
    skip_empty = TRUE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate file
  # ---------------------------------------------------------------------------

  if (
    !is.character(file) ||
    length(file) != 1L ||
    !nzchar(file)
  ) {
    stop(
      "`file` must be a valid file path.",
      call. = FALSE
    )
  }

  # Add .gpkg if missing
  if (!grepl("\\.gpkg$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".gpkg")
  }

  file <- path.expand(file)


  # ---------------------------------------------------------------------------
  # 2. Check output directory
  # ---------------------------------------------------------------------------

  output_dir <- dirname(file)

  if (!dir.exists(output_dir)) {
    stop(
      "The output directory does not exist:\n",
      output_dir,
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Handle existing GeoPackage
  # ---------------------------------------------------------------------------

  if (file.exists(file)) {

    if (!overwrite) {
      stop(
        "The output file already exists:\n",
        file,
        "\nUse `overwrite = TRUE` to replace it.",
        call. = FALSE
      )
    }

    deleted <- file.remove(file)

    if (!deleted) {
      stop(
        "Unable to overwrite the existing file:\n",
        file,
        call. = FALSE
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 4. Single sf object
  # ---------------------------------------------------------------------------

  if (inherits(x, "sf")) {

    if (nrow(x) == 0L && skip_empty) {
      stop(
        "The sf object contains no features.",
        call. = FALSE
      )
    }

    if (is.null(layer)) {
      layer <- "dbsn"
    }

    if (
      !is.character(layer) ||
      length(layer) != 1L ||
      !nzchar(layer)
    ) {
      stop(
        "`layer` must be a valid layer name.",
        call. = FALSE
      )
    }

    if (!quiet) {
      message(
        "Exporting ",
        layer,
        " (",
        format(nrow(x), big.mark = ","),
        " features)..."
      )
    }

    sf::st_write(
      obj = x,
      dsn = file,
      layer = layer,
      driver = "GPKG",
      append = FALSE,
      quiet = TRUE
    )

    if (!quiet) {
      message("GeoPackage created: ", file)
    }

    return(
      invisible(
        normalizePath(
          file,
          winslash = "/",
          mustWork = FALSE
        )
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Validate list
  # ---------------------------------------------------------------------------

  if (!is.list(x)) {
    stop(
      "`x` must be an sf object or a named list of sf objects.",
      call. = FALSE
    )
  }

  if (length(x) == 0L) {
    stop(
      "`x` contains no layers.",
      call. = FALSE
    )
  }

  if (
    is.null(names(x)) ||
    any(is.na(names(x))) ||
    any(!nzchar(names(x)))
  ) {
    stop(
      "`x` must be a named list.",
      call. = FALSE
    )
  }

  valid_sf <- vapply(
    x,
    function(z) inherits(z, "sf"),
    logical(1)
  )

  if (!all(valid_sf)) {
    stop(
      "All elements of `x` must be sf objects.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Remove empty layers
  # ---------------------------------------------------------------------------

  if (skip_empty) {

    keep <- vapply(
      x,
      function(z) nrow(z) > 0L,
      logical(1)
    )

    if (!all(keep) && !quiet) {
      skipped <- names(x)[!keep]

      message(
        "Skipping empty layer(s): ",
        paste(skipped, collapse = ", ")
      )
    }

    x <- x[keep]
  }

  if (length(x) == 0L) {
    stop(
      "No non-empty layers are available for export.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 7. Export layers
  # ---------------------------------------------------------------------------

  layer_names <- names(x)

  for (i in seq_along(x)) {

    layer_name <- layer_names[i]
    obj <- x[[i]]

    if (!quiet) {
      message(
        "Exporting ",
        layer_name,
        " (",
        format(nrow(obj), big.mark = ","),
        " features)..."
      )
    }

    sf::st_write(
      obj = obj,
      dsn = file,
      layer = layer_name,
      driver = "GPKG",
      append = i > 1L,
      quiet = TRUE
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Final message
  # ---------------------------------------------------------------------------

  if (!quiet) {
    message("")
    message(
      "Export completed: ",
      length(x),
      " layer(s)."
    )

    message(
      "GeoPackage: ",
      file
    )
  }


  # ---------------------------------------------------------------------------
  # 9. Return path invisibly
  # ---------------------------------------------------------------------------

  invisible(
    normalizePath(
      file,
      winslash = "/",
      mustWork = FALSE
    )
  )
}
