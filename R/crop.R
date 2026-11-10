#' Crop DBSN spatial data
#'
#' Crops an sf object or a list of sf objects using a spatial area.
#'
#' The cropping area can be an sf object, an sfc geometry, or an sf bbox.
#' The area is automatically transformed to the CRS of each input layer.
#'
#' @param x An sf object or a named list of sf objects, such as the
#'   result returned by `dbsn_merge()`.
#'
#' @param area Cropping area. It can be:
#'   \itemize{
#'     \item an sf object;
#'     \item an sfc object;
#'     \item an sf bbox object.
#'   }
#'
#' @param method Cropping method. Either `"intersects"` or `"clip"`.
#'   `"intersects"` keeps complete features intersecting the area.
#'   `"clip"` geometrically cuts features at the boundary of the area.
#'
#' @param keep_empty Logical. When `x` is a list, determines whether
#'   layers containing zero features after cropping should be retained.
#'
#' @param quiet Logical. If FALSE, progress messages are displayed.
#'
#' @return
#' An sf object when `x` is an sf object.
#' A named list of sf objects when `x` is a list.
#'
#' @export
dbsn_crop <- function(
    x,
    area,
    method = c("intersects", "clip"),
    keep_empty = FALSE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate method
  # ---------------------------------------------------------------------------

  method <- match.arg(method)


  # ---------------------------------------------------------------------------
  # 2. Validate input data
  # ---------------------------------------------------------------------------

  is_sf <- inherits(x, "sf")

  is_sf_list <- is.list(x) &&
    all(vapply(
      x,
      function(z) inherits(z, "sf"),
      logical(1)
    ))

  if (!is_sf && !is_sf_list) {
    stop(
      "`x` must be an sf object or a list of sf objects.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Validate cropping area
  # ---------------------------------------------------------------------------

  if (inherits(area, "bbox")) {

    area <- sf::st_as_sfc(area)

  } else if (inherits(area, "sf")) {

    area <- sf::st_geometry(area)

  } else if (!inherits(area, "sfc")) {

    stop(
      "`area` must be an sf object, sfc object, or bbox.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Internal cropping function
  # ---------------------------------------------------------------------------

  crop_one <- function(layer, layer_name = NULL) {

    if (!inherits(layer, "sf")) {
      return(NULL)
    }

    if (nrow(layer) == 0L) {
      return(layer)
    }

    layer_crs <- sf::st_crs(layer)
    area_crs <- sf::st_crs(area)

    if (is.na(layer_crs)) {
      stop(
        "The input layer has no CRS.",
        call. = FALSE
      )
    }

    if (is.na(area_crs)) {
      stop(
        "The cropping area has no CRS.",
        call. = FALSE
      )
    }


    # -------------------------------------------------------------------------
    # Transform area to layer CRS
    # -------------------------------------------------------------------------

    crop_area <- area

    if (area_crs != layer_crs) {

      crop_area <- sf::st_transform(
        crop_area,
        layer_crs
      )
    }


    # -------------------------------------------------------------------------
    # Make one cropping geometry
    # -------------------------------------------------------------------------

    crop_area <- sf::st_union(
      crop_area
    )


    # -------------------------------------------------------------------------
    # Crop
    # -------------------------------------------------------------------------

    if (method == "intersects") {

      idx <- lengths(
        sf::st_intersects(
          layer,
          crop_area
        )
      ) > 0L

      result <- layer[
        idx,
        ,
        drop = FALSE
      ]

    } else {

      result <- suppressWarnings(
        sf::st_intersection(
          layer,
          crop_area
        )
      )
    }


    # -------------------------------------------------------------------------
    # Progress message
    # -------------------------------------------------------------------------

    if (!quiet && !is.null(layer_name)) {

      message(
        "  ",
        layer_name,
        ": ",
        nrow(layer),
        " -> ",
        nrow(result),
        " features"
      )
    }

    result
  }


  # ---------------------------------------------------------------------------
  # 5. Single sf object
  # ---------------------------------------------------------------------------

  if (is_sf) {

    if (!quiet) {
      message("Cropping sf object...")
    }

    return(
      crop_one(x)
    )
  }


  # ---------------------------------------------------------------------------
  # 6. List returned by dbsn_merge()
  # ---------------------------------------------------------------------------

  if (!quiet) {
    message(
      "Cropping ",
      length(x),
      " DBSN layer(s)..."
    )
  }

  result <- vector(
    "list",
    length(x)
  )

  names(result) <- names(x)

  for (i in seq_along(x)) {

    layer_name <- names(x)[i]

    if (
      is.null(layer_name) ||
      is.na(layer_name) ||
      !nzchar(layer_name)
    ) {
      layer_name <- paste0(
        "layer_",
        i
      )
    }

    result[[i]] <- crop_one(
      layer = x[[i]],
      layer_name = layer_name
    )
  }


  # ---------------------------------------------------------------------------
  # 7. Remove empty layers if requested
  # ---------------------------------------------------------------------------

  if (!keep_empty) {

    keep <- vapply(
      result,
      function(z) {
        !is.null(z) && nrow(z) > 0L
      },
      logical(1)
    )

    result <- result[keep]
  }

  result
}
