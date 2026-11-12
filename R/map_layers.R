#' Interactive map for multiple sf layers
#'
#' Displays multiple sf objects on a single interactive leaflet map.
#'
#' Each object is automatically added as a separate layer with its own
#' color and can be shown or hidden using a layer control.
#'
#' @param ... One or more sf objects.
#' @param basemap Basemap displayed below the spatial data.
#'   One of `"street"`, `"topo"`, or `"satellite"`.
#' @param layer_names Optional character vector containing the names of
#'   the layers. If `NULL`, object names are used when available.
#' @param colors Optional character vector containing layer colors.
#'   If `NULL`, colors are assigned automatically.
#' @param popup Optional column name to use for popups. The column is used
#'   only in layers where it exists.
#' @param hover Optional column name to use for hover labels. The column is
#'   used only in layers where it exists.
#' @param control Logical. If `TRUE`, adds a layer control.
#'
#' @return A leaflet map.
#'
#' @examples
#' \dontrun{
#' dbsn_map_layers(
#'   roads,
#'   buildings,
#'   places,
#'   basemap = "topo"
#' )
#'
#' dbsn_map_layers(
#'   roads,
#'   buildings,
#'   layer_names = c("Roads", "Buildings"),
#'   colors = c("#E31A1C", "#33A02C")
#' )
#' }
#'
#' @export

dbsn_map_layers <- function(
    ...,
    basemap = "street",
    layer_names = NULL,
    colors = NULL,
    popup = NULL,
    hover = NULL,
    control = TRUE
) {

  # -----------------------------------------------------------------------
  # Collect layers
  # -----------------------------------------------------------------------

  dots <- list(...)

  if (length(dots) == 0) {
    stop(
      "At least one sf object must be supplied.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Validate layers
  # -----------------------------------------------------------------------

  valid_sf <- vapply(
    dots,
    inherits,
    logical(1),
    what = "sf"
  )

  if (!all(valid_sf)) {

    bad <- which(!valid_sf)

    stop(
      "All objects supplied through `...` must be sf objects. ",
      "Invalid object position(s): ",
      paste(bad, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  empty_layers <- vapply(
    dots,
    nrow,
    integer(1)
  ) == 0

  if (any(empty_layers)) {

    stop(
      "Empty sf layer(s) found at position(s): ",
      paste(which(empty_layers), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  missing_crs <- vapply(
    dots,
    function(x) is.na(sf::st_crs(x)),
    logical(1)
  )

  if (any(missing_crs)) {

    stop(
      "Missing CRS for layer(s) at position(s): ",
      paste(which(missing_crs), collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Basemap
  # -----------------------------------------------------------------------

  basemap <- match.arg(
    basemap,
    c(
      "street",
      "topo",
      "satellite"
    )
  )

  # -----------------------------------------------------------------------
  # Layer names
  # -----------------------------------------------------------------------

  expression_names <- as.character(
    substitute(list(...))
  )[-1]

  if (is.null(layer_names)) {

    layer_names <- names(dots)

    if (
      is.null(layer_names) ||
      any(!nzchar(layer_names))
    ) {
      layer_names <- expression_names
    }

    bad_names <- !nzchar(layer_names)

    if (any(bad_names)) {
      layer_names[bad_names] <- paste0(
        "Layer ",
        which(bad_names)
      )
    }
  }

  if (length(layer_names) != length(dots)) {
    stop(
      "`layer_names` must have one name for each sf object.",
      call. = FALSE
    )
  }

  if (
    anyNA(layer_names) ||
    any(!nzchar(layer_names))
  ) {
    stop(
      "`layer_names` cannot contain missing or empty names.",
      call. = FALSE
    )
  }

  if (anyDuplicated(layer_names)) {
    stop(
      "`layer_names` must contain unique names.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Colors
  # -----------------------------------------------------------------------

  default_colors <- c(
    "#2171B5",
    "#E31A1C",
    "#33A02C",
    "#FF7F00",
    "#6A3D9A",
    "#A65628",
    "#E7298A",
    "#1B9E77",
    "#D95F02",
    "#7570B3"
  )

  if (is.null(colors)) {

    colors <- rep(
      default_colors,
      length.out = length(dots)
    )

  } else {

    if (length(colors) == 1) {
      colors <- rep(
        colors,
        length(dots)
      )
    }

    if (length(colors) != length(dots)) {
      stop(
        "`colors` must have length 1 or one color for each sf object.",
        call. = FALSE
      )
    }
  }

  # -----------------------------------------------------------------------
  # Validate general arguments
  # -----------------------------------------------------------------------

  if (
    !is.logical(control) ||
    length(control) != 1 ||
    is.na(control)
  ) {
    stop(
      "`control` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (
    !is.null(popup) &&
    (
      !is.character(popup) ||
      length(popup) != 1 ||
      is.na(popup)
    )
  ) {
    stop(
      "`popup` must be NULL or a single column name.",
      call. = FALSE
    )
  }

  if (
    !is.null(hover) &&
    (
      !is.character(hover) ||
      length(hover) != 1 ||
      is.na(hover)
    )
  ) {
    stop(
      "`hover` must be NULL or a single column name.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Create base map
  # -----------------------------------------------------------------------

  map <- leaflet::leaflet()

  if (basemap == "street") {

    map <- map |>
      leaflet::addTiles()
  }

  if (basemap == "topo") {

    map <- map |>
      leaflet::addProviderTiles(
        leaflet::providers$OpenTopoMap
      )
  }

  if (basemap == "satellite") {

    map <- map |>
      leaflet::addProviderTiles(
        leaflet::providers$Esri.WorldImagery
      )
  }

  # -----------------------------------------------------------------------
  # Process layers
  # -----------------------------------------------------------------------

  for (i in seq_along(dots)) {

    x <- dots[[i]]

    # ---------------------------------------------------------------------
    # Remove empty geometries
    # ---------------------------------------------------------------------

    empty <- sf::st_is_empty(x)

    if (any(empty)) {

      n_empty <- sum(empty)

      x <- x[!empty, , drop = FALSE]

      message(
        layer_names[i],
        ": removed ",
        n_empty,
        if (n_empty == 1) {
          " empty geometry."
        } else {
          " empty geometries."
        }
      )
    }

    if (nrow(x) == 0) {

      message(
        layer_names[i],
        ": skipped because no valid geometries remain."
      )

      next
    }

    # ---------------------------------------------------------------------
    # Repair invalid geometries
    # ---------------------------------------------------------------------

    valid <- sf::st_is_valid(x)

    invalid <- is.na(valid) | !valid

    if (any(invalid)) {

      n_invalid <- sum(invalid)

      x <- sf::st_make_valid(x)

      message(
        layer_names[i],
        ": repaired ",
        n_invalid,
        if (n_invalid == 1) {
          " invalid geometry."
        } else {
          " invalid geometries."
        }
      )
    }

    # ---------------------------------------------------------------------
    # Transform to EPSG:4326
    # ---------------------------------------------------------------------

    x <- sf::st_transform(
      x,
      4326
    )

    # ---------------------------------------------------------------------
    # Popup
    # ---------------------------------------------------------------------

    popup_values <- NULL

    if (
      !is.null(popup) &&
      popup %in% names(x)
    ) {
      popup_values <- as.character(
        x[[popup]]
      )
    }

    # ---------------------------------------------------------------------
    # Hover
    # ---------------------------------------------------------------------

    hover_values <- NULL

    if (
      !is.null(hover) &&
      hover %in% names(x)
    ) {
      hover_values <- as.character(
        x[[hover]]
      )
    }

    # ---------------------------------------------------------------------
    # Geometry type
    # ---------------------------------------------------------------------

    geom_types <- unique(
      as.character(
        sf::st_geometry_type(x)
      )
    )

    point_types <- c(
      "POINT",
      "MULTIPOINT"
    )

    line_types <- c(
      "LINESTRING",
      "MULTILINESTRING"
    )

    polygon_types <- c(
      "POLYGON",
      "MULTIPOLYGON"
    )

    # ---------------------------------------------------------------------
    # POINT / MULTIPOINT
    # ---------------------------------------------------------------------

    if (all(geom_types %in% point_types)) {

      map <- map |>
        leaflet::addCircleMarkers(
          data = x,
          group = layer_names[i],
          popup = popup_values,
          label = hover_values,
          radius = 5,
          stroke = TRUE,
          color = colors[i],
          weight = 1,
          fillColor = colors[i],
          fillOpacity = 0.8
        )

      # ---------------------------------------------------------------------
      # LINESTRING / MULTILINESTRING
      # ---------------------------------------------------------------------

    } else if (all(geom_types %in% line_types)) {

      map <- map |>
        leaflet::addPolylines(
          data = x,
          group = layer_names[i],
          popup = popup_values,
          label = hover_values,
          color = colors[i],
          weight = 3,
          opacity = 0.9,
          highlightOptions =
            leaflet::highlightOptions(
              weight = 5,
              color = colors[i],
              bringToFront = TRUE
            )
        )

      # ---------------------------------------------------------------------
      # POLYGON / MULTIPOLYGON
      # ---------------------------------------------------------------------

    } else if (all(geom_types %in% polygon_types)) {

      map <- map |>
        leaflet::addPolygons(
          data = x,
          group = layer_names[i],
          popup = popup_values,
          label = hover_values,
          color = colors[i],
          weight = 2,
          fillColor = colors[i],
          fillOpacity = 0.25,
          highlightOptions =
            leaflet::highlightOptions(
              weight = 4,
              color = colors[i],
              fillOpacity = 0.4,
              bringToFront = TRUE
            )
        )

    } else {

      warning(
        "Layer `",
        layer_names[i],
        "` contains unsupported or mixed geometry types: ",
        paste(geom_types, collapse = ", "),
        ". Layer skipped.",
        call. = FALSE
      )
    }
  }

  # -----------------------------------------------------------------------
  # Layer control
  # -----------------------------------------------------------------------

  if (isTRUE(control)) {

    map <- map |>
      leaflet::addLayersControl(
        overlayGroups = layer_names,
        options = leaflet::layersControlOptions(
          collapsed = FALSE
        )
      )
  }

  # -----------------------------------------------------------------------
  # Return map
  # -----------------------------------------------------------------------

  map
}
