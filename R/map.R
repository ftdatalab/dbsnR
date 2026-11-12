#' Interactive map for sf objects
#'
#' Displays an sf object on an interactive leaflet map.
#'
#' The function automatically detects point, line, and polygon geometries.
#' Features can optionally be colored using a numeric or categorical column.
#'
#' @param x An sf object.
#' @param basemap Basemap displayed below the spatial data.
#'   One of `"street"`, `"topo"`, or `"satellite"`.
#' @param popup Optional column name used for popups.
#' @param hover Optional column name used for hover labels.
#' @param color_by Optional column name used to color features.
#'   Numeric columns use a continuous palette. Character, factor, and logical
#'   columns use a categorical palette.
#' @param palette Color palette used when `color_by` is provided.
#'   Examples include `"viridis"`, `"magma"`, `"plasma"`, `"Blues"`,
#'   `"YlOrRd"`, and `"Set2"`.
#' @param legend Logical. If `TRUE`, displays a color legend when
#'   `color_by` is provided.
#'
#' @return A leaflet map.
#'
#' @examples
#' \dontrun{
#' dbsn_map(x)
#'
#' dbsn_map(
#'   x,
#'   basemap = "topo",
#'   color_by = "quota",
#'   hover = "quota"
#' )
#' }
#'
#' @export

dbsn_map <- function(
    x,
    basemap = "street",
    popup = NULL,
    hover = NULL,
    color_by = NULL,
    palette = "viridis",
    legend = TRUE
) {

  # -----------------------------------------------------------------------
  # Validate x
  # -----------------------------------------------------------------------

  if (!inherits(x, "sf")) {
    stop(
      "`x` must be an sf object.",
      call. = FALSE
    )
  }

  if (nrow(x) == 0) {
    stop(
      "`x` contains no features.",
      call. = FALSE
    )
  }

  if (is.na(sf::st_crs(x))) {
    stop(
      "`x` has no coordinate reference system.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Validate general arguments
  # -----------------------------------------------------------------------

  basemap <- match.arg(
    basemap,
    c(
      "street",
      "topo",
      "satellite"
    )
  )

  if (!is.character(palette) || length(palette) != 1 || is.na(palette)) {
    stop(
      "`palette` must be a single character value.",
      call. = FALSE
    )
  }

  if (!is.logical(legend) || length(legend) != 1 || is.na(legend)) {
    stop(
      "`legend` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Validate requested columns
  # -----------------------------------------------------------------------

  validate_column <- function(column, argument) {

    if (is.null(column)) {
      return(invisible(NULL))
    }

    if (!is.character(column) || length(column) != 1 || is.na(column)) {
      stop(
        "`",
        argument,
        "` must be NULL or a single column name.",
        call. = FALSE
      )
    }

    if (!column %in% names(x)) {
      stop(
        "`",
        argument,
        "` column `",
        column,
        "` was not found in `x`.",
        call. = FALSE
      )
    }

    invisible(NULL)
  }

  validate_column(
    popup,
    "popup"
  )

  validate_column(
    hover,
    "hover"
  )

  validate_column(
    color_by,
    "color_by"
  )

  # -----------------------------------------------------------------------
  # Remove empty geometries
  # -----------------------------------------------------------------------

  empty <- sf::st_is_empty(x)

  if (any(empty)) {

    n_empty <- sum(empty)

    x <- x[!empty, , drop = FALSE]

    message(
      n_empty,
      if (n_empty == 1) {
        " empty geometry removed."
      } else {
        " empty geometries removed."
      }
    )
  }

  if (nrow(x) == 0) {
    stop(
      "`x` contains only empty geometries.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Repair invalid geometries
  # -----------------------------------------------------------------------

  valid <- sf::st_is_valid(x)

  invalid <- is.na(valid) | !valid

  if (any(invalid)) {

    n_invalid <- sum(invalid)

    x <- sf::st_make_valid(x)

    message(
      n_invalid,
      if (n_invalid == 1) {
        " invalid geometry repaired."
      } else {
        " invalid geometries repaired."
      }
    )
  }

  # -----------------------------------------------------------------------
  # Transform data for Leaflet
  # -----------------------------------------------------------------------

  x <- sf::st_transform(
    x,
    4326
  )

  # -----------------------------------------------------------------------
  # Popup and hover values
  # -----------------------------------------------------------------------

  popup_values <- NULL

  if (!is.null(popup)) {
    popup_values <- as.character(
      x[[popup]]
    )
  }

  hover_values <- NULL

  if (!is.null(hover)) {
    hover_values <- as.character(
      x[[hover]]
    )
  }

  # -----------------------------------------------------------------------
  # Detect geometry family
  # -----------------------------------------------------------------------

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

  if (all(geom_types %in% point_types)) {

    geometry_family <- "point"

  } else if (all(geom_types %in% line_types)) {

    geometry_family <- "line"

  } else if (all(geom_types %in% polygon_types)) {

    geometry_family <- "polygon"

  } else {

    stop(
      paste0(
        "`x` contains unsupported or mixed geometry types: ",
        paste(geom_types, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Prepare color palette
  # -----------------------------------------------------------------------

  color_palette <- NULL
  color_values <- NULL
  feature_colors <- NULL
  color_type <- NULL

  if (!is.null(color_by)) {

    color_values <- x[[color_by]]

    if (all(is.na(color_values))) {
      stop(
        "`color_by` column contains only missing values.",
        call. = FALSE
      )
    }

    # Numeric palette
    if (
      is.numeric(color_values) ||
      inherits(color_values, "Date") ||
      inherits(color_values, "POSIXt")
    ) {

      color_type <- "numeric"

      if (inherits(color_values, "Date")) {
        color_values <- as.numeric(color_values)
      }

      if (inherits(color_values, "POSIXt")) {
        color_values <- as.numeric(color_values)
      }

      color_palette <- leaflet::colorNumeric(
        palette = palette,
        domain = color_values,
        na.color = "#BDBDBD"
      )

      feature_colors <- color_palette(
        color_values
      )

    } else {

      # Categorical palette
      color_type <- "categorical"

      color_values <- as.character(
        color_values
      )

      color_palette <- leaflet::colorFactor(
        palette = palette,
        domain = color_values,
        na.color = "#BDBDBD"
      )

      feature_colors <- color_palette(
        color_values
      )
    }
  }

  # -----------------------------------------------------------------------
  # Create map
  # -----------------------------------------------------------------------

  map <- leaflet::leaflet(
    data = x
  )

  # -----------------------------------------------------------------------
  # Add basemap
  # -----------------------------------------------------------------------

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
  # POINT / MULTIPOINT
  # -----------------------------------------------------------------------

  if (geometry_family == "point") {

    if (is.null(color_by)) {

      map <- map |>
        leaflet::addCircleMarkers(
          popup = popup_values,
          label = hover_values,
          radius = 5,
          stroke = TRUE,
          color = "#2171B5",
          weight = 1,
          fillColor = "#2171B5",
          fillOpacity = 0.8
        )

    } else {

      map <- map |>
        leaflet::addCircleMarkers(
          popup = popup_values,
          label = hover_values,
          radius = 5,
          stroke = TRUE,
          color = "#404040",
          weight = 1,
          fillColor = feature_colors,
          fillOpacity = 0.8
        )
    }
  }

  # -----------------------------------------------------------------------
  # LINESTRING / MULTILINESTRING
  # -----------------------------------------------------------------------

  if (geometry_family == "line") {

    if (is.null(color_by)) {

      map <- map |>
        leaflet::addPolylines(
          popup = popup_values,
          label = hover_values,
          color = "#2171B5",
          weight = 3,
          opacity = 1,
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            color = "#08306B",
            bringToFront = TRUE
          )
        )

    } else {

      map <- map |>
        leaflet::addPolylines(
          popup = popup_values,
          label = hover_values,
          color = feature_colors,
          weight = 3,
          opacity = 1,
          highlightOptions = leaflet::highlightOptions(
            weight = 5,
            bringToFront = TRUE
          )
        )
    }
  }

  # -----------------------------------------------------------------------
  # POLYGON / MULTIPOLYGON
  # -----------------------------------------------------------------------

  if (geometry_family == "polygon") {

    if (is.null(color_by)) {

      map <- map |>
        leaflet::addPolygons(
          popup = popup_values,
          label = hover_values,
          color = "#2171B5",
          weight = 1,
          fillColor = "#9ECAE1",
          fillOpacity = 0.25,
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color = "#08306B",
            fillOpacity = 0.4,
            bringToFront = TRUE
          )
        )

    } else {

      map <- map |>
        leaflet::addPolygons(
          popup = popup_values,
          label = hover_values,
          color = "#404040",
          weight = 1,
          fillColor = feature_colors,
          fillOpacity = 0.5,
          highlightOptions = leaflet::highlightOptions(
            weight = 3,
            color = "#08306B",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        )
    }
  }

  # -----------------------------------------------------------------------
  # Add legend
  # -----------------------------------------------------------------------

  if (
    !is.null(color_by) &&
    isTRUE(legend)
  ) {

    if (color_type == "numeric") {

      map <- map |>
        leaflet::addLegend(
          position = "bottomright",
          pal = color_palette,
          values = color_values,
          title = color_by,
          opacity = 1,
          labFormat = leaflet::labelFormat(
            big.mark = " "
          )
        )

    } else {

      map <- map |>
        leaflet::addLegend(
          position = "bottomright",
          pal = color_palette,
          values = color_values,
          title = color_by,
          opacity = 1
        )
    }
  }

  # -----------------------------------------------------------------------
  # Return map
  # -----------------------------------------------------------------------

  map
}
