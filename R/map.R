#' Interactive map for sf objects
#'
#' Displays an sf object on an interactive leaflet map.
#'
#' @param x An sf object.
#' @param basemap One of:
#'   "street", "topo", or "satellite".
#' @param popup Optional column name used for popups.
#' @param hover Optional column name used for hover labels.
#'
#' @return A leaflet map.
#'
#' @export

dbsn_map <- function(
    x,
    basemap = "street",
    popup = NULL,
    hover = NULL
) {

  # -----------------------------------------------------------------------
  # Validation
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


  basemap <- match.arg(
    basemap,
    c(
      "street",
      "topo",
      "satellite"
    )
  )

  # -----------------------------------------------------------------------
  # Leaflet uses EPSG:4326
  # -----------------------------------------------------------------------

  x <- sf::st_transform(
    x,
    4326
  )

  # -----------------------------------------------------------------------
  # Build popup formula
  # -----------------------------------------------------------------------

  popup_formula <- NULL

  if (!is.null(popup)) {

    if (!popup %in% names(x)) {
      stop(
        "`popup` is not a column of `x`.",
        call. = FALSE
      )
    }

    popup_formula <- stats::as.formula(
      paste0("~", popup)
    )
  }

  # -----------------------------------------------------------------------
  # Build hover formula
  # -----------------------------------------------------------------------

  label_formula <- NULL

  if (!is.null(hover)) {

    if (!hover %in% names(x)) {
      stop(
        "`hover` is not a column of `x`.",
        call. = FALSE
      )
    }

    label_formula <- stats::as.formula(
      paste0("~", hover)
    )
  }

  # -----------------------------------------------------------------------
  # Create map
  # -----------------------------------------------------------------------

  map <- leaflet::leaflet(
    data = x
  )

  # -----------------------------------------------------------------------
  # Basemap
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
  # Geometry type
  # -----------------------------------------------------------------------

  geom_types <- unique(
    as.character(
      sf::st_geometry_type(x)
    )
  )

  # -----------------------------------------------------------------------
  # POINT / MULTIPOINT
  # -----------------------------------------------------------------------

  if (
    all(
      geom_types %in%
      c(
        "POINT",
        "MULTIPOINT"
      )
    )
  ) {

    map <- map |>
      leaflet::addCircleMarkers(
        popup = popup_formula,
        label = label_formula,
        radius = 5,
        stroke = TRUE,
        weight = 1,
        fillOpacity = 0.8
      )

  }

  # -----------------------------------------------------------------------
  # LINESTRING / MULTILINESTRING
  # -----------------------------------------------------------------------

  else if (
    all(
      geom_types %in%
      c(
        "LINESTRING",
        "MULTILINESTRING"
      )
    )
  ) {

    map <- map |>
      leaflet::addPolylines(
        popup = popup_formula,
        label = label_formula,
        color = "#2171b5",
        weight = 3,
        highlightOptions =
          leaflet::highlightOptions(
            weight = 5,
            color = "#08306b",
            bringToFront = TRUE
          )
      )

  }

  # -----------------------------------------------------------------------
  # POLYGON / MULTIPOLYGON
  # -----------------------------------------------------------------------

  else {

    map <- map |>
      leaflet::addPolygons(
        popup = popup_formula,
        label = label_formula,
        color = "#2171b5",
        weight = 1,
        fillColor = "#9ecae1",
        fillOpacity = 0.25,
        highlightOptions =
          leaflet::highlightOptions(
            weight = 3,
            color = "#08306b",
            fillOpacity = 0.4,
            bringToFront = TRUE
          )
      )

  }

  map
}
