#' DBSN catalogue
#'
#' Returns the catalogue extracted from the official IGM DBSN
#' content specifications.
#'
#' @param layer Optional DBSN layer name, for example `"loc_sg"`
#'   or `"el_idr"`. If `NULL`, the complete catalogue is returned.
#'
#' @return A list containing classes, attributes and coded domains.
#'
#' @importFrom rlang .data
#'
#' @export
dbsn_catalog <- function(layer = NULL) {

  if (is.null(layer)) {
    return(catalogo_dbsn)
  }

  layer_name <- tolower(layer)

  if (!layer_name %in% catalogo_dbsn$classi$layer) {
    stop(
      "Layer '",
      layer_name,
      "' not found in the DBSN catalogue.",
      call. = FALSE
    )
  }

  list(
    classe = catalogo_dbsn$classi |>
      dplyr::filter(.data$layer == layer_name),

    attributi = catalogo_dbsn$attributi |>
      dplyr::filter(.data$layer == layer_name),

    domini = catalogo_dbsn$domini |>
      dplyr::filter(.data$layer == layer_name)
  )
}
