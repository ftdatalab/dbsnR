#' Quick plot for DBSN spatial objects
#'
#' Creates a quick static plot of an `sf` object.
#'
#' This function is intended for rapid visual inspection of DBSN layers
#' and other `sf` objects.
#'
#' @param x An `sf` object.
#' @param col Feature colour. Defaults to `"steelblue"`.
#' @param border Border colour. Defaults to `"grey30"`.
#' @param lwd Line width. Defaults to `1`.
#' @param pch Point symbol. Defaults to `16`.
#' @param main Plot title. Defaults to `NULL`.
#' @param ... Additional arguments passed to `plot()`.
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' \dontrun{
#'
#' dbsn_plot(loc_sg)
#'
#' dbsn_plot(
#'   bosco,
#'   col = "darkgreen"
#' )
#'
#' dbsn_plot(
#'   tr_str,
#'   col = "red",
#'   lwd = 2
#' )
#'
#' }
#'
#' @export

dbsn_plot <- function(
    x,
    col = "steelblue",
    border = "grey30",
    lwd = 1,
    pch = 16,
    main = NULL,
    ...
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

  geom_type <- unique(
    as.character(
      sf::st_geometry_type(x)
    )
  )

  # -----------------------------------------------------------------------
  # Plot
  # -----------------------------------------------------------------------

  if (all(
    geom_type %in% c(
      "POINT",
      "MULTIPOINT"
    )
  )) {

    plot(
      sf::st_geometry(x),
      col = col,
      pch = pch,
      main = main,
      ...
    )

  } else if (all(
    geom_type %in% c(
      "LINESTRING",
      "MULTILINESTRING"
    )
  )) {

    plot(
      sf::st_geometry(x),
      col = col,
      lwd = lwd,
      main = main,
      ...
    )

  } else {

    plot(
      sf::st_geometry(x),
      col = col,
      border = border,
      lwd = lwd,
      main = main,
      ...
    )
  }

  invisible(x)
}
