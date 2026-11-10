#' Select layers from a DBSN dataset
#'
#' Selects one or more layers from a named list of sf objects,
#' such as the result returned by `dbsn_merge()`.
#'
#' @param x A named list of sf objects.
#'
#' @param layers Character vector containing the layer names to select.
#'
#' @param strict Logical. If TRUE, an error is raised when one or more
#'   requested layers are not present. If FALSE, missing layers are
#'   ignored with a warning.
#'
#' @param quiet Logical. If FALSE, information about the selection
#'   is displayed.
#'
#' @return A named list containing the selected sf layers.
#'
#' @examples
#' \dontrun{
#' territory <- dbsn_select(
#'   dbsn,
#'   c("loc_sg", "bosco", "tr_str")
#' )
#' }
#'
#' @export
dbsn_select <- function(
    x,
    layers,
    strict = FALSE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate x
  # ---------------------------------------------------------------------------

  if (!is.list(x)) {
    stop(
      "`x` must be a named list of sf objects.",
      call. = FALSE
    )
  }

  if (is.null(names(x)) || any(!nzchar(names(x)))) {
    stop(
      "`x` must be a named list.",
      call. = FALSE
    )
  }

  is_sf <- vapply(
    x,
    function(z) inherits(z, "sf"),
    logical(1)
  )

  if (!all(is_sf)) {
    stop(
      "All elements of `x` must be sf objects.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Validate layers
  # ---------------------------------------------------------------------------

  if (
    !is.character(layers) ||
    length(layers) == 0L
  ) {
    stop(
      "`layers` must be a non-empty character vector.",
      call. = FALSE
    )
  }

  layers <- unique(layers)


  # ---------------------------------------------------------------------------
  # 3. Check requested layers
  # ---------------------------------------------------------------------------

  available_layers <- names(x)

  missing_layers <- setdiff(
    layers,
    available_layers
  )

  if (length(missing_layers) > 0L) {

    msg <- paste0(
      "The following layers were not found: ",
      paste(missing_layers, collapse = ", ")
    )

    if (strict) {
      stop(
        msg,
        call. = FALSE
      )
    } else {
      warning(
        msg,
        call. = FALSE
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 4. Select available requested layers
  # ---------------------------------------------------------------------------

  selected_layers <- layers[
    layers %in% available_layers
  ]

  if (length(selected_layers) == 0L) {
    stop(
      "None of the requested layers is available.",
      call. = FALSE
    )
  }

  result <- x[
    selected_layers
  ]


  # ---------------------------------------------------------------------------
  # 5. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Selected ",
      length(result),
      " of ",
      length(x),
      " layer(s)."
    )

    message(
      "Layers: ",
      paste(
        names(result),
        collapse = ", "
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Return
  # ---------------------------------------------------------------------------

  result
}
