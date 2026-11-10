#' Filter a DBSN sf layer by attribute values
#'
#' Filters a DBSN sf object using one or more attribute criteria while
#' preserving the spatial geometry.
#'
#' Multiple values supplied for the same field are interpreted as OR.
#' Conditions on different fields are combined using AND.
#'
#' @param x An sf object.
#'
#' @param ... Named filtering criteria. Names must correspond to columns
#'   in `x`. Multiple values can be supplied using a vector.
#'
#' @param ignore_case Logical. If TRUE, character comparisons are
#'   case-insensitive.
#'
#' @param keep_na Logical. If TRUE, NA values in filtered columns are
#'   retained.
#'
#' @param quiet Logical. If FALSE, displays the number of features before
#'   and after filtering.
#'
#' @return A filtered sf object.
#'
#' @examples
#' \dontrun{
#' dbsn_filter(loc_sg, province = "RI")
#'
#' dbsn_filter(
#'   loc_sg,
#'   province = c("RI", "AQ")
#' )
#'
#' dbsn_filter(
#'   loc_sg,
#'   province = "RI",
#'   loc_sg_sgn = c("01", "04")
#' )
#' }
#'
#' @export
dbsn_filter <- function(
    x,
    ...,
    ignore_case = FALSE,
    keep_na = FALSE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate input
  # ---------------------------------------------------------------------------

  if (!inherits(x, "sf")) {
    stop(
      "`x` must be an sf object.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Collect filters
  # ---------------------------------------------------------------------------

  filters <- list(...)

  if (length(filters) == 0L) {
    stop(
      "At least one filtering criterion must be provided.",
      call. = FALSE
    )
  }

  filter_names <- names(filters)

  if (
    is.null(filter_names) ||
    any(is.na(filter_names)) ||
    any(!nzchar(filter_names))
  ) {
    stop(
      "All filtering criteria must be named.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Check column names
  # ---------------------------------------------------------------------------

  missing_columns <- setdiff(
    filter_names,
    names(x)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "The following columns were not found in `x`: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Start with all rows selected
  # ---------------------------------------------------------------------------

  keep <- rep(
    TRUE,
    nrow(x)
  )


  # ---------------------------------------------------------------------------
  # 5. Apply filters
  # ---------------------------------------------------------------------------

  for (field in filter_names) {

    values <- filters[[field]]

    if (length(values) == 0L) {
      stop(
        "Filter `",
        field,
        "` contains no values.",
        call. = FALSE
      )
    }

    column <- x[[field]]


    # -------------------------------------------------------------------------
    # Character comparison
    # -------------------------------------------------------------------------

    if (
      ignore_case &&
      (is.character(column) || is.factor(column))
    ) {

      column_compare <- tolower(
        as.character(column)
      )

      values_compare <- tolower(
        as.character(values)
      )

      match_field <- column_compare %in% values_compare

    } else {

      match_field <- column %in% values
    }


    # -------------------------------------------------------------------------
    # Handle NA
    # -------------------------------------------------------------------------

    if (keep_na) {

      match_field <- match_field |
        is.na(column)

    } else {

      match_field[
        is.na(match_field)
      ] <- FALSE
    }


    # -------------------------------------------------------------------------
    # AND between fields
    # -------------------------------------------------------------------------

    keep <- keep & match_field
  }


  # ---------------------------------------------------------------------------
  # 6. Create result
  # ---------------------------------------------------------------------------

  result <- x[
    keep,
    ,
    drop = FALSE
  ]

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 7. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Filtered ",
      format(
        nrow(x),
        big.mark = ","
      ),
      " -> ",
      format(
        nrow(result),
        big.mark = ","
      ),
      " features."
    )
  }


  # ---------------------------------------------------------------------------
  # 8. Return
  # ---------------------------------------------------------------------------

  result
}
