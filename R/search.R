#' Search text in a DBSN spatial layer
#'
#' Searches for text across character and factor attributes of an sf
#' object while preserving its geometry.
#'
#' @param x An sf object.
#'
#' @param pattern Character string to search for.
#'
#' @param fields Optional character vector specifying the attributes to
#'   search. If NULL, all character and factor attributes are searched.
#'
#' @param exact Logical. If TRUE, only exact matches are returned.
#'   If FALSE, partial matches are allowed.
#'
#' @param ignore_case Logical. If TRUE, matching is case-insensitive.
#'
#' @param fixed Logical. If TRUE, `pattern` is treated as literal text.
#'   If FALSE, regular expressions can be used.
#'
#' @param add_match Logical. If TRUE, adds a `.matched_fields` column
#'   indicating which attributes matched the search.
#'
#' @param quiet Logical. If FALSE, displays search information.
#'
#' @return An sf object containing matching features.
#'
#' @examples
#' \dontrun{
#' dbsn_search(loc_sg, "Orvinio")
#'
#' dbsn_search(
#'   edifc,
#'   "chiesa",
#'   fields = "edifc_nome"
#' )
#' }
#'
#' @export
dbsn_search <- function(
    x,
    pattern,
    fields = NULL,
    exact = FALSE,
    ignore_case = TRUE,
    fixed = TRUE,
    add_match = TRUE,
    quiet = FALSE
) {

  # ---------------------------------------------------------------------------
  # 1. Validate x
  # ---------------------------------------------------------------------------

  if (!inherits(x, "sf")) {
    stop(
      "`x` must be an sf object.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Validate pattern
  # ---------------------------------------------------------------------------

  if (
    !is.character(pattern) ||
    length(pattern) != 1L ||
    is.na(pattern) ||
    !nzchar(pattern)
  ) {
    stop(
      "`pattern` must be a single non-empty character string.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 3. Determine searchable fields
  # ---------------------------------------------------------------------------

  geometry_column <- attr(
    x,
    "sf_column"
  )

  attribute_names <- setdiff(
    names(x),
    geometry_column
  )

  if (is.null(fields)) {

    searchable <- vapply(
      x[attribute_names],
      function(z) {
        is.character(z) ||
          is.factor(z)
      },
      logical(1)
    )

    fields <- attribute_names[
      searchable
    ]

  } else {

    if (
      !is.character(fields) ||
      length(fields) == 0L
    ) {
      stop(
        "`fields` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    fields <- unique(fields)

    missing_fields <- setdiff(
      fields,
      names(x)
    )

    if (length(missing_fields) > 0L) {
      stop(
        "The following fields were not found in `x`: ",
        paste(
          missing_fields,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    if (geometry_column %in% fields) {
      stop(
        "The geometry column cannot be searched.",
        call. = FALSE
      )
    }
  }


  if (length(fields) == 0L) {
    stop(
      "No searchable attributes were found.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Match function
  # ---------------------------------------------------------------------------

  match_field <- function(values) {

    values <- as.character(values)

    valid <- !is.na(values)

    result <- rep(
      FALSE,
      length(values)
    )

    # Exact matching
    if (exact) {

      if (ignore_case) {

        result[valid] <- tolower(values[valid]) ==
          tolower(pattern)

      } else {

        result[valid] <- values[valid] == pattern
      }

      return(result)
    }

    # Partial fixed-text matching
    if (fixed) {

      if (ignore_case) {

        result[valid] <- grepl(
          pattern = tolower(pattern),
          x = tolower(values[valid]),
          fixed = TRUE
        )

      } else {

        result[valid] <- grepl(
          pattern = pattern,
          x = values[valid],
          fixed = TRUE
        )
      }

    } else {

      # Regular-expression matching
      result[valid] <- grepl(
        pattern = pattern,
        x = values[valid],
        ignore.case = ignore_case
      )
    }

    result
  }


  # ---------------------------------------------------------------------------
  # 5. Search fields
  # ---------------------------------------------------------------------------

  match_matrix <- vapply(
    fields,
    function(field) {

      match_field(
        x[[field]]
      )
    },
    logical(nrow(x))
  )


  # vapply simplifies to a vector when only one field is searched

  if (length(fields) == 1L) {

    match_matrix <- matrix(
      match_matrix,
      ncol = 1L,
      dimnames = list(
        NULL,
        fields
      )
    )
  }


  # ---------------------------------------------------------------------------
  # 6. Determine matching rows
  # ---------------------------------------------------------------------------

  matched_rows <- rowSums(
    match_matrix
  ) > 0L

  result <- x[
    matched_rows,
    ,
    drop = FALSE
  ]


  # ---------------------------------------------------------------------------
  # 7. Add matched fields
  # ---------------------------------------------------------------------------

  if (add_match && nrow(result) > 0L) {

    matched_matrix <- match_matrix[
      matched_rows,
      ,
      drop = FALSE
    ]

    matched_fields <- apply(
      matched_matrix,
      1L,
      function(z) {

        paste(
          fields[z],
          collapse = ", "
        )
      }
    )

    result$.matched_fields <- matched_fields
  }


  # ---------------------------------------------------------------------------
  # 8. Clean row names
  # ---------------------------------------------------------------------------

  rownames(result) <- NULL


  # ---------------------------------------------------------------------------
  # 9. Information
  # ---------------------------------------------------------------------------

  if (!quiet) {

    message(
      "Search: \"",
      pattern,
      "\""
    )

    message(
      "Fields searched: ",
      paste(
        fields,
        collapse = ", "
      )
    )

    message(
      "Matches: ",
      format(
        nrow(result),
        big.mark = ","
      ),
      " of ",
      format(
        nrow(x),
        big.mark = ","
      ),
      " features."
    )
  }


  # ---------------------------------------------------------------------------
  # 10. Return
  # ---------------------------------------------------------------------------

  result
}
