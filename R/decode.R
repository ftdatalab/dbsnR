#' Decode DBSN coded attributes
#'
#' Decodes coded DBSN attributes using the official domains stored in
#' the internal DBSN catalogue.
#'
#' Original coded attributes are preserved and new columns ending in
#' `_label` are added with the corresponding official descriptions.
#'
#' @param x An `sf` object containing DBSN data.
#'
#' @param layer Optional DBSN layer name. If NULL, the function attempts
#'   to identify the layer automatically from the attribute names.
#'
#' @param attributes Optional character vector specifying which attributes
#'   should be decoded. If NULL, all coded attributes found both in `x`
#'   and in the DBSN catalogue are decoded.
#'
#' @param suffix Character suffix used for decoded columns.
#'   Default is `"_label"`.
#'
#' @param keep_unknown Logical. If TRUE, values without a matching catalogue
#'   entry retain their original code in the decoded column. If FALSE,
#'   unmatched values are returned as NA.
#'
#' @param quiet Logical. If FALSE, progress messages are displayed.
#'
#' @return The original `sf` object with additional decoded columns.
#'
#' @examples
#' \dontrun{
#' loc_sg_decoded <- dbsn_decode(loc_sg)
#'
#' loc_sg_decoded <- dbsn_decode(
#'   loc_sg,
#'   attributes = c("loc_sg_ty", "loc_sg_sgn", "meta_ist")
#' )
#' }
#'
#' @export
dbsn_decode <- function(
    x,
    layer = NULL,
    attributes = NULL,
    suffix = "_label",
    keep_unknown = TRUE,
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

  if (
    !is.character(suffix) ||
    length(suffix) != 1L ||
    !nzchar(suffix)
  ) {
    stop(
      "`suffix` must be a non-empty character string.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 2. Identify DBSN layer
  # ---------------------------------------------------------------------------

  catalogue_layers <- unique(
    catalogo_dbsn$domini$layer
  )

  if (is.null(layer)) {

    object_columns <- names(x)

    scores <- vapply(
      catalogue_layers,
      function(layer_name) {

        catalogue_attributes <- unique(
          catalogo_dbsn$domini$attributo[
            catalogo_dbsn$domini$layer == layer_name
          ]
        )

        sum(
          catalogue_attributes %in% object_columns
        )
      },
      integer(1)
    )

    if (
      length(scores) == 0L ||
      max(scores) == 0L
    ) {
      stop(
        "Unable to identify the DBSN layer from the attributes in `x`. ",
        "Specify `layer` explicitly.",
        call. = FALSE
      )
    }

    best_layers <- catalogue_layers[
      scores == max(scores)
    ]

    # Prefer a layer-specific attribute when more than one catalogue
    # entry has the same score.
    if (length(best_layers) > 1L) {

      layer_specific <- best_layers[
        vapply(
          best_layers,
          function(layer_name) {

            any(
              startsWith(
                names(x),
                paste0(layer_name, "_")
              )
            )
          },
          logical(1)
        )
      ]

      if (length(layer_specific) == 1L) {
        layer <- layer_specific
      } else {
        stop(
          "Unable to identify the DBSN layer unambiguously. ",
          "Possible layers: ",
          paste(best_layers, collapse = ", "),
          ". Specify `layer` explicitly.",
          call. = FALSE
        )
      }

    } else {

      layer <- best_layers
    }

  } else {

    if (
      !is.character(layer) ||
      length(layer) != 1L ||
      !nzchar(layer)
    ) {
      stop(
        "`layer` must be NULL or a single DBSN layer name.",
        call. = FALSE
      )
    }

    layer <- tolower(layer)

    if (!layer %in% catalogue_layers) {
      stop(
        "Layer '",
        layer,
        "' was not found in the DBSN catalogue.",
        call. = FALSE
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 3. Get domains for selected layer
  # ---------------------------------------------------------------------------

  domains <- catalogo_dbsn$domini[
    catalogo_dbsn$domini$layer == layer,
    ,
    drop = FALSE
  ]

  if (nrow(domains) == 0L) {
    stop(
      "No coded domains were found for layer '",
      layer,
      "'.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 4. Determine attributes to decode
  # ---------------------------------------------------------------------------

  catalogue_attributes <- unique(
    domains$attributo
  )

  available_attributes <- intersect(
    catalogue_attributes,
    names(x)
  )

  if (is.null(attributes)) {

    attributes_to_decode <- available_attributes

  } else {

    if (
      !is.character(attributes) ||
      length(attributes) == 0L
    ) {
      stop(
        "`attributes` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    attributes <- unique(attributes)

    missing_object_attributes <- setdiff(
      attributes,
      names(x)
    )

    if (length(missing_object_attributes) > 0L) {
      warning(
        "The following attributes were not found in `x`: ",
        paste(
          missing_object_attributes,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    missing_catalogue_attributes <- setdiff(
      attributes,
      catalogue_attributes
    )

    if (length(missing_catalogue_attributes) > 0L) {
      warning(
        "The following attributes have no coded domain for layer '",
        layer,
        "': ",
        paste(
          missing_catalogue_attributes,
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    attributes_to_decode <- intersect(
      attributes,
      available_attributes
    )
  }


  if (length(attributes_to_decode) == 0L) {
    stop(
      "No decodable DBSN attributes were found.",
      call. = FALSE
    )
  }


  # ---------------------------------------------------------------------------
  # 5. Decode attributes
  # ---------------------------------------------------------------------------

  result <- x

  if (!quiet) {
    message(
      "DBSN layer: ",
      layer
    )

    message(
      "Decoding ",
      length(attributes_to_decode),
      " attribute(s)..."
    )
  }


  for (attribute_name in attributes_to_decode) {

    domain <- domains[
      domains$attributo == attribute_name,
      ,
      drop = FALSE
    ]


    # -------------------------------------------------------------------------
    # Remove unusable catalogue entries
    # -------------------------------------------------------------------------

    valid_domain <- (
      !is.na(domain$codice) &
        nzchar(domain$codice)
    )

    domain <- domain[
      valid_domain,
      ,
      drop = FALSE
    ]

    if (nrow(domain) == 0L) {

      if (!quiet) {
        message(
          "  - ",
          attribute_name,
          ": no usable codes, skipped"
        )
      }

      next
    }


    # -------------------------------------------------------------------------
    # Protect against duplicate codes
    # -------------------------------------------------------------------------

    duplicate_codes <- duplicated(
      domain$codice
    )

    if (any(duplicate_codes)) {

      if (!quiet) {
        message(
          "  - ",
          attribute_name,
          ": duplicate catalogue codes detected; using first occurrence"
        )
      }

      domain <- domain[
        !duplicate_codes,
        ,
        drop = FALSE
      ]
    }


    # -------------------------------------------------------------------------
    # Match codes
    # -------------------------------------------------------------------------

    original_values <- result[[attribute_name]]

    character_values <- as.character(
      original_values
    )

    match_index <- match(
      character_values,
      domain$codice
    )

    decoded_values <- domain$descrizione_raw[
      match_index
    ]


    # -------------------------------------------------------------------------
    # Preserve unknown codes if requested
    # -------------------------------------------------------------------------

    unknown <- (
      !is.na(character_values) &
        is.na(match_index)
    )

    if (keep_unknown) {
      decoded_values[unknown] <- character_values[unknown]
    }


    # Keep true NA values as NA

    decoded_values[
      is.na(original_values)
    ] <- NA_character_


    # -------------------------------------------------------------------------
    # Add decoded column
    # -------------------------------------------------------------------------

    output_name <- paste0(
      attribute_name,
      suffix
    )

    result[[output_name]] <- decoded_values


    # -------------------------------------------------------------------------
    # Progress information
    # -------------------------------------------------------------------------

    if (!quiet) {

      matched_count <- sum(
        !is.na(match_index)
      )

      unknown_count <- sum(
        unknown
      )

      message(
        "  - ",
        attribute_name,
        " -> ",
        output_name,
        " (",
        format(matched_count, big.mark = ","),
        " matched",
        if (unknown_count > 0L) {
          paste0(
            ", ",
            format(unknown_count, big.mark = ","),
            " unknown"
          )
        } else {
          ""
        },
        ")"
      )
    }
  }


  # ---------------------------------------------------------------------------
  # 6. Return decoded sf object
  # ---------------------------------------------------------------------------

  result
}
