library(pdftools)
library(dplyr)
library(tibble)
library(stringr)

# ============================================================
# DBSN OFFICIAL SPECIFICATIONS
# ============================================================

pdf_file <- file.path(
  "data-raw",
  "dbsn_specs_v4.pdf"
)

stopifnot(file.exists(pdf_file))

pages <- pdftools::pdf_text(pdf_file)

stopifnot(length(pages) == 296)

text <- paste(
  pages,
  collapse = "\n"
)

# ============================================================
# EXTRACT DBSN CLASSES
# ============================================================

pattern_class <- paste0(
  "CLASSE:\\s*",
  "([^\\n\\r]+?)\\s*",
  "\\(([A-Z0-9_]+)\\s*[-–]\\s*([0-9]{6})\\)"
)

matches <- stringr::str_match_all(
  text,
  pattern_class
)[[1]]

classi <- tibble::tibble(
  classe = stringr::str_squish(matches[, 2]),
  layer = tolower(matches[, 3]),
  codice_classe = matches[, 4]
) |>
  dplyr::distinct(
    codice_classe,
    .keep_all = TRUE
  ) |>
  dplyr::mutate(
    strato = substr(codice_classe, 1, 2),
    tema = substr(codice_classe, 1, 4)
  ) |>
  dplyr::select(
    strato,
    tema,
    codice_classe,
    layer,
    classe
  ) |>
  dplyr::arrange(codice_classe)

# ============================================================
# CHECK
# ============================================================

print(classi, n = Inf)

cat(
  "\nNumero di classi:",
  nrow(classi),
  "\n"
)

# ============================================================
# EXTRACT DBSN ATTRIBUTES
# ============================================================

pattern_attr <- paste0(
  "(?m)^\\s*",
  "([0-9]{8,9})\\s+",
  "([A-Za-z][A-Za-z0-9_]*)?\\s*",
  "(.*?)\\s+",
  "(String\\([0-9]+\\)|Enum|Integer|Real|Boolean|Data)",
  "(?:\\s|$)"
)

attr_matches <- stringr::str_match_all(
  text,
  pattern_attr
)[[1]]

attributi <- tibble::tibble(
  codice_attributo = attr_matches[, 2],
  attributo = tolower(attr_matches[, 3]),
  descrizione = stringr::str_squish(attr_matches[, 4]),
  tipo = attr_matches[, 5]
) |>
  dplyr::filter(
    !is.na(attributo),
    attributo != ""
  ) |>
  dplyr::distinct(
    codice_attributo,
    attributo,
    .keep_all = TRUE
  )

# ============================================================
# LINK ATTRIBUTES TO DBSN CLASSES
# ============================================================

attributi <- attributi |>
  dplyr::mutate(
    codice_classe = substr(
      codice_attributo,
      1,
      6
    )
  ) |>
  dplyr::left_join(
    classi |>
      dplyr::select(
        codice_classe,
        layer
      ),
    by = "codice_classe"
  ) |>
  dplyr::select(
    layer,
    codice_classe,
    codice_attributo,
    attributo,
    descrizione,
    tipo
  ) |>
  dplyr::arrange(
    codice_classe,
    codice_attributo
  )

# ============================================================
# MANUAL FIXES FOR PDF LINE BREAKS
# ============================================================
#
# pdftools splits these two attributes across lines in the
# official PDF, so they cannot be recovered by the parser.
# ============================================================

attributi_fix <- tibble::tribble(
  ~layer,   ~codice_classe, ~codice_attributo, ~attributo,  ~descrizione,      ~tipo,

  "v_rete", "000101",       "00010103",        "v_rete_id", "identificatore", "String(50)",
  "caposd", "000102",       "00010203",        "caposd_id", "identificatore", "String(50)",
  "bi_ind", "010210",       "010210000",       "meta_ist",  "",               "Enum"
)

attributi <- dplyr::bind_rows(
  attributi,
  attributi_fix
) |>
  dplyr::distinct(
    codice_attributo,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    codice_classe,
    codice_attributo
  )

# ============================================================
# ATTRIBUTE CHECKS
# ============================================================

cat(
  "\nNumero di attributi:",
  nrow(attributi),
  "\n"
)

duplicati_attributi <- attributi |>
  dplyr::count(codice_attributo) |>
  dplyr::filter(n > 1)

attributi_senza_classe <- attributi |>
  dplyr::filter(is.na(layer))

cat(
  "Codici attributo duplicati:",
  nrow(duplicati_attributi),
  "\n"
)

cat(
  "Attributi senza classe:",
  nrow(attributi_senza_classe),
  "\n"
)

# ============================================================
# CLASSES WITHOUT EXTRACTED ATTRIBUTES
# ============================================================

classi_senza_attributi <- classi |>
  dplyr::anti_join(
    attributi |>
      dplyr::distinct(layer),
    by = "layer"
  )

cat(
  "\nClassi senza attributi estratti:",
  nrow(classi_senza_attributi),
  "\n"
)

print(classi_senza_attributi)

# ============================================================
# PREPARE LINES FOR DOMAIN EXTRACTION
# ============================================================

righe <- unlist(
  strsplit(
    text,
    "\n",
    fixed = TRUE
  )
)

righe <- trimws(righe)

# ============================================================
# FUNCTION: PARSE DOMAINS FOR ONE DBSN CLASS
# ============================================================

parse_domini_classe <- function(
    layer_name,
    class_code,
    righe,
    attributi
) {

  # ----------------------------------------------------------
  # Find the real class definition
  # ----------------------------------------------------------

  pattern_classe <- paste0(
    "^CLASSE:.*\\(",
    toupper(layer_name),
    "\\s*[-–]\\s*",
    class_code,
    "\\)"
  )

  idx <- which(
    grepl(
      pattern_classe,
      righe,
      ignore.case = TRUE
    )
  )

  if (length(idx) == 0) {
    return(NULL)
  }

  # The last occurrence is normally the actual specification,
  # after introduction and index.
  inizio <- max(idx)

  # ----------------------------------------------------------
  # Find end of class
  # ----------------------------------------------------------

  idx_next <- which(
    seq_along(righe) > inizio &
      grepl("^CLASSE:", righe)
  )

  if (length(idx_next) > 0) {
    fine <- min(idx_next) - 1
  } else {
    fine <- length(righe)
  }

  blocco_classe <- righe[
    inizio:fine
  ]

  # ----------------------------------------------------------
  # Enum attributes belonging to this class
  # ----------------------------------------------------------

  enum_classe <- attributi |>
    dplyr::filter(
      layer == layer_name,
      tipo == "Enum"
    )

  if (nrow(enum_classe) == 0) {
    return(NULL)
  }

  # ----------------------------------------------------------
  # Find domain headers
  # ----------------------------------------------------------

  idx_domini <- grep(
    "^Dominio\\s*\\(",
    blocco_classe,
    ignore.case = TRUE
  )

  if (length(idx_domini) == 0) {
    return(NULL)
  }

  risultati <- list()

  # ----------------------------------------------------------
  # Parse each domain
  # ----------------------------------------------------------

  for (j in seq_along(idx_domini)) {

    idx_dominio <- idx_domini[j]

    header <- blocco_classe[
      idx_dominio
    ]

    nome_dominio <- stringr::str_match(
      header,
      "Dominio\\s*\\(([^)]+)\\)"
    )[, 2]

    # --------------------------------------------------------
    # Determine domain end
    # --------------------------------------------------------

    pattern_attributo <- paste0(
      "^",
      class_code,
      "[0-9]{2,3}\\s+"
    )

    candidati_fine <- c(
      grep(
        "^Dominio\\s*\\(",
        blocco_classe,
        ignore.case = TRUE
      ),
      grep(
        pattern_attributo,
        blocco_classe
      ),
      grep(
        "^Componenti spaziali",
        blocco_classe,
        ignore.case = TRUE
      )
    )

    candidati_fine <- candidati_fine[
      candidati_fine > idx_dominio
    ]

    if (length(candidati_fine) > 0) {
      fine_dominio <- min(candidati_fine) - 1
    } else {
      fine_dominio <- length(blocco_classe)
    }

    if (fine_dominio <= idx_dominio) {
      next
    }

    dominio_lines <- blocco_classe[
      (idx_dominio + 1):fine_dominio
    ]

    # --------------------------------------------------------
    # Extract domain values
    # --------------------------------------------------------

    domain_rows <- dominio_lines[
      grepl(
        "^[0-9]{2}([0-9]{2})*\\s+",
        dominio_lines
      )
    ]

    if (length(domain_rows) == 0) {
      next
    }

    extracted <- stringr::str_match(
      domain_rows,
      "^([0-9]+)\\s+(.+)$"
    )

    valori <- tibble::tibble(
      layer = layer_name,
      codice_classe = class_code,
      dominio = nome_dominio,
      codice = extracted[, 2],
      descrizione_raw = stringr::str_squish(
        extracted[, 3]
      )
    )

    risultati[[length(risultati) + 1]] <- valori
  }

  if (length(risultati) == 0) {
    return(NULL)
  }

  dplyr::bind_rows(
    risultati
  )
}

# ============================================================
# EXTRACT DOMAINS FOR ALL DBSN CLASSES
# ============================================================

domini_list <- vector(
  "list",
  nrow(classi)
)

for (i in seq_len(nrow(classi))) {

  domini_list[[i]] <- parse_domini_classe(
    layer_name = classi$layer[i],
    class_code = classi$codice_classe[i],
    righe = righe,
    attributi = attributi
  )
}

domini <- dplyr::bind_rows(
  domini_list
)

# ============================================================
# DOMAIN CHECKS
# ============================================================

cat(
  "\nNumero valori di dominio:",
  nrow(domini),
  "\n"
)

cat(
  "Classi con domini:",
  dplyr::n_distinct(domini$layer),
  "\n"
)

cat(
  "Domini distinti:",
  dplyr::n_distinct(
    paste(
      domini$layer,
      domini$dominio
    )
  ),
  "\n"
)

duplicati_domini <- domini |>
  dplyr::count(
    layer,
    dominio,
    codice
  ) |>
  dplyr::filter(n > 1)

cat(
  "Valori di dominio duplicati:",
  nrow(duplicati_domini),
  "\n"
)

domini_senza_classe <- domini |>
  dplyr::filter(
    is.na(layer),
    is.na(codice_classe)
  )

cat(
  "Domini senza classe:",
  nrow(domini_senza_classe),
  "\n"
)

# ============================================================
# VALIDATION TEST: LOC_SG
# ============================================================

domini_test <- parse_domini_classe(
  layer_name = "loc_sg",
  class_code = "080101",
  righe = righe,
  attributi = attributi
)

stopifnot(
  nrow(domini_test) == 58
)

stopifnot(
  !any(
    grepl(
      "^080101",
      domini_test$codice
    )
  )
)

cat(
  "\nTest LOC_SG: OK -",
  nrow(domini_test),
  "valori di dominio\n"
)

# ============================================================
# LINK DOMAINS TO ENUM ATTRIBUTES
# ============================================================

enum_attributi <- attributi |>
  dplyr::filter(
    tipo == "Enum"
  ) |>
  dplyr::mutate(
    dominio_key = dplyr::case_when(
      attributo == "meta_ist" ~ "meta_ist",
      TRUE ~ stringr::str_to_lower(
        stringr::str_squish(descrizione)
      )
    )
  )

domini <- domini |>
  dplyr::mutate(
    dominio_key = stringr::str_to_lower(
      stringr::str_squish(dominio)
    )
  ) |>
  dplyr::left_join(
    enum_attributi |>
      dplyr::select(
        layer,
        attributo,
        codice_attributo,
        dominio_key
      ),
    by = c(
      "layer",
      "dominio_key"
    )
  ) |>
  dplyr::select(
    layer,
    codice_classe,
    attributo,
    codice_attributo,
    dominio,
    codice,
    descrizione_raw
  )

# ============================================================
# AUTOMATIC LINK CHECK
# ============================================================

domini_senza_attributo <- domini |>
  dplyr::filter(
    is.na(attributo)
  )

cat(
  "\nValori di dominio senza attributo prima delle correzioni:",
  nrow(domini_senza_attributo),
  "\n"
)

# ============================================================
# MANUAL DOMAIN-ATTRIBUTE MAPPING
# ============================================================
#
# In alcuni casi il nome del dominio nel PDF ufficiale non
# coincide con la descrizione dell'attributo Enum.
#
# Esempio:
#   EL_IDR_GRH ha descrizione "gerarchia"
#   ma il dominio è chiamato "Significatività".
# ============================================================

domain_map_fix <- tibble::tribble(
  ~layer,   ~dominio,                    ~attributo,

  "ar_nat", "Classificazione",            "ar_nat_ty",
  "ar_str", "Stato strade",               "ar_str_sta",
  "el_acq", "Destinazione",               "el_acq_des",
  "el_idr", "Significatività",            "el_idr_grh",
  "galler", "Galler_ty",                  "galler_ty",
  "lm_am",  "Tipo limite",                "lm_am_ty",
  "ln_btm", "Attendibilita",              "ln_btm_cer",
  "meta",   "Tipologia dell'estensione",  "meta_ty",
  "pt_btm", "Significatività alla scala", "pt_btm_sca",
  "tr_str", "Classe di larghezza",        "tr_str_cl",
  "tr_str", "Fondo stradale",             "tr_str_fon",
  "tralic", "Tipo traliccio",             "tral_ty"
)

# ============================================================
# ADD ATTRIBUTE CODES TO MANUAL MAPPING
# ============================================================

domain_map_fix <- domain_map_fix |>
  dplyr::left_join(
    attributi |>
      dplyr::select(
        layer,
        attributo,
        codice_attributo
      ),
    by = c(
      "layer",
      "attributo"
    )
  )

# Check that all manual mappings found an attribute
domain_map_fix_senza_codice <- domain_map_fix |>
  dplyr::filter(
    is.na(codice_attributo)
  )

cat(
  "Mappature manuali senza codice attributo:",
  nrow(domain_map_fix_senza_codice),
  "\n"
)

if (nrow(domain_map_fix_senza_codice) > 0) {
  print(
    domain_map_fix_senza_codice,
    n = Inf
  )
}

stopifnot(
  nrow(domain_map_fix_senza_codice) == 0
)

# ============================================================
# APPLY MANUAL DOMAIN-ATTRIBUTE MAPPING
# ============================================================

domini <- domini |>
  dplyr::left_join(
    domain_map_fix |>
      dplyr::rename(
        attributo_fix = attributo,
        codice_attributo_fix = codice_attributo
      ),
    by = c(
      "layer",
      "dominio"
    )
  ) |>
  dplyr::mutate(
    attributo = dplyr::coalesce(
      attributo,
      attributo_fix
    ),
    codice_attributo = dplyr::coalesce(
      codice_attributo,
      codice_attributo_fix
    )
  ) |>
  dplyr::select(
    layer,
    codice_classe,
    attributo,
    codice_attributo,
    dominio,
    codice,
    descrizione_raw
  ) |>
  dplyr::arrange(
    codice_classe,
    codice_attributo,
    codice
  )

# ============================================================
# FINAL DOMAIN CHECKS
# ============================================================

domini_senza_attributo <- domini |>
  dplyr::filter(
    is.na(attributo) |
      is.na(codice_attributo)
  )

cat(
  "\nValori di dominio senza attributo dopo le correzioni:",
  nrow(domini_senza_attributo),
  "\n"
)

if (nrow(domini_senza_attributo) > 0) {
  print(
    domini_senza_attributo |>
      dplyr::distinct(
        layer,
        dominio
      ),
    n = Inf
  )
}

duplicati_domini_finali <- domini |>
  dplyr::count(
    layer,
    attributo,
    codice
  ) |>
  dplyr::filter(
    n > 1
  )

cat(
  "Valori di dominio duplicati dopo il collegamento:",
  nrow(duplicati_domini_finali),
  "\n"
)

stopifnot(
  nrow(domini_senza_attributo) == 0
)

stopifnot(
  nrow(duplicati_domini_finali) == 0
)

# ============================================================
# PARENT CODES FOR HIERARCHICAL DOMAINS
# ============================================================

domini <- domini |>
  dplyr::mutate(
    codice = as.character(codice),

    parent_code = dplyr::case_when(
      nchar(codice) > 2 ~ substr(
        codice,
        1,
        nchar(codice) - 2
      ),
      TRUE ~ NA_character_
    )
  )

# ============================================================
# FINAL SUMMARY
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "DBSN PARSING SUMMARY\n"
)

cat(
  "============================================================\n"
)

cat(
  "Classi:",
  nrow(classi),
  "\n"
)

cat(
  "Attributi:",
  nrow(attributi),
  "\n"
)

cat(
  "Attributi Enum:",
  sum(attributi$tipo == "Enum"),
  "\n"
)

cat(
  "Valori di dominio:",
  nrow(domini),
  "\n"
)

cat(
  "Classi con domini:",
  dplyr::n_distinct(domini$layer),
  "\n"
)

cat(
  "Associazioni attributo-dominio:",
  domini |>
    dplyr::distinct(
      layer,
      attributo,
      dominio
    ) |>
    nrow(),
  "\n"
)

cat(
  "Classi senza attributi estratti:",
  nrow(classi_senza_attributi),
  "\n"
)

cat(
  "Valori dominio senza attributo:",
  nrow(domini_senza_attributo),
  "\n"
)

cat(
  "Duplicati dominio:",
  nrow(duplicati_domini_finali),
  "\n"
)

# ============================================================
# FINAL LOC_SG VALIDATION
# ============================================================

loc_sg_check <- domini |>
  dplyr::filter(
    layer == "loc_sg"
  )

stopifnot(
  nrow(loc_sg_check) == 58
)

stopifnot(
  setequal(
    unique(loc_sg_check$attributo),
    c(
      "loc_sg_sgn",
      "loc_sg_ty",
      "meta_ist"
    )
  )
)

cat(
  "\nControllo finale LOC_SG: OK\n"
)

print(
  loc_sg_check |>
    dplyr::distinct(
      attributo,
      dominio
    ) |>
    dplyr::arrange(
      attributo
    ),
  n = Inf
)

# ============================================================
# BUILD FINAL DBSN CATALOGUE
# ============================================================

catalogo_dbsn <- list(
  classi = classi,
  attributi = attributi,
  domini = domini,
  classi_senza_attributi = classi_senza_attributi
)

# ============================================================
# SAVE INTERNAL PACKAGE DATA
# ============================================================

usethis::use_data(
  catalogo_dbsn,
  internal = TRUE,
  overwrite = TRUE
)

cat(
  "\nCatalogo DBSN salvato in R/sysdata.rda\n"
)
