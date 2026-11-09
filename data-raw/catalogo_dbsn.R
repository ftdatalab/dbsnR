# ============================================================
# DBSN CATALOGUE
# Internal data used by dbsnR
# ============================================================

# ============================================================
# 1. DBSN CLASSES
# ============================================================

classi <- tibble::tribble(
  ~strato, ~tema, ~codice_classe, ~layer, ~classe,

  "00", "0001", "000101", "v_rete", "Vertice di rete",
  "00", "0001", "000102", "caposd", "Caposaldo",
  "00", "0002", "000202", "meta", "Ambito omogeneo per la metainformazione",

  "01", "0101", "010104", "ar_str", "Area stradale",
  "01", "0101", "010105", "ar_vms", "Viabilità mista secondaria",
  "01", "0101", "010109", "tr_str", "Tratto stradale",
  "01", "0101", "010110", "iz_str", "Intersezione stradale",
  "01", "0101", "010115", "rt_st2", "Rete stradale livello 2",
  "01", "0101", "010116", "el_vms", "Elemento di viabilità mista secondaria",

  "01", "0102", "010201", "sd_fer", "Sede di trasporto su ferro",
  "01", "0102", "010202", "el_fer", "Elemento ferroviario",
  "01", "0102", "010203", "gz_fer", "Giunzione ferroviaria",
  "01", "0102", "010204", "el_trv", "Elemento tranviario",
  "01", "0102", "010205", "gz_trv", "Giunzione tranviaria",
  "01", "0102", "010206", "el_met", "Elemento di metropolitana",
  "01", "0102", "010207", "gz_met", "Giunzione di metropolitana",
  "01", "0102", "010208", "el_fun", "Elemento funicolare",
  "01", "0102", "010209", "gz_fun", "Giunzione funicolare",
  "01", "0102", "010210", "bi_ind", "Binario industriale",
  "01", "0102", "010211", "rt_fer", "Rete ferroviaria",
  "01", "0102", "010213", "rt_met", "Rete metropolitana",

  "01", "0103", "010301", "el_fne", "Elemento di trasporto a fune",
  "01", "0103", "010302", "el_acq", "Elemento di trasporto su acqua",

  "02", "0201", "020102", "edifc", "Edificio",
  "02", "0201", "020106", "edi_min", "Edificio minore",

  "02", "0202", "020201", "mn_ind", "Manufatto industriale",
  "02", "0202", "020202", "mn_mau", "Manufatto monumentale e di arredo urbano",
  "02", "0202", "020204", "attr_sp", "Attrezzatura sportiva",
  "02", "0202", "020205", "man_tr", "Manufatto di infrastruttura di trasporto",
  "02", "0202", "020206", "aatt", "Area attrezzata del suolo",
  "02", "0202", "020207", "tralic", "Sostegno a traliccio",
  "02", "0202", "020210", "mu_div", "Muro o divisione",
  "02", "0202", "020214", "mn_int", "Localizzazione manufatto industriale/trasporto",

  "02", "0203", "020301", "ponte", "Ponte/viadotto/cavalcavia",
  "02", "0203", "020303", "galler", "Galleria",

  "02", "0204", "020401", "mu_sos", "Muro di sostegno e ritenuta",

  "02", "0205", "020501", "diga", "Diga",
  "02", "0205", "020502", "argine", "Argine",
  "02", "0205", "020503", "op_reg", "Opera idraulica di regolazione",
  "02", "0205", "020505", "op_por", "Opera portuale e difesa delle coste",

  "04", "0401", "040101", "ab_cda", "Area bagnata di corso d'acqua",
  "04", "0401", "040102", "sp_acq", "Specchio d'acqua",
  "04", "0401", "040103", "invaso", "Invaso artificiale",
  "04", "0401", "040104", "af_acq", "Affioramento naturale dell'acqua",
  "04", "0401", "040105", "cascata", "Cascata",

  "04", "0402", "040201", "cs_mar", "Linea di costa marina cartografica",
  "04", "0402", "040202", "ar_mar", "Area di mare",
  "04", "0402", "040205", "ar_int", "Area intercotidale",

  "04", "0403", "040301", "ghi_nv", "Ghiacciaio/nevaio perenne",

  "04", "0404", "040401", "el_idr", "Elemento idrico",
  "04", "0404", "040402", "condot", "Condotta",
  "04", "0404", "040403", "nd_idr", "Nodo idrico",
  "04", "0404", "040408", "rt_idr", "Reticolo idrografico",

  "05", "0501", "050101", "cv_liv", "Curva di livello",
  "05", "0501", "050102", "pt_quo", "Punto quotato",

  "05", "0502", "050201", "ln_btm", "Curva batimetrica",
  "05", "0502", "050202", "pt_btm", "Punto batimetrico",

  "05", "0503", "050301", "f_nter", "Forma naturale del terreno",
  "05", "0503", "050302", "scarpt", "Scarpata",
  "05", "0503", "050303", "sc_dis", "Area di scavo o discarica",
  "05", "0503", "050305", "alveo", "Alveo",

  "05", "0504", "050401", "z_tin", "TIN",

  "06", "0601", "060101", "bosco", "Bosco",
  "06", "0601", "060105", "ps_inc", "Pascolo o incolto",
  "06", "0601", "060106", "cl_agr", "Coltura agricola",
  "06", "0604", "060401", "ar_vrd", "Area verde",

  "07", "0701", "070101", "rt_aac", "Rete approvvigionamento idrico",
  "07", "0701", "070102", "nd_aac", "Nodo rete approvvigionamento idrico",
  "07", "0703", "070301", "tr_ele", "Tratto rete elettrica",
  "07", "0703", "070302", "nd_ele", "Nodo rete elettrica",
  "07", "0704", "070401", "tr_gas", "Tratto rete gas",
  "07", "0704", "070402", "nd_gas", "Nodo rete gas",
  "07", "0706", "070601", "tr_ole", "Tratto oleodotto",
  "07", "0706", "070602", "nd_ole", "Nodo oleodotto",

  "08", "0801", "080101", "loc_sg", "Località significativa",
  "08", "0802", "080201", "scr_cr", "Scritta cartografica",

  "09", "0901", "090101", "comune", "Comune",
  "09", "0901", "090105", "provin", "Provincia",
  "09", "0901", "090106", "region", "Regione",
  "09", "0901", "090107", "acq_ter", "Acqua territoriale",
  "09", "0901", "090108", "acq_in", "Acqua interna",
  "09", "0901", "090109", "stato", "Stato",
  "09", "0901", "090110", "lm_am", "Limiti amministrativi",
  "09", "0901", "090111", "a_scom", "Suddivisione sub-comunale",
  "09", "0901", "090112", "cm_mon", "Comunità montana",
  "09", "0901", "090113", "ar_nat", "Area naturale protetta",

  "10", "1001", "100101", "sv_str", "Area a servizio stradale",
  "10", "1001", "100102", "sv_fer", "Area a servizio del trasporto su ferro",
  "10", "1001", "100103", "sv_por", "Area a servizio portuale",
  "10", "1001", "100104", "sv_aer", "Area a servizio aeroportuale",
  "10", "1002", "100201", "pe_uins", "Unità insediativa",
  "10", "1003", "100302", "cv_aes", "Area estrattiva",
  "10", "1003", "100303", "cv_dis", "Discarica"
)

# ============================================================
# 2. META_IST
# ============================================================

meta_ist <- tibble::tribble(
  ~codice, ~descrizione,

  "01", "IGM",
  "0101", "EuroGeographics",
  "02", "Ortofoto AGEA",
  "03", "OpenStreetMap",
  "04", "Database regionale / CTR / geoportale regionale",
  "05", "Pubblica Amministrazione",
  "0501", "CORINE",
  "0502", "ISPRA",
  "050201", "Carta Natura ISPRA",
  "0503", "ISTAT",
  "0504", "Agenzia delle Entrate 2017",
  "06", "Comitato Glaciologico Italiano",
  "07", "Open Data Web",
  "20", "IGM + ISTAT + Agenzia Entrate",
  "21", "OSM + Agenzia Entrate",
  "22", "IGM + DB regionali + ISTAT",
  "23", "Ortofoto AGEA + OSM"
)

# ============================================================
# 3. NULL VALUES
# ============================================================

valori_nulli <- tibble::tribble(
  ~codice, ~descrizione,

  "91", "Non conosciuto",
  "93", "Non definito",
  "94", "Non applicabile"
)

# ============================================================
# 4. CODED DOMAINS
# ============================================================
#
# IMPORTANTE:
# i codici devono rimanere character.
# "0301" NON deve diventare 301.
#

domini <- tibble::tribble(
  ~layer, ~attributo, ~codice, ~descrizione,

  # ==========================================================
  # INCOLLA QUI IL BLOCCO "domini" DEL TUO SCRIPT ATTUALE
  # ==========================================================

  "loc_sg", "loc_sg_sgn", "01", "Principale / primo ordine",
  "loc_sg", "loc_sg_sgn", "02", "Secondario / secondo ordine",
  "loc_sg", "loc_sg_sgn", "03", "Terzo ordine",
  "loc_sg", "loc_sg_sgn", "04", "Quarto ordine",
  "loc_sg", "loc_sg_sgn", "05", "Quinto ordine",
  "loc_sg", "loc_sg_sgn", "06", "Ordine superiore al quinto",
  "loc_sg", "loc_sg_sgn", "91", "Non conosciuto",

  "loc_sg", "loc_sg_ty", "01", "Località",
  "loc_sg", "loc_sg_ty", "0101", "Capoluogo",
  "loc_sg", "loc_sg_ty", "010101", "Capitale di Stato",
  "loc_sg", "loc_sg_ty", "010102", "Capoluogo di regione",
  "loc_sg", "loc_sg_ty", "010103", "Capoluogo di provincia",
  "loc_sg", "loc_sg_ty", "010104", "Capoluogo di comune",
  "loc_sg", "loc_sg_ty", "01010401", "Comune >= 50000 abitanti",
  "loc_sg", "loc_sg_ty", "01010402", "Comune da 10000 a 49999 abitanti",
  "loc_sg", "loc_sg_ty", "01010403", "Comune < 10000 abitanti",
  "loc_sg", "loc_sg_ty", "0102", "Centro abitato",
  "loc_sg", "loc_sg_ty", "010201", "Centro abitato > 1000 abitanti",
  "loc_sg", "loc_sg_ty", "010202", "Centro abitato < 1000 abitanti",
  "loc_sg", "loc_sg_ty", "0103", "Nucleo abitato",
  "loc_sg", "loc_sg_ty", "0104", "Case sparse",

  "loc_sg", "loc_sg_ty", "02", "Area geografica",
  "loc_sg", "loc_sg_ty", "0201", "Monti, massicci, murge, vulcani, contrafforti",
  "loc_sg", "loc_sg_ty", "0202", "Altopiani",
  "loc_sg", "loc_sg_ty", "0203", "Valli / vallate",
  "loc_sg", "loc_sg_ty", "0204", "Colli",
  "loc_sg", "loc_sg_ty", "0205", "Passi, selle, valichi",
  "loc_sg", "loc_sg_ty", "0206", "Creste",
  "loc_sg", "loc_sg_ty", "0207", "Gole, crateri, doline, depressioni",
  "loc_sg", "loc_sg_ty", "0208", "Promontori",
  "loc_sg", "loc_sg_ty", "0209", "Scogli / secche",
  "loc_sg", "loc_sg_ty", "0210", "Foci / bocche",
  "loc_sg", "loc_sg_ty", "0211", "Coste, cale, golfi, stretti",
  "loc_sg", "loc_sg_ty", "0212", "Isole",
  "loc_sg", "loc_sg_ty", "0213", "Aree umide",
  "loc_sg", "loc_sg_ty", "021301", "Palude",
  "loc_sg", "loc_sg_ty", "021302", "Torbiera",
  "loc_sg", "loc_sg_ty", "021303", "Valle",
  "loc_sg", "loc_sg_ty", "0301", "Antichità / particolare di interesse archeologico",
  "loc_sg", "loc_sg_ty", "93", "Non definito",
  "loc_sg", "loc_sg_ty", "95", "Altro"

  # QUI CONTINUA CON:
  #
  # pe_uins
  # edi_min
  # mn_mau
  # mu_div
  # el_vms
  # af_acq
  # pt_quo
  # f_nter
  # cl_agr
  # bosco
  # alveo
  # el_idr
  #
  # esattamente come nel tuo catalogo originale.
) |>
  dplyr::mutate(
    layer = tolower(.data$layer),
    attributo = tolower(.data$attributo),
    codice = as.character(.data$codice),

    parent_code = dplyr::case_when(
      nchar(.data$codice) > 2 ~
        substr(
          .data$codice,
          1,
          nchar(.data$codice) - 2
        ),
      TRUE ~ NA_character_
    )
  )

# ============================================================
# 5. MAIN ATTRIBUTES
# ============================================================

attributi <- tibble::tribble(
  ~layer, ~attributo, ~descrizione, ~tipo,

  "loc_sg", "loc_sg_sgn", "Significatività", "Enum",
  "loc_sg", "loc_sg_top", "Toponimo", "String",
  "loc_sg", "loc_sg_ty", "Tipo", "Enum",
  "loc_sg", "meta_ist", "Fonte del dato", "Enum",

  "pe_uins", "pe_uins_nm", "Nome", "String",
  "pe_uins", "pe_uins_pa", "Patrimonialità", "Enum",
  "pe_uins", "pe_uins_ty", "Tipo", "Enum",
  "pe_uins", "meta_ist", "Fonte del dato", "Enum",

  "edi_min", "edi_min_at", "Altezza", "Real",
  "edi_min", "edi_min_nm", "Nome", "String",
  "edi_min", "edi_min_st", "Stato", "Enum",
  "edi_min", "edi_min_ty", "Tipologia edilizia", "Enum",
  "edi_min", "edi_min_pr", "Struttura precaria", "Enum",
  "edi_min", "meta_ist", "Fonte del dato", "Enum",

  "mn_mau", "mn_mau_nom", "Nome", "String",
  "mn_mau", "mn_mau_ty", "Tipo", "Enum",
  "mn_mau", "meta_ist", "Fonte del dato", "Enum",

  "mu_div", "mu_div_at", "Altezza", "Real",
  "mu_div", "mu_div_ty", "Tipo", "Enum",
  "mu_div", "meta_ist", "Fonte del dato", "Enum",

  "pt_quo", "pt_quo_nam", "Nome", "String",
  "pt_quo", "pt_quo_q", "Quota", "Real",
  "pt_quo", "pt_quo_ril", "Tipo acquisizione", "Enum",
  "pt_quo", "pt_quo_sca", "Significatività alla scala", "Enum",
  "pt_quo", "pt_quo_sed", "Sede", "Enum",
  "pt_quo", "meta_ist", "Fonte del dato", "Enum",

  "af_acq", "af_acq_nom", "Nome", "String",
  "af_acq", "af_acq_ty", "Tipo", "Enum",
  "af_acq", "af_acq_cap", "Captazione", "Enum",
  "af_acq", "af_acq_ter", "Termalità", "Enum",
  "af_acq", "meta_ist", "Fonte del dato", "Enum",

  "f_nter", "f_nter_nom", "Nome", "String",
  "f_nter", "f_nter_ty", "Tipo", "Enum",
  "f_nter", "meta_ist", "Fonte del dato", "Enum",

  "cl_agr", "cl_agr_ty", "Tipo coltura", "Enum",
  "cl_agr", "cl_agr_tz", "Terrazzamento", "Enum",
  "cl_agr", "meta_ist", "Fonte del dato", "Enum",

  "el_idr", "el_idr_nom", "Nome", "String",
  "el_idr", "el_idr_ty", "Tipo", "Enum",
  "el_idr", "el_idr_rg", "Regime idrologico", "Enum",
  "el_idr", "el_idr_art", "Stato alveo", "Enum",
  "el_idr", "el_idr_grh", "Gerarchia", "Enum",
  "el_idr", "meta_ist", "Fonte del dato", "Enum"
) |>
  dplyr::mutate(
    layer = tolower(.data$layer),
    attributo = tolower(.data$attributo)
  )

# ============================================================
# 6. BUILD INTERNAL CATALOGUE
# ============================================================

catalogo_dbsn <- list(
  classi = classi,
  attributi = attributi,
  domini = domini,
  meta_ist = meta_ist,
  valori_nulli = valori_nulli
)

# ============================================================
# 7. SAVE AS INTERNAL PACKAGE DATA
# ============================================================

usethis::use_data(
  catalogo_dbsn,
  internal = TRUE,
  overwrite = TRUE
)
