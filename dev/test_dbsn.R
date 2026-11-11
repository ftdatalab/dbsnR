
# --> installing libraries

options(download.file.method = "wininet")
#install.packages("pak")

# --> devtools

devtools::document()
devtools::load_all()
devtools::load_all("~/dbsnR")
devtools::check()
devtools::test()
devtools::build()

#--> tests

dbsn_catalog()

loc_sg <- dbsn_catalog("loc_sg")

#dbsn_cache_path("TR", create = TRUE)

#sources <- dbsn_sources(verify_ssl = FALSE)

gdb <- dbsn_download("TE",verify_ssl = FALSE,overwrite = FALSE)

loc_sg <- dbsn_merge("loc_sg")

#-->

area <- sf::st_bbox(
  c(
    xmin = 12.8,  # longitudine Ovesthttp://127.0.0.1:25713/graphics/b96439bd-dece-48a5-8d12-58e04d9bc4fd.png
    ymin = 42.3,  # latitudine Sud
    xmax = 13.1,  # longitudine Est
    ymax = 42.4   # latitudine Nord
  ),
  crs = sf::st_crs(4326)
)

dbsn_cut <- dbsn_crop(
  x=loc_sg,
  area = area
)

province_rieti <- c(
  "RI", "AP", "AQ", "PG",
  "RM", "TE", "TR", "VT"
)

layers <- dbsn_layers(
  province = province_rieti
)

head(layers)
nrow(layers)

dbsn_layers(
  province = "RI"
)

info_bosco <- dbsn_info(
  "bosco",
  province = province_rieti
)

info_bosco

loc_sg <- dbsn_merge(
  layer = "loc_sg",
  province = province_rieti
)

dbsn <- dbsn_merge(
  province = province_rieti
)

territorio <- dbsn_select(
  x = dbsn,
  layers = c(
    "loc_sg",
    "edifc",
    "tr_str",
    "el_vms",
    "bosco",
    "cl_agr",
    "scarpt"
  )
)

territorio_cut <- dbsn_crop(
  x = territorio,
  area = area_buffer
)

dbsn_export(
  x = territorio_cut,
  file = "output/orvinio_dbsn.gpkg"
)

loc_sg_decoded <- dbsn_decode(
  x = loc_sg,
  layer = "loc_sg",
  attributes = c(
    "loc_sg_ty",
    "loc_sg_sgn",
    "meta_ist"
  )
)


