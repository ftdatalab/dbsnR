
# --> installing libraries

options(download.file.method = "wininet")
#install.packages("pak")

# --> devtools

devtools::document()
devtools::load_all()
devtools::load_all("~/dbsnR")
devtools::test()
devtools::build()

#--> tests

loc_sg <- dbsn_catalog("loc_sg")


dbsn_cache_path("RI", create = TRUE)
