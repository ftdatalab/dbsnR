
# --> installing libraries

options(download.file.method = "wininet")
#install.packages("pak")

# --> devtools

devtools::load_all("~/dbsnR")

#--> tests

loc_sg <- dbsn_catalog("loc_sg")
