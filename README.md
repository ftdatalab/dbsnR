
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dbsnR

<!-- badges: start -->

<!-- badges: end -->

`dbsnR` is an R package for working with the Italian Database di Sintesi
Nazionale (DBSN).

The package provides a structured workflow to download, cache, inspect,
merge, filter, crop, analyse, and export DBSN spatial data using `sf`.

The main goal of `dbsnR` is to simplify repetitive DBSN operations while
keeping the resulting spatial objects fully compatible with the standard
R spatial ecosystem.

## Installation

You can install the development version of `dbsnR` from GitHub with:

``` r
# install.packages("pak")
pak::pak("ftdatalab/dbsnR")
```

## Basic workflow

A typical `dbsnR` workflow consists of:

``` text
Download
   |
   v
Cache
   |
   v
Inspect
   |
   v
Merge
   |
   v
Crop / Filter / Select
   |
   v
Spatial analysis
   |
   v
Export
```

## Download DBSN data

Provincial DBSN datasets can be downloaded directly:

``` r
dbsn_download("RI")
dbsn_download("AQ")
dbsn_download("RM")
```

Downloaded datasets are stored in the local `dbsnR` cache and can be
reused without downloading them again.

The cache location can be inspected with:

``` r
dbsn_cache_dir()
```

## Explore available data

Available layers can be inspected without loading all geometries into
memory:

``` r
dbsn_layers(
  province = c("RI", "AQ", "RM")
)
```

Detailed information about a specific layer can be obtained with:

``` r
dbsn_info(
  "loc_sg",
  province = c("RI", "AQ", "RM")
)
```

## Merge provinces

The same DBSN layer can be merged across several provincial
geodatabases:

``` r
loc_sg <- dbsn_merge(
  layer = "loc_sg",
  province = c(
    "RI",
    "AP",
    "AQ",
    "PG",
    "RM",
    "TE",
    "TR",
    "VT"
  )
)
```

The result is a standard `sf` object.

## Filter and select data

Features can be filtered using their attributes:

``` r
rieti <- dbsn_filter(
  loc_sg,
  province = "RI"
)
```

Selected thematic layers can be extracted from a complete DBSN dataset:

``` r
territory <- dbsn_select(
  x = dbsn,
  layers = c(
    "loc_sg",
    "edifc",
    "tr_str",
    "bosco",
    "cl_agr"
  )
)
```

## Study areas

Buffers can be created around spatial objects:

``` r
study_area <- dbsn_buffer(
  x = study_point,
  distance = 20000
)
```

DBSN data can then be cropped to the study area:

``` r
territory_crop <- dbsn_crop(
  x = territory,
  area = study_area
)
```

## Spatial operations

`dbsnR` provides several functions for spatial operations and analysis.

Examples include:

``` text
dbsn_intersect()     spatial intersection
dbsn_join()          spatial join
dbsn_difference()    geometric difference
dbsn_union()         geometric union
dbsn_corridor()      corridor around linear features
dbsn_nearest()       nearest spatial feature
```

## Spatial measurements

Lengths can be calculated with:

``` r
roads <- dbsn_length(tr_str)
```

Areas can be calculated with:

``` r
areas <- dbsn_area(bosco)
```

## Locate features along a line

`dbsn_along()` determines the position of features along a reference
LINESTRING.

``` r
along <- dbsn_along(
  line = road,
  objects = loc_sg
)

along[, c(
  "distance_m",
  "distance_km",
  "distance_pct"
)]
```

The position is measured from the start of the reference line.

The direction of the LINESTRING therefore determines the origin of the
measurement.

## Distance analysis

`dbsn_distance()` calculates the minimum distance from every feature in
one layer to the nearest feature in another layer.

``` r
distance <- dbsn_distance(
  x = loc_sg,
  y = tr_str
)

distance[, c(
  "nearest_id",
  "distance_m",
  "distance_km"
)]
```

The output preserves the original `sf` geometry and adds the index of
the nearest reference feature together with the distance in metres and
kilometres.

## Search DBSN data

DBSN features can be searched using text patterns:

``` r
orvinio <- dbsn_search(
  x = loc_sg,
  pattern = "Orvinio"
)
```

## Summarise a layer

A compact summary of a DBSN layer can be obtained with:

``` r
dbsn_summary(
  loc_sg,
  layer = "loc_sg"
)
```

## Interactive mapping

DBSN spatial layers can be displayed on interactive Leaflet maps using
`dbsn_map()`.

The function automatically supports point, line, and polygon geometries.

``` r
dbsn_map(
  loc_sg,
  basemap = "topo",
  hover = "loc_sg_top"
)
```

Features can also be coloured using a numeric or categorical attribute.

For example, contour lines can be coloured according to elevation:

``` r
dbsn_map(
  cv_liv,
  basemap = "topo",
  color_by = "cv_liv_q",
  hover = "cv_liv_q"
)
```

Multiple spatial layers can be displayed together with
`dbsn_map_layers()`:

``` r
dbsn_map_layers(
  cv_orvinio,
  orvinio,
  orvinio_5km,
  basemap = "topo",
  layer_names = c(
    "Contour lines",
    "Orvinio",
    "5 km buffer"
  ),
  colors = c(
    "#8C510A",
    "#E31A1C",
    "#2166AC"
  )
)
```

Each layer can be independently shown or hidden using the Leaflet layer
control.

## Export

A single `sf` layer can be exported to a GeoPackage:

``` r
dbsn_export(
  x = loc_sg,
  file = "loc_sg.gpkg"
)
```

A named list of spatial layers can also be exported to the same
GeoPackage:

``` r
dbsn_export(
  x = territory_crop,
  file = "study_area_dbsn.gpkg"
)
```

## Documentation

A complete DBSN workflow is available in the package vignette:

``` r
vignette(
  "dbsn-workflow",
  package = "dbsnR"
)
```

`dbsnR` functions are designed to return standard `sf` objects whenever
possible, allowing package workflows to be combined directly with `sf`,
`dplyr`, `ggplot2`, `leaflet`, QGIS, and other spatial tools.
