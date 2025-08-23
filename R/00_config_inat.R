# LOAD PACKAGES ----
if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)}

if(!isTRUE(require(sf, quietly = TRUE))) {
  install.packages("sf", dependencies = TRUE); library(sf)
} else {
  require(sf)}

if(!isTRUE(require(sp, quietly = TRUE))) {
  install.packages("sp", dependencies = TRUE); library(sp)
} else {
  require(sp)}

if(!isTRUE(require(proj4, quietly = TRUE))) {
  install.packages("proj4", dependencies = TRUE); library(proj4)
} else {
  require(proj4)}

if(!isTRUE(require(matrixStats, quietly = TRUE))) {
  install.packages("matrixStats", dependencies = TRUE); library(matrixStats)
} else {
  require(matrixStats)}

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

if(!isTRUE(require(lubridate, quietly = TRUE))) {
  install.packages("lubridate", dependencies = TRUE); library(lubridate)
} else {
  require(lubridate)}

if(!isTRUE(require(condformat, quietly = TRUE))) {
  install.packages("condformat", dependencies = TRUE); library(condformat)
} else {
  require(condformat)}

if(!isTRUE(require(openxlsx, quietly = TRUE))) {
  install.packages("openxlsx", dependencies = TRUE); library(openxlsx)
} else {
  require(openxlsx)}

if(!isTRUE(require(raster, quietly = TRUE))) {
  install.packages("raster", dependencies = TRUE); library(raster)
} else {
  require(raster)}

if(!isTRUE(require(stars, quietly = TRUE))) {
  install.packages("stars", dependencies = TRUE); library(stars)
} else {
  require(stars)}

if(!isTRUE(require(ggsn, quietly = TRUE))) {
  install.packages("ggsn", dependencies = TRUE); library(ggsn)
} else {
  require(ggsn)}

if(!isTRUE(require(zoo, quietly = TRUE))) {
  install.packages("zoo", dependencies = TRUE); library(zoo)
} else {
  require(zoo)}

if(!isTRUE(require(rgdal, quietly = TRUE))) {
  install.packages("rgdal", dependencies = TRUE); library(rgdal)
} else {
  require(rgdal)}

if(!isTRUE(require(ggsn, quietly = TRUE))) {
  install.packages("ggsn", dependencies = TRUE); library(ggsn)
} else {
  require(ggsn)}

if(!isTRUE(require(grid, quietly = TRUE))) {
  install.packages("grid", dependencies = TRUE); library(grid)
} else {
  require(grid)}

if(!isTRUE(require(gridGraphics, quietly = TRUE))) {
  install.packages("gridGraphics", dependencies = TRUE); library(gridGraphics)
} else {
  require(gridGraphics)}

write_csv2_win1250 <- function(x, path, ...) {
  # write UTF-8 to a temporary file
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv2(x, tmp, ...)
  
  # re-encode to Windows-1250
  txt <- readr::read_file(tmp)
  txt <- iconv(txt, from = "UTF-8", to = "Windows-1250")
  readr::write_file(txt, path)
}

# LOAD DATA ----
#--------------------------------------------------#
## Load remote data -----
#--------------------------------------------------#
# Borders of Czechia
czechia_border <- 
  RCzechia::republika(
    resolution = "high"
  ) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) 

# Data on protected areas and mapping fields
endpoint <- "http://gis.nature.cz/arcgis/services/Aplikace/Opendata/MapServer/WFSServer?"
caps_url <- base::paste0(endpoint, "request=GetCapabilities&service=WFS")

layer_name_evl <- "Opendata:Evropsky_vyznamne_lokality"
layer_name_mzchu <- "Opendata:Maloplosna_zvlaste_chranena_uzemi__MZCHU_"
layer_name_sitmap1rad <- "Opendata:Mapovaci_sit_-_deleni_1.radu"

getfeature_url_evl <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_evl
)
getfeature_url_mzchu <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_mzchu
)
getfeature_url_sitmap1rad <- paste0(
  endpoint,
  "service=WFS&version=2.0.0&request=GetFeature&typeName=", layer_name_sitmap1rad
)


evl <- sf::st_read(getfeature_url_evl) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) 
mzchu <- sf::st_read(getfeature_url_mzchu) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  )
sitmap <- sf::st_read(getfeature_url_sitmap1rad) %>%
  sf::st_transform(
    ., 
    st_crs("+init=epsg:5514")
  ) %>%
  sf::st_crop(
    .,
    czechia_border  # crop by the border of Czechia
  ) %>%
  dplyr::select(
    POLE
  ) %>%
  dplyr::rename(
    SITMAP_1 = POLE
  )

# Sites where the species are target features
rn2kcz::load_n2k_sites()

# load occurrence data ----
## Coleoptera ----
data_luccer <- read.csv2(
  "Data/Input/nalezy-luccer.csv",
  fileEncoding = "Windows-1250"
  )
data_cuccin <- read.csv2(
  "Data/Input/nalezy-cuccin.csv", 
  fileEncoding = "Windows-1250"
  )

data_evd <- dplyr::bind_rows(
  data_luccer,
  data_cuccin
  ) %>%
  dplyr::mutate(
    DATUM_OD = lubridate::ymd(DATUM_OD),
    DATUM_DO = lubridate::ymd(DATUM_OD),
    NEGATIVNI = dplyr::case_when(
      NEGATIV == "ne" ~ 0,
      NEGATIV == "ano" ~ 1
    )
    )%>%
  sf::st_as_sf(
    ., 
    coords = c("X", "Y"), 
    crs = "+init=epsg:5514"
    ) %>%
  sf::st_intersection(
    .,
    sitmap
  ) %>%
  dplyr::filter(
    DATUM_OD >= lubridate::ymd("2013-01-01")
  )

## Hymenoptera ----
data_osmia <- read.csv2(
  "Data/Input/nalezy-osmia.csv", 
  fileEncoding = "Windows-1250"
  )
data_andrena <- read.csv2(
  "Data/Input/nalezy-andrena.csv", 
  fileEncoding = "Windows-1250"
  )
data_halictus <- read.csv2(
  "Data/Input/nalezy-halictus.csv", 
  fileEncoding = "Windows-1250"
)
data_bombus <- read.csv2(
  "Data/Input/nalezy-bombus.csv", 
  fileEncoding = "Windows-1250"
)
data_hym <- dplyr::bind_rows(
  data_osmia,
  data_andrena,
  data_halictus,
  data_bombus
  ) %>%
  dplyr::mutate(
    DATUM_OD = lubridate::ymd(DATUM_OD),
    DATUM_DO = lubridate::ymd(DATUM_OD),
    NEGATIVNI = dplyr::case_when(
      NEGATIV == "ne" ~ 0,
      NEGATIV == "ano" ~ 1
      )
    )%>%
  sf::st_as_sf(
    ., 
    coords = c("X", "Y"), 
    crs = "+init=epsg:5514"
    ) %>%
  sf::st_intersection(
    .,
    sitmap
  ) %>%
  dplyr::filter(
    DATUM_OD >= lubridate::ymd("2013-01-01")
  )

data_hym_rl <- read.csv2(
  "Data/Input/nalezy-hym-rl.csv", 
  fileEncoding = "Windows-1250"
  ) %>%
  dplyr::mutate(
    DATUM_OD = lubridate::ymd(DATUM_OD),
    DATUM_DO = lubridate::ymd(DATUM_OD),
    NEGATIVNI = dplyr::case_when(
      NEGATIV == "ne" ~ 0,
      NEGATIV == "ano" ~ 1
      )
    )%>%
  sf::st_as_sf(
    ., 
    coords = c("X", "Y"),
    crs = "+init=epsg:5514"
    ) %>%
  sf::st_intersection(
    .,
    sitmap
  ) %>%
  dplyr::filter(
    DATUM_OD >= lubridate::ymd("2013-01-01")
  )

## Diptera ----
data_bombilius <- read.csv2(
  "Data/Input/nalezy-bombilius.csv", 
  fileEncoding = "Windows-1250"
)

data_dip <- dplyr::bind_rows(
  data_bombilius
) %>%
  dplyr::mutate(
    DATUM_OD = lubridate::ymd(DATUM_OD),
    DATUM_DO = lubridate::ymd(DATUM_OD),
    NEGATIVNI = dplyr::case_when(
      NEGATIV == "ne" ~ 0,
      NEGATIV == "ano" ~ 1
    )
  ) %>%
  sf::st_as_sf(
    ., 
    coords = c("X", "Y"), 
    crs = "+init=epsg:5514"
  ) %>%
  sf::st_intersection(
    .,
    sitmap
  ) %>%
  dplyr::filter(
    DATUM_OD >= lubridate::ymd("2013-01-01")
    )
