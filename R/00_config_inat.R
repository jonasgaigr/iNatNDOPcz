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


# LOAD DATA ----
# VRSTVA EVL 


# VRSTVA HRANIC CZ 
czechia <- st_read("//bali.nature.cz/du/SpravniCleneni/CR/HraniceCR.shp")
czechia <- st_transform(czechia, CRS("+init=epsg:5514"))
czechia_line <- st_cast(czechia, "LINESTRING")

# seznam EVL a predmetu ochrany ----
rn2kcz::load_n2k_sites()
sites_habitats <- sites_subjects %>%
  filter(feature_type == "stanoviště")

# load ndop data ----
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
    DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
    DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y'),
    NEGATIVNI = dplyr::case_when(
      NEGATIV == "ne" ~ 0,
      NEGATIV == "ano" ~ 1
    )
    )%>%
  sf::st_as_sf(
    ., 
    coords = c("X", "Y"), 
    crs = "+init=epsg:5514"
    )

data_osmia <- read.csv2(
  "Data/Input/nalezy-osmia.csv", 
  fileEncoding = "Windows-1250"
  )
data_andrena <- read.csv2(
  "Data/Input/nalezy-andrena.csv", 
  fileEncoding = "Windows-1250"
  )
data_hym <- dplyr::bind_rows(
  data_osmia,
  data_andrena
  ) %>%
  dplyr::mutate(
    DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
    DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y'),
    NEGATIVNI = dplyr::case_when(
      NEGATIV == "ne" ~ 0,
      NEGATIV == "ano" ~ 1
      )
    )%>%
  sf::st_as_sf(
    ., 
    coords = c("X", "Y"), 
    crs = "+init=epsg:5514"
    )

data_hym_rl <- read.csv2(
  "Data/Input/nalezy-hym-rl.csv", 
  fileEncoding = "Windows-1250"
  ) %>%
  dplyr::mutate(
    DATUM_OD = as.Date(DATUM_OD, format = '%d.%m.%Y'),
    DATUM_DO = as.Date(DATUM_DO, format = '%d.%m.%Y'),
    NEGATIVNI = dplyr::case_when(
      NEGATIV == "ne" ~ 0,
      NEGATIV == "ano" ~ 1
      )
    )%>%
  sf::st_as_sf(
    ., 
    coords = c("X", "Y"),
    crs = "+init=epsg:5514"
    )
