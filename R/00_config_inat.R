# LOAD DATA ----
load_or_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

pkgs <- c(
  "tidyverse", "sf", "sp", "proj4", "matrixStats", "leaflet", "lubridate",
  "condformat", "openxlsx", "raster", "stars", "ggsn", "zoo", "rgdal", 
  "grid", "gridGraphics", "officer", "flextable"
)

invisible(lapply(pkgs, load_or_install))

write_csv2_win1250 <- function(x, path, ...) {
  # write UTF-8 to a temporary file
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv2(x, tmp, ...)
  
  # re-encode to Windows-1250
  txt <- readr::read_file(tmp)
  txt <- iconv(txt, from = "UTF-8", to = "Windows-1250")
  readr::write_file(txt, path)
}

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
data_cercer <- read.csv2(
  "Data/Input/nalezy-cercer.csv", 
  fileEncoding = "Windows-1250"
)
data_cuccin <- read.csv2(
  "Data/Input/nalezy-cuccin.csv", 
  fileEncoding = "Windows-1250"
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
data_hym_rl <- read.csv2(
  "Data/Input/nalezy-hym-rl.csv", 
  fileEncoding = "Windows-1250"
) 

## Diptera ----
data_bombilius <- read.csv2(
  "Data/Input/nalezy-bombilius.csv", 
  fileEncoding = "Windows-1250"
)

# Combine taxa ---- 
data <- dplyr::bind_rows(
  # Coleoptera
  data_luccer,
  data_cercer,
  data_cuccin,
  data_osmia,
  data_andrena,
  data_halictus,
  data_bombus,
  data_hym_rl,
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
  dplyr::filter(
    DATUM_OD >= lubridate::ymd("2013-01-01")
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
  sf::st_intersection(
    .,
    evl
  ) %>%
  sf::st_intersection(
    .,
    mzchu
  )

# END SCRIPT ----
