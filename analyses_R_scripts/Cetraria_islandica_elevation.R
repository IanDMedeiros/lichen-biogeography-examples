library(elevatr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(here)

here::i_am("analyses_R_scripts/Cetraria_biogeography.R")


# Cetraria islandica ------------------------------------------------------
## Load data
Cetraria_islandica_data <- read.csv(here("data", "CoLH__1708909841_CET_ISL_25FEB2024.csv")) %>%
  filter(coordinateUncertaintyInMeters <= 10000 | is.na(coordinateUncertaintyInMeters))

## Split by datum
Cetraria_islandica_wgs84 <- Cetraria_islandica_data %>% filter(geodeticDatum == "WGS84" | geodeticDatum == "unknown")
Cetraria_islandica_ed50 <- Cetraria_islandica_data %>% filter(geodeticDatum == "ED50")
Cetraria_islandica_nad27 <- Cetraria_islandica_data %>% filter(geodeticDatum == "NAD27")
Cetraria_islandica_nad83 <- Cetraria_islandica_data %>% filter(geodeticDatum == "NAD83")

crs_wgs84 <- 4326
crs_ed50 <- 4230
crs_nad27 <- 4267
crs_nad83 <- 4269

## Convert to sf objects
Cetraria_islandica_wgs84_sf <- sf::st_as_sf(Cetraria_islandica_wgs84, coords = c("decimalLongitude", "decimalLatitude"), crs = crs_wgs84)
Cetraria_islandica_ed50_sf <- sf::st_as_sf(Cetraria_islandica_ed50, coords = c("decimalLongitude", "decimalLatitude"), crs = crs_ed50)
Cetraria_islandica_nad27_sf <- sf::st_as_sf(Cetraria_islandica_nad27, coords = c("decimalLongitude", "decimalLatitude"), crs = crs_nad27)
Cetraria_islandica_nad83_sf <- sf::st_as_sf(Cetraria_islandica_nad83, coords = c("decimalLongitude", "decimalLatitude"), crs = crs_nad83)

## Transform other datums to WGS84
Cetraria_islandica_ed50_to_wgs84 <- st_transform(Cetraria_islandica_ed50_sf, crs = crs_wgs84)
Cetraria_islandica_nad27_to_wgs84 <- st_transform(Cetraria_islandica_nad27_sf, crs = crs_wgs84)
Cetraria_islandica_nad83_to_wgs84 <- st_transform(Cetraria_islandica_nad83_sf, crs = crs_wgs84)

## Get elevations
Cetraria_islandica_wgs84_elev_epqs <- get_elev_point(Cetraria_islandica_wgs84_sf, prj = crs_wgs84, src = "aws", z = 6)
Cetraria_islandica_ed50_elev_epqs <- get_elev_point(Cetraria_islandica_ed50_to_wgs84, prj = crs_wgs84, src = "aws", z = 6)
Cetraria_islandica_nad27_elev_epqs <- get_elev_point(Cetraria_islandica_nad27_to_wgs84, prj = crs_wgs84, src = "aws", z = 6)
Cetraria_islandica_nad83_elev_epqs <- get_elev_point(Cetraria_islandica_nad83_to_wgs84, prj = crs_wgs84, src = "aws", z = 6)

## Merge everything to one object
Cetraria_islandica_elev_corrected <- do.call("rbind", list(
  Cetraria_islandica_wgs84_elev_epqs, 
  Cetraria_islandica_ed50_elev_epqs,
  Cetraria_islandica_nad27_elev_epqs,
  Cetraria_islandica_nad83_elev_epqs)) %>%
  filter(elevation >= -10) %>%.      # Remove entries with elevation less than -10 m (accounts for coastline uncertainty)
  mutate(long = unlist(map(Cetraria_islandica_elev_corrected$geometry,1)),
         lat = unlist(map(Cetraria_islandica_elev_corrected$geometry,2)))

Cetraria_islandica_elev_corrected$elevation[Cetraria_islandica_elev_corrected$elevation < 0] <- 0

## Plot elevation by latitude
Cetraria_islandica_plot <- ggplot(Cetraria_islandica_elev_corrected, aes(x=lat, y=elevation, alpha = 0.3)) + 
  geom_point(show.legend = FALSE) +
  xlab("Latitude")

Cetraria_islandica_plot

## Save file
ggsave("Cetraria_islandica.pdf", 
       plot = Cetraria_islandica_plot, 
       device = "pdf",
       path = here("figures"),
       bg = "white"
)

# Cetraria aculeata ------------------------------------------------------



