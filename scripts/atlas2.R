### Run for the Czech Bird Atlas #2 #####
library(sf)
library(tidyverse)
library(lubridate)
library(sparta)

geopackage <- st_read("data/frescalo_CZ.gpkg")

## Create the ecological data
myData <-
  geopackage %>%
    select(cellID, verbatim_name, start_year) %>%
    filter(start_year == 1985) %>%
    mutate(tp = 1,
           time_period = years(1985)) %>%
    rename(site = cellID,taxa=verbatim_name,year = start_year) %>%
    st_drop_geometry()

## Create the period data
myTimePeriods <- data.frame(start = 1985, end = 1985)

## Get the euclidean distance between the grid cells
# First extract the coordinates
coords <-
geopackage %>%
  filter(start_year == 1985) %>%
  distinct(cellID, .keep_all = T) %>%
  st_coordinates() %>%
  cbind(
    geopackage %>%
      st_drop_geometry() %>%
      filter(start_year == 1985) %>%
      distinct(cellID)
  )

## Create the distance matrix
dist <- as.matrix(dist(coords))
# dist[upper.tri(dist)] <- NA
## Transform into a df and names rows and cols
dist <-
as.matrix(dist) %>%
  as.data.frame() %>%
  mutate(cellID = coords$cellID) %>%
  remove_rownames() %>%
  column_to_rownames("cellID")

colnames(dist) <- coords$cellID

## Transform into a long format
myDistances <-
  dist %>%
    rownames_to_column(var = "x") %>%
    pivot_longer(cols = -x,
                 names_to = "y",
                 values_to = "dist") %>%
    as.data.frame()

## Null habitat data
myHabitatData <-
  data.frame(site = coords$cellID, x = 1)

## Compute the weights
myWeights <- createWeights(distances = myDistances,
                           attributes = myHabitatData,
                           dist_sub = 20,
                           sim_sub = 10)


## Run Frescalo
frescalo_results <- frescalo(Data = myData,
                             frespath = "/Users/leroy/Desktop/phd_folder/frescalo/Frescalo_3a_windows.exe",
                             time_periods = myTimePeriods,
                             site_col = "site",
                             sp_col = "taxa",
                             year = "year",
                             Fres_weights = myWeights,
                             sinkdir = "/Users/leroy/Desktop/phd_folder/frescalo/output/")
