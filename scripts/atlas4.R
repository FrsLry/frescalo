### Run for the Czech Bird Atlas #2 #####
library(sf)
library(tidyverse)
library(lubridate)
library(sparta)

geopackage <- st_read("data/frescalo_CZ.gpkg") %>%
  st_transform(., 5514)

## Create the ecological data
myData <-
  geopackage %>%
  select(cellID, verbatim_name, start_year) %>%
  filter(start_year == 2014) %>%
  mutate(tp = 1,
         time_period = years(2014)) %>%
  rename(site = cellID,taxa=verbatim_name,year = start_year) %>%
  st_drop_geometry()

## Create the period data
myTimePeriods <- data.frame(start = 2014, end = 2014)

## Get the euclidean distance between the grid cells
# First extract the coordinates
coords <-
  geopackage %>%
  filter(start_year == 2014) %>%
  distinct(cellID, .keep_all = T) %>%
  st_coordinates() %>%
  cbind(
    geopackage %>%
      st_drop_geometry() %>%
      filter(start_year == 2014) %>%
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
                           dist_sub = 200,
                           sim_sub = 100)


## Run Frescalo
frescalo_results <- frescalo(Data = myData,
                             frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                             time_periods = myTimePeriods,
                             site_col = "site",
                             sp_col = "taxa",
                             year = "year",
                             Fres_weights = myWeights,
                             sinkdir = paste0(getwd(),"/output/"),
                             phi = NULL)

## Get data from atlas 2
atlas4 <- geopackage %>% filter(start_year == 2014)

## Get sampling effort for atlas 4
effort_atlas <- atlas4 %>%
  select(cellID, effort) %>%
  filter(effort != 0) %>%
  rename(observers = effort) %>%
  distinct()

## Compute effort from frescalo output
frescalo_results$stat$effortFresc <- 1/frescalo_results$stat$Alpha

## Plot effort atlas vs frescalo
pdf("figures/fresc_vs_cards_atlas4.pdf",
    height = 4.13, width = 5.83)

frescalo_results$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  ggplot(aes(effortFresc, observers))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()

dev.off()

## Maps
pdf("figures/map_fresc_atlas4.pdf",
    width = 6.09, height = 3.46)

frescalo_results$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  # pivot_longer(cols = c("effortFresc", "atlasCards"),
  #              names_to = "effort_type",
  #              values_to = "effort") %>%
  ggplot()+
  geom_sf(aes(color = effortFresc, geometry = geom), size = 4)+
  scale_color_viridis_c()+
  # facet_wrap(~effort_type)+
  theme_bw()

dev.off()

pdf("figures/map_cardNumber_atlas4.pdf",
    width = 6.09, height = 3.46)

frescalo_results$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  # pivot_longer(cols = c("effortFresc", "atlasCards"),
  #              names_to = "effort_type",
  #              values_to = "effort") %>%
  ggplot()+
  geom_sf(aes(color = observers, geometry = geom), size = 4)+
  scale_color_viridis_c()+
  # facet_wrap(~effort_type)+
  theme_bw()

dev.off()
