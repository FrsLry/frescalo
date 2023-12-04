### Run for the Czech Bird Atlas #2 #####
library(sf)
library(tidyverse)
library(lubridate)
library(sparta)

### Atlas 2 #####
geopackage <- st_read("data/frescalo_CZ.gpkg") %>%
  st_transform(., 5514)

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
atlas2 <- geopackage %>% filter(start_year == 1985)

## Get sampling effort for atlas 2
effort_atlas <- atlas2 %>%
  select(cellID, effort) %>%
  filter(effort != 0) %>%
  rename(atlasCards = effort) %>%
  distinct()

## Compute effort from frescalo output
frescalo_results$stat$effortFresc <- 1/frescalo_results$stat$Alpha

## Plot effort atlas vs frescalo
pdf("figures/fresc_vs_cards_atlas2.pdf",
    height = 4.13, width = 5.83)

frescalo_results$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  ggplot(aes(effortFresc, atlasCards))+
  geom_point()+
  geom_smooth()+
  theme_bw()

dev.off()

## Maps
pdf("figures/map_fresc_atlas2.pdf",
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

pdf("figures/map_cardNumber_atlas2.pdf",
    width = 6.09, height = 3.46)

frescalo_results$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  # pivot_longer(cols = c("effortFresc", "atlasCards"),
  #              names_to = "effort_type",
  #              values_to = "effort") %>%
  ggplot()+
  geom_sf(aes(color = atlasCards, geometry = geom), size = 4)+
  scale_color_viridis_c()+
  # facet_wrap(~effort_type)+
  theme_bw()

dev.off()

sr_atlas2 <-
  atlas2 %>% st_drop_geometry() %>%  group_by(cellID) %>% summarise(sr = length(unique(verbatim_name)))

corD <- sr_atlas2 %>%
  left_join(effort_atlas) %>%
  left_join(frescalo_results$stat %>% select(Location, effortFresc) %>% rename(cellID = Location)) %>%
  mutate(atlas = "atlas2")

### Atlas 3 ######
geopackage <- st_read("data/frescalo_CZ.gpkg") %>%
  st_transform(., 5514)

## Create the ecological data
myData <-
  geopackage %>%
  select(cellID, verbatim_name, start_year) %>%
  filter(start_year == 2001) %>%
  mutate(tp = 1,
         time_period = years(2001)) %>%
  rename(site = cellID,taxa=verbatim_name,year = start_year) %>%
  st_drop_geometry()

## Create the period data
myTimePeriods <- data.frame(start = 2001, end = 2001)

## Get the euclidean distance between the grid cells
# First extract the coordinates
coords <-
  geopackage %>%
  filter(start_year == 2001) %>%
  distinct(cellID, .keep_all = T) %>%
  st_coordinates() %>%
  cbind(
    geopackage %>%
      st_drop_geometry() %>%
      filter(start_year == 2001) %>%
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
atlas3 <- geopackage %>% filter(start_year == 2001)

## Get sampling effort for atlas 3
effort_atlas <- atlas3 %>%
  select(cellID, effort) %>%
  filter(effort != 0) %>%
  rename(atlasCards = effort) %>%
  distinct()

## Compute effort from frescalo output
frescalo_results$stat$effortFresc <- 1/frescalo_results$stat$Alpha

## Plot effort atlas vs frescalo
pdf("figures/fresc_vs_cards_atlas3.pdf",
    height = 4.13, width = 5.83)

frescalo_results$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  ggplot(aes(effortFresc, atlasCards))+
  geom_point()+
  geom_smooth()+
  theme_bw()

dev.off()

## Maps
pdf("figures/map_fresc_atlas3.pdf",
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

pdf("figures/map_cardNumber_atlas3.pdf",
    width = 6.09, height = 3.46)

frescalo_results$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  # pivot_longer(cols = c("effortFresc", "atlasCards"),
  #              names_to = "effort_type",
  #              values_to = "effort") %>%
  ggplot()+
  geom_sf(aes(color = atlasCards, geometry = geom), size = 4)+
  scale_color_viridis_c()+
  # facet_wrap(~effort_type)+
  theme_bw()

dev.off()

sr_atlas3 <-
  atlas3 %>% st_drop_geometry() %>%  group_by(cellID) %>% summarise(sr = length(unique(verbatim_name)))

corD <-
  rbind(corD,
        sr_atlas3 %>%
          left_join(effort_atlas) %>%
          left_join(frescalo_results$stat %>% select(Location, effortFresc) %>% rename(cellID = Location)) %>%
          mutate(atlas = "atlas3")
  )

### Atlas 4 ####
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
  geom_smooth()+
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

sr_atlas4 <-
  atlas4 %>% st_drop_geometry() %>%  group_by(cellID) %>% summarise(sr = length(unique(verbatim_name)))

corD <-
  bind_rows(corD,
        sr_atlas4 %>%
          left_join(effort_atlas) %>%
          left_join(frescalo_results$stat %>% select(Location, effortFresc) %>% rename(cellID = Location)) %>%
          mutate(atlas = "atlas4")
  )

pdf("figures/corplot.pdf" ,
    width = 11.69, height = 8.27)
corD %>% select(sr, atlasCards, effortFresc, observers) %>%  GGally::ggpairs() + theme_bw()
dev.off()
