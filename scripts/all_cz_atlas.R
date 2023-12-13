### Run for the Czech Bird Atlas #2 #####
library(sf)
library(tidyverse)
library(lubridate)
library(sparta)

### CZ Atlas 2 #####
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
highWeights <- createWeights(distances = myDistances,
                             attributes = myHabitatData,
                             dist_sub = 200,
                             sim_sub = 100)
lowWeights <- createWeights(distances = myDistances,
                             attributes = myHabitatData,
                             dist_sub = 20,
                             sim_sub = 10)


## Run Frescalo
frescalo_results_highWeights <- frescalo(Data = myData,
                             frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                             time_periods = myTimePeriods,
                             site_col = "site",
                             sp_col = "taxa",
                             year = "year",
                             Fres_weights = highWeights,
                             sinkdir = paste0(getwd(),"/output/"),
                             phi = NULL)
frescalo_results_lowWeights <- frescalo(Data = myData,
                                         frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                                         time_periods = myTimePeriods,
                                         site_col = "site",
                                         sp_col = "taxa",
                                         year = "year",
                                         Fres_weights = lowWeights,
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
frescalo_results_highWeights$stat$effortFresc <- 1/frescalo_results_highWeights$stat$Alpha
frescalo_results_lowWeights$stat$effortFresc <- 1/frescalo_results_lowWeights$stat$Alpha

## Plot effort atlas vs frescalo
pdf("figures/fresc_vs_cards_atlas2.pdf",
    height = 4.13, width = 5.83)

frescalo_results_highWeights$stat %>%
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

frescalo_results_highWeights$stat %>%
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

frescalo_results_highWeights$stat %>%
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

corD <-
  sr_atlas2 %>%
  left_join(effort_atlas) %>%
  left_join(
    rbind(frescalo_results_highWeights$stat %>% select(Location, effortFresc) %>% rename(cellID = Location) %>% mutate(weights = "high"),
          frescalo_results_lowWeights$stat %>% select(Location, effortFresc) %>% rename(cellID = Location) %>% mutate(weights = "low"))
  ) %>%
  mutate(atlas = "CZ_atlas2")

### CZ Atlas 3 ######
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
lowWeights <- createWeights(distances = myDistances,
                           attributes = myHabitatData,
                           dist_sub = 20,
                           sim_sub = 10)
highWeights <- createWeights(distances = myDistances,
                            attributes = myHabitatData,
                            dist_sub = 200,
                            sim_sub = 100)


## Run Frescalo
frescalo_results_highWeights <- frescalo(Data = myData,
                             frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                             time_periods = myTimePeriods,
                             site_col = "site",
                             sp_col = "taxa",
                             year = "year",
                             Fres_weights = highWeights,
                             sinkdir = paste0(getwd(),"/output/"),
                             phi = NULL)
frescalo_results_lowWeights <- frescalo(Data = myData,
                                         frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                                         time_periods = myTimePeriods,
                                         site_col = "site",
                                         sp_col = "taxa",
                                         year = "year",
                                         Fres_weights = lowWeights,
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
frescalo_results_highWeights$stat$effortFresc <- 1/frescalo_results_highWeights$stat$Alpha
frescalo_results_lowWeights$stat$effortFresc <- 1/frescalo_results_lowWeights$stat$Alpha

## Plot effort atlas vs frescalo
pdf("figures/fresc_vs_cards_atlas3.pdf",
    height = 4.13, width = 5.83)

frescalo_results_highWeights$stat %>%
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

frescalo_results_highWeights$stat %>%
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

frescalo_results_highWeights$stat %>%
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
          left_join(
            rbind(frescalo_results_highWeights$stat %>% select(Location, effortFresc) %>% rename(cellID = Location) %>% mutate(weights = "high"),
                  frescalo_results_lowWeights$stat %>% select(Location, effortFresc) %>% rename(cellID = Location) %>% mutate(weights = "low"))
          ) %>%
          mutate(atlas = "CZ_atlas3")
  )

### CZ Atlas 4 ####
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
highWeights <- createWeights(distances = myDistances,
                           attributes = myHabitatData,
                           dist_sub = 200,
                           sim_sub = 100)
lowWeights <- createWeights(distances = myDistances,
                             attributes = myHabitatData,
                             dist_sub = 20,
                             sim_sub = 10)


## Run Frescalo
frescalo_results_lowWeights <- frescalo(Data = myData,
                             frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                             time_periods = myTimePeriods,
                             site_col = "site",
                             sp_col = "taxa",
                             year = "year",
                             Fres_weights = lowWeights,
                             sinkdir = paste0(getwd(),"/output/"),
                             phi = NULL)
frescalo_results_highWeights <- frescalo(Data = myData,
                                        frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                                        time_periods = myTimePeriods,
                                        site_col = "site",
                                        sp_col = "taxa",
                                        year = "year",
                                        Fres_weights = highWeights,
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
frescalo_results_highWeights$stat$effortFresc <- 1/frescalo_results_highWeights$stat$Alpha
frescalo_results_lowWeights$stat$effortFresc <- 1/frescalo_results_lowWeights$stat$Alpha

## Plot effort atlas vs frescalo
pdf("figures/fresc_vs_cards_atlas4.pdf",
    height = 4.13, width = 5.83)

frescalo_results_highWeights$stat %>%
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

frescalo_results_highWeights$stat %>%
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

frescalo_results_highWeights$stat %>%
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
          left_join(
            rbind(frescalo_results_highWeights$stat %>% select(Location, effortFresc) %>% rename(cellID = Location) %>% mutate(weights = "high"),
                  frescalo_results_lowWeights$stat %>% select(Location, effortFresc) %>% rename(cellID = Location) %>% mutate(weights = "low"))
          ) %>%
          mutate(atlas = "CZ_atlas4")
  )


### New-York atlas 1 #####
geopackage <- st_read("data/frescalo_NY.gpkg") %>%
  st_transform(., 4326)

geopackage <- geopackage %>% st_cast(to = "POINT", group_or_split = FALSE, do_split = FALSE)

## Create the ecological data
myData <-
  geopackage %>%
  select(cellID, verbatim_name, start_year) %>%
  filter(start_year == 1980) %>%
  mutate(tp = 1,
         time_period = years(1980)) %>%
  rename(site = cellID,taxa=verbatim_name,year = start_year) %>%
  st_drop_geometry()

## Create the period data
myTimePeriods <- data.frame(start = 1980, end = 1980)

## Get the euclidean distance between the grid cells
# First extract the coordinates
coords <-
  geopackage %>%
  filter(start_year == 1980) %>%
  distinct(cellID, .keep_all = T) %>%
  st_coordinates() %>%
  cbind(
    geopackage %>%
      st_drop_geometry() %>%
      filter(start_year == 1980) %>%
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
highWeights <- createWeights(distances = myDistances,
                             attributes = myHabitatData,
                             dist_sub = 200,
                             sim_sub = 100)
lowWeights <- createWeights(distances = myDistances,
                            attributes = myHabitatData,
                            dist_sub = 20,
                            sim_sub = 10)


## Run Frescalo
frescalo_results_highWeights <- frescalo(Data = myData,
                                         frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                                         time_periods = myTimePeriods,
                                         site_col = "site",
                                         sp_col = "taxa",
                                         year = "year",
                                         Fres_weights = highWeights,
                                         sinkdir = paste0(getwd(),"/output/"),
                                         phi = NULL)
frescalo_results_lowWeights <- frescalo(Data = myData,
                                        frespath = paste0(getwd(),"/Frescalo_3a_windows.exe"),
                                        time_periods = myTimePeriods,
                                        site_col = "site",
                                        sp_col = "taxa",
                                        year = "year",
                                        Fres_weights = lowWeights,
                                        sinkdir = paste0(getwd(),"/output/"),
                                        phi = NULL)


## Get data from atlas 2
NY_atlas1 <- geopackage %>% filter(start_year == 1980)

## Get sampling effort for atlas 4
effort_atlas <- NY_atlas1 %>%
  select(cellID, effort) %>%
  filter(effort != 0) %>%
  rename(hours = effort) %>%
  distinct()

## Compute effort from frescalo output
frescalo_results_highWeights$stat$effortFresc <- 1/frescalo_results_highWeights$stat$Alpha
frescalo_results_lowWeights$stat$effortFresc <- 1/frescalo_results_lowWeights$stat$Alpha

## Plot effort atlas vs frescalo
pdf("figures/fresc_vs_hours_NYatlas1_highWeights.pdf",
    height = 4.13, width = 5.83)

frescalo_results_highWeights$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  ggplot(aes(effortFresc, hours))+
  geom_point()+
  geom_smooth()+
  theme_bw()

dev.off()

## Plot effort atlas vs frescalo
pdf("figures/fresc_vs_hours_NYatlas1_lowWeights.pdf",
    height = 4.13, width = 5.83)

frescalo_results_lowWeights$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  ggplot(aes(effortFresc, hours))+
  geom_point()+
  geom_smooth()+
  theme_bw()

dev.off()

## Maps
pdf("figures/map_fresc_NY_atlas1_highWeights.pdf",
    width = 6.09, height = 3.46)

frescalo_results_highWeights$stat %>%
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

pdf("figures/map_fresc_NY_atlas1_lowWeights.pdf",
    width = 6.09, height = 3.46)

frescalo_results_lowWeights$stat %>%
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

pdf("figures/map_cardNumber_NYatlas1.pdf",
    width = 6.09, height = 3.46)

frescalo_results_highWeights$stat %>%
  select(Location, effortFresc) %>%
  rename(cellID = Location) %>%
  right_join(effort_atlas, by  = "cellID") %>%
  # pivot_longer(cols = c("effortFresc", "atlasCards"),
  #              names_to = "effort_type",
  #              values_to = "effort") %>%
  ggplot()+
  geom_sf(aes(color = hours, geometry = geom), size = 4)+
  scale_color_viridis_c()+
  # facet_wrap(~effort_type)+
  theme_bw()

dev.off()

sr_NYatlas1 <-
  NY_atlas1 %>% st_drop_geometry() %>%  group_by(cellID) %>% summarise(sr = length(unique(verbatim_name)))

corD <-
  bind_rows(corD %>% mutate(cellID = as.character(cellID)),
            sr_NYatlas1 %>%
              left_join(effort_atlas) %>%
              left_join(
                rbind(frescalo_results_highWeights$stat %>% select(Location, effortFresc) %>% rename(cellID = Location) %>% mutate(weights = "high"),
                      frescalo_results_lowWeights$stat %>% select(Location, effortFresc) %>% rename(cellID = Location) %>% mutate(weights = "low"))
              ) %>%
              mutate(atlas = "NY_atlas1")
  )

### Correlation plot #####
library(recipes)
library(ggforce)

tmp <-
  corD %>%
  pivot_wider(names_from = weights,
              values_from = effortFresc,
              names_prefix = "frescalo_weights_") %>%
  select(sr, atlasCards, observers, contains("frescalo_weight"), hours,  atlas)

pdf("figures/corplot.pdf" ,
    width = 8.27, height = 5.83)

ggplot(tmp, aes(x = .panel_x, y = .panel_y, fill = atlas, colour = atlas)) +
  geom_point(aes(), shape = 16, size = 1, position = 'auto') +
  geom_autodensity(alpha = .5, position = 'identity') +
  facet_matrix(vars(-atlas),
               layer.diag = 2,
               grid.y.diag = F)+
  theme_bw()

dev.off()


# tmp %>%
#   select(sr, atlas) %>%
#   ggplot(aes(x = .panel_x, y = .panel_y))+
#   geom_autodensity(aes(colour = atlas, group = atlas), position = "auto")+
#   facet_matrix(vars(-atlas),
#                layer.diag = 1,
#                grid.y.diag = F)


