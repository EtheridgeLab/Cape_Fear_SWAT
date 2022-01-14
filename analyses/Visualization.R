#Visualization of simulation results

#Package installation

install.packages("dplyr")
install.packages("dygraphs")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("mapview")
install.packages("purrr")
install.packages("plotly")
install.packages("sf")
install.packages("tibble")
install.packages("tidyr")
install.packages("xts")

#Loading R Packages

library(SWATplusR)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(lubridate)
library(mapview)
library(plotly)
library(sf)
library(tibble)
library(tidyr)


#Loading demo data

# The path where the SWAT demo project will be written
demo_path <- "C:/Users/julie/Documents/R/"

# Loading the SWAT+ demo project on your hard drive
path_plus <- load_demo(dataset = "project",
                       version = "plus",
                       path = demo_path,)
                       

q_obs <- load_demo(dataset = "observation")

#Parameter Sampling
#Use 7 SWAT model parameters that are frequently calibrated for discharge
#Name parameters using specific syntax requested by run_swat*() functions

par <- tibble("cn2.hru | change = abschg" = c(-4.56, -4.47, -4.89, -14.4),
              "lat_ttime.hru | change = absval" = c(1.64, 2.55, 1.08, 0.844),
              "lat_len.hru | change = absval" = c(95.2, 94.8, 42.2, 87.8),
              "k.sol | change = pctchg" = c(-45.0, -37.8, -41.1, -48.5),
              "z.sol | change = pctchg" = c(-41.0, 25.7, 12.7, 27.9),
              "esco.hru | change = absval" = c(0.809, 0.437, 0.368, 0.848),
              "epco.hru | change = absval" = c(0.165, 0.507, 0.197, 0.353))

#Visualization of time series data

q_day <- run_swatplus(project_path = path_plus,
                      output = list(q_sim = define_output(file = "channel",
                                                          variable = "flo_out",
                                                          unit = 1)),
                      parameter = par,
                      start_date = "2000-01-01",
                      end_date = "2007-12-31",
                      years_skip = 3,
                      n_thread = 4)

#Function view_timeseries requires simulated and observed time series in date.frame format

view_timeseries <- function(q_sim, q_obs) {
  names(q_obs) <- c("date", "q_obs")
  
  q_xts <- q_sim %>% 
    dplyr::left_join(., q_obs, by = "date") %>% 
    xts::xts(x = .[,c(2:ncol(.))], order.by = .$date)
  
  n_sim <- ncol(q_sim) - 1
  
  dygraph(q_xts) %>% 
    dyRangeSelector() %>% 
    dyOptions(colors = 
                c(colorRampPalette( 
                  RColorBrewer::brewer.pal(8, "Paired"))(n_sim), 
                  "black"))
}

#Function view_timeseries can compare the 4 simulated time series with discharge observations
#Make sure to convert discharge for daily time steps by dividing by 8.64

q_sim <- q_day$simulation$q_sim %>% 
  mutate_if(., is.numeric, ~(./8.64)) 

view_timeseries(q_sim = q_sim, q_obs = q_obs)

#Visualization of the water balance components
#Overview of shares of water balance components in model setup can provide info on how well the SWAT model setup represents a catchment
#Below simulates basin avg of water balance components and visualize them using interactive plot

par_wb <- par[1,]

wb_mon <- run_swatplus(project_path = path_plus,
                       output = list(pet = define_output(file = "basin_wb",
                                                         variable = "pet",
                                                         unit = 1),
                                     pcp = define_output(file = "basin_wb",
                                                         variable = "precip",
                                                         unit = 1),
                                     aet = define_output(file = "basin_wb",
                                                         variable = "et",
                                                         unit = 1),
                                     q_sur = define_output(file = "basin_wb",
                                                           variable = "surq_gen",
                                                           unit = 1),
                                     q_lat = define_output(file = "basin_wb",
                                                           variable = "latq",
                                                           unit = 1)),
                       parameter = par_wb,
                       start_date = "2000-01-01",
                       end_date = "2007-12-31",
                       output_interval = "m",
                       years_skip = 3)

wb_comp <- wb_mon$simulation %>% 
  mutate(month = month(date)) %>% 
  select(-date) %>% 
  group_by(month) %>% 
  summarise_all(., .funs = mean)

wb_land <- wb_comp %>% 
  select(-pet, -pcp) %>% 
  gather(data = ., key = "Land_phase", value = "value", - month) %>% 
  mutate(Land_phase = factor(Land_phase, levels = c("aet", "q_sur", "q_lat")))

wb_atm <- wb_comp %>% 
  select(month, pet, pcp) %>% 
  gather(data = ., key = "Atmosphere", value = "value", - month) %>% 
  mutate(Atmosphere = factor(Atmosphere, levels = c("pet", "pcp")))


gg_wb <- ggplot() + 
  geom_col(data = wb_land, 
           aes(x = month, y = value, fill = Land_phase)) + 
  geom_line(data = wb_atm, 
            aes(x = month, y = value, col = Atmosphere)) + 
  scale_fill_manual(values = c("#41AB5D", "#9ECAE1", "#08519C")) + 
  scale_color_manual(values = c("#CB181D", "#08519C")) + 
  scale_x_continuous(breaks = 1:12, labels = substr(month.abb, 1,1)) + 
  theme_bw()

ggplotly(gg_wb)

#Viewing Spatial Data
#Spatially distributed functions that are provided in traditional GIS can be useful
#Below calculates avg annual values for 4 water balance components on HRU scae and visualize them in interactive map

wb_hru <- run_swatplus(project_path = path_plus,
                       output = list(aet = define_output(file = "hru_wb",
                                                         variable = "et",
                                                         unit = 1:131),
                                     wyld = define_output(file = "hru_wb",
                                                          variable = "wateryld",
                                                          unit = 1:131),
                                     q_sur = define_output(file = "hru_wb",
                                                           variable = "surq_gen",
                                                           unit = 1:131),
                                     q_lat = define_output(file = "hru_wb",
                                                           variable = "latq",
                                                           unit = 1:131)),
                       parameter = par_wb,
                       start_date = "2000-01-01",
                       end_date = "2007-12-31",
                       output_interval = "y",
                       years_skip = 3)

#Simulatd outputs provided by SWATplusR do not have required form for visualization
#Below is neccessary data wrangling

wb_hru_aa <- wb_hru$simulation %>% 
  select(-date) %>% 
  summarise_all(., .funs = mean) %>% 
  gather(.) %>% 
  mutate(hru = gsub("[^[:digit:]]", "",key) %>% as.numeric(.),
         wb_comp = gsub("[^[:alpha:]]", "",key)) %>% 
  select(hru, wb_comp, value) %>% 
  spread(., key = wb_comp, value = value)

#To visualize the simulated values for each HRU we require spatial info for all HRUs
#HRU map for SWAT+ project can be acquired from SWATdata
#Can use R package sf which is easy to link the spatial reference to our simulated data

hru_path <- load_demo("hru", version = "plus")

hru <- read_sf(hru_path) %>% 
  select(HRUS) %>% 
  mutate(HRUS = as.numeric(HRUS)) %>% 
  rename(hru = HRUS)
#> Warning: NAs introduced by coercion

#Advantage of sf is that spatial data is in data.frame format
#Easy to link spatial reference to simulated data

hru <- left_join(hru, wb_hru_aa, by = "hru")

#Plot spatial info with mapview
#Below compares different simulated components

aet <- mapview(hru, zcol = "aet")
wyld <- mapview(hru, zcol = "wyld")
qlat <- mapview(hru, zcol = "qlat")
qsur <- mapview(hru, zcol = "qsur")

#Can not find function "sync" to view spatial data
sync(aet, wyld, qlat, qsur)