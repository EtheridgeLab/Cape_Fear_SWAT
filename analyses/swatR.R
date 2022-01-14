#Getting Started with SWATplusR

#Loading R packages

library(usethis)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(devtools)
library(SWATplusR)
library(SWATdata)

#The path where the SWAT demo project will be written
demo_path <- "C:/Users/julie/Documents/R/"  

#Loading a SWAT+ demo project

#Specific model revision can be loaded by adding revision = # to load_demo() 
#If no revision number is provided, a project with most recent revision number available is loaded

#Load_demo loads demo project data set and saves in demo_path
#Returns final demo project path as a character string back to R

path_plus <- load_demo(dataset = "project",
                       version = "plus",
                       path = demo_path)
                      
#Observation data of daily discharge records at main outlet
q_obs <- load_demo(dataset = "observation")

#Tibble of observation data set
q_obs

#Plot of observation data set
plot(q_obs, type = "l")

#Spatial data
#Shape files of subbasins, river network, and HRUs

# Path to the subbasin shape file
sub_path <- load_demo(dataset = "subbasin", version = "plus")

#Path to the subbasin shape file
riv_path <- load_demo(dataset = "river", version = "plus")

#Define paths
riv_path <- "C:/Users/julie/Documents/R/win-library/4.1/SWATdata/extdata/plus_shapes/riv.shp"
sub_path <- "C:/Users/julie/Documents/R/win-library/4.1/SWATdata/extdata/plus_shapes/sub.shp"

# Loading the spatial data as simple features
sub <- read_sf(sub_path)
riv <- read_sf(riv_path)

#Plot to visualize simulation results spatially 
ggplot() +
  geom_sf(data = sub) +
  geom_sf(data = riv, col = "royalblue", lwd = 0.75) +
  geom_sf_label(data = riv, aes(label = Channel)) +
  theme_bw()

#SWAT model runs

#Provide project_path to project folder on hard drive
#Define simulation outputs that simulation should return to R
#Output variable that should be returned must be defined using function define_output()
#Running SWAT provides info. on progress- convenient for long simulation runs
#Quiet function with argument 'quiet=true'

q_sim_plus <- run_swatplus(project_path = path_plus,
                           output = define_output(file = "channel",
                                                  variable = "flo_out",
                                                            unit = 1))
#Output definition

#Extract multiple variables by combining in list()
#Unit=1 because there is only one spatial unit (entire basin) written to file
#Can assign names to variable using 'list(name_a=define_output(...))'

wb_sim <- run_swatplus(project_path = path_plus,
                       output = list(precip = define_output(file = "basin_wb",
                                                            variable = "precip",
                                                            unit = 1),
                                     q_sur  = define_output(file = "basin_wb",
                                                            variable = "surq_gen",
                                                            unit = 1),
                                     q_lat  = define_output(file = "basin_wb",
                                                            variable = "latq",
                                                            unit = 1),
                                     eta    = define_output(file = "basin_wb",
                                                            variable = "et",
                                                            unit = 1)))

#Tibble with date column and columns with simulated values of extracted variables
wb_sim

q_sim_plus

#Plot 
q_plot <- q_sim_plus %>%
  select(date, flo_out) %>%
  rename(q_sim = flo_out) %>%
  left_join(., q_obs, by = "date") %>%
  rename(q_obs = discharge) %>%
  gather(., key = "variable", value = "discharge", -date)

ggplot(data = q_plot) +
  geom_line(aes(x = date, y = discharge, col = variable, lty = variable)) +
  scale_color_manual(values = c("black", "tomato3")) +
  theme_bw()

#Model Parameter Alteration

#Single parameter set- vector; many parameter sets- tibble
#Change parameters

  #'relchg' and 'pctchg' alter parameter by a fraction or percentage of initial value
  #'abschg' add an absolute value to initial parameter value
  #'absval' replace parameter by an absolute value

#For parameter definition, "initial parameter name::actual parameter definition"

par_mod <- c("cn2.hru|change = abschg" = 5)
par_mod <- c("CN2.mgt|change = abschg | sub == 1:5 | luse %in% c('WWHT', 'CORN') " = 5)
par_mod <- c("my_name::cn2.hru|change = abschg" = 5)

par_single <- c("cn2.hru|change = pctchg" = - 5,
                "alpha.hru|change = absval" = 0.5)

q_sim<- run_swatplus(project_path = path_plus,
                      output = define_output(file = "channel",
                                             variable = "flo_out",
                                             unit = 1),
                      parameter = par_single)


# This is the code to use for multiple parameters

#Parameter sets must be defined in a tibble

#par_set <- tibble("cn2.hru|change = abschg" = runif(8,-15,10),
#                  "alpha.gw|change = absval" = runif(8, 0, 1))

#par_set

#q_sim <- run_swatplus(project_path = path_plus,
#                      output = list(q_sur  = define_output(file = "basin_wb",
#                                                           variable = "surq_gen",
#                                                           unit = 1),
#                                    q_lat  = define_output(file = "basin_wb",
#                                                           variable = "latq",
#                                                           unit = 1)),
#                      parameter = par_set,
#                      n_thread = 4)


# Exploring simulations with parameter alterations

# Output$parameter provides all info on the parameter changes 
# Output$simulation provies simulation results
# $parameter holds two tibbles
# First tibble shows values of parameter changes of each model evaluation (each row diff parameter set)
# Second tibble provides info on parameter names, actual names, type of change, parameter constraints


q_sim$parameter

# $simulation provides simulation runs that corresponds to each parameter set
# Simulated variables saved in list of tibbles 
# Each simulated variable saved in one tibble

q_sim$simulation

# Saving and loading simulations
# Simulations can be saved in SQLite database on local hard drive
# Use save_file in run_swatplus()
# Simulations only saved in database and not returned back to R, use return_output=FALSE

run_swatplus(project_path = path_plus,
             output = list(q_sur  = define_output(file = "basin_wb",
                                                  variable = "surq_gen",
                                                  unit = 1),
                           q_lat  = define_output(file = "basin_wb",
                                                  variable = "latq",
                                                  unit = 1)),
             parameter = par_single,
             n_thread = 4,
             save_file = "q_sur_lat",
             return_output = FALSE)

#Loading large databases into R can take a long time

path_saved_q <- "C:/Users/julie/Documents/R/swatplus_rev59_demo/q_sur_lat"

# Use scan_swat_run() to see successful simulations

scan_swat_run(save_dir = path_saved_q)

# Use load_swat_run to load simulations or load parts of simulation

q_subset <- load_swat_run(save_dir = path_saved_q,
                          variable = "q_lat",
                          run = 1:3)

q_subset

#Further SWATplusR input arguments
# Use start_date and end_date to control simulation period
# Use output_interval to define simulations written with daily, monthly, yearly time steps
# Use years_skip to define simulated years that should be skipped 
# Use run_index to simulate a subset of provided parameter combinations