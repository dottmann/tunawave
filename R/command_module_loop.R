# Script name: Command module
#
#
# Authors: Daniel Ottmann
# Email: daniel.ottmann.riera@gmail.com
#
# Last update:  August 2026
#
# ---------------------------
#
# Readme:
#
# This script loops all modules of the model to make historic and future projections
# Each run can take up to 20h.
#
# Test runs:
#---------------------------------------------------------------------
#
# This script generates annual simulations using SST pf historic and IPCC projection data.
# To run different test runs, model parameters described in this script need to be edited.
# Edit "ystart", "yend" to set historic or future projections.
# For future projections, edit "mhw" to superpose/remove a marine heat wave. 
#
# ---------------------------


#####################################################################
# Clear environment:
rm(list = ls())

# Load packages:
require(tidyverse)
require(patchwork)
require(ggthemes)
require(scales)
library(viridis)
library(mgcv)
library(suncalc)
library(gggibbous)
require(here)
require(lubridate)
require(cubelyr)

#--------------------------------------------------------------------
# Run lightfield module:
# * In this study, we exclude the effect of mesopelagic fish predators, which modulate their migration following ambient light.
# Therefore, the effect of moon light in this study is negligible: it marginaly affects the visual capacity of tuna at sunrise and sunset by adding a few extra photons to the sun irradiance.
# Settings:
moon_effect = T                        # Include/exclude moonlight
year_moon = 2021                       # Lunar cycles belonging to 2021 or 2022
source("R/lightfield_module.R")        # Estimated time: 25s

#--------------------------------------------------------------------
# Prey scalar values:
prey_scalars <- c(0.75, 1, 1.125, 1.25, 1.5)

# Make a table with year ids:
# * Edit depending on whether if we do historical or future projections:
ystart <- 2026 #1982 # 2026
yend <- 2100 #2025 # 2100
year_ids <- data.frame(year = ystart:yend, time = 1:length(ystart:yend)) 

# Indicate whether if we superimpose a MHW on the IPCC projections:
# *Use only for future projections.
mhw <- T
if (mhw == TRUE & (ystart != 2026 | yend != 2100)) {
  stop("Artificial MHW can only be superposed to IPCC scenarios")
}

# Create a lookup table mapping nicknames to actual temperature filenames
file_map <- c(
  "historic_projections"   = "data/temperature_projections_historic.Rdata",    # Historic projections
  "annual" = "data/temperature_data_annual_1982_2025.Rdata",  # Interanual trend
  "ssp126"   = "data/temperature_projections_ssp126.Rdata",  
  "ssp245"   = "data/temperature_projections_ssp245.Rdata",
  "ssp370"   = "data/temperature_projections_ssp370.Rdata",
  "ssp585"   = "data/temperature_projections_ssp585.Rdata"
)

# Time in:
time00 <- Sys.time()

# Make a list where we will save all the permutated fitness indexes:
permutation_list <- list()

for(perm in 1:length(prey_scalars)) {
  scale_cope_dens <- prey_scalars[perm]
  scale_clado_dens <- prey_scalars[perm]
  scale_naup_dens <- prey_scalars[perm]  
  
  #--------------------------------------------------------------------
  # Run  for loop for all the years:
  fitness_index <- c()
  
  for(t in year_ids[year_ids$year %in% ystart:yend, ][[2]]) { # (1:(yend - ystart + 1)) 
    
    print(paste0("Working on year ", t,", permutation ", perm))
    #--------------------------------------------------------------------
    # Run environmental module:
    # Settings:
    scenario_choice <- "ssp126"    # historic_projections/annual/ssp126/ssp245/ssp370/ssp585
    constant_food = F                      # Constant/observed
    constant_light_hours = F               # Constant/observed
    constant_invertebrates = F             # Constant/observed
    invertebrate_scalar <- 1 #1               # Scale invertebrate density
    sensitivity_padding_invertebrates <- F # Sensitivity to invertebrate padding
    nauplia_dens <- 2000                   # Nauplii m^-3 - smaller larvae only eat nauplii. Density is from field surveys
    constant_nauplii <- T                  # constant/following lunar bloom pattern with 7 days lag
    source("R/environmental_module_loop.R")     # Estimated time: 15s
    
    #--------------------------------------------------------------------
    # Run mesopelagic fish module:
    # Settings:
    myctophid_scalar <- 0 #1                  # Scale mesopelagics density
    source("R/myctophid_module_loop.R")         # Estimated time: 2s
    
    #--------------------------------------------------------------------
    # Run fitness module:
    # Settings:
    test_run <- paste0("loop_", t)                          # Replace with the test run code above
    background_mortality_scale <- 0.9 #.85      # Scale background mortality down to "discount" the mortality by mesopelagics and Pelagia
    source("R/fitness_module_loop.R")           # Estimated time: 1-4 minutes
    
    # Save fitness index in a vector:
    master_df <- as.data.frame(master_df)
    
    
    master_df <- master_df %>%
      mutate(date = as.Date(datetime),
             month = month(date)) %>%
      
      group_by(month, test_run) %>%
      reframe(datetime = datetime,
              time_id = time_id,
              yday = yday,
              date = date,
              moon_fraction = moon_fraction,
              temperature = temperature,
              egg_total_survival = egg_total_survival,
              ysl_total_survival = ysl_total_survival,
              fitness = fitness,
              test_run = as.factor(test_run),
              full_moon = case_when(moon_fraction == max(moon_fraction) ~ T,
                                    T ~ F),
              new_moon = case_when(moon_fraction == min(moon_fraction) ~ T,
                                   T ~ F)) %>%
      ungroup() 
    
    
    findex <- sum(master_df$fitness)
    fitness_index <- c(fitness_index, findex)
  }
  # Put it in a table:
  fitness_index <- data.frame(fitness_index, year = ystart:yend)
  
  permutation_list[[perm]] <- fitness_index
}

# Outfile index:
if(mhw == F) {
  save(permutation_list, file = paste0("out/loop/fitness_index_permut_2000_", ystart, "_", yend, ".RData"))
} else {
  save(permutation_list, file = paste0("out/loop/fitness_index_permut_2000_", ystart, "_", yend, "_ssp126_heatwave_191_199.RData")) # _heatwave_201_210
}
 
# Time out:
print(Sys.time() - time00)

#                                          END OF SCRIPT
#############################################################################################################