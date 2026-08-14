# Script name: Command module for heatwave simulations
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
# This script generates simulations where we superpose a marine heatwave on that starts at different times of an "average"
# To run different test runs, model parameters described in this script need to be edited
# Edit "food_scalar" and "test_run" for different food scenarios
# Edit "window_length" for 5 or 10 day heatwaves
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
library(beepr)


######################################################################
# Test runs:
#---------------------------------------------------------------------
# 0         - Basic run with mean temperature

######################################################################


#--------------------------------------------------------------------
# Run lightfield module:
# * In this study, we exclude the effect of mesopelagic fish predators, which modulate their migration following ambient light.
# Therefore, the effect of moon light in this study is negligible: it maringaly affects the visual capacity of tuna at sunrise and sunset by adding a few extra photons to the sun irradiance.
# Settings:
moon_effect = T                        # Include/exclude moonlight
year_moon = 2021                       # Lunar cycles belonging to 2022 vs 2021
source("R/lightfield_module.R")        # Estimated time: 25s

#--------------------------------------------------------------------
# Run environmental module:
# Settings:
constant_temperature = F               # Constant/observed
regular_year_sst = T                   # Average temperature over 1982-2024
constant_food = F                      # Constant/observed
constant_light_hours = F               # Constant/observed
constant_invertebrates = F             # Constant/observed
invertebrate_scalar <- 1 #1            # Scale invertebrate density
sensitivity_padding_invertebrates <- F # Sensitivity to invertebrate padding
nauplia_dens <- 2000                   # Nauplii m^-3 - smaller larvae only eat nauplii. Density is from field surveys
constant_nauplii <- T                  # constant/following lunar bloom pattern with 7 days lag
food_scalar <- 1.5                     # 0.75 = very low food; 1 = low food; 1.125 = moderate food; 1.25 high food; 1.5 very high food
source("R/environmental_module.R")     # Estimated time: 15s

#--------------------------------------------------------------------
# Run mesopelagic fish module:
# Settings:
myctophid_scalar <- 0 #1               # Scale mesopelagics density
source("R/myctophid_module.R")         # Estimated time: 2s

#--------------------------------------------------------------------
# Run fitness module:
# Settings:
nauplia_dens <- 2000                   # Nauplii m^-3 - smaller larvae only eat nauplii. Density is from field surveys
food_scalar <- 1.5                     # 0.75; 1; 1.125; 1.25; 1.5 
food <- as.character(nauplia_dens * food_scalar)

# Load environment: 
load("data/environmental_df.RData")

# Save original environment (without heatwave)
original_environmental_df <- environmental_df

# Define background mortality:
background_mortality_scale <- 0.9# .85  # Scale background mortality down to "discount" the mortality by mesopelagics and Pelagia

# 1. Store the baseline temperature to prevent cumulative heating
original_temp <- environmental_df$temperature

# 2. Define the starting points for your 5- or 10-unit intervals
window_length <- 10                    # 5; 10
start_points <- c(0, seq(130, 270, by = 5))

# Set parameters for output folder:
heatwave_scenario <- T
mhw_length_folder <- paste0(window_length, "d")

# 3. Start the loop
for (i in start_points) {
  
  # --- STEP A: Create the modified data frame ---
  # We start with the original template
  environmental_df <- original_environmental_df
  
  # --- STEP B: Define indices and apply the +2 shift ---
  start_idx <- 24 * i
  end_idx   <- 24 * (i + window_length)
  
  environmental_df$temperature[start_idx:end_idx] <- original_temp[start_idx:end_idx] + 2
  save(environmental_df, file = "data/environmental_df.RData")
  
  # --- STEP C: Set the 'test_run' variable for the script ---
  # This creates the name that your save() command inside the script expects
  test_run <- paste0(food, "_", i, "_to_", i + window_length)
  
  # --- STEP D: Run the model ---
  message("Launching model run for: ", test_run)
  
  # source() will use the 'environmental_df' and 'test_run' we just prepared
  source("R/fitness_module.R")
  
  # --- STEP E: Optional Cleanup ---
  # If the script creates very large objects, you might want to remove 
  # master_df from memory before the next run starts
  rm(master_df) 
}

beepr::beep(3)
message("All model runs complete. Check the 'out/' folder.")

#                                          END OF SCRIPT
#############################################################################################################