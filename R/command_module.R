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
# This script runs all modules of the model.
# It enables to modify model parameters to run different tests.
#
# Test runs:
#---------------------------------------------------------------------
# To run different test runs, model parameters described in this script need to be edited
# Edit "food_scalar" and "test_run" for different food scenarios
# Edit "temp_mod_label" and "background_mortality_scale" for temperature modifications and sensitivity on background mortality
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


#--------------------------------------------------------------------
# Run lightfield module:
# * In this study, we exclude the effect of mesopelagic fish predators, which modulate their migration following ambient light.
# Therefore, the effect of moon light in this study is negligible: it marginaly affects the visual capacity of tuna at sunrise and sunset by adding a few extra photons to the sun irradiance.
# Settings:
moon_effect = T                        # Include/exclude moonlight
year_moon = 2021                       # Lunar cycles belonging to 2021 or 2022
source("R/lightfield_module.R")        # Estimated time: 25s

#--------------------------------------------------------------------
# Run environmental module:
# * In this study, we only modified the "food_scalar" parameter. All other parameters hould remain as they are.
# Settings:
constant_temperature = F               # Constant/observed
regular_year_sst = T                   # Average temperature over 1982-2025
constant_food = F                      # Constant/observed
constant_light_hours = F               # Constant/observed
constant_invertebrates = F             # Constant/observed
invertebrate_scalar <- 1               # Scale invertebrate density
sensitivity_padding_invertebrates <- F # Sensitivity to invertebrate padding
nauplia_dens <- 2000                   # Nauplii m^-3 - smaller larvae only eat nauplii. Density is from field surveys
constant_nauplii <- T                  # constant/following lunar bloom pattern with 7 days lag
food_scalar <- 1                    # 0.75 = very low food; 1 = low food; 1.125 = moderate food; 1.25 high food; 1.5 very high food
source("R/environmental_module.R")     # Estimated time: 15s

#--------------------------------------------------------------------
# Run mesopelagic fish module:
# Settings:
myctophid_scalar <- 0 #1               # Scale mesopelagics density. Set to 0 means we ignore the effect of mesopelagic predators.
source("R/myctophid_module.R")         # Estimated time: 2s 

#--------------------------------------------------------------------
# Run fitness module:
# Settings:
test_run <- "low_food"                 # Set to: very_low_food; low_food; moderate_food;high_food; very_high_food
temp_mod_label <- "+0"                 # Label of temperature modification: +0; +2; -2
temp_mod <- as.numeric(temp_mod_label) # Temperature modification
background_mortality_scale <- 0.9      # Scale background mortality down to "discount" the mortality by and Pelagia noctiluca. Original: 0.9. Sensitivity 0.85 and 0.95
sensitivity_folder <- ifelse(background_mortality_scale == 0.95, "/mcgurck_sensitivity/95",
                             ifelse(background_mortality_scale == 0.85, "/mcgurck_sensitivity/85",
                                    ""))
source("R/fitness_module.R")           # Estimated time: 1-4 minutes
beep(1)                                # Give acoustic warning

#--------------------------------------------------------------------
# Run model plots:
# Settings:
# *Need to edit speciffic settings in the plotting file:
source("R/plotting_module.R")            
p

#                                          END OF SCRIPT
#############################################################################################################