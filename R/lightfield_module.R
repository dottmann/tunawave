## Script name: Light field module
##
##
## Authors: Daniel Ottmann & Tom Langbehn
## Email:  tom.langbehn@uib.no
## Email: daniel.ottmann.riera@gmail.com
##
## Last update:  August 2026
##
## ---------------------------
##
## Readme:
##
## This script creates a time-depth partitioned light field from surface irradiance 
## Surface irradiance is obtained from a light model developed by Stacey Connan et al
## The light field represent hour-to-hour light intensity accross one year in Lat Lon 39.150000N, 2.93000E (39?09'00"N, 2?55'48"E)
##
## ---------------------------


#####################
# Set time:
time0 <- Sys.time()

# Create an "those that are not within" function:
"%ni%" <- Negate("%in%")


####################
# Source functions:
source("R/lightfield_functions.R")

# #############
# # Read data:
load("data/light_surf_2d_hour.RData")


# Add simulated moon light:
if (moon_effect == T) {
  
  # Construct the path dynamically
  file_to_load <- paste0("data/moon_sim_", year_moon, ".RData")
  
  # Check if the file exists before loading to prevent a crash
  if (file.exists(file_to_load)) {
    load(file_to_load)
  } else {
    stop("Year not recognized or file missing!")
  }
  
  moon_sim <- moon_sim %>%
    select(moon_irradiance, yday)
  
  irradiance <- irradiance %>%
    left_join(moon_sim, by = "yday") %>%
    mutate(irradiance = irradiance + moon_irradiance)
}

# Keep editing the data frame:
irradiance2 <- irradiance %>%
  mutate(hour = hour(datetime),
         light_lim = case_when(irradiance < 2.17 ~ 0,           # Modified from Hilder et al (2017): 0.1 umol quanta * 0.217 conversion factor * 1000 mW/W * 0.1 (larvae can feed at less than 0.1 umol quanta)
                               T ~ 1)) %>%
  group_by(yday, hour) %>%
  summarise(irradiance = mean(irradiance),
            light_lim = mean(light_lim))

irradiance <- irradiance %>%
  dplyr::select(yday, hour, datetime) %>%
  group_by(yday, hour) %>%
  slice(1)

irradiance <- irradiance2 %>%
  left_join(irradiance, by = c("yday", "hour")) %>%
  filter(yday %in% 1:365)


# Read solar elevation from file:
solalt <- bin2long("data/sun_height_2d.bin") %>%     
  dplyr::select(-c(ilatlon)) %>%                   # We can drop the index value for lat & lon here as we only are interested in one station
  dplyr::rename(solalt = data)                     # Rename "data" column to "irradiance"


solalt <- solalt %>%
  mutate(hour = hour(datetime)) %>%
  group_by(yday, hour) %>%
  summarise(solalt = mean(solalt))


# Merge these two data frames into one:
irradiance_df <- left_join(irradiance, solalt, by = c("yday", "hour"))

# Read attenuation_coefficients from file:
attenuation_df <- read.delim("data/modeled_coeff_light_extinction_tuna.txt") %>%
  rename(K = modeled_coefficient_extinction)


############################################################################################

##################################################
# Create vertically resolved ambient light field:

light_field_df <- expand_grid(attenuation_df, irradiance_df) %>%
  group_by(datetime) %>%
  mutate(Kcum = cumsum(K)) %>%
  mutate(Ez = irradiance * exp(-Kcum)) 


# Outfile the data:
save(light_field_df, file = "data/light_field.RData")
save(irradiance_df, file = "data/irradiance_df.RData")

# Time out:
print(Sys.time() - time0)

# Clean environment:
rm(list = ls())

#                                          END OF SCRIPT
#############################################################################################################