#####################################################################
## Script name: IPCC
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Last update: August 2026
##
## ---------------------------
## README:
## This script down-scales IPCC predictions of SST to the spawning area.
## It generates annual and daily SST files that will be used to run the tuna physiology model.
## The SST IPCC predictions (CMIP6) were downloaded from the Copernicus Climate Data Store https://cds.climate.copernicus.eu/datasets/projections-cmip6?tab=overview
## ---------------------------
#####################################################################

# Load libraries:
library(tidyverse)
library(ncdf4)
library(reshape2)

# Set working directory:
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path.expand(script_dir))
setwd('../')

# Prepare dataframe for ssp1.26: 
file_path <- "C:/Users/danot/Documents/IPCC/sst_CMIP6_ssp126_mon_201501-210012.nc"

nc <- nc_open(file_path)

# Extract time dimension and convert to seconds:
time <- ncvar_get(nc, "time")
# Time is in days since 1850-01-01 Convert time to date:
start_date <- as.Date("1850-01-01")
# Date:
date <- start_date + time

# Extract lat and lon dimensions:
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

# Extract and crop variables:
sst <- ncvar_get(nc, "sst")

# Define summer months (May to August):
full_series <- 1:dim(sst)[3]

# We look for remainders of 5, 6, 7, and 8 when divided by 12
summer_months <- full_series[full_series %% 12 %in% 5:8]

# Subset area arround balearics and summer months:
sst <- sst[182:185, 129:131, summer_months, ]

# Take athe average of all 28 member simulations:
sst <- apply(sst, c(1, 2, 3), mean, na.rm = TRUE)

dim(sst)

# Convert to data frmae with year and sst columns:
# 1. Setup metadata for the dimensions
# We know we have 86 years, and each year now has 4 months (May, Jun, Jul, Aug)
lon_vals <- 1:4
lat_vals <- 1:3
years    <- 1:86
months   <- c("May", "Jun", "Jul", "Aug")

# 2. Convert the 3D array (sst) to a data frame
# Providing dimnames allows us to convert directly to a "long" format
dimnames(sst) <- list(lon = lon_vals, lat = lat_vals, time = 1:344)

sst_df <- as.data.frame.table(sst, responseName = "sst") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat)),
    time = as.numeric(as.character(time))
  )

# 3. Add Year column and calculate the annual summer mean
# Since the 344 months are ordered (Yr1: M,J,J,A, Yr2: M,J,J,A...), 
# we can assign years using integer division
df_ssp126 <- sst_df %>%
  mutate(
    # (time - 1) %/% 4 gives 0 for the first 4 months, 1 for the next 4, etc.
    year = ((time - 1) %/% 4) + 1 
  ) %>%
  group_by(year) %>%
  summarize(
    sst = mean(sst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = year + 2014,
         scenario = "ssp126")


# Repeat for
file_path <- "C:/Users/danot/Documents/IPCC/sst_CMIP6_ssp245_mon_201501-210012.nc"


nc <- nc_open(file_path)

# Extract time dimension and convert to seconds:
time <- ncvar_get(nc, "time")
# Time is in days since 1850-01-01 Convert time to date:
start_date <- as.Date("1850-01-01")
# Date:
date <- start_date + time

# Extract lat and lon dimensions:
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

# Extract and crop variables:
sst <- ncvar_get(nc, "sst")

# Define summer months (May to August):
full_series <- 1:dim(sst)[3]

# We look for remainders of 5, 6, 7, and 8 when divided by 12
summer_months <- full_series[full_series %% 12 %in% 5:8]

# Subset area arround balearics and summer months:
sst <- sst[182:185, 129:131, summer_months, ]

# Take athe average of all 28 member simulations:
sst <- apply(sst, c(1, 2, 3), mean, na.rm = TRUE)

dim(sst)

# Convert to data frmae with year and sst columns:
# 1. Setup metadata for the dimensions
# We know we have 86 years, and each year now has 4 months (May, Jun, Jul, Aug)
lon_vals <- 1:4
lat_vals <- 1:3
years    <- 1:86
months   <- c("May", "Jun", "Jul", "Aug")

# 2. Convert the 3D array (sst) to a data frame
# Providing dimnames allows us to convert directly to a "long" format
dimnames(sst) <- list(lon = lon_vals, lat = lat_vals, time = 1:344)

sst_df <- as.data.frame.table(sst, responseName = "sst") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat)),
    time = as.numeric(as.character(time))
  )

# 3. Add Year column and calculate the annual summer mean
# Since the 344 months are ordered (Yr1: M,J,J,A, Yr2: M,J,J,A...), 
# we can assign years using integer division
df_ssp245 <- sst_df %>%
  mutate(
    # (time - 1) %/% 4 gives 0 for the first 4 months, 1 for the next 4, etc.
    year = ((time - 1) %/% 4) + 1 
  ) %>%
  group_by(year) %>%
  summarize(
    sst = mean(sst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = year + 2014,
         scenario = "ssp245")


# Repeat for ssp3.70: 
file_path <- "C:/Users/danot/Documents/IPCC/sst_CMIP6_ssp370_mon_201501-210012.nc"

nc <- nc_open(file_path)

# Extract time dimension and convert to seconds:
time <- ncvar_get(nc, "time")
# Time is in days since 1850-01-01 Convert time to date:
start_date <- as.Date("1850-01-01")
# Date:
date <- start_date + time

# Extract lat and lon dimensions:
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

# Extract and crop variables:
sst <- ncvar_get(nc, "sst")

# Define summer months (May to August):
full_series <- 1:dim(sst)[3]

# We look for remainders of 5, 6, 7, and 8 when divided by 12
summer_months <- full_series[full_series %% 12 %in% 5:8]

# Subset area arround balearics and summer months:
sst <- sst[182:185, 129:131, summer_months, ]

# Take athe average of all 28 member simulations:
sst <- apply(sst, c(1, 2, 3), mean, na.rm = TRUE)

dim(sst)

# Convert to data frmae with year and sst columns:
# 1. Setup metadata for the dimensions
# We know we have 86 years, and each year now has 4 months (May, Jun, Jul, Aug)
lon_vals <- 1:4
lat_vals <- 1:3
years    <- 1:86
months   <- c("May", "Jun", "Jul", "Aug")

# 2. Convert the 3D array (sst) to a data frame
# Providing dimnames allows us to convert directly to a "long" format
dimnames(sst) <- list(lon = lon_vals, lat = lat_vals, time = 1:344)

sst_df <- as.data.frame.table(sst, responseName = "sst") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat)),
    time = as.numeric(as.character(time))
  )

# 3. Add Year column and calculate the annual summer mean
# Since the 344 months are ordered (Yr1: M,J,J,A, Yr2: M,J,J,A...), 
# we can assign years using integer division
df_ssp370 <- sst_df %>%
  mutate(
    # (time - 1) %/% 4 gives 0 for the first 4 months, 1 for the next 4, etc.
    year = ((time - 1) %/% 4) + 1 
  ) %>%
  group_by(year) %>%
  summarize(
    sst = mean(sst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = year + 2014,
         scenario = "ssp370")


# Repeat for ssp5.85: 
file_path <- "C:/Users/danot/Documents/IPCC/sst_CMIP6_ssp585_mon_201501-210012.nc"

nc <- nc_open(file_path)

# Extract time dimension and convert to seconds:
time <- ncvar_get(nc, "time")
# Time is in days since 1850-01-01 Convert time to date:
start_date <- as.Date("1850-01-01")
# Date:
date <- start_date + time

# Extract lat and lon dimensions:
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

# Extract and crop variables:
sst <- ncvar_get(nc, "sst")

# Define summer months (May to August):
full_series <- 1:dim(sst)[3]

# We look for remainders of 5, 6, 7, and 8 when divided by 12
summer_months <- full_series[full_series %% 12 %in% 5:8]

# Subset area arround balearics and summer months:
sst <- sst[182:185, 129:131, summer_months, ]

# Take athe average of all 28 member simulations:
sst <- apply(sst, c(1, 2, 3), mean, na.rm = TRUE)

dim(sst)

# Convert to data frmae with year and sst columns:
# 1. Setup metadata for the dimensions
# We know we have 86 years, and each year now has 4 months (May, Jun, Jul, Aug)
lon_vals <- 1:4
lat_vals <- 1:3
years    <- 1:86
months   <- c("May", "Jun", "Jul", "Aug")

# 2. Convert the 3D array (sst) to a data frame
# Providing dimnames allows us to convert directly to a "long" format
dimnames(sst) <- list(lon = lon_vals, lat = lat_vals, time = 1:344)

sst_df <- as.data.frame.table(sst, responseName = "sst") %>%
  mutate(
    lon = as.numeric(as.character(lon)),
    lat = as.numeric(as.character(lat)),
    time = as.numeric(as.character(time))
  )

# 3. Add Year column and calculate the annual summer mean
# Since the 344 months are ordered (Yr1: M,J,J,A, Yr2: M,J,J,A...), 
# we can assign years using integer division
df_ssp585 <- sst_df %>%
  mutate(
    # (time - 1) %/% 4 gives 0 for the first 4 months, 1 for the next 4, etc.
    year = ((time - 1) %/% 4) + 1 
  ) %>%
  group_by(year) %>%
  summarize(
    sst = mean(sst, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year = year + 2014,
         scenario = "ssp585")



# Merge all data frames:
df <- rbind(df_ssp245, df_ssp585, df_ssp126, df_ssp370)


# Plot it out:
(p <- ggplot() +
    geom_point(data = df, aes(x = year, y = sst, color = scenario))
    )

# Outfile SSP scenarios:
save(df, file = "data/ssp_scenarios_annual_means.Rdata")

# Mean temperature during the spawning season between 1982 and 2025 calculated in heatwaves.R:
mean_t_1982_2025 <- 22.84628

# Make projections of temperature differences:
ssp126 <- df_ssp126$sst[12:nrow(df_ssp126)] + 1.2 - mean_t_1982_2025   # Need to add 1.2ºC for calibration with satellite data
ssp126 <- data.frame(year = 2026:2100, t_diff = ssp126)

ssp245 <- df_ssp245$sst[12:nrow(df_ssp245)] + 1.2 - mean_t_1982_2025   # Need to add 1.2ºC for calibration with satellite data
ssp245 <- data.frame(year = 2026:2100, t_diff = ssp245)

ssp370 <- df_ssp370$sst[12:nrow(df_ssp370)] + 1.2 - mean_t_1982_2025   # Need to add 1.2ºC for calibration with satellite data
ssp370 <- data.frame(year = 2026:2100, t_diff = ssp370)

ssp585 <- df_ssp585$sst[12:nrow(df_ssp585)] + 1.2 - mean_t_1982_2025   # Need to add 1.2ºC for calibration with satellite data
ssp585 <- data.frame(year = 2026:2100, t_diff = ssp585)

load(file = "data/temperature_data_mean_1982_2025_no_smooth.Rdata")

df_project <- df %>%
  dplyr::select(yday, overall_mean_sst) %>%
  rename(mean_sst = overall_mean_sst) %>%
  slice(1:365) 


# combine all ydays with all years for ssp126:
df_all <- df_project %>%
  crossing(ssp126) %>%               
  mutate(
    temperature = mean_sst + t_diff
  ) %>%
  dplyr::select(
    yday,
    temperature,
    year
  )

# Save it in a list:
temperature_data_annual <- df_all %>%
  group_split(year) %>%
  set_names(unique(df_all$year))

save(temperature_data_annual, file = "data/temperature_projections_ssp126.Rdata")

# combine all ydays with all years for ssp245:
df_all <- df_project %>%
  crossing(ssp245) %>%               
  mutate(
    temperature = mean_sst + t_diff
  ) %>%
  dplyr::select(
    yday,
    temperature,
    year
  )

# Save it in a list:
temperature_data_annual <- df_all %>%
  group_split(year) %>%
  set_names(unique(df_all$year))

save(temperature_data_annual, file = "data/temperature_projections_ssp245.Rdata")


# combine all ydays with all years for ssp370:
df_all <- df_project %>%
  crossing(ssp370) %>%               
  mutate(
    temperature = mean_sst + t_diff
  ) %>%
  dplyr::select(
    yday,
    temperature,
    year
  )

# Save it in a list:
temperature_data_annual <- df_all %>%
  group_split(year) %>%
  set_names(unique(df_all$year))

save(temperature_data_annual, file = "data/temperature_projections_ssp370.Rdata") # combine all ydays with all years for ssp585:
df_all <- df_project %>%
  crossing(ssp585) %>%               
  mutate(
    temperature = mean_sst + t_diff
  ) %>%
  dplyr::select(
    yday,
    temperature,
    year
  )

# Save it in a list:
temperature_data_annual <- df_all %>%
  group_split(year) %>%
  set_names(unique(df_all$year))

save(temperature_data_annual, file = "data/temperature_projections_ssp585.Rdata")


#                END OF SCRIPT