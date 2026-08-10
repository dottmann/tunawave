#####################################################################
## Script name: heatwaves
##
## Authors: Daniel Ottmann 
## Email: daniel.ottmann.riera@gmail.com
##
## Last update: August 2026
##
## ---------------------------
## README:
## This script manipulates and visualizes SST data from the spawning area around the Balearic Islands.
## It also sources IPCC.R code and loads IPCC projections down-scaled to the spawning area.
## It then generates and visualizes historic and regressed daily SST files that will be used to run the tuna physiology model.
## SST data of the spawning area was provided by Mélanie Juza from a Copernicus satellite model.
## ---------------------------
#####################################################################

# Create "exclude" function:
"%ni%" <- Negate("%in%")

# Clean environment:
rm(list=ls())

# Load packges:
library(tidyverse)
library(ncdf4)
library(ggthemes)
library(zoo)

# Get the directory path of the currently running script and set directory:
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path.expand(script_dir))
setwd('../')

# Load data fo 1982-2025:
nc_data <- nc_open('data/daily_SST_MED_SST_L4_REP_OBSERVATIONS_010_021_MHW_ref1982-2021_1982-2025_TUNAWAVE_Balears.nc')


# Get dimension:
time <- ncvar_get(nc_data, "time")

# Get variables:
mean_sst <- ncvar_get(nc_data, "mean_sst")
std_sst <- ncvar_get(nc_data, "std_sst")
mhw_int_abovemean <- ncvar_get(nc_data, "mhw_int_abovemean")
mhw_int_abovethres <- ncvar_get(nc_data, "mhw_int_abovethres")
mhw_category <- ncvar_get(nc_data, "mhw_category")

###################################################

# Convert -999s to NAs:
mhw_int_abovemean[mhw_int_abovemean == -999] <- NA
mhw_int_abovethres[mhw_int_abovethres == -999] <- NA
mhw_category[mhw_category == -999] <- 0


# Time units are seconds since 1981-01-01, but variable units are in day
# Convert seconds to days:
time <- time/(60*60*24)

# Convert to date:
# Initial date:
start_date <- as.Date("1981-01-01")

# Date:
date <- start_date + time
# # Heat wave date:

# Put it in a table:
df <- data.frame(id = 1:length(time),
                 time = time,
                 date = date,
                 year = year(date),
                 month = month(date),
                 day = day(date),
                 yday = yday(date),
                 # mhw_date,
                 mean_sst = mean_sst,
                 std_sst = std_sst, 
                 mhw_int_abovemean = mhw_int_abovemean,
                 mhw_int_abovethres = mhw_int_abovethres,
                 mhw_category = as.factor(mhw_category))


# Calculate overall mean sst of all years so we can estimate the temperature anomaly:
overall_mean_sst <- df %>%
  group_by(yday) %>%
  summarise(overall_mean_sst = mean(mean_sst),
            sd_sst = sd(mean_sst),
            p90 = quantile(mean_sst, probs = 0.9, na.rm = TRUE)) %>% 
  arrange(yday) %>% 
  mutate(
    p90_running_avg = rollmean(p90, k = 5, fill = NA, align = "center")
  )

df <- df %>%
  left_join(overall_mean_sst, by = "yday") %>%
  mutate(sst_anomaly = mean_sst - overall_mean_sst)


# Create df for heatwaves type 1 and type 2:
df_hw1 <- df %>%
  mutate(mean_sst = case_when(mhw_category == 1 ~ mean_sst,
                              T ~ NA),
         sst_anomaly =  case_when(mhw_category == 1 ~ sst_anomaly,
                                  T ~ NA))
df_hw2 <- df %>%
  mutate(mean_sst = case_when(mhw_category == 2 ~ mean_sst,
                              T ~ NA),
         sst_anomaly =  case_when(mhw_category == 2 ~ sst_anomaly,
                                  T ~ NA))


# Plot it out:
my_palette <- c("gray", "#FF3333", "#990000")
ymax_sst = max(df$mean_sst)
ymin_sst = min(df$mean_sst)
ymax_anom = max(df$sst_anomaly)
ymin_anom = min(df$sst_anomaly)


# Annual SST:
p <- ggplot() +
  geom_line(data = df, 
            aes(x = yday, y = mean_sst, group = year, color = year), size = .2, alpha = .9) + # , color = my_palette[1]
  scale_color_gradient2(low = "deepskyblue", mid = "gray", high = "orange", midpoint = 2000) +
  geom_line(data = overall_mean_sst,
            aes(x = yday, y = p90_running_avg), color = my_palette[3], size = .6) +
  geom_line(data = overall_mean_sst, 
            aes(x = yday, y = overall_mean_sst), size = 1) +
  geom_vline(xintercept = c(121, 243), linetype = "dotted") +
  geom_label(data = overall_mean_sst, 
             label = "May 1st", x = 121, y = 14, label.size = 0.3, color = "black", hjust = 0) +
  geom_label(data = overall_mean_sst, 
             label = "August 31th", x = 243, y = 14, label.size = 0.3, color = "black", hjust = 0) +
  
  geom_label(data = overall_mean_sst,
             label = "Average", x = 0, y = 29, label.size = 0.3, color = "black", hjust = 0, size = 5) +
  geom_label(data = overall_mean_sst,
             label = "Percentile 90", x = 0, y = 27, label.size = 0.3, color = "#990000", hjust = 0, size = 5) +
  scale_x_continuous(breaks = c(1, 60, 121, 182, 244, 305), 
                     labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  xlab("Month") +
  ylab("Sea surface temperature (ºC)") +
  theme_base() +
  theme(plot.background = element_blank(),
        panel.background = element_blank())

p
ggsave("plots/mean_sst.png", p, height = 60 , width = 80, units = "mm", scale = 2) 



df_hw <- rbind(df_hw1, df_hw2)


# SST:
(p <- ggplot() +
    geom_line(data = subset(df, year > 1899), 
              aes(x = yday, y = overall_mean_sst), color = "black", size = .5) +    
    geom_line(data = subset(df, year > 1899), 
              aes(x = yday, y = mean_sst), color = my_palette[1]) +
    geom_line(data = subset(df_hw1, year > 1899), 
              aes(x = yday, y = mean_sst), color = my_palette[2]) +
    geom_line(data = subset(df_hw2, year > 1899), 
              aes(x = yday, y = mean_sst), color = my_palette[3]) +
    geom_vline(xintercept = c(152, 232), linetype = "dotted") +
    facet_wrap(~year) +
    theme_bw() +
    theme(#plot.background = element_blank(),
          #panel.background = element_blank()
          )
)
ggsave("plots/sst_by_year.png", p, height = 60 , width = 100, units = "mm", scale = 4)


# SST anomaly:
selected_stations <- c(1982:2025) # :1986, 2000:2004, 2020

(p <- ggplot() +
    geom_line(data = subset(df, year %in% selected_stations), 
              aes(x = yday, y = sst_anomaly), color = my_palette[1]) +
    geom_line(data = subset(df_hw1, year %in% selected_stations), 
              aes(x = yday, y = sst_anomaly), color = my_palette[2]) +
    geom_line(data = subset(df_hw2, year %in% selected_stations), 
              aes(x = yday, y = sst_anomaly), color = my_palette[3]) +
    geom_hline(yintercept = 0, size = .3) +
    geom_vline(xintercept = c(121, 243), linetype = "dotted") +
    scale_x_continuous(breaks = c(1, 91, 182, 274), 
      labels = c("Jan", "Apr", "Jul", "Oct")) +
    facet_wrap(~year, ncol = 7) +
    xlab("Month") + 
    ylab("SST anomaly (Tº)") +
    ylim(c(-4, 4.55)) +
    theme_base() +
    theme(plot.background = element_blank(),
          panel.background = element_blank())
)
ggsave("plots/sst_anomaly_by_year.png", p, height = 80 , width = 100, units = "mm", scale = 4) 


# Mean sst is a bit noisy. Make a table with running mean SST over 9-day periods:
df <- df %>%
  mutate(running_sst = rollmean(overall_mean_sst, k = 9, fill = NA, align = "center"))

# Compare:
p <- ggplot() +
  geom_line(data = df, 
            aes(x = yday, y = overall_mean_sst), size = 1) +
  theme_base() +
  theme(panel.background = element_blank())

p

p <- ggplot() +
  geom_line(data = df, 
            aes(x = yday, y = running_sst), size = 1) +
  theme_base() +
  theme(panel.background = element_blank())

p

# Save temperatures of an average year:
temperature_data_mean <- df %>%
  slice(1:365) %>%
  dplyr::select(yday, running_sst) %>%
  rename(temperature = running_sst) %>%
  mutate(temperature = replace_na(temperature, 15.4))

# Loop it for every year:
df_loop <- df %>%
  dplyr::select(yday, mean_sst, year) %>%
  rename(temperature = mean_sst)

temperature_data_annual <- split(df_loop, df_loop$year)

# Outifle data:
save(df, file = "data/temperature_data_mean_1982_2025_no_smooth.Rdata")
save(temperature_data_mean, file = "data/temperature_data_mean_1982_2025.Rdata")
save(temperature_data_annual, file = "data/temperature_data_annual_1982_2025.Rdata")


# Look at temperature in the spawning season:
df_season <- df %>%
  filter(month %in% 5:8)

# For each year, take the mean and sd of the temperature:
df_season <- df_season %>%
  group_by(year) %>%
  reframe(seasonal_mean_sst = mean(mean_sst, na.rm = T),
          sd_sst = sd(mean_sst, na.rm = T),
          sd_low = seasonal_mean_sst - sd_sst,
          sd_high = seasonal_mean_sst + sd_sst,
          se_sst = sd_sst / sqrt(n()),
          se_low = seasonal_mean_sst - se_sst,
          se_high = seasonal_mean_sst - se_sst,)

mean_t_1982_2025 <- mean(df_season$seasonal_mean_sst) # To be used in IPCC.R


# Plot it out:
p <- ggplot(data = df_season) +
  geom_smooth(aes(x = year, y = seasonal_mean_sst), method = "lm", color = "gray", se = F, size = 1) +
  geom_ribbon(aes(x = year, y = seasonal_mean_sst, ymin = sd_low, ymax = sd_high), fill = "red", alpha = .1) +
  geom_path(aes(x = year, y = seasonal_mean_sst), color = "red", size = 1) +
  ylab("May-August temperature (ºC)") +
  theme_base() +
  theme(panel.background = element_blank(),
        plot.background = element_blank())

p

my_model <- lm(seasonal_mean_sst ~ year, data = df_season)

summary(my_model)

mean_t_1982_2025 <- mean(predict(my_model, newdata = data.frame(year = 1982:2025))) # Same as mean(df_season$seasonal_mean_sst)


df_season <- df_season %>%
  mutate(type = "historic")

# Run IPCC.R code, then continue here:
source("R/IPCC.R")

# Load SSP predictions:
load(file = "data/ssp_scenarios_annual_means.Rdata")


df_season <- rbind(df_season,
                   data.frame(year = 2026:2100,
                              seasonal_mean_sst = subset(df, scenario == "ssp126")$sst[12:length(subset(df, scenario == "ssp126")$sst)] + 1.2, # Need to add 1.2ºC for calibration with satellite data
                              sd_sst = NA,
                              sd_low = NA, 
                              sd_high = NA,
                              se_sst = NA,
                              se_low = NA,
                              se_high = NA,
                              type = "ssp126"))

df_season <- rbind(df_season,
                   data.frame(year = 2026:2100,
                              seasonal_mean_sst = subset(df, scenario == "ssp245")$sst[12:length(subset(df, scenario == "ssp245")$sst)] + 1.2, # Need to add 1.2ºC for calibration with satellite data
                              sd_sst = NA,
                              sd_low = NA, 
                              sd_high = NA,
                              se_sst = NA,
                              se_low = NA,
                              se_high = NA,
                              type = "ssp245"))

df_season <- rbind(df_season,
                   data.frame(year = 2026:2100,
                              seasonal_mean_sst = subset(df, scenario == "ssp370")$sst[12:length(subset(df, scenario == "ssp370")$sst)] + 1.2, # Need to add 1.2ºC for calibration with satellite data
                              sd_sst = NA,
                              sd_low = NA, 
                              sd_high = NA,
                              se_sst = NA,
                              se_low = NA,
                              se_high = NA,
                              type = "ssp370"))

df_season <- rbind(df_season,
                   data.frame(year = 2026:2100,
                              seasonal_mean_sst =subset(df, scenario == "ssp585")$sst[12:length(subset(df, scenario == "ssp585")$sst)] + 1.2, # Need to add 1.2ºC for calibration with satellite data
                              sd_sst = NA,
                              sd_low = NA, 
                              sd_high = NA,
                              se_sst = NA,
                              se_low = NA,
                              se_high = NA,
                              type = "ssp585"))


p <- ggplot() +
  geom_smooth(data = subset(df_season, type == "historic"), aes(x = year, y = seasonal_mean_sst), method = "lm", color = "gray", se = F, size = 1) +
  geom_ribbon(data = subset(df_season, type == "historic"), aes(x = year, y = seasonal_mean_sst, ymin = sd_low, ymax = sd_high), fill = "black", alpha = .1) +
  geom_path(data = df_season, aes(x = year, y = seasonal_mean_sst, group = type, color = type),  size = 1) +
  scale_color_manual(values = c("black", "olivedrab", "gold","red", "red4"), labels = c("Historic", "SSP126", "SSP245", "SSP370", "SSP585"), name = "Projection") +
  # scale_linetype_manual(values = c("solid", "solid", "solid")) +
  ylab("May-August temperature (ºC)  ") +
  scale_y_continuous(breaks = seq(15, 29, by = 2))+
  theme_base() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.position = c(0.85, 0.3))

p
ggsave("plots/sst_ineranual_trend_projections.png", p, height = 60 , width = 120, units = "mm", scale = 1.5) 

# Make projections of temperature differences:
historic <- predict(my_model, newdata = data.frame(year = 1982:2025)) - mean_t_1982_2025
historic <- data.frame(year = 1982:2025, t_diff = historic)

df_project <- temperature_data_mean %>%
  dplyr::select(yday, temperature) %>%
  rename(mean_sst = temperature) %>%
  slice(1:365) 


# Combine all ydays with all years for historic projections:
df_all <- temperature_data_mean %>%
  crossing(historic) %>%               
  mutate(
    temperature = temperature + t_diff
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

save(temperature_data_annual, file = "data/temperature_projections_historic.Rdata")

#                          END OF SCRIPT