## Script name: Model plotting module
##
##
## Author: Daniel Ottmann
## Email: daniel.ottmann.riera@gmail.com
##
## Last update:  August 2026
##
## ---------------------------
##
## Readme:
##
## This script plots survival curves of bluefin tuna using averaged daily temperatures observed between 1982-2025
##
########################################################################################################################################


##################################################################
# Set time:
time0 <- Sys.time()

# Create an "those that are not within" function:
"%ni%" <- Negate("%in%")

# Set size and colors:
size1 = 1.2
col1 <- "#619CFF"
col2 <- "darkgoldenrod1"
col3 <- "#CC3333"

# Make function to get daily values:
process_master_data <- function(df) {
  
  # 1. Initial transformations and moon phase flags
  # Note: I replaced reframe() with mutate() for the first part 
  # as you are keeping all original columns. It's much cleaner!
  processed_df <- df %>%
    mutate(
      date = as.Date(datetime),
      month = month(date),
      test_run = as.factor(test_run)
    ) %>%
    group_by(month, test_run) %>%
    mutate(
      full_moon = moon_fraction == max(moon_fraction, na.rm = TRUE),
      new_moon  = moon_fraction == min(moon_fraction, na.rm = TRUE)
    ) %>%
    ungroup()
  
  # 2. Daily aggregation
  master_df_day <- processed_df %>%
    group_by(yday, test_run) %>%
    reframe(
      date = first(date),
      moon_fraction = first(moon_fraction),
      temperature = mean(temperature, na.rm = TRUE),
      
      mean_egg_total_survival = mean(egg_total_survival, na.rm = TRUE),
      min_egg_total_survival = min(egg_total_survival, na.rm = TRUE),
      max_egg_total_survival = max(egg_total_survival, na.rm = TRUE),
      
      mean_ysl_total_survival = mean(ysl_total_survival, na.rm = TRUE),
      min_ysl_total_survival = min(ysl_total_survival, na.rm = TRUE),
      max_ysl_total_survival = max(ysl_total_survival, na.rm = TRUE),
      
      mean_fitness = mean(fitness, na.rm = TRUE),
      min_fitness = min(fitness, na.rm = TRUE),
      max_fitness = max(fitness, na.rm = TRUE),
      
      full_moon = any(full_moon),
      # Adding manual logic for March (yday 61)
      new_moon = case_when(
        yday == 61 ~ TRUE,
        TRUE ~ any(new_moon)
      ),
      .groups = "drop"
    )
  
  # 3. Apply fitness threshold for plotting
  final_df <- master_df_day %>%
    mutate(
      mean_fitness = if_else(mean_fitness > 10^-10, mean_fitness, 0),
      min_fitness  = if_else(mean_fitness > 10^-10, min_fitness, 0),
      max_fitness  = if_else(mean_fitness > 10^-10, max_fitness, 0)
    )
  
  return(final_df)
}

# Load data:
# *To load sensitivity plots, add: #/mcgurck_sensitivity/95 or /mcgurck_sensitivity/85 after /scenarios/

load("out/scenarios/mcgurck_sensitivity/85/temperature+0/master_df_very_low_food.Rdata") 
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_0_very_low_food <- master_df_day_temp %>%
  mutate(test_run = "Very low food",
         temperature_modification = 0)

load("out/scenarios/mcgurck_sensitivity/85/temperature+0/master_df_low_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_0_low_food <- master_df_day_temp %>%
  mutate(test_run = "Low food",
         temperature_modification = 0)

load("out/scenarios/mcgurck_sensitivity/85/temperature+0/master_df_moderate_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_0_mid_food <- master_df_day_temp %>%
  mutate(test_run = "Moderate food",
         temperature_modification = 0)

load("out/scenarios/mcgurck_sensitivity/85/temperature+0/master_df_high_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_0_high_food <- master_df_day_temp %>%
  mutate(test_run = "High food",
         temperature_modification = 0)

load("out/scenarios/mcgurck_sensitivity/85/temperature+0/master_df_very_high_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_0_very_high_food <- master_df_day_temp %>%
  mutate(test_run = "Very high food",
         temperature_modification = 0)

load("out/scenarios/mcgurck_sensitivity/85/temperature+2/master_df_very_low_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_2_very_low_food <- master_df_day_temp %>%
  mutate(test_run = "Very low food",
         temperature_modification = 2)

load("out/scenarios/mcgurck_sensitivity/85/temperature+2/master_df_low_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_2_low_food <- master_df_day_temp %>%
  mutate(test_run = "Low food",
         temperature_modification = 2)

load("out/scenarios/mcgurck_sensitivity/85/temperature+2/master_df_moderate_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_2_mid_food <- master_df_day_temp %>%
  mutate(test_run = "Moderate food",
         temperature_modification = 2)

load("out/scenarios/mcgurck_sensitivity/85/temperature+2/master_df_high_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_2_high_food <- master_df_day_temp %>%
  mutate(test_run = "High food",
         temperature_modification = 2)

load("out/scenarios/mcgurck_sensitivity/85/temperature+2/master_df_very_high_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp_2_very_high_food <- master_df_day_temp %>%
  mutate(test_run = "Very high food",
         temperature_modification = 2)

load("out/scenarios/mcgurck_sensitivity/85/temperature-2/master_df_very_low_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp__2_very_low_food <- master_df_day_temp %>%
  mutate(test_run = "Very low food",
         temperature_modification = -2)

load("out/scenarios/mcgurck_sensitivity/85/temperature-2/master_df_low_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp__2_low_food <- master_df_day_temp %>%
  mutate(test_run = "Low food",
         temperature_modification = -2)

load("out/scenarios/mcgurck_sensitivity/85/temperature-2/master_df_moderate_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp__2_mid_food <- master_df_day_temp %>%
  mutate(test_run = "Moderate food",
         temperature_modification = -2)

load("out/scenarios/mcgurck_sensitivity/85/temperature-2/master_df_high_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp__2_high_food <- master_df_day_temp %>%
  mutate(test_run = "High food",
         temperature_modification = -2)

load("out/scenarios/mcgurck_sensitivity/85/temperature-2/master_df_very_high_food.Rdata")
master_df_day_temp <- process_master_data(master_df)
master_df_day_temp__2_very_high_food <- master_df_day_temp %>%
  mutate(test_run = "Very high food",
         temperature_modification = -2)

master_df_day <- rbind(master_df_day_temp_0_very_low_food, master_df_day_temp_0_low_food, master_df_day_temp_0_mid_food, master_df_day_temp_0_high_food, master_df_day_temp_0_very_high_food,
                       master_df_day_temp_2_very_low_food, master_df_day_temp_2_low_food, master_df_day_temp_2_mid_food, master_df_day_temp_2_high_food, master_df_day_temp_2_very_high_food, 
                       master_df_day_temp__2_very_low_food, master_df_day_temp__2_low_food, master_df_day_temp__2_mid_food, master_df_day_temp__2_high_food, master_df_day_temp__2_very_high_food) %>%
  mutate(temperature_modification = as.factor(temperature_modification),
         test_run = as.factor(test_run))

# Convert test_run into a factor with the desired order:
master_df_day$test_run <- factor(
  master_df_day$test_run,
  levels = c("Very low food", "Low food", "Moderate food", "High food", "Very high food")
)


(p <- ggplot(data = subset(master_df_day, yday %in% 110:283)) +
    geom_ribbon(aes(x = yday, ymin = min_fitness, ymax = max_fitness, fill = temperature_modification), alpha = .3) +
    geom_path(aes(yday, mean_fitness, colour = temperature_modification), linetype = 1, size = size1) +
    labs(x = "Spawning time (year h)", y = "Larval survival", color = "Temperature modification (ºC)", fill = "Temperature modification (ºC)") +
    scale_color_manual(values = c(col1, col2, col3)) +
    scale_fill_manual(values = c(col1, col2, col3)) +
    # scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
    # ylim(c(1e-11, 7e-7)) +
    # scale_y_log10(lim = c(1e-11, 3e-5),
    #               breaks = trans_breaks("log10", function(x) 10^x),
    #               labels = trans_format("log10", math_format(10^.x))) +
    # annotation_logticks(sides = "l") +
    scale_x_continuous(breaks = c(121, 182,  244), 
                       labels = c("May", "Jul", "Sep")) +
    facet_wrap(~ test_run, scales = "free_y") + 
    theme_base() +
    theme(plot.background = element_blank(),
          panel.background = element_blank(),
          strip.background = element_blank(),
          # strip.text.x = element_blank(),
          axis.title.x.bottom = element_blank(),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.position = c(0.845, .2)) 
)

# Save:
# *Add _mcgurck85 or _mcgurck95 for sensitivity plots
ggsave("plots/scenarios/fitness_plot_low_mid_high_temperature_scenarios_free_scale_mcgurck85.png", p, height = 110 , width = 185, units = "mm", scale = 1)

#                                          END OF SCRIPT
#############################################################################################################