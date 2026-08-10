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
## This script plots larval tuna survival under different marine heat wace scenarios
## It also generates a gif with a running MHW and its effect on survivalunder different food conditions
##
########################################################################################################################################

# Load giph library:
library(gifski)
library(patchwork)
library(grid)
library(magick)
library(cowplot)

##################################################################
# Set time:
time0 <- Sys.time()

# Create an "those that are not within" function:
"%ni%" <- Negate("%in%")

# Make color palette & specify size:
col0 <- "black"
col1 <- "darkorange"
col2 <- "red"

size1 = 1.2

##############
# * In this file, only the window length needs to be edited.

# 1. Food settings:
nauplia_dens <- 2000  
window_length <- 10       # 5; 10
# Food scalars:
v_food_scalars <- c(0.75, 1, 1.125, 1.25, 1.5)
# Y limits:
y_lim_list <- list(c(0, 7e-10),  # For 1500
                   c(0, 1.3e-6), # For 2000
                   c(0, 7.5e-6), # For 2250
                   c(0, 3e-5),   # For 2500
                   c(0, 8e-5))   # For 3000

# Start bigger loop:
for(j in 1:length(v_food_scalars)){
  
  food_scalar <- v_food_scalars[j]
  food <- as.character(nauplia_dens * food_scalar)
  
  print(paste0("Working on food = ", food))
  
  y_limits <- y_lim_list[[j]]
  
  # y_limits <- c(0, 7e-10) # For 1500
  # y_limits <- c(0, 1.3e-6) # For 2000
  # y_limits <- c(0, 7.5e-6) # For 2250 
  # y_limits <- c(0, 3e-5) # For 2500
  # y_limits <- c(0, 8e-5) # For 3000
  
  # 2. Define the starting points for your 5-unit intervals
  start_points <- c(0, seq(130, 270, by = 5))
  
  ifit <- data.frame()
  
  # 3. Start the small loop
  for (i in start_points) {
    
    # Define test run:
    test_run <- paste0(food, "_", i, "_to_", i + window_length)
    
    # Load data:
    load(paste0("out/scenarios/heat_wave/", window_length, "d/master_df_", test_run, ".RData"))
    master_df <- as.data.frame(master_df)
    
    
    ##################################################################
    
    # Standardize variables
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
    
    # Save integrated fitness:
    master_df <- master_df %>%
      mutate(fitness = case_when(fitness < 1e-10 ~ 0,
                                 T ~ fitness))
    ifit <- rbind(ifit, data.frame(window = paste0(i), food = food, ifitness = sum(master_df$fitness)))
    
    
    # Calculate  daily values:
    master_df_day <- master_df %>%
      group_by(yday, test_run) %>%
      
      reframe(date = date,
              moon_fraction = moon_fraction,
              temperature = temperature,
              test_run,
              
              mean_egg_total_survival = mean(egg_total_survival),
              min_egg_total_survival = min(egg_total_survival),
              max_egg_total_survival = max(egg_total_survival),
              
              mean_ysl_total_survival = mean(ysl_total_survival),
              min_ysl_total_survival = min(ysl_total_survival),
              max_ysl_total_survival = max(ysl_total_survival),
              
              mean_fitness = mean(fitness),
              min_fitness = min(fitness),
              max_fitness = max(fitness),
              full_moon = full_moon,
              new_moon = case_when(yday == 61 ~ T,           # Need to add this one manually because March  has 2 new moons
                                   T ~ new_moon)) 
    
    
    # For plotting purposes, set values of fitness < 10^10 = 0
    master_df_day_temp <- master_df_day %>% mutate(mean_fitness = case_when(mean_fitness > 10^-10 ~ mean_fitness,
                                                                            T ~ 0),
                                                   min_fitness = case_when(mean_fitness > 10^-10 ~ min_fitness,
                                                                           T ~ 0),
                                                   max_fitness = case_when(mean_fitness > 10^-10 ~ max_fitness,
                                                                           T ~ 0))
    
    p11c <- ggplot(data = subset(master_df_day_temp, yday %in% 110:283)) + # , yday %in% 150:190
      
      geom_ribbon(aes(x = date, ymin = min_fitness, ymax = max_fitness, fill = test_run), alpha = .3) +
      geom_path(aes(date, mean_fitness, colour = test_run), linetype = 1, size = size1) +
      labs(x = "Spawning time (year h)", y = "Larval survival", color = "Test run", fill = "Test run") +
      scale_color_manual(values = c(col1, col2, col0)) +
      scale_fill_manual(values = c(col1, col2, col0)) +
      scale_x_date(date_labels = "%b", breaks = "months", expand = expansion(mult = c(0, 0))) +
      # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
      #               labels = trans_format("log10", math_format(10^.x))) +
      ylim(y_limits) +
      annotation_logticks(sides = "l") +
      theme_base() +
      theme(plot.background = element_blank(),
            panel.background = element_blank(),
            axis.title.x.bottom = element_blank(),
            legend.position = "none",
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 10)) #+
    # guides(color = guide_legend(nrow = 1, byrow = TRUE))
    
    ggsave(paste0("plots/scenarios/heat_waves/", window_length, "d/plot_fitness_", test_run, ".png"), p11c, height = 100 , width = 100, units = "mm", scale = 1)
    
    
    # Plot temperature heatwave:
    p <- ggplot(data = subset(master_df_day_temp, yday %in% 110:283)) +
      geom_point(aes(date, temperature), colour = col2, size = size1) +
      labs(x = "Spawning time (year h)", y = "ºC") +
      theme_base() +
      ylim(c(15, 28.5)) +
      theme(plot.background = element_blank(),
            panel.background = element_blank(),
            axis.title.x.bottom = element_blank(),
            # legend.position = "bottom", #"none"
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 10),
            plot.margin = margin(t = 5.5, b = 5.5, r = 5.5, l = 20)) 
    
    ggsave(paste0("plots/scenarios/heat_waves/", window_length, "d/plot_temperature_", i, "_to_", i + window_length, ".png"), p, height = 35 , width = 87.45, units = "mm", scale = 1)
    
    
  }
  
  # Save integrated fitness table:
  save(ifit, file = paste0("out/scenarios/heat_wave/", window_length, "d/integrated_fitness_MHW_", window_length, "d_", food, ".RData"))
  
}

# Time out:
beep(1)
print(Sys.time() - time0)


# Load all the integrated fitness values:
integrated_fitness <- data.frame()

load(file = paste0("out/scenarios/heat_wave/", window_length, "d/integrated_fitness_MHW_", window_length, "d_1500.RData"))
integrated_fitness <- rbind(integrated_fitness, ifit)

load(file = paste0("out/scenarios/heat_wave/", window_length, "d/integrated_fitness_MHW_", window_length, "d_2000.RData"))
integrated_fitness <- rbind(integrated_fitness, ifit)

load(file = paste0("out/scenarios/heat_wave/", window_length, "d/integrated_fitness_MHW_", window_length, "d_2250.RData"))
integrated_fitness <- rbind(integrated_fitness, ifit)

load(file = paste0("out/scenarios/heat_wave/", window_length, "d/integrated_fitness_MHW_", window_length, "d_2500.RData"))
integrated_fitness <- rbind(integrated_fitness, ifit)

load(file = paste0("out/scenarios/heat_wave/", window_length, "d/integrated_fitness_MHW_", window_length, "d_3000.RData"))
integrated_fitness <- rbind(integrated_fitness, ifit)

# Manipulate ifit to plot integrated survival:
baselines <- integrated_fitness %>% 
  filter(window == 0)

integrated_fitness <- integrated_fitness %>% 
  group_by(food) %>%
  slice(-1) %>%
  left_join(baselines, by = "food", suffix = c("", "_baseline")) %>%
  mutate(ifitness_normalized = (ifitness / ifitness_baseline) - 1) %>%
  select(-ifitness_baseline, -window_baseline) %>%
  mutate(window = as.numeric(window),
         date = as.Date(window, origin = "2026-01-01") - 1)

(p <- ggplot(data = integrated_fitness) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_line(aes(x = date, y = 100 * ifitness_normalized, color = food)) + 
    geom_point(aes(x = date, y = 100 * ifitness_normalized, fill = food), shape = 21, alpha = .7) +
    scale_color_manual(values = c("#F8A31B", "#F27314", "#E25033", "#AA3929", "#771C19"),
                       labels = c("Very low", "Low", "Moderate", "High", "Very high"),
                       name = "Food") +
    scale_fill_manual(values = c("#F8A31B", "#F27314", "#E25033", "#AA3929", "#771C19"),
                      labels = c("Very low", "Low", "Moderate", "High", "Very high"),
                      name = "Food") +
    ylab("% change in survival") +
    xlab("Date of heatwave") +
    theme_base() +
    theme(panel.background = element_blank(),
          plot.background = element_blank())
)

ggsave(paste0("plots/scenarios/heat_waves/", window_length, "d/fitness_change_heatwave_", window_length, "d_sm.png"), p, height = 40 , width = 80, units = "mm", scale = 2)


# Prepare images for GIF slides:

# Define start points:
start_points <- seq(130, 270, by = 5)

# Make a loop to lad and patch plots:
for (i in start_points) {
  
  # Read images:
  p1 <- ggdraw() + 
    draw_image(paste0("plots/scenarios/heat_waves/", window_length, "d/plot_fitness_1500_", i, "_to_", i + window_length, ".png")) +
    draw_label("Very low food",
               x = 0.6, y = 0.9,
               hjust = 0.5, vjust = 1,
               size = 5)
  p2 <- ggdraw() + 
    draw_image(paste0("plots/scenarios/heat_waves/", window_length, "d/plot_fitness_2000_", i, "_to_", i + window_length, ".png")) +
    draw_label("Low food",
               x = 0.6, y = 0.9,
               hjust = 0.5, vjust = 1,
               size = 5)
  p3 <- ggdraw() + 
    draw_image(paste0("plots/scenarios/heat_waves/", window_length, "d/plot_fitness_2250_", i, "_to_", i + window_length, ".png")) +
    draw_label("Moderate food",
               x = 0.6, y = 0.9,
               hjust = 0.5, vjust = 1,
               size = 5)
  p4 <- ggdraw() + 
    draw_image(paste0("plots/scenarios/heat_waves/", window_length, "d/plot_fitness_2500_", i, "_to_", i + window_length, ".png")) +
    draw_label("High food",
               x = 0.6, y = 0.9,
               hjust = 0.5, vjust = 1,
               size = 5)
  p5 <- ggdraw() + 
    draw_image(paste0("plots/scenarios/heat_waves/", window_length, "d/plot_fitness_3000_", i, "_to_", i + window_length, ".png")) +
    draw_label("Very high food",
               x = 0.6, y = 0.9,
               hjust = 0.5, vjust = 1,
               size = 5)
  p6 <- ggdraw() + 
    draw_image(paste0("plots/scenarios/heat_waves/", window_length, "d/plot_temperature_", i, "_to_", i + window_length, ".png"))
  
  
  # Make columns:
  empty_plot <- ggdraw() +
    draw_label(paste0(window_length, "-day window"),
               x = 0.6, y = 0.5,
               size = 5,
               fontface = "bold")
  
  left_col <- (p1 / p3 / p5 / p6) +
    plot_layout(heights = c(1, 1, 1, 0.5))
  
  right_col <- (p2 / p4 / p6 / empty_plot) +
    plot_layout(heights = c(1, 1, 0.5, 1))
  
  final_plot <- left_col | right_col
  
  
  ggsave(paste0("plots/scenarios/heat_waves/", window_length, "d/animation/gif_slide_", i, "_to_", i + window_length, ".png"), final_plot, height = 70 , width = 40, units = "mm", dpi = 800)
  
}



# Make gif:
png_files <- list.files(
  paste0("plots/scenarios/heat_waves/", window_length, "d/animation/"),
  pattern = "\\.png$",
  full.names = TRUE
)

gifski(
  png_files,
  gif_file = paste0("plots/scenarios/heat_waves/", window_length, "d/animation.gif"),
  delay = .3,
  width = 900,
  height = 1600
)

#                                          END OF SCRIPT
#############################################################################################################