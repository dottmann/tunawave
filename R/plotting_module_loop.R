






# Load temporal outputs:
load("out/loop/fitness_index_permut_2000_1982_2025.RData")
permutation_list_1982_2025 <- permutation_list
# Add New column to tables:
permutation_list_1982_2025 <- lapply(
  permutation_list_1982_2025,
  function(df) {
    df$projection <- "Historic real"
    df
  })

load("out/loop/fitness_index_permut_2000_1982_2025_projections.RData")
permutation_list_1982_2025_projections <- permutation_list
# Add New column to tables:
permutation_list_1982_2025_projections <- lapply(
  permutation_list_1982_2025_projections,
  function(df) {
    df$projection <- "Historic projections"
    df
  })

load("out/loop/fitness_index_permut_2000_2026_2100_SSP126.RData")
permutation_list_2026_2100_ssp126 <- permutation_list
# Add New column to tables:
permutation_list_2026_2100_ssp126 <- lapply(
  permutation_list_2026_2100_ssp126,
  function(df) {
    df$projection <- "SSP126"
    df
  })

load("out/loop/fitness_index_permut_2000_2026_2100_SSP245.RData")
permutation_list_2026_2100_ssp245 <- permutation_list
# Add New column to tables:
permutation_list_2026_2100_ssp245 <- lapply(
  permutation_list_2026_2100_ssp245,
  function(df) {
    df$projection <- "SSP245"
    df
  })

load("out/loop/fitness_index_permut_2000_2026_2100_SSP370.RData")
permutation_list_2026_2100_ssp370 <- permutation_list
# Add New column to tables:
permutation_list_2026_2100_ssp370 <- lapply(
  permutation_list_2026_2100_ssp370,
  function(df) {
    df$projection <- "SSP370"
    df
  })

load("out/loop/fitness_index_permut_2000_2026_2100_SSP585.RData")
permutation_list_2026_2100_ssp585 <- permutation_list
# Add New column to tables:
permutation_list_2026_2100_ssp585 <- lapply(
  permutation_list_2026_2100_ssp585,
  function(df) {
    df$projection <- "SSP585"
    df
  })

load("out/loop/fitness_index_permut_2000_2026_2100_ssp126_heatwave_191_199.RData")
permutation_list_2026_2100_ssp126_MHW <- permutation_list
# Add New column to tables:
permutation_list_2026_2100_ssp126_MHW <- lapply(
  permutation_list_2026_2100_ssp126_MHW,
  function(df) {
    df$projection <- "SSP126_MHW"
    df
  })

load("out/loop/fitness_index_permut_2000_2026_2100_SSP245_heatwave_191_199.RData")
permutation_list_2026_2100_ssp245_MHW <- permutation_list
# Add New column to tables:
permutation_list_2026_2100_ssp245_MHW <- lapply(
  permutation_list_2026_2100_ssp245_MHW,
  function(df) {
    df$projection <- "SSP245_MHW"
    df
  })

load("out/loop/fitness_index_permut_2000_2026_2100_SSP370_heatwave_191_199.RData")
permutation_list_2026_2100_ssp370_MHW <- permutation_list
# Add New column to tables:
permutation_list_2026_2100_ssp370_MHW <- lapply(
  permutation_list_2026_2100_ssp370_MHW,
  function(df) {
    df$projection <- "SSP370_MHW"
    df
  })

load("out/loop/fitness_index_permut_2000_2026_2100_SSP585_heatwave_191_199.RData")
permutation_list_2026_2100_ssp585_MHW <- permutation_list
# Add New column to tables:
permutation_list_2026_2100_ssp585_MHW <- lapply(
  permutation_list_2026_2100_ssp585_MHW,
  function(df) {
    df$projection <- "SSP585_MHW"
    df
  })



permutation_list <- c(permutation_list_1982_2025,
                      permutation_list_1982_2025_projections,
                      permutation_list_2026_2100_ssp126,
                      permutation_list_2026_2100_ssp245,
                      permutation_list_2026_2100_ssp370,
                      permutation_list_2026_2100_ssp585,
                      permutation_list_2026_2100_ssp126_MHW,
                      permutation_list_2026_2100_ssp245_MHW,
                      permutation_list_2026_2100_ssp370_MHW,
                      permutation_list_2026_2100_ssp585_MHW)

combined_df <- do.call(
  rbind,
  c(permutation_list_1982_2025,
    permutation_list_1982_2025_projections,
    permutation_list_2026_2100_ssp126,
    permutation_list_2026_2100_ssp245,
    permutation_list_2026_2100_ssp370,
    permutation_list_2026_2100_ssp585,
    permutation_list_2026_2100_ssp126_MHW,
    permutation_list_2026_2100_ssp245_MHW,
    permutation_list_2026_2100_ssp370_MHW,
    permutation_list_2026_2100_ssp585_MHW)
)



combined_df_averaged <- combined_df %>%
  # mutate(fitness_index = log10(fitness_index)) %>%
  group_by(year, projection) %>%
  summarise(fitness_mean = mean(fitness_index),
            fitness_sd = sd(fitness_index),
            upper_sd = fitness_mean + fitness_sd,
            lower_sd = fitness_mean - fitness_sd,
            fitness_se = sd(fitness_index) / sqrt(n()),
            upper_se = fitness_mean + fitness_se,
            lower_se = fitness_mean - fitness_se)

# Define your IDs
ids <- c("1500", "2000", "2250", "2500", "3000")

# Combine your lists into one large list
all_lists <- list(
  p_1982_2025             = permutation_list_1982_2025,
  p_1982_2025_projections = permutation_list_1982_2025_projections,
  p_2026_2100_ssp126       = permutation_list_2026_2100_ssp126,
  p_2026_2100_ssp245       = permutation_list_2026_2100_ssp245,
  p_2026_2100_ssp370       = permutation_list_2026_2100_ssp370,
  p_2026_2100_ssp585       = permutation_list_2026_2100_ssp585,
  p_2026_2100_ssp126_MHW   = permutation_list_2026_2100_ssp126_MHW,
  p_2026_2100_ssp245_MHW   = permutation_list_2026_2100_ssp245_MHW,
  p_2026_2100_ssp370_MHW   = permutation_list_2026_2100_ssp370_MHW,
  p_2026_2100_ssp585_MHW   = permutation_list_2026_2100_ssp585_MHW
)

# Apply IDs to the sub-lists and merge
combined_df <- all_lists %>%
  map_dfr(function(sublist) {
    names(sublist) <- ids # Assign the names ps0.75, etc.
    bind_rows(sublist, .id = "prey_scalar") # Collapse sublist into a DF
  }, .id = "list_source")





# combined_df <- map2_dfr(permutation_list, seq_along(permutation_list), ~
#                           mutate(.x, permut_id = .y))
# 
# combined_df <- combined_df %>%
#   arrange(year) %>%
#   mutate(permut_id_2 = rep(1:27, length(unique(combined_df$year))))

# Change the names of the projections:
combined_df <- combined_df %>% 
  mutate(projection = case_when(projection == "Historic projections" ~ "Historic regression",
                                projection == "Historic real" ~ "Historic",
                                T ~ projection)) %>%
  group_by(prey_scalar) %>%
  mutate(relative_fitness_index = (fitness_index - min(fitness_index, na.rm = TRUE)) / 
           (max(fitness_index, na.rm = TRUE) - min(fitness_index, na.rm = TRUE))
  ) %>%
  ungroup()



# Filter projections without marine heatwaves:
combined_df_no_mhw <- combined_df %>%
  filter(!projection %in% c("SSP126_MHW", "SSP245_MHW", "SSP370_MHW", "SSP585_MHW"))

(p <- ggplot() +
    
    # geom_ribbon(data = combined_df,
    #             aes(x = year, y = fitness_mean, ymin = lower_se, ymax = upper_se, fill = projection, alpha = projection)) + # fill = "lightblue"
    geom_line(data = combined_df_no_mhw,
              aes(x = year, y = fitness_index,
                  group = interaction(prey_scalar, projection), 
                  linetype = projection, 
                  color = projection
              )) +
    geom_line(data = combined_df_no_mhw,
              aes(x = year, y = fitness_index,
                  group = interaction(prey_scalar, projection), 
                  color = projection
              ), alpha = .3) +
    scale_color_manual(values = c("lightblue", "black", "olivedrab", "#D4A437","red", "red4"), name = "Projection") +
    # scale_fill_manual(values = c("gray", "lightblue", "gold", "red")) +
    scale_linetype_manual(values = c("solid", "dotted", "solid", "solid", "solid", "solid"), name = "Projection") +
    # scale_y_log10() +
    ylab("Integrated survival") +
    xlab("Year") +
    theme_classic() +
    facet_wrap(~prey_scalar,
               scales = "free_y",
               labeller = as_labeller(c("1500" = "Very low food",
                                        "2000" = "Low food",
                                        "2250" = "Moderate food",
                                        "2500" = "High food",
                                        "3000" = "Very high food")),
               ncol= 1) +
    # scale_x_discrete(
    #   breaks = unique(combined_df$year)[seq(1, length(unique(combined_df$year)), by = 2)]) +
    theme(panel.background = element_blank(),
          strip.background = element_blank()))

# ggsave("plots/fitness_index_permut_naup_2000_1982_2100_sm.png", p, height = 40 , width = 80, units = "mm", scale = 2) 
# ggsave("plots/fitness_index_permut_naup_2000_1982_2100_pannels_sm.png", p, height = 100 , width = 60, units = "mm", scale = 2) 


# Filter projections with marine heatwaves:
combined_df_mhw <- combined_df %>%
  filter(!projection %in% c("SSP126", "SSP245", "SSP370", "SSP585"))

(p <- ggplot() +
    
    # geom_ribbon(data = combined_df,
    #             aes(x = year, y = fitness_mean, ymin = lower_se, ymax = upper_se, fill = projection, alpha = projection)) + # fill = "lightblue"
    geom_line(data = combined_df_mhw,
              aes(x = year, y = fitness_index,
                  group = interaction(prey_scalar, projection), 
                  linetype = projection, 
                  color = projection
              )) +
    geom_line(data = combined_df_mhw,
              aes(x = year, y = fitness_index,
                  group = interaction(prey_scalar, projection), 
                  color = projection
              ), alpha = .3) +
    scale_color_manual(values = c("lightblue", "black", "olivedrab", "#D4A437","red", "red4"), name = "Projection") +
    # scale_fill_manual(values = c("gray", "lightblue", "gold", "red")) +
    scale_linetype_manual(values = c("solid", "dotted", "dotted", "dotted", "dotted", "dotted"), name = "Projection") +
    # scale_y_log10() +
    ylab("Integrated survival") +
    xlab("Year") +
    theme_classic() +
    facet_wrap(~prey_scalar, 
               scales = "free_y",
               labeller = as_labeller(c("1500" = "Very low food",
                                        "2000" = "Low food",
                                        "2250" = "Moderate food",
                                        "2500" = "High food",
                                        "3000" = "Very high food")),
               ncol= 1) +
    # scale_x_discrete(
    #   breaks = unique(combined_df$year)[seq(1, length(unique(combined_df$year)), by = 2)]) +
    theme(panel.background = element_blank(),
          strip.background = element_blank()))

# ggsave("plots/fitness_index_permut_naup_2000_1982_2100.png", p, height = 40 , width = 80, units = "mm", scale = 2) 
# ggsave("plots/fitness_index_permut_naup_2000_1982_2100_pannels_sm_MHW.png", p, height = 100 , width = 60, units = "mm", scale = 2) 


# Filter projections with and without marine heatwaves:
combined_df_solid <- combined_df %>%
  filter(prey_scalar != "1500", projection %in% c("Historic", "SSP126", "SSP245", "SSP370", "SSP585")) 

combined_df_dotted <- combined_df %>%
  filter(prey_scalar != "1500", projection %in% c("Historic regression", "SSP126_MHW", "SSP245_MHW", "SSP370_MHW", "SSP585_MHW")) 

(p <- ggplot() +
    
    geom_line(data = combined_df_solid,
              aes(x = year, y = relative_fitness_index,
                  group = interaction(prey_scalar, projection), 
                  color = projection
              )) +
    geom_line(data = combined_df_dotted,
              aes(x = year, y = relative_fitness_index,
                  group = interaction(prey_scalar, projection), 
                  color = projection, 
                  alpha = projection
              ), alpha = 0.3) +
    geom_line(data = combined_df_dotted,
              aes(x = year, y = relative_fitness_index,
                  group = interaction(prey_scalar, projection), 
                  color = projection
              ), linetype = "dotted") +
    scale_color_manual(values = c("lightblue", "black", "olivedrab", "olivedrab", "#D4A437", "#D4A437","red", "red", "red4", "red4"), name = "Projection") +
    # scale_alpha_manual(values = c(1, .1, 1, .3, 1, .3, 1, .3, 1, .3)) +
    # scale_linetype_manual(values = c("solid", "dotted", "dotted", "dotted", "dotted", "dotted", "dotted", "dotted", "dotted", "dotted"), name = "Projection") +
    # scale_y_log10() +
    ylab("Relative integrated survival") +
    xlab("Year") +
    theme_classic() +
    facet_wrap(~prey_scalar, 
               scales = "free_y",
               labeller = as_labeller(c("1500" = "Very low food",
                                        "2000" = "Low food",
                                        "2250" = "Moderate food",
                                        "2500" = "High food",
                                        "3000" = "Very high food")),
               ncol= 1) +
    # scale_x_discrete(
    #   breaks = unique(combined_df$year)[seq(1, length(unique(combined_df$year)), by = 2)]) +
    theme(panel.background = element_blank(),
          strip.background = element_blank()))

# ggsave("plots/fitness_index_permut_naup_2000_1982_2100.png", p, height = 40 , width = 80, units = "mm", scale = 2) 
# ggsave("plots/fitness_index_permut_naup_2000_1982_2100_pannels_sm_all.png", p, height = 100 , width = 60, units = "mm", scale = 2) 



# Remove some projections:
combined_df_simplified <- combined_df %>%
  filter(prey_scalar != "1500", projection %in% c("Historic", "SSP245", "SSP370", "SSP245_MHW", "SSP370_MHW")) 



(p <- ggplot() +
    
    # geom_ribbon(data = combined_df,
    #             aes(x = year, y = fitness_mean, ymin = lower_se, ymax = upper_se, fill = projection, alpha = projection)) + # fill = "lightblue"
    geom_line(data = combined_df_simplified,
              aes(x = year, y = relative_fitness_index,
                  group = interaction(prey_scalar, projection), 
                  linetype = projection, 
                  color = projection
              )) +
    scale_color_manual(values = c("lightblue", "#D4A437", "#D4A437", "red", "red"), name = "Projection") +
    scale_linetype_manual(values = c("solid", "solid", "dotted", "solid", "dotted"), name = "Projection") +
    
    geom_line(data = subset(combined_df_simplified, projection %in% c("SSP245_MHW", "SSP370_MHW")),
              aes(x = year, y = relative_fitness_index,
                  group = interaction(prey_scalar, projection), 
                  color = projection
              ), alpha = .3) +
    
    # scale_y_log10() +
    ylab("Relative integrated survival") +
    xlab("Year") +
    theme_classic() +
    facet_wrap(~prey_scalar, 
               scales = "free_y",
               labeller = as_labeller(c("1500" = "Very low food",
                                        "2000" = "Low food",
                                        "2250" = "Moderate food",
                                        "2500" = "High food",
                                        "3000" = "Very high food")),
               ncol= 1) +
    # scale_x_discrete(
    #   breaks = unique(combined_df$year)[seq(1, length(unique(combined_df$year)), by = 2)]) +
    theme(panel.background = element_blank(),
          strip.background = element_blank()))

# ggsave("plots/fitness_index_permut_naup_2000_1982_2100.png", p, height = 40 , width = 80, units = "mm", scale = 2) 
# ggsave("plots/fitness_index_permut_naup_2000_1982_2100_pannels.png", p, height = 80 , width = 60, units = "mm", scale = 2) 











(p <- ggplot() +
  # geom_boxplot(data = combined_df_averaged,
  #            aes(x = as.factor(year), y = fitness_index)) +
  geom_ribbon(data = combined_df_averaged,
              aes(x = year, y = fitness_mean, ymin = lower_se, ymax = upper_se, fill = projection, alpha = projection)) + # fill = "lightblue"
  geom_line(data = combined_df_averaged,
            aes(x = year, y = fitness_mean, group = projection, linetype = projection, color = projection)) +
    scale_color_manual(values = c("black", "lightblue4","gold", "red")) +
    scale_fill_manual(values = c("gray", "lightblue", "gold", "red")) +
    scale_linetype_manual(values = c("dotted", "solid", "dashed", "dashed")) +
    scale_alpha_manual(values = c(0.3, 0.7, 0.3, 0.3)) +
  # scale_y_log10() +
  ylab("Fitness index") +
  xlab("Year") +
  theme_classic() +
  # scale_x_discrete(
  #   breaks = unique(combined_df_averaged$year)[seq(1, length(unique(combined_df_averaged$year)), by = 2)]) +
  theme(panel.background = element_blank()))

# ggsave("plots/fitness_index_permut_naup_2000_mean_se_1982_2100.png", p, height = 40 , width = 80, units = "mm", scale = 2) 
