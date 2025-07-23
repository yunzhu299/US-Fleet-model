## Apply Survival Curve to EV Volumes ------
## YZC July 2025

# Load required libraries and inputs ------
source("Scripts/00-Libraries.R", encoding = "UTF-8")
sales_data <- read_csv("Parameters/TotalSalesSurvival.csv")

# Parameters ------
max_age <- 30
mean_ev_life <- 17
sd_ev_life <- 4
mean_batt_life <- 15
sd_batt_life <- 4
distribution_type <- "Normal"

# Define survival probability function ------
survival_probability <- function(age, mean, sd, dist = "Normal") {
  if (dist == "Normal") {
    denom <- 1 - pnorm(age, mean = mean, sd = sd)
    if (denom == 0) return(0)
    return((1 - pnorm(age + 1, mean = mean, sd = sd)) / denom)
  } else if (dist == "Logistic") {
    scale <- sd * sqrt(3) / pi
    denom <- 1 - plogis(age, location = mean, scale = scale)
    if (denom == 0) return(0)
    return((1 - plogis(age + 1, location = mean, scale = scale)) / denom)
  } else {
    stop("Invalid distribution type.")
  }
}

# Initialize results ------
results_list <- list()

for (year in unique(sales_data$`Sale Year`)) {
  total_initial_sales <- sales_data %>%
    filter(`Sale Year` == year) %>%
    pull(`Total Sales`)
  
  if (is.na(total_initial_sales) || total_initial_sales < 1) next
  
  stock <- matrix(0, nrow = max_age + 1, ncol = max_age + 1)
  stock[1, 1] <- total_initial_sales
  
  all_active_ev <- c()
  battery_replaced <- c()
  ev_retired <- c()
  all_fail <- c()
  
  recycle_from_both_fail <- c()
  recycle_from_battery_only <- c()
  reuse_from_ev_fail <- c()
  repurpose_from_ev_fail <- c()
  recycle_from_ev_fail <- c()
  
  for (sim_year in year:2050) {
    updated_stock <- matrix(0, nrow = max_age + 1, ncol = max_age + 1)
    
    ev_alive <- 0
    need_new_battery <- 0
    ev_exit <- 0
    both_exit <- 0
    
    recycle_both <- 0
    both_fail_recycle_battery <- 0
    reuse_battery <- 0
    repurpose_battery <- 0
    ev_fail_recycle_battery <- 0
    
    for (ev_age in 0:(max_age - 1)) {
      for (lib_age in 0:(max_age - 1)) {
        current_stock <- stock[ev_age + 1, lib_age + 1]
        if (current_stock < 1) next
        
        ev_survive <- survival_probability(ev_age, mean_ev_life, sd_ev_life, dist = distribution_type)
        batt_survive <- survival_probability(lib_age, mean_batt_life, sd_batt_life, dist = distribution_type)
        
        both_live <- ev_survive * batt_survive * current_stock
        batt_fail <- ev_survive * (1 - batt_survive) * current_stock
        ev_fail <- (1 - ev_survive) * batt_survive * current_stock
        both_fail <- (1 - ev_survive) * (1 - batt_survive) * current_stock
        
        ev_alive <- ev_alive + both_live
        need_new_battery <- need_new_battery + batt_fail
        ev_exit <- ev_exit + ev_fail
        both_exit <- both_exit + both_fail
        
        recycle_both <- recycle_both + both_fail
        both_fail_recycle_battery <- both_fail_recycle_battery + batt_fail
        reuse_battery <- reuse_battery + 0.5 * ev_fail
        repurpose_battery <- repurpose_battery + 0.25 * ev_fail
        ev_fail_recycle_battery <- ev_fail_recycle_battery + 0.25 * ev_fail
        
        if (ev_age + 1 <= max_age && lib_age + 1 <= max_age) {
          updated_stock[ev_age + 2, lib_age + 2] <- updated_stock[ev_age + 2, lib_age + 2] + both_live
        }
        if (ev_age + 1 <= max_age) {
          updated_stock[ev_age + 2, 1] <- updated_stock[ev_age + 2, 1] + batt_fail
        }
        if (lib_age + 1 <= max_age) {
          updated_stock[1, lib_age + 2] <- updated_stock[1, lib_age + 2] + 0.5 * ev_fail
  
        }
      }
    }
    
    stock <- updated_stock
    
    all_active_ev <- c(all_active_ev, ev_alive)
    battery_replaced <- c(battery_replaced, need_new_battery)
    ev_retired <- c(ev_retired, ev_exit)
    all_fail <- c(all_fail, both_exit)
    
    recycle_from_both_fail <- c(recycle_from_both_fail, recycle_both)
    recycle_from_battery_only <- c(recycle_from_battery_only, both_fail_recycle_battery)
    reuse_from_ev_fail <- c(reuse_from_ev_fail, reuse_battery)
    repurpose_from_ev_fail <- c(repurpose_from_ev_fail, repurpose_battery)
    recycle_from_ev_fail <- c(recycle_from_ev_fail, ev_fail_recycle_battery)
  }
  
  results_list[[as.character(year)]] <- tibble(
    sale_year = year,
    year = year:2050,
    all_active_ev = all_active_ev,
    battery_replaced = battery_replaced,
    ev_retired = ev_retired,
    all_fail = all_fail,
    recycle_from_both_fail = recycle_from_both_fail,
    recycle_from_battery_only = recycle_from_battery_only,
    reuse_from_ev_fail = reuse_from_ev_fail,
    repurpose_from_ev_fail = repurpose_from_ev_fail,
    recycle_from_ev_fail = recycle_from_ev_fail
  )
}

# Combine and write output to csv ------
results_hist_df <- bind_rows(results_list)
write_csv(results_hist_df, "Parameters/EVSurvivalResults.csv")


# save data ------
exploded_hist_sale <- results_hist_df %>%
  select(year, recycle_from_ev_fail, recycle_from_battery_only, recycle_from_both_fail) %>%
  unnest(cols = c(year, recycle_from_ev_fail, recycle_from_battery_only, recycle_from_both_fail))

# Figures
hist_sale_plot_df <- exploded_hist_sale %>%
  group_by(year) %>%
  summarise(
    `EV Fail` = sum(recycle_from_ev_fail, na.rm = TRUE),
    `Battery Fails` = sum(recycle_from_battery_only, na.rm = TRUE),
    `Both Failing` = sum(recycle_from_both_fail, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = year)


hist_sale_plot_long <- hist_sale_plot_df %>%
  rename(
    `EV Fails` = `EV Fail`,
    `Both Fails` = `Both Failing`
  ) %>%
  pivot_longer(
    cols = c("Battery Fails", "EV Fails", "Both Fails"),
    names_to = "Source",
    values_to = "Recycled"
  ) %>%
  mutate(Source = factor(Source, levels = c("Battery Fails", "EV Fails", "Both Fails")))

nature_colors <- c(
  "Battery Fails" = "#4DBBD5",  # soft blue
  "EV Fails" = "#00A087",       # soft green
  "Both Fails" = "#E64B35"      # soft red
)

ggplot(hist_sale_plot_long, aes(x = Year, y = Recycled, fill = Source)) +
  geom_area(color = "black", size = 0.2, alpha = 0.9) +
  scale_fill_manual(
    values = nature_colors,
    name = "Battery Failure Source"
  ) +
  scale_x_continuous(
    breaks = seq(2015, 2050, 5),
    minor_breaks = seq(2015, 2050, 1),
    expand = expansion(mult = c(0, 0.03))  
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.05)) 
  ) +
  labs(
    title = "Battery Recycling Timeline of Existing EVs",
    x = "Year",
    y = "Recycled Batteries"
  ) +
  theme_classic(base_size = 13) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.ticks.length = unit(5, "pt"),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.line = element_blank(),
    legend.position = c(0.05, 0.90),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

ggsave(
  filename = "Figures/Battery_Recycling_Timeline.png",
  plot = last_plot(),  # Uses the most recently drawn plot
  width = 18,
  height = 12,
  units = "cm",
  dpi = 600
)
