#!/usr/bin/env Rscript
# Figure 3: vehicle survival curves used in the fleet model.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
root <- normalizePath(file.path(script_dir, "..", ".."))
out_dir <- file.path(root, "Results", "Figures")
data_dir <- file.path(root, "Results", "Data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

survival_logistic <- function(age, mu, b) 1 / (1 + exp((age - mu) / b))

curves <- crossing(
  Age = 0:50,
  Type = c("Car", "Truck")
) %>%
  mutate(
    mu = if_else(Type == "Car", 16, 19),
    b = if_else(Type == "Car", 4, 4.5),
    Survival = survival_logistic(Age, mu, b)
  )

readr::write_csv(curves, file.path(data_dir, "Fig03_Vehicle_Survival_Curves.csv"))

figure <- ggplot(curves, aes(Age, Survival, color = Type)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("Car" = "#0072B2", "Truck" = "#D55E00")) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1), limits = c(0, 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(breaks = seq(0, 50, 5), expand = c(0.01, 0.01)) +
  labs(
    title = "Vehicle Survival Curves (Logistic Model)",
    subtitle = "Cumulative Survival Probability by Vehicle Age",
    x = "Vehicle Age (years)", y = "Survival Probability (%)", color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, color = "gray30"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    legend.position = c(0.80, 0.82),
    legend.background = element_blank(),
    legend.key.size = unit(1.2, "lines"),
    legend.text = element_text(size = 13, face = "bold"),
    plot.margin = margin(t = 10, r = 16, b = 10, l = 14)
  )

output <- file.path(out_dir, "Fig03_Vehicle_Survival_Curves.png")
ggsave(output, figure, width = 6, height = 4, dpi = 450, bg = "white")
cat(output, "\n")
