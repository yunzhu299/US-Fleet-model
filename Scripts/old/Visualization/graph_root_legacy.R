
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

scenario_tag <- "Repeal"   # 或 "Repeal"
f <- paste0("Outputs/ClosedLoop_AddRetire_byStateSegment_", scenario_tag, ".csv")
df <- read_csv(f)

# 1) 按年×动力类型汇总：新增(add_)和退役(ret_)总量
yr_pt <- df %>%
  group_by(Year) %>%
  summarise(
    add_ICE = sum(add_ICE, na.rm = TRUE),
    add_BEV = sum(add_BEV, na.rm = TRUE),
    add_PHEV = sum(add_PHEV, na.rm = TRUE),
    retire_ICE = sum(ret_ICE, na.rm = TRUE),
    retire_BEV = sum(ret_BEV, na.rm = TRUE),
    retire_PHEV = sum(ret_PHEV, na.rm = TRUE),
    .groups = "drop"
  )

# 2) 变成长表，便于画图或导出
yr_pt_long <- yr_pt %>%
  pivot_longer(-Year, names_to = "metric", values_to = "Vehicles") %>%
  separate(metric, into = c("flow","Powertrain"), sep = "_") %>%
  mutate(flow = recode(flow, add = "Additions", retire = "Retirements"),
         Powertrain = factor(Powertrain, levels = c("ICE","BEV","PHEV")))

# 3) （可选）计算净增量 = 新增 - 退役
net_change <- yr_pt %>%
  transmute(
    Year,
    ICE  = add_ICE  - retire_ICE,
    BEV  = add_BEV  - retire_BEV,
    PHEV = add_PHEV - retire_PHEV
  ) %>%
  pivot_longer(-Year, names_to = "Powertrain", values_to = "NetChange")

# 4) （可选）合并 EV（BEV+PHEV）的总新增/退役/净增
ev_totals <- yr_pt %>%
  transmute(
    Year,
    add_EV    = add_BEV + add_PHEV,
    retire_EV = retire_BEV + retire_PHEV,
    net_EV    = add_EV - retire_EV
  )

# 5) 快速对比图（按动力类型画新增与退役）
ggplot(yr_pt_long, aes(x = Year, y = Vehicles, color = Powertrain, linetype = flow)) +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = 2020:2050) +                               # 每年一个刻度
  scale_y_continuous(labels = scales::comma_format()) +                  # 千位分隔符
  scale_linetype_manual(values = c("Additions" = "solid", "Retirements" = "dashed")) +
  labs(
    title = paste0("Annual Additions vs Retirements by Powertrain (", scenario_tag, ")"),
    y = "Vehicles",
    x = "Year",
    linetype = "Flow",
    color = "Powertrain"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )
library(dplyr)

EV_historical %>%
  group_by(`Sale Year`, Propulsion) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Propulsion, values_from = Total_Sales) %>%
  mutate(Total_EV = BEV + PHEV) %>%
  arrange(`Sale Year`)

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)

# --- 读取并合并两个情景 ---
get_nat_long <- function(scenario = c("ACCII","Repeal")) {
  scenario <- match.arg(scenario)
  f <- paste0("Outputs/ClosedLoop_StateTotals_", scenario, ".csv")
  df <- read_csv(f, show_col_types = FALSE)
  df %>%
    group_by(Year) %>%
    summarise(
      add_ICE  = sum(add_ICE,  na.rm = TRUE),
      add_BEV  = sum(add_BEV,  na.rm = TRUE),
      add_PHEV = sum(add_PHEV, na.rm = TRUE),
      ret_ICE  = sum(ret_ICE,  na.rm = TRUE),
      ret_BEV  = sum(ret_BEV,  na.rm = TRUE),
      ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-Year, names_to = "metric", values_to = "Vehicles") %>%
    separate(metric, into = c("flow","Powertrain"), sep = "_") %>%
    mutate(
      Flow = recode(flow, add = "New Sales", ret = "Retirements"),
      Scenario = scenario
    ) %>%
    select(Scenario, Year, Powertrain, Flow, Vehicles)
}

nat_all <- bind_rows(
  get_nat_long("ACCII"),
  get_nat_long("Repeal")
)

# 调色
cols <- c("ICE"="#E16A5B", "BEV"="#4DB06D", "PHEV"="#3F87C5")

p_newsales <- nat_all %>%
  filter(Flow == "New Sales") %>%
  mutate(Vehicles_k = Vehicles / 1000,
         Powertrain = factor(Powertrain, levels = c("ICE", "BEV", "PHEV"))) %>%  # 👈 调整顺序
  ggplot(aes(x = Year, y = Vehicles_k,
             color = Powertrain,
             linetype = Scenario,
             shape = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("ACCII" = "solid", "Repeal" = "dotted")) +
  scale_shape_manual(values = c("ACCII" = 17, "Repeal" = 16)) +
  scale_x_continuous(breaks = 2020:2050) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 30000, 1000),
    name = "Vehicles (thousands)"
  ) +
  labs(
    title = "Annual New Sales by Powertrain (ACCII vs Repeal)",
    x = "Year",
    color = "Powertrain",
    linetype = "Scenario",
    shape = "Scenario"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = c(0.15, 0.55),
    legend.box = "vertical",
    legend.justification = "center",
    legend.background = element_rect(fill = alpha("white", 0.5), color = "gray70"),
    legend.box.background = element_rect(color = "white", size = 0.3),
    legend.key.width  = unit(1.4, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.spacing.y  = unit(0, "pt"),
    legend.spacing.x  = unit(2, "pt"),
    legend.box.spacing = unit(0, "pt"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  ) +
  guides(
    color = guide_legend(order = 1, ncol = 1, reverse = FALSE),   # 👈 按设定的levels显示顺序
    linetype = guide_legend(order = 2, ncol = 1),
    shape = guide_legend(order = 2, ncol = 1)
  )

p_newsales

# --------------------
# 折线图 2: Retirements
# --------------------
# 配色保持一致
cols <- c("ICE"="#E16A5B", "BEV"="#4DB06D", "PHEV"="#3F87C5")

p_retire <- nat_all %>%
  filter(Flow == "Retirements") %>%
  mutate(Vehicles_k = Vehicles / 1000,
         Powertrain = factor(Powertrain, levels = c("ICE", "BEV", "PHEV"))) %>%
  ggplot(aes(x = Year, y = Vehicles_k,
             color = Powertrain,
             linetype = Scenario,
             shape = Scenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("ACCII" = "solid", "Repeal" = "dotted")) +
  scale_shape_manual(values = c("ACCII" = 17, "Repeal" = 16)) +
  scale_x_continuous(breaks = 2020:2050) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 30000, 1000),
    name = "Vehicles (thousands)"
  ) +
  labs(
    title = "Annual Retirements by Powertrain (ACCII vs Repeal)",
    x = "Year",
    color = "Powertrain",
    linetype = "Scenario",
    shape = "Scenario"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = c(0.15, 0.55),
    legend.box = "vertical",
    legend.justification = "center",
    legend.background = element_rect(fill = alpha("white", 0.5), color = "gray70"),
    legend.box.background = element_rect(color = "white", size = 0.3),
    legend.key.width  = unit(1.4, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.spacing.y  = unit(0, "pt"),
    legend.spacing.x  = unit(2, "pt"),
    legend.box.spacing = unit(0, "pt"),
    legend.title = element_text(size = 11),
    legend.text  = element_text(size = 10)
  ) +
  guides(
    color = guide_legend(order = 1, ncol = 1, reverse = FALSE),
    linetype = guide_legend(order = 2, ncol = 1),
    shape = guide_legend(order = 2, ncol = 1)
  )

p_retire
p_retire <- nat_all %>%
  filter(Flow == "Retirements") %>%
  mutate(Vehicles_k = Vehicles / 1000) %>%
  ggplot(aes(x = Year, y = Vehicles_k, color = Powertrain)) +
  scale_x_continuous(breaks = 2020:2050) +
  geom_line(aes(linetype = Scenario), linewidth = 1.2) +
  geom_point(aes(shape = Scenario), size = 2) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("ACCII" = "solid", "Repeal" = "dotted")) +
  scale_shape_manual(values = c("ACCII" = 16, "Repeal" = 17)) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 30000, 1000),
    name = "Vehicles (thousands)"
  ) +
  labs(
    title = "Annual Retirements by Powertrain (ACCII vs Repeal)",
    x = "Year",
    color = "Powertrain",
    linetype = "Scenario",
    shape = "Scenario"
  ) +
  annotate(
    "text",
    x = 2021, y = 15000,                     # 左侧中间位置
    label = "Retirements",
    color = "black",
    size = 5,
    fontface = "bold",
    hjust = 0
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

p_retire




library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)

# --- 读取并合并两个情景 ---
get_nat_long <- function(scenario = c("ACCII","Repeal")) {
  scenario <- match.arg(scenario)
  f <- paste0("Outputs/ClosedLoop_StateTotals_", scenario, ".csv")
  df <- read_csv(f, show_col_types = FALSE)
  df %>%
    group_by(Year) %>%
    summarise(
      add_ICE  = sum(add_ICE,  na.rm = TRUE),
      add_BEV  = sum(add_BEV,  na.rm = TRUE),
      add_PHEV = sum(add_PHEV, na.rm = TRUE),
      ret_ICE  = sum(ret_ICE,  na.rm = TRUE),
      ret_BEV  = sum(ret_BEV,  na.rm = TRUE),
      ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-Year, names_to = "metric", values_to = "Vehicles") %>%
    separate(metric, into = c("flow","Powertrain"), sep = "_") %>%
    mutate(
      Flow = recode(flow, add = "New Sales", ret = "Retirements"),
      Scenario = scenario
    ) %>%
    select(Scenario, Year, Powertrain, Flow, Vehicles)
}

nat_all <- bind_rows(
  get_nat_long("ACCII"),
  get_nat_long("Repeal")
)

# --- 配色 ---
cols <- c("ICE"="#E16A5B", "BEV"="#4DB06D", "PHEV"="#3F87C5")

# --- 绘图：Retirements ---
p_retire <- nat_all %>%
  filter(Flow == "Retirements") %>%
  mutate(Vehicles_k = Vehicles / 1000) %>%
  ggplot(aes(x = Year, y = Vehicles_k,
             color = Powertrain,
             linetype = Scenario,
             shape = Scenario)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.8) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("ACCII" = "solid", "Repeal" = "dotted")) +
  scale_shape_manual(values = c("ACCII" = 17, "Repeal" = 16)) +
  scale_x_continuous(breaks = seq(2020, 2050, 5), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(
    labels = comma,
    name = "Vehicles (thousands)",
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = "Annual Retirements by Powertrain",
  
    x = "Year",
    color = "Powertrain",
    linetype = "Scenario",
    shape = "Scenario"
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 13, hjust = 0),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.6, "cm"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 15)
  )

# --- 展示图像 ---
p_retire

# --- 保存高分辨率图像 ---
ggsave("Outputs/National_Retirements_ACCII_vs_Repeal.png",
       p_retire, width = 12, height = 6.5, dpi = 400)


library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)

# --- 读取并合并两个情景 ---
get_nat_long <- function(scenario = c("ACCII","Repeal")) {
  scenario <- match.arg(scenario)
  f <- paste0("Outputs/ClosedLoop_StateTotals_", scenario, ".csv")
  df <- read_csv(f, show_col_types = FALSE)
  df %>%
    group_by(Year) %>%
    summarise(
      add_ICE  = sum(add_ICE,  na.rm = TRUE),
      add_BEV  = sum(add_BEV,  na.rm = TRUE),
      add_PHEV = sum(add_PHEV, na.rm = TRUE),
      ret_ICE  = sum(ret_ICE,  na.rm = TRUE),
      ret_BEV  = sum(ret_BEV,  na.rm = TRUE),
      ret_PHEV = sum(ret_PHEV, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(-Year, names_to = "metric", values_to = "Vehicles") %>%
    separate(metric, into = c("flow","Powertrain"), sep = "_") %>%
    mutate(
      Flow = recode(flow, add = "New Sales", ret = "Retirements"),
      Scenario = scenario
    ) %>%
    select(Scenario, Year, Powertrain, Flow, Vehicles)
}

nat_all <- bind_rows(
  get_nat_long("ACCII"),
  get_nat_long("Repeal")
)

# --- 配色（与Retirements一致） ---
cols <- c("ICE"="#E16A5B", "BEV"="#4DB06D", "PHEV"="#3F87C5")

# --- 绘图：New Sales ---
p_newsales <- nat_all %>%
  filter(Flow == "New Sales") %>%
  mutate(Vehicles_k = Vehicles / 1000) %>%
  ggplot(aes(x = Year, y = Vehicles_k,
             color = Powertrain,
             linetype = Scenario,
             shape = Scenario)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 2.8) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("ACCII" = "solid", "Repeal" = "dotted")) +
  scale_shape_manual(values = c("ACCII" = 17, "Repeal" = 16)) +
  scale_x_continuous(breaks = seq(2020, 2050, 5), expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(
    labels = comma,
    name = "Vehicles (thousands)",
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = "Annual New Sales by Powertrain",
    
    x = "Year",
    color = "Powertrain",
    linetype = "Scenario",
    shape = "Scenario"
  ) +
  theme_bw(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 13, hjust = 0),
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 13),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(1.2, "cm"),
    legend.key.height = unit(0.6, "cm"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 15)
  )

# --- 展示图像 ---
p_newsales

# --- 保存高分辨率图像 ---
ggsave("Outputs/National_NewSales_ACCII_vs_Repeal.png",
       p_newsales, width = 12, height = 6.5, dpi = 400)
# ---------- 图1：New Sales 堆积图 ----------
p_stack_sales <- nat_all %>%
  filter(Flow == "New Sales") %>%
  mutate(Vehicles_k = Vehicles / 1000) %>%  # 转为千辆
  ggplot(aes(x = Year, y = Vehicles_k, fill = Powertrain)) +
  geom_area(position = "stack", alpha = 0.9) +
  facet_wrap(~ Scenario, ncol = 2) +      # ACCII 和 Repeal 并排
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = 2020:2050) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 30000, 1000),
    name = "Vehicles (thousands)"
  ) +
  labs(
    title = "Stacked New Sales by Powertrain (ACCII vs Repeal)",
    x = "Year", fill = "Powertrain"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

p_stack_sales


# ---------- 图2：Retirements 堆积图 ----------
p_stack_ret <- nat_all %>%
  filter(Flow == "Retirements") %>%
  mutate(Vehicles_k = Vehicles / 1000) %>%
  ggplot(aes(x = Year, y = Vehicles_k, fill = Powertrain)) +
  geom_area(position = "stack", alpha = 0.9) +
  facet_wrap(~ Scenario, ncol = 2) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(breaks = 2020:2050) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 30000, 1000),
    name = "Vehicles (thousands)"
  ) +
  labs(
    title = "Stacked Retirements by Powertrain (ACCII vs Repeal)",
    x = "Year", fill = "Powertrain"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

p_stack_ret


# --- Academic-style Additions vs Retirements by State (2021–2050) ---

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ==== 读取数据 ====
acc <- read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv") %>%
  mutate(Scenario = "ACCII")
rep <- read_csv("Outputs/ClosedLoop_StateTotals_Repeal.csv") %>%
  mutate(Scenario = "Repeal")

# ==== 统一配色 ====
cols <- c("ICE" = "#E16A5B", "BEV" = "#009E73", "PHEV" = "#3F87C5")

# ==== 绘图函数 ====
plot_addretire_state <- function(df, scenario_name) {
  df %>%
    filter(Year >= 2021, Year <= 2050) %>%
    select(State, Year,
           add_ICE, add_BEV, add_PHEV,
           ret_ICE, ret_BEV, ret_PHEV) %>%
    pivot_longer(
      cols = c(add_ICE, add_BEV, add_PHEV, ret_ICE, ret_BEV, ret_PHEV),
      names_to = c("FlowType", "Powertrain"),
      names_sep = "_",
      values_to = "Vehicles"
    ) %>%
    mutate(
      FlowType = recode(FlowType,
                        add = "New Sales",
                        ret = "Retirements"),
      Powertrain = factor(Powertrain, levels = c("ICE", "BEV", "PHEV"))
    ) %>%
    ggplot(aes(x = Year, y = Vehicles, color = Powertrain, linetype = FlowType)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c("New Sales" = "solid", "Retirements" = "dashed")) +
    scale_y_continuous(labels = scales::label_number_si()) +
    facet_wrap(~State, scales = "free_y", ncol = 6) +
    labs(
      title = paste0("Vehicle Additions and Retirements by State, 2021–2050 (", scenario_name, ")"),
      x = "Year",
      y = "Number of Vehicles (units)",
      color = "Powertrain",
      linetype = "Flow Type"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 9, color = "black"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.key.width = unit(1.5, "cm"),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3),
      strip.text = element_text(size = 9, face = "bold"),
      plot.margin = margin(6, 10, 6, 10)
    )
}

# ==== 生成两张图 ====
p_acc <- plot_addretire_state(acc, "ACCII")
p_rep <- plot_addretire_state(rep, "Repeal")

# ==== 保存（高分辨率学术版） ====
ggsave("Outputs/AddRetire_byState_ACCII_academic.jpg", p_acc, width = 10, height = 6, dpi = 450)
ggsave("Outputs/AddRetire_byState_Repeal_academic.jpg", p_rep, width = 10, height = 6, dpi = 450)

# 可选展示
p_acc
p_rep


# --- Academic Additions vs Retirements by State, split by Section 177 group ---

# --- Academic Additions vs Retirements by State, split by Section 177 group ---

# --- Academic Additions vs Retirements by State (2021–2050)
# --- Split by Section 177 group, unified y-axis in 'k' units

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ==== Section 177 states ====
section177_states <- c(
  "California","Colorado","Connecticut","Delaware","Maine","Maryland",
  "Massachusetts","New Jersey","New Mexico","New York","Oregon",
  "Rhode Island","Vermont","Washington","Pennsylvania","Nevada",
  "Minnesota","Virginia","District of Columbia"
)

# ==== Load Data ====
acc <- read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv") %>%
  mutate(Scenario = "ACCII")
rep <- read_csv("Outputs/ClosedLoop_StateTotals_Repeal.csv") %>%
  mutate(Scenario = "Repeal")

# ==== Colors ====
cols <- c("ICE" = "#E16A5B", "BEV" = "#009E73", "PHEV" = "#3F87C5")

# ==== Plot Function ====
plot_addretire_state <- function(df, scenario_name, group_name, states_vec) {
  df %>%
    filter(State %in% states_vec, Year >= 2021, Year <= 2050) %>%
    select(State, Year,
           add_ICE, add_BEV, add_PHEV,
           ret_ICE, ret_BEV, ret_PHEV) %>%
    pivot_longer(
      cols = c(add_ICE, add_BEV, add_PHEV, ret_ICE, ret_BEV, ret_PHEV),
      names_to = c("FlowType", "Powertrain"),
      names_sep = "_",
      values_to = "Vehicles"
    ) %>%
    mutate(
      FlowType = recode(FlowType, add = "New Sales", ret = "Retirements"),
      Powertrain = factor(Powertrain, levels = c("ICE", "BEV", "PHEV")),
      Vehicles_k = Vehicles / 1000  # convert to thousands
    ) %>%
    ggplot(aes(x = Year, y = Vehicles_k, color = Powertrain, linetype = FlowType)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c("New Sales" = "solid", "Retirements" = "dashed")) +
    scale_y_continuous(
      name = "Number of Vehicles (thousands)",
      labels = label_number(accuracy = 1),
      expand = expansion(mult = c(0, 0.05))
    ) +
    facet_wrap(~State, scales = "free_y", ncol = 5) +
    labs(
      title = paste0("Vehicle Additions and Retirements (", scenario_name, " – ", group_name, ")"),
      x = "Year",
      color = "Powertrain",
      linetype = "Flow Type"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10, color = "black"),
      strip.text = element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.key.width = unit(1.8, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray80", linewidth = 0.4),
      plot.margin = margin(6, 10, 6, 10)
    )
}

# ==== Generate Plots ====
p_acc_177   <- plot_addretire_state(acc, "ACCII",  "Section 177 States",   section177_states)
p_acc_non   <- plot_addretire_state(acc, "ACCII",  "Non-177 States",       setdiff(unique(acc$State), section177_states))
p_rep_177   <- plot_addretire_state(rep, "Repeal", "Section 177 States",   section177_states)
p_rep_non   <- plot_addretire_state(rep, "Repeal", "Non-177 States",       setdiff(unique(rep$State), section177_states))


# ==== Save Figures ====
ggsave("Outputs/AddRetire_ACCII_177_kaxis.jpg",  p_acc_177, width = 11, height = 6, dpi = 450)
ggsave("Outputs/AddRetire_ACCII_non177_kaxis.jpg", p_acc_non, width = 11, height = 6, dpi = 450)
ggsave("Outputs/AddRetire_Repeal_177_kaxis.jpg",  p_rep_177, width = 11, height = 6, dpi = 450)
ggsave("Outputs/AddRetire_Repeal_non177_kaxis.jpg", p_rep_non, width = 11, height = 6, dpi = 450)

# 可选展示
p_acc_177
p_acc_non
p_rep_177
p_rep_non

# 假设你已有：
# p_acc_177, p_acc_non, p_rep_177, p_rep_non
# 四张图（纵轴已是 k，图例在底部）

library(patchwork)
library(grid)
library(ggplot2)
# ==== 1) 四宫格 + 合并图例 ====
grid4 <- (
  (p_acc_177 | p_acc_non) /
    (p_rep_177 | p_rep_non)
) +
  plot_layout(guides = "collect") &
  ggplot2::theme(legend.position = "bottom")

# ==== 2) 用 grid 在中间画箭头 ====
arrow_grob <- function(x0, y0, x1, y1, label, pos = "center",
                       size = 0.7, fontsize = 11, col = "black") {
  # 绘制箭头
  grid::grid.lines(
    x = unit(c(x0, x1), "npc"),
    y = unit(c(y0, y1), "npc"),
    arrow = grid::arrow(length = unit(0.02, "npc"), type = "closed"),
    gp = grid::gpar(col = col, lwd = size * 2)
  )
  # 在箭头上方写文字
  if (pos == "center") {
    grid::grid.text(label, x = (x0 + x1)/2, y = (y0 + y1)/2 + 0.02,
                    gp = grid::gpar(fontsize = fontsize))
  }
}

# ==== 3) 组合 + 添加箭头层 ====
grid.newpage()
grid.draw(ggplotGrob(grid4))

# 水平箭头 (Section 177 → Non-177)
arrow_grob(0.34, 0.52, 0.66, 0.52, "Section 177 → Non-177",
           fontsize = 11, size = 0.8)

# 垂直箭头 (ACCII → Repeal)
arrow_grob(0.50, 0.67, 0.50, 0.36, "ACCII ↓ Repeal",
           fontsize = 11, size = 0.8)

# ==== 4) 保存为 PNG ====
png("Outputs/AddRetire_4panel_with_arrows_patchwork.png", width = 12, height = 10, units = "in", res = 450)
grid.newpage()
grid.draw(ggplotGrob(grid4))
arrow_grob(0.34, 0.52, 0.66, 0.52, "Section 177 → Non-177",
           fontsize = 11, size = 0.8)
arrow_grob(0.50, 0.67, 0.50, 0.36, "ACCII ↓ Repeal",
           fontsize = 11, size = 0.8)
dev.off()
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ==== 读取数据 ====
acc <- read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv") %>%
  mutate(Scenario = "ACCII")
rep <- read_csv("Outputs/ClosedLoop_StateTotals_Repeal.csv") %>%
  mutate(Scenario = "Repeal")

cols <- c("ICE"="#E16A5B", "BEV"="#4DB06D", "PHEV"="#3F87C5")

# ==== 函数：生成一个图 ====
plot_addretire_state <- function(df, scenario_name) {
  df %>%
    filter(Year >= 2021, Year <= 2050) %>%
    select(State, Year,
           add_ICE, add_BEV, add_PHEV,
           ret_ICE, ret_BEV, ret_PHEV) %>%
    pivot_longer(
      cols = c(add_ICE, add_BEV, add_PHEV, ret_ICE, ret_BEV, ret_PHEV),
      names_to = c("FlowType", "Powertrain"),
      names_sep = "_",
      values_to = "Vehicles"
    ) %>%
    mutate(
      FlowType = recode(FlowType,
                        add = "New Sales",
                        ret = "Retirements"),
      Powertrain = factor(Powertrain, levels = c("ICE", "BEV", "PHEV"))
    ) %>%
    ggplot(aes(x = Year, y = Vehicles, color = Powertrain, linetype = FlowType)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = cols) +
    scale_linetype_manual(values = c("New Sales" = "solid", "Retirements" = "dashed")) +
    scale_y_continuous(labels = comma) +
    facet_wrap(~State, scales = "free_y") +
    labs(
      title = paste0("Vehicle Additions (New Sales) and Retirements by State (2021–2050, ", scenario_name, ")"),
      x = "Year",
      y = "Number of Vehicles",
      color = "Powertrain",
      linetype = "Flow Type"
    ) +
    theme_bw(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.minor = element_blank()
    )
}

# ==== 生成两张图 ====
p_acc <- plot_addretire_state(acc, "ACCII")
p_rep <- plot_addretire_state(rep, "Repeal")

# ==== 保存 ====
ggsave("Outputs/AddRetire_byState_ACCII.png", p_acc, width = 14, height = 7, dpi = 300)
ggsave("Outputs/AddRetire_byState_Repeal.png", p_rep, width = 14, height = 7, dpi = 300)

# 显示图像（可选）
p_acc
p_rep



# --- 7) 2020 Car vs Truck ratio per state (academic small figure) ---

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readr)

# 颜色与前文一致
fill_scheme <- c("Car" = "#0072B2", "Truck" = "#D55E00")

# 只取 2020 的类型占比（你之前已经算好）
state_ratio_2020 <- state_type_share %>%
  dplyr::filter(yearID == 2020) %>%
  transmute(
    State = state,
    Car   = type_share_Car,
    Truck = type_share_Truck
  )

# 可选：保存原始表
write_csv(state_ratio_2020, "Outputs/state_car_truck_share_2020.csv")

# 长表用于绘图
state_ratio_long <- state_ratio_2020 %>%
  pivot_longer(cols = c(Car, Truck), names_to = "Segment", values_to = "Share")

# 按 Truck 占比从高到低排序，差异更清晰
state_order <- state_ratio_2020 %>%
  arrange(desc(Truck)) %>%
  pull(State)

state_ratio_long <- state_ratio_long %>%
  mutate(State = factor(State, levels = state_order))

# 小尺寸期刊风图
p_ratio <- ggplot(state_ratio_long, aes(x = State, y = Share, fill = Segment)) +
  geom_col(position = "fill", width = 0.9, color = NA) +
  scale_fill_manual(values = fill_scheme, name = NULL) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Car vs Truck Stock Ratio by State",
    x = "State",
    y = "Share of LDV 2020 Stock"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
    axis.text.y = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.3),
    legend.position = "top",
    legend.text = element_text(size = 12, face = "bold"),
    plot.title.position = "plot",
    plot.margin = margin(10, 14, 8, 14)
  )

# 展示与保存（小尺寸高分辨率）
p_ratio
ggsave("Outputs/State_CarTruck_Ratio_2020_small.jpg", p_ratio, width = 8, height = 5, dpi = 450)
ggsave("Outputs/State_CarTruck_Ratio_2020_small.pdf",  p_ratio, width = 8, height = 5, device = cairo_pdf)
# print to viewer; or save to file
print(p_ratio)
ggsave("Outputs/state_car_truck_ratio_2020.png", p_ratio, width = 13, height = 7, dpi = 300)

# (optional) 如果想看每州Truck占比的条形图
p_truck <- ggplot(state_ratio_2020, aes(x = factor(State, levels = state_order), y = Truck)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Truck Share of 2020 LDV Stock by State",
       x = "State", y = "Truck Share (2020)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# print/保存
print(p_truck)
ggsave("Outputs/state_truck_share_2020.png", p_truck, width = 13, height = 7, dpi = 300)



# ---------------------------
# Vehicle Survival Curves (Academic Small Figure Version)
# ---------------------------

library(ggplot2)
library(dplyr)
library(scales)

# Logistic survival function
S_log <- function(age, mu, b) 1 / (1 + exp((age - mu) / b))

# Parameters from fleet model
params <- tibble(
  Type = c("Car", "Truck"),
  mu = c(16, 19),
  b  = c(4, 4.5)
)

# Build survival table
survival_curves <- params %>%
  group_by(Type) %>%
  do({
    tibble(
      Age = 0:50,
      Survival = S_log(0:50, .$mu, .$b)
    )
  }) %>%
  ungroup()

# Academic color palette
col_scheme <- c("Car" = "#0072B2", "Truck" = "#D55E00")

# Plot compact academic-style figure
p_surv <- ggplot(survival_curves, aes(x = Age, y = Survival, color = Type)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = col_scheme) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_x_continuous(breaks = seq(0, 50, 5), expand = c(0.01, 0.01)) +
  labs(
    title = "Vehicle Survival Curves (Logistic Model)",
    subtitle = "Cumulative Survival Probability by Vehicle Age",
    x = "Vehicle Age (years)",
    y = "Survival Probability (%)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, color = "gray30"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text  = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    legend.position = c(0.80, 0.82),
    legend.background = element_blank(),
    legend.key.size = unit(1.2, "lines"),      # 放大图例标记
    legend.text = element_text(size = 13, face = "bold"),  # 放大文字
    plot.margin = margin(t = 10, r = 16, b = 10, l = 14)
  )

# 展示与保存（更小的输出尺寸）
p_surv
ggsave("Outputs/SurvivalCurve_CarTruck_small.jpg", p_surv, width = 6, height = 4, dpi = 450)
ggsave("Outputs/SurvivalCurve_CarTruck_small.pdf", p_surv, width = 6, height = 4, device = cairo_pdf)
# ggsave("Outputs/survival_curve_car_truck.png", p_surv, width = 10, height = 6, dpi = 300)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(patchwork)

# --- Section 177 清单（含 DC） ---
section177_states <- c(
  "California","Colorado","Connecticut","Delaware","Maine","Maryland",
  "Massachusetts","New Jersey","New Mexico","New York","Oregon",
  "Rhode Island","Vermont","Washington","Pennsylvania","Nevada",
  "Minnesota","Virginia","District of Columbia"
)

# --- 整理 EV share ---
ev_share_df <- function(PR_tbl, scenario_label){
  data_std <- PR_tbl %>%
    mutate(
      State = str_trim(State),
      Year  = as.integer(Year)
    )

  if (all(c("Propulsion", "Fraction") %in% names(data_std))) {
    data_std <- data_std %>%
      filter(Propulsion %in% c("BEV", "PHEV")) %>%
      select(State, Year, Propulsion, Fraction) %>%
      pivot_wider(names_from = Propulsion, values_from = Fraction, values_fill = 0) %>%
      mutate(EV_share = pmax(0, pmin(1, BEV + PHEV)))
  } else if (all(c("add_BEV", "add_PHEV", "add_ICE") %in% names(data_std))) {
    data_std <- data_std %>%
      mutate(
        Total_add = pmax(0, add_BEV + add_PHEV + add_ICE),
        EV_share  = if_else(Total_add > 0, (add_BEV + add_PHEV) / Total_add, 0)
      ) %>%
      select(State, Year, EV_share)
  } else {
    stop("ev_share_df() needs either (Propulsion, Fraction) or (add_BEV, add_PHEV, add_ICE).")
  }

  data_std %>%
    mutate(
      Group = if_else(State %in% section177_states, "Section 177", "Non-177"),
      Scenario = scenario_label
    )
}

baseline_src <- if (exists("P_R_policybaseline")) {
  get("P_R_policybaseline")
} else if (exists("P_R_ACCII")) {
  get("P_R_ACCII")
} else {
  readr::read_csv("Outputs/ClosedLoop_StateTotals_ACCII.csv", show_col_types = FALSE)
}

rollback_src <- if (exists("P_R_policyrollback")) {
  get("P_R_policyrollback")
} else if (exists("P_R_Repeal")) {
  get("P_R_Repeal")
} else {
  readr::read_csv("Outputs/ClosedLoop_StateTotals_Repeal.csv", show_col_types = FALSE)
}

df_base <- ev_share_df(baseline_src, "PolicyBaseline")
df_roll <- ev_share_df(rollback_src, "PolicyRollback")

# --- 期刊风主题（A4 横向画布 11.69 × 8.27 in） ---
theme_pub <- theme_bw(base_size = 21) +
  theme(
    plot.title    = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 17, hjust = 0, color = "gray30"),
    axis.title.x  = element_text(size = 22, face = "bold"),
    axis.title.y  = element_text(size = 15, face = "bold"),
    axis.text.x   = element_text(size = 19, face = "bold", color = "black"),
    axis.text.y   = element_text(size = 14, face = "bold", color = "black"),
    strip.text    = element_text(size = 18, face = "bold"),
    legend.position = "bottom",
    legend.title  = element_text(size = 17, face = "bold"),
    legend.text   = element_text(size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.35),
    panel.spacing = unit(0.9, "lines"),
    plot.margin   = margin(t = 12, r = 20, b = 12, l = 20)
  )

# --- 平均线颜色表 ---
avg_colors <- c(
  "PolicyBaseline-Section 177" = "#0050A4",   # 深蓝
  "PolicyBaseline-Non-177"     = "#7FB3D5",   # 浅蓝
  "PolicyRollback-Section 177" = "#D55E00",   # 深橙
  "PolicyRollback-Non-177"     = "#FDB863"    # 浅橙
)

# --- 绘图函数（灰线 + 平均彩线，无CA标注） ---
make_plot <- function(df, scen, group_lab){
  data_s <- df %>% filter(Scenario==scen, Group==group_lab)
  
  avg_line <- data_s %>%
    group_by(Year) %>%
    summarise(EV_share = mean(EV_share, na.rm=TRUE), .groups="drop") %>%
    mutate(Key = paste0(scen,"-",group_lab))
  
  xmin <- min(data_s$Year, na.rm = TRUE)
  xmax <- max(data_s$Year, na.rm = TRUE)
  
  ggplot() +
    geom_line(
      data = data_s,
      aes(x=Year, y=EV_share, group=State),
      linewidth = 0.6, color="gray65", alpha=0.6
    ) +
    geom_line(
      data = avg_line,
      aes(x=Year, y=EV_share, color=Key),
      linewidth = 2
    ) +
    scale_color_manual(values = avg_colors, guide="none") +
    scale_x_continuous(limits = c(xmin, xmax), expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = expansion(mult = c(0.02, 0.03))
    ) +
    labs(
      title = paste0(scen, " - ", group_lab),
      x = "Year",
      y = "EV New Sales Share"
    ) +
    theme_pub
}

# --- 四张图 ---
p1 <- make_plot(df_base, "PolicyBaseline", "Section 177")
p2 <- make_plot(df_base, "PolicyBaseline", "Non-177")
p3 <- make_plot(df_roll, "PolicyRollback", "Section 177")
p4 <- make_plot(df_roll, "PolicyRollback", "Non-177")

wrap <- (p1 | p2) / (p3 | p4)

wrap <- wrap +
  plot_annotation(
    title = "EV Share of New Light-Duty Vehicle Sales under PolicyBaseline and PolicyRollback Scenarios",
    subtitle = "Gray lines represent individual states, while bold colored lines represent the group mean.",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 26, hjust = 0),
      plot.subtitle = element_text(size = 18, hjust = 0.01, color = "gray30")
    )
  )

# --- 输出 ---
wrap

# A4 landscape: 297 mm × 210 mm ≈ 11.69 in × 8.27 in
EV_FIG_W <- 11.69
EV_FIG_H <- 8.27

ggsave("Outputs/EV_penetration_PolicyBaseline_PolicyRollback_clean.png", wrap, width = EV_FIG_W, height = EV_FIG_H, dpi = 450)
ggsave("Outputs/EV_penetration_PolicyBaseline_PolicyRollback_clean.pdf", wrap, width = EV_FIG_W, height = EV_FIG_H, device = "pdf")
ggsave("Outputs/EV_penetration_ACCII_Repeal_clean.png", wrap, width = EV_FIG_W, height = EV_FIG_H, dpi = 450)
ggsave("Outputs/EV_penetration_ACCII_Repeal_clean.pdf", wrap, width = EV_FIG_W, height = EV_FIG_H, device = "pdf")