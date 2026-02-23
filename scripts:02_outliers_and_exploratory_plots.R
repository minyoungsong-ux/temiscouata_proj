# ============================================
# Temiscouata Project – Outliers & EDA on Phenotypic Measurements
# MSc Thesis
# Author: Sarah Minyoung Song
# ============================================
# This script:
# 1. Loads the processed phenotypic dataset produced in Script 01
# 2. Defines relevant year groups (1980–1981, 2021–2023, 2025)
# 3. Identifies numeric phenotypic traits
# 4. Produces exploratory plots of traits grouped by lake and year

library(tidyverse)

stickleback_all <- read_csv(
  "data/stickleback_all_measurements.csv"
)

stickleback_all <- stickleback_all |> 
  filter(!is.na(lake))

# Plot palette
pastel_palette <- c(
  "1980–1981" = "#B3CDE3",  # soft blue
  "2021–2023" = "#CCEBC5",  # soft green
  "2025"      = "#DECBE4"   # soft purple
)

# Define numeric traits
outlier_traits <- c(
  "sl", "body_depth", "ap_length", "ap_width", 
  "jaw_length", "ppl", "ppw", "dorsal_1",
  "dorsal_2", "pspine_r", "pspine_l"
)

stickleback_long <- stickleback_all |>
  pivot_longer(
    cols = all_of(outlier_traits),
    names_to = "trait",
    values_to = "value"
  ) |>
  filter(!is.na(value),
         value != 0
  )

# Visualize and flag outliers using IQR (interquartile range)
outlier_table <- stickleback_long |>
  group_by(trait) |>
  mutate(
    Q1 = quantile(value, 0.25),
    Q3 = quantile(value, 0.75),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    is_outlier = value < lower_bound | value > upper_bound
  ) |>
  ungroup() |>
  filter(is_outlier)

outlier_summary <- outlier_table |>
  select(
    lake,
    year,
    year_group,
    trait,
    value,
    lower_bound,
    upper_bound
  ) |>
  arrange(trait, lake, value)

View(outlier_summary)

# Create a clean year group variable
stickleback_all <- stickleback_all |> 
  mutate(
    year_group = case_when(
      year %in% c(1980, 1981) ~ "1980–1981",
      year >= 2021 & year <= 2023 ~ "2021–2023",
      year == 2025 ~ "2025",
      TRUE ~ NA_character_
    )
  )

# Sanity check:
count(stickleback_all, year_group)

# Define numeric traits
traits <- c(
  "sl", "body_depth", "ap_length", "ap_width", 
  "jaw_length", "ppl", "ppw", "dorsal_1",
  "dorsal_2", "pspine_r", "pspine_l"
)

# Reshape data for easy plotting
stickleback_long <- stickleback_all |> 
  pivot_longer(
    cols = all_of(traits),
    names_to = "trait",
    values_to = "value"
  )

# Boxplots of each lake per trait (e.g. jaw length) separated by year group
ggplot(
  stickleback_all,
  aes(x = lake, y = jaw_length, fill = year_group)
) +
  geom_boxplot(outlier.shape = NA, colour = "grey30") +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2),
    alpha = 0.35,
    size = 1,
    colour = "grey20"
  ) +
  scale_fill_manual(values = pastel_palette, drop = TRUE) +
  theme_bw() +
  labs(
    x = "Lake",
    y = "Jaw length (mm)",
    fill = "Sampling period"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Scatter plot for a single trait vs. standard length by lake
# (e.g. body depth)
traits_no_sl <- c(
  "body_depth",
  "ap_length",
  "ap_width",
  "jaw_length",
  "ppl",
  "ppw",
  "pspine_r",
  "pspine_l",
  "dorsal_1",
  "dorsal_2"
)

ggplot(
  stickleback_all,
  aes(x = sl, y = body_depth, colour = year_group)
) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ lake) +
  scale_colour_manual(values = pastel_palette) +
  theme_bw() +
  labs(
    x = "Standard length (mm)",
    y = "Body depth (mm)",
    colour = "Sampling period",
    title = "Body depth vs SL by lake"
  )

# Scatter plot for all lakes on one graph, faceted by year group
# (e.g. body depth)
ggplot(
  stickleback_all,
  aes(x = sl, y = body_depth, colour = lake)
) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year_group) +
  scale_colour_brewer(palette = "Paired") +
  theme_bw() +
  labs(
    x = "Standard length (mm)",
    y = "Body depth (mm)",
    colour = "Lake",
    title = "Body depth vs SL across lakes and time"
  ) +
  theme(
    legend.position = "top"
  )

# Scatter plot for pelvic spine length vs. standard length
# (excluding N/A values aka stickleback without pelvic spine)
ggplot(
  stickleback_all |> 
    filter(
      !is.na(pspine_l),
      pspine_l > 0
    ),
  aes(x = sl, y = pspine_l, colour = lake)
) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year_group) +
  scale_colour_brewer(palette = "Paired") +
  theme_bw() +
  labs(
    x = "Standard length (mm)",
    y = "Pelvic spine length (mm)",
    colour = "Lake",
    title = "Pelvic spine_L vs SL across lakes and time"
  ) +
  theme(
    legend.position = "top"
  )

# Proportional stacked bar plots for plate morph frequencies
ggplot(
  stickleback_all |>
    filter(
      !is.na(plate_l),
      !is.na(year_group),
      !is.na(lake)
    ),
  aes(x = year_group, fill = plate_l)
) +
  geom_bar(position = "fill") +
  facet_wrap(~ lake) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "L" = "#A8DADC",
      "P" = "#F1FAEE",
      "C" = "#457B9D"
    )
  ) +
  theme_bw() +
  labs(
    x = "Sampling period",
    y = "Proportion of individuals",
    fill = "Plate morph",
    title = "Plate morph (left side) frequency across lakes and sampling periods"
  )

# End of Script 02
# No data values are altered in this script.