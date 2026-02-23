# ============================================
# Temiscouata Project – Foraging Traits (Gill Rakers)
# MSc Thesis
# Author: Sarah Minyoung Song
# ============================================
# This script:
# 1. Imports raw measurements from CSV files
# 2. Cleans and converts gill raker measurements for analysis
# 3. Produces exploratory plots of gill raker traits grouped by lake and year

library(tidyverse)

gill_2123 <- read_csv("data/gill raker data_2021-2023.csv")
gill_2025 <- read_csv("data/gill raker data_2025.csv")

gill_raw <- bind_rows(gill_2123, gill_2025)

# Convert pixel measurements to mm
gill_raw <- gill_raw |>
  mutate(
    conversion_factor = 1 / ruler_pixels
  )

# Convert lengths and widths to mm -> average replicates
gill_raw <- gill_raw |>
  mutate(
    length_3_mm = length_3 * conversion_factor,
    length_4_mm = length_4 * conversion_factor,
    length_5_mm = length_5 * conversion_factor,
    width_1_mm  = width_1  * conversion_factor,
    width_2_mm  = width_2  * conversion_factor,
    width_3_mm  = width_3  * conversion_factor
  )

gill_final <- gill_raw |>
  rowwise() |>
  mutate(
    mean_length_mm = mean(c_across(c(length_3_mm, length_4_mm, length_5_mm)), na.rm = TRUE),
    mean_width_mm  = mean(c_across(c(width_1_mm, width_2_mm, width_3_mm)), na.rm = TRUE)
  ) |>
  ungroup()

# Clean final dataset -> save clean version
gill_final <- gill_final |> 
  mutate(
    year_group = case_when(
      year >= 2021 & year <= 2023 ~ "2021–2023",
      year == 2025 ~ "2025",
      TRUE ~ NA_character_
    )
  )

gill_final <- gill_final |>
  select(
    lake,
    year,
    year_group,
    SL,
    count,
    mean_length_mm,
    mean_width_mm
  ) |>
  filter(
    !is.na(mean_length_mm),
    !is.na(count)
  )

write_csv(gill_final, "data/gill_raker_cleaned.csv")

# Scatter plot for gill raker length vs. standard length by lake
ggplot(
  gill_final |> 
    filter(
      !is.na(mean_length_mm),
      !is.na(SL),
      mean_length_mm > 0
    ),
  aes(x = SL, y = mean_length_mm, colour = lake)
) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year_group) +
  scale_colour_brewer(palette = "Paired") +
  theme_bw() +
  labs(
    x = "Standard length (mm)",
    y = "Mean gill raker length (mm)",
    colour = "Lake",
    title = "Gill raker length vs SL across lakes and years"
  )

# Scatter plot for gill raker gap width vs. standard length by lake
ggplot(
  gill_final |> 
    filter(
      !is.na(mean_width_mm),
      !is.na(SL),
      mean_width_mm > 0
    ),
  aes(x = SL, y = mean_width_mm, colour = lake)
) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year_group) +
  scale_colour_brewer(palette = "Paired") +
  theme_bw() +
  labs(
    x = "Standard length (mm)",
    y = "Mean gill raker gap width (mm)",
    colour = "Lake",
    title = "Gill raker gap width vs SL across lakes and years"
  )

# Box plots for gill raker number by lake and year group
ggplot(gill_final, aes(x = lake, y = count, fill = year_group)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  labs(
    title = "Gill raker number by lake and year group",
    x = "Lake",
    y = "Gill raker count",
    fill = "Year group"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )