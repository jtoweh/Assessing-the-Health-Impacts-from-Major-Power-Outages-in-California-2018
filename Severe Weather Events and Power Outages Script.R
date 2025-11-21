###############################################
# Supplementary Materials: R Code Preamble
# Project: Assessing the Health impacts of Major California Power Outages in 2018
# Author: Jayson Toweh
# Date: November 2025
# Description:
#   This preamble loads required packages, sets
#   reproducible settings, defines helper functions,
#   and establishes the working environment used
#   throughout the analyses in this manuscript.
###############################################

############################################################
# Supplementary R Code
# Paper: Assessing the Health Impacts from Major Power
#        Outages in California 2018
# Author: Jayson U. Toweh
# Description:
#   This script reproduces key descriptive figures:
#   - Monthly outage hours (statewide, 2018)
#   - County-level outage hours (choropleth)
#   - County-level wildfire acreage (choropleth)
#   - LOESS regression for respiratory hospitalizations
############################################################

## 0. Packages ----
# Uncomment if needed:
# install.packages(c("tidyverse", "sf", "tigris", "viridis", "scales", "lubridate", "readr"))

library(tidyverse)
library(sf)
library(tigris)
library(viridis)
library(scales)
library(lubridate)
library(readr)

options(tigris_use_cache = TRUE)
theme_set(theme_minimal())


## 1. File paths (edit to your structure) ----
data_dir <- "PATH/TO/YOUR/DATA"  # e.g., "data/"

# Daily outages with FIPS & date
power_daily_fips_path <- file.path(data_dir, "California_Power_Outages_Date_2017_2023_FIPS.csv")

# Outage totals by county (2018)
power_2018_by_county_path <- file.path(data_dir, "CA_2018_CustomerHoursOut_by_County_FIPS.csv")

# Wildfires by county (2018, accurate county allocation)
fires_2018_path <- file.path(data_dir, "California Wildfires 2018-Accurate.csv")

# LOESS data example (Riverside; analogous files for Orange, etc.)
riverside_regression_path <- file.path(data_dir, "Riverside_Data_Regression.csv")
orange_regression_path    <- file.path(data_dir, "Orange_Data_Regression.csv")

# Output directory for figures
fig_dir <- file.path(data_dir, "figures")
if (!dir.exists(fig_dir)) dir.create(fig_dir)


## 2. Monthly outage hours (statewide, 2018) ----
# Uses the daily outage dataset with Year / Month / CustomerHoursOutTotal
power_df <- read_csv(
  power_daily_fips_path,
  show_col_types = FALSE
)

power_2018 <- power_df %>%
  filter(Year == 2018)

monthly_outages <- power_2018 %>%
  group_by(Month) %>%
  summarise(
    TotalHoursOut = sum(CustomerHoursOutTotal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    MonthName = factor(month.name[Month], levels = month.name)
  )

p_monthly_outages <- ggplot(monthly_outages,
                            aes(x = MonthName,
                                y = TotalHoursOut,
                                group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.4) +
  geom_point(color = "darkred", size = 3) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(0, NA),
    expand = c(0, 0)
  ) +
  labs(
    title = "Monthly Power Outages in California (2018)",
    x     = "Month",
    y     = "Total Customer Hours Out"
  ) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_text(face = "bold")
  )

ggsave(
  filename = file.path(fig_dir, "FigX_California_2018_Monthly_Outages.png"),
  plot     = p_monthly_outages,
  width    = 8,
  height   = 5,
  dpi      = 300
)


## 3. County shapes for California ----
# Use TIGRIS county geometries for consistent mapping
ca_counties <- counties(state = "CA", year = 2018, cb = TRUE) %>%
  st_as_sf() %>%
  mutate(
    FIPS = paste0(STATEFP, COUNTYFP),      # 5-digit county FIPS
    FIPS = str_pad(FIPS, 5, pad = "0")
  )


## 4. Outage hours by county (choropleth, 2018) ----
# Input: CA_2018_CustomerHoursOut_by_County_FIPS.csv
# Expected columns: county, FIPS, CustomerHoursOutTotal

out_2018 <- read_csv(
  power_2018_by_county_path,
  col_types = cols(
    county              = col_character(),
    FIPS                = col_character(),
    CustomerHoursOutTotal = col_double()
  )
) %>%
  mutate(
    FIPS = str_pad(FIPS, 5, pad = "0")
  )

outage_map_df <- ca_counties %>%
  left_join(out_2018, by = "FIPS")

p_outage_map <- ggplot(outage_map_df) +
  geom_sf(aes(fill = CustomerHoursOutTotal),
          color = "white", size = 0.3) +
  scale_fill_viridis(
    option  = "C",
    trans   = "pseudo_log",   # handles heavy skew
    na.value = "grey90",
    name    = "Customer Hours Out (2018)",
    labels  = label_comma()
  ) +
  labs(
    title    = "California County Power Outage Hours (2018)",
    subtitle = "Total customer hours without electricity by county",
    caption  = "Source: PowerOutage.us (processed by author)"
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.title       = element_blank(),
    plot.title       = element_text(face = "bold")
  )

ggsave(
  filename = file.path(fig_dir, "Fig2B_California_2018_County_Outage_Map.png"),
  plot     = p_outage_map,
  width    = 8,
  height   = 10,
  dpi      = 300
)


## 5. Wildfire acreage by county (choropleth, 2018) ----
# Input: California Wildfires 2018-Accurate.csv
# Expected columns (example): CountyName, Year, AcresBurned (or similar)

fires_raw <- read_csv(
  fires_2018_path,
  show_col_types = FALSE
)

# Adjust column names here if your headers differ
# (this is the simplified version of your flexible header-detection script).
fires_norm <- fires_raw %>%
  rename(
    County      = CountyName,    # edit if needed
    Year        = Year,          # edit if needed
    AcresBurned = AcresBurned    # edit if needed
  ) %>%
  mutate(
    County      = as.character(County),
    Year        = as.integer(Year),
    AcresBurned = as.numeric(AcresBurned)
  )

fires_2018 <- fires_norm %>%
  filter(Year == 2018) %>%
  mutate(
    COUNTY_UP = County |>
      str_trim() |>
      str_remove(regex("\\s*County$", ignore_case = TRUE)) |>
      str_to_upper()
  ) %>%
  group_by(COUNTY_UP) %>%
  summarise(
    AcresBurnedTotal = sum(AcresBurned, na.rm = TRUE),
    .groups = "drop"
  )

# Build a county-name key on the shapes side
ca_counties_named <- ca_counties %>%
  mutate(
    COUNTY_UP = NAME |>
      as.character() |>
      str_trim() |>
      str_remove(regex("\\s*County$", ignore_case = TRUE)) |>
      str_to_upper()
  )

wildfire_map_df <- ca_counties_named %>%
  left_join(fires_2018, by = "COUNTY_UP")

p_wildfire_map <- ggplot(wildfire_map_df) +
  geom_sf(aes(fill = AcresBurnedTotal),
          color = "white", size = 0.3) +
  scale_fill_viridis(
    option   = "C",
    trans    = "pseudo_log",
    na.value = "grey90",
    name     = "Acres burned (2018)",
    labels   = label_comma()
  ) +
  labs(
    title    = "California Major Wildfires (2018)",
    subtitle = "Total acres burned by county (≥300-acre fires)",
    caption  = "Source: CalFire Redbook (processed by author)"
  ) +
  theme(
    panel.grid.major = element_blank(),
    axis.title       = element_blank(),
    plot.title       = element_text(face = "bold")
  )

ggsave(
  filename = file.path(fig_dir, "Fig2A_California_2018_Wildfire_Map.png"),
  plot     = p_wildfire_map,
  width    = 8,
  height   = 10,
  dpi      = 300
)


## 6. LOESS regression for respiratory infections (Riverside & Orange) ----
# These plots correspond to LOESS-smoothed daily respiratory infections
# around outage dates (e.g., Figure 6). This replaces the longer
# base-graphics version with a compact ggplot workflow.

# Helper: generic LOESS plotting function
plot_loess_resp <- function(csv_path,
                            date_col   = "Converted.Date",
                            outcome_col = "Respiratory.Infections",
                            span        = 0.75,
                            main_title  = "County LOESS Regression: Respiratory Infections") {
  
  df <- read_csv(csv_path, show_col_types = FALSE)
  
  # Ensure numeric/time variable for LOESS; here `Converted.Date` is already
  # a numeric day index (e.g., -30 to +30). If it is Date, convert to numeric.
  x <- df[[date_col]]
  y <- df[[outcome_col]]
  
  loess_fit <- loess(y ~ x, span = span, data = df)
  
  pred <- predict(loess_fit, se = TRUE)
  
  plot_df <- df %>%
    mutate(
      x     = x,
      y     = y,
      fit   = pred$fit,
      upper = pred$fit + 1.96 * pred$se.fit,
      lower = pred$fit - 1.96 * pred$se.fit
    )
  
  ggplot(plot_df, aes(x = x, y = y)) +
    geom_point(alpha = 0.6) +
    geom_line(aes(y = fit), color = "steelblue", linewidth = 1.1) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.2, fill = "steelblue") +
    labs(
      title = main_title,
      x     = "Days before and after outage",
      y     = "Respiratory infection hospitalizations"
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# Riverside
p_riverside_loess <- plot_loess_resp(
  csv_path   = riverside_regression_path,
  main_title = "Riverside County: LOESS Regression of Respiratory Infections"
)

ggsave(
  filename = file.path(fig_dir, "Fig6A_Riverside_LOESS_RespiratoryInfections.png"),
  plot     = p_riverside_loess,
  width    = 7,
  height   = 5,
  dpi      = 300
)

# Orange
p_orange_loess <- plot_loess_resp(
  csv_path   = orange_regression_path,
  main_title = "Orange County: LOESS Regression of Respiratory Infections"
)

ggsave(
  filename = file.path(fig_dir, "Fig6B_Orange_LOESS_RespiratoryInfections.png"),
  plot     = p_orange_loess,
  width    = 7,
  height   = 5,
  dpi      = 300
)




############################################################
# End of Supplementary R Code
############################################################
