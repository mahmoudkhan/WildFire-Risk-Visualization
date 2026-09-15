library(tidyverse)
library(readxl)
library(scales)
library(GGally)

# Import data
setwd("c:/Users/msohr/Desktop/NU/DDS-8510/8")
wrc <- read_excel(
  "wrc_download_20260415.xlsx",
  sheet = "Counties"
)
svi <- read_csv(
  "SVI_2022_US_county.csv",
  show_col_types = FALSE
)

# Clean county identifiers and select variables
wrc_clean <- wrc |>
  transmute(
    FIPS = str_pad(as.character(GEOID), 5, pad = "0"),
    state = STUSPS,
    county = NAME,
    buildings = TOTAL_BUILDINGS,
    minimal_exposure = BUILDINGS_FRACTION_ME,
    indirect_exposure = BUILDINGS_FRACTION_IE,
    direct_exposure = BUILDINGS_FRACTION_DE,
    burn_probability = BP_NATIONAL_RANK,
    wildfire_risk = RISK_NATIONAL_RANK
  )
svi_clean <- svi |>
  transmute(
    FIPS = str_pad(as.character(FIPS), 5, pad = "0"),
    population = E_TOTPOP,
    svi = RPL_THEMES,
    svi_socioeconomic = RPL_THEME1,
    svi_household = RPL_THEME2,
    svi_minority = RPL_THEME3,
    svi_housing_transport = RPL_THEME4
  )

# Join datasets
county_data <- wrc_clean |>
  inner_join(svi_clean, by = "FIPS") |>
  filter(
    !is.na(wildfire_risk),
    !is.na(svi)
  ) |>
  mutate(
    county_label = paste0(county, ", ", state),
    svi_quartile = case_when(
      svi <= .25 ~ "Q1: Least vulnerable",
      svi <= .50 ~ "Q2",
      svi <= .75 ~ "Q3",
      TRUE       ~ "Q4: Most vulnerable"
    ),
    high_high =
      wildfire_risk >= .75 &
      svi >= .75,
    # Exploratory measure only; not an official federal score
    compound_priority =
      (wildfire_risk + svi) / 2
  )

# Statistical analysis
cor.test(
  county_data$wildfire_risk,
  county_data$svi,
  method = "spearman",
  exact = FALSE
)

# SVI theme correlations
theme_correlations <- county_data |>
  summarise(
    overall = cor(wildfire_risk, svi,
                  method = "spearman",
                  use = "complete.obs"),
    socioeconomic =
      cor(wildfire_risk, svi_socioeconomic,
          method = "spearman",
          use = "complete.obs"),
    household =
      cor(wildfire_risk, svi_household,
          method = "spearman",
          use = "complete.obs"),
    minority =
      cor(wildfire_risk, svi_minority,
          method = "spearman",
          use = "complete.obs"),
    housing_transport =
      cor(wildfire_risk, svi_housing_transport,
          method = "spearman",
          use = "complete.obs")
  )
print(theme_correlations)

# High–high counties
high_high_counties <- county_data |> filter(high_high)
nrow(high_high_counties)
sum(high_high_counties$population, na.rm = TRUE)
sum(high_high_counties$buildings, na.rm = TRUE)

# Figure 1: Scatter/bubble plot
ggplot(
  county_data,
  aes(
    x = svi,
    y = wildfire_risk,
    size = buildings
  )
) +
  geom_point(
    alpha = .30,
    color = "#1f77b4"
  ) +
  geom_vline(
    xintercept = .75,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = .75,
    linetype = "dashed"
  ) +
  scale_size_continuous(
    range = c(1, 8),
    labels = comma
  ) +
  labs(
    x = "CDC/ATSDR Social Vulnerability Index percentile",
    y = "USDA Wildfire Risk to Potential Structures percentile",
    size = "Buildings"
  ) +
  theme_minimal()

# Figure 2: Wildfire risk by SVI quartile
ggplot(
  county_data,
  aes(
    x = svi_quartile,
    y = wildfire_risk
  )
) +
  geom_boxplot(
    outlier.shape = NA
  ) +
  labs(
    x = "Social vulnerability quartile",
    y = "Wildfire-risk percentile"
  ) +
  theme_minimal()

# Figure 3: Exposure-zone composition
exposure_summary <- county_data |>
  group_by(svi_quartile) |>
  summarise(
    Minimal = mean(
      minimal_exposure,
      na.rm = TRUE
    ),
    Indirect = mean(
      indirect_exposure,
      na.rm = TRUE
    ),
    Direct = mean(
      direct_exposure,
      na.rm = TRUE
    )
  ) |>
  pivot_longer(
    Minimal:Direct,
    names_to = "Exposure",
    values_to = "Fraction"
  )
ggplot(
  exposure_summary,
  aes(
    x = svi_quartile,
    y = Fraction,
    fill = Exposure
  )
) +
  geom_col() +
  scale_fill_manual(
    values = c(
      "Minimal" = "#1f77b4",
      "Indirect" = "#ff7f0e",
      "Direct" = "#2ca02c"
    )
  ) +
  labs(
    x = "Social vulnerability quartile",
    y = "Mean fraction of buildings",
    fill = NULL
  ) +
  theme_minimal()

# Figure 4: Ranked dot plot
top15 <- county_data |>
  slice_max(
    compound_priority,
    n = 15
  ) |>
  arrange(compound_priority) |>
  mutate(
    county_label =
      factor(
        county_label,
        levels = county_label
      )
  )
ggplot(
  top15,
  aes(
    x = compound_priority,
    y = county_label
  )
) +
  geom_segment(
    aes(
      x = .88,
      xend = compound_priority,
      yend = county_label
    )
  ) +
  geom_point(size = 3) +
  labs(
    x = "Exploratory compound priority index",
    y = NULL
  ) +
  theme_minimal()

# Figure 5: Multidimensional heatmap
top20_long <- county_data |>
  slice_max(
    compound_priority,
    n = 20
  ) |>
  select(
    county_label,
    wildfire_risk,
    burn_probability,
    svi_socioeconomic,
    svi_household,
    svi_minority,
    svi_housing_transport
  ) |>
  pivot_longer(
    -county_label,
    names_to = "Measure",
    values_to = "Value"
  )
ggplot(
  top20_long,
  aes(
    x = Measure,
    y = county_label,
    fill = Value
  )
) +
  geom_tile() +
  scale_fill_viridis_c(
    limits = c(0, 1)
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Percentile"
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 35,
        hjust = 1
      )
  )

# Figure 6: Parallel coordinates
top12 <- county_data |>
  slice_max(
    compound_priority,
    n = 12
  ) |>
  select(
    county_label,
    wildfire_risk,
    burn_probability,
    svi,
    svi_socioeconomic,
    svi_household,
    svi_housing_transport,
    direct_exposure
  )
ggparcoord(
  top12,
  columns = 2:8,
  groupColumn = 1,
  scale = "globalminmax",
  alphaLines = .65
) +
  labs(
    x = NULL,
    y = "Normalized value"
  ) +
  theme_minimal()

# Export cleaned table for Tableau
write_csv(county_data, "tableau_wildfire_svi_counties.csv")
