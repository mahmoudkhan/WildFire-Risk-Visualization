library(tidyverse)
library(readxl)
library(scales)
library(GGally)

# Import data
wrc <- read_excel(
  "wrc_download_20260415.xlsx",
  sheet = "Counties"
)
svi <- read_csv(
  "SVI_2022_US_COUNTY.csv",
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
    county_label = county,
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

# Significance tests for SVI theme correlations

cor.test(
  county_data$wildfire_risk,
  county_data$svi_socioeconomic,
  method = "spearman",
  exact = FALSE
)

cor.test(
  county_data$wildfire_risk,
  county_data$svi_household,
  method = "spearman",
  exact = FALSE
)

cor.test(
  county_data$wildfire_risk,
  county_data$svi_minority,
  method = "spearman",
  exact = FALSE
)

cor.test(
  county_data$wildfire_risk,
  county_data$svi_housing_transport,
  method = "spearman",
  exact = FALSE
)

# High–high counties
high_high_counties <- county_data |> filter(high_high)
nrow(high_high_counties)
sum(high_high_counties$population, na.rm = TRUE)
sum(high_high_counties$buildings, na.rm = TRUE)

# Figure 1: Scatter/bubble plot
fig1 = ggplot(
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
    y = "Wildfire Risk to Potential Structures percentile",
    size = "Buildings"
  ) +
  theme_minimal()
print(fig1)
ggsave(
    "Figure1.tiff",
    plot = fig1,
    width = 6.5,
    height = 3.5,
    units = "in",
    dpi = 600,
    compression = "lzw"
  )

# Figure 2: Wildfire risk by SVI quartile
fig2 = ggplot(
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
print(fig2)
ggsave(
  "Figure2.tiff",
  plot = fig2,
  width = 6.5,
  height = 3.3,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

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
fig3 = ggplot(
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
print(fig3)
ggsave(
  "Figure3.tiff",
  plot = fig3,
  width = 6.5,
  height = 3.3,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# Figure 4 and Supplementary Figure S1 were created in Tableau Desktop 2026.2
# using the exported county-level dataset below.
# Bivariate classes used the thresholds:
# Low: 0 to < .333
# Moderate: .333 to < .667
# High: .667 to 1.000

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
fig5 = ggplot(
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
  scale_x_discrete(
    labels = c(
      "burn_probability" = "Burn probability",
      "svi_household" = "Household characteristics",
      "svi_housing_transport" = "Housing/transportation",
      "svi_minority" = "Racial/ethnic minority",
      "svi_socioeconomic" = "Socioeconomic status",
      "wildfire_risk" = "Wildfire risk"
    )
  ) +
  theme_minimal() +
  theme(
    axis.text.x =
      element_text(
        angle = 35,
        hjust = 1
      )
  )
print(fig5)
ggsave(
  "Figure5.tiff",
  plot = fig5,
  width = 6.5,
  height = 3.3,
  units = "in",
  dpi = 600,
  compression = "lzw"
)

# Export cleaned table for Tableau
write_csv(county_data, "Supplementary_Data_S1.csv")

# Table 3: wildfire risk by SVI quartile
quartile_summary <- county_data |>
  group_by(svi_quartile) |>
  summarise(
    n = n(),
    mean_risk = mean(wildfire_risk, na.rm = TRUE),
    median_risk = median(wildfire_risk, na.rm = TRUE)
  )

print(quartile_summary)

# Table 4: highest compound-priority counties
top10_priority <- county_data |>
  slice_max(
    order_by = compound_priority,
    n = 10,
    with_ties = FALSE
  ) |>
  select(
    county_label,
    wildfire_risk,
    svi,
    compound_priority
  )

print(top10_priority)

# Wildfire risk versus Burn Probability
cor.test(
  county_data$wildfire_risk,
  county_data$burn_probability,
  method = "spearman",
  exact = FALSE
)
