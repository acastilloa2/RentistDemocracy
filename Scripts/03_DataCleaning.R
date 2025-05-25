# ============================================================
# Replication Script: Rentist Democracy – Data Cleaning
# Author: Alejandro Castillo Ardila
# Contact: a.castilloa2@uniandes.edu.co
# Date: February 12, 2024
# Description: This script merges and cleans panel data from CEDE,
# conflict data (IICA), oil & gas data (ANH), and electoral data.
# ============================================================

# -------------------------------
# 1. Load Required Packages
# -------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, skimr, haven, rio, readxl, tibble, janitor)

# -------------------------------
# 2. Load CEDE Panel Data
# -------------------------------

panel_bg  <- read_dta("Data/PANEL_BUEN_GOBIERNO(2024).dta")
panel_sys <- read_dta("Data/PANEL_SALUD_Y_SERVICIOS.dta")
panel_cg  <- read_dta("Data/PANEL_CARACTERISTICAS_GENERALES(2024).dta")
panel_cyv <- read_dta("Data/PANEL_CONFLICTO_Y_VIOLENCIA(2024).dta")

panel <- panel_bg %>%
  left_join(panel_sys, by = c("codmpio", "ano")) %>%
  left_join(panel_cg,  by = c("codmpio", "ano")) %>%
  left_join(panel_cyv, by = c("codmpio", "ano"))

rm(panel_bg, panel_sys, panel_cg)  # Remove individual datasets

# -------------------------------
# 3. Load GDP Deflator (Colombia)
# -------------------------------

gdp_deflactor <- tribble(
  ~country, ~anoCEDE, ~gdp_deflactor,
  "Colombia", 1999, 43.58, "Colombia", 2000, 48.44, "Colombia", 2001, 51.60,
  "Colombia", 2002, 54.68, "Colombia", 2003, 58.41, "Colombia", 2004, 62.66,
  "Colombia", 2005, 65.64, "Colombia", 2006, 69.45, "Colombia", 2007, 73.07,
  "Colombia", 2008, 78.68, "Colombia", 2009, 81.87, "Colombia", 2010, 84.99,
  "Colombia", 2011, 90.42, "Colombia", 2012, 93.69, "Colombia", 2013, 95.47,
  "Colombia", 2014, 97.61, "Colombia", 2015, 100.00, "Colombia", 2016, 105.15,
  "Colombia", 2017, 110.55, "Colombia", 2018, 115.67, "Colombia", 2019, 120.29,
  "Colombia", 2020, 122.08, "Colombia", 2021, 131.60, "Colombia", 2022, 151.17,
  "Colombia", 2023, 160.74
)

# -------------------------------
# 4. Load and Clean Conflict Data (IICA)
# -------------------------------

conflict <- read_csv("Data/IICA_clean.csv") %>%
  rename(ano = year, iica = IICA, iica_cat = IICA_cat) %>%
  select(codmpio, ano, iica, iica_cat) %>%
  filter(ano %in% c(1999, 2002, 2006, 2010, 2014, 2018)) %>%
  filter(iica != 0)

# -------------------------------
# 5. Load and Clean Oil & Gas Data (ANH)
# -------------------------------

anh_data <- read_dta("Data/ANH/anh_clean.dta") %>%
  mutate(codmpio = as.double(codmpio))

# -------------------------------
# 6. Merge Panel with Conflict and ANH Data
# -------------------------------

panel_clean <- panel %>%
  left_join(conflict, by = c("codmpio", "ano")) %>%
  mutate(
    iica = ifelse(is.na(iica), 0, iica),
    iica_cat = ifelse(is.na(iica_cat), 0, iica_cat)
  ) %>%
  left_join(anh_data, by = "codmpio", relationship = "many-to-many") %>%
  mutate(
    binary_CRUDO = ifelse(is.na(binary_CRUDO), 0, binary_CRUDO),
    binary_GAS = ifelse(is.na(binary_GAS), 0, binary_GAS)
  )

# -------------------------------
# 7. Load and Merge Electoral Data
# -------------------------------

# Load elections data
elections_files <- list.files("Data", pattern = "alcaldia\\.dta$", full.names = TRUE)
elections_list <- lapply(elections_files, function(file) {
  read_dta(file) %>% select(-circunscripcion)
})
elecciones <- bind_rows(elections_list)
rm(elections_list)

# -------------------------------
# 8. Create Electoral Competitiveness Measures
# -------------------------------

# Winner Strength (WS)
eleccionesWS <- elecciones %>%
  filter(!nombres %in% c("TARJETAS NO MARCADAS", "VOTOS NULOS", "VOTOS EN BLANCO")) %>%
  group_by(ano, codmpio) %>%
  filter(sum(elegido == 1) == 1) %>%
  mutate(
    votosG = votos[elegido == 1],
    total_votos = sum(votos, na.rm = TRUE)
  ) %>%
  filter(elegido == 1) %>%
  mutate(
    WS = votosG / total_votos,
    anoEle = ano
  ) %>%
  ungroup()

# Margin of Victory (MV)
eleccionesMV <- elecciones %>%
  group_by(codmpio, ano) %>%
  arrange(desc(votos)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 2) %>%
  mutate(
    votosG = max(votos),
    votosS = if_else(n() > 1, nth(votos, 2), 0),
    total_votos = sum(votos, na.rm = TRUE)
  ) %>%
  filter(rank == 1) %>%
  mutate(MV = ((votosG / total_votos) * 100) - ((votosS / total_votos) * 100)) %>%
  select(codmpio, ano, MV)

# Merge WS and MV
eleccionesWS <- eleccionesWS %>%
  left_join(eleccionesMV, by = c("codmpio", "ano"))

# -------------------------------
# 9. Visualize Winner Strength (Optional)
# -------------------------------

eleccionesWS %>%
  filter(departamento %in% c("CASANARE", "META", "ARAUCA", "VICHADA")) %>%
  ggplot(aes(x = WS)) +
  geom_histogram(binwidth = 0.1, fill = "royalblue", color = "black") +
  facet_wrap(~ano) +
  geom_density(position = "identity") +
  labs(
    title = "Winner Strength (WF) by Year",
    x = "Winner Strength (WF)",
    y = "Frequency"
  ) +
  theme_minimal()

# -------------------------------
# 10. Visualize Margin of Victory (Optional)
# -------------------------------

# Histogram of Margin of Victory (MV) by year
eleccionesWS %>%
  filter(departamento %in% c("CASANARE", "META", "ARAUCA", "VICHADA")) %>% 
  ggplot(aes(x = MV)) +
  geom_histogram(binwidth = 5, fill = "royalblue", color = "black") +
  facet_wrap(~ano) +
  geom_density() +
  labs(
    title = "Margin of Victory (MV) by Year",
    x = "Margin of Victory (MV)",
    y = "Frequency"
  ) +
  theme_minimal()

# -------------------------------
# 11. Merge Panel with Elections and Create Treatment Variables
# -------------------------------

panel1 <- panel_clean %>%
  filter(ano %in% c(1999, 2002, 2006, 2010, 2014, 2018)) %>% 
  mutate(anoEle = ano + 1) %>% 
  left_join(eleccionesWS, by = c("codmpio", "anoEle")) %>%
  mutate(
    treatment = ifelse(ano.x > 2011, 1, 0),
    treatment = as.factor(treatment),
    treatment_2 = ifelse(ano.x > 2011, ano.x - 2012, 0)
  ) %>%
  rename(
    anoCEDE = ano.x,
    anoCEDE_Ele = ano.y
  )

# Explanation:
# - `treatment`: Binary variable indicating whether the observation occurs after the 2012 reform.
# - `treatment_2`: Categorical variable indicating the number of elections since the reform (0 if before 2012).

# -------------------------------
# 12. Adjust for Inflation Using GDP Deflator
# -------------------------------

# Normalize GDP deflator to base 100
gdp_deflactor$gdp_deflactor <- gdp_deflactor$gdp_deflactor / 100

# Merge deflator and adjust royalties
panel1 <- panel1 %>%
  left_join(gdp_deflactor, by = c("anoCEDE" = "anoCEDE")) %>%
  mutate(y_cap_regalias_cons = y_cap_regalias / gdp_deflactor) %>%
  select(-gdp_deflactor)

# -------------------------------
# 13. Final Cleaning and Export
# -------------------------------

# Clean variable names
panel1 <- panel1 %>% clean_names()

# Truncate variable names to 32 characters (for Stata compatibility)
panel1 <- panel1 %>%
  rename_with(~ substr(.x, 1, 32))

# Export final dataset
write_dta(panel1, "Data/panel1.dta")
write_csv(panel1, "Data/panel1.csv")
