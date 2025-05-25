# ============================================================
# Replication Script: Rentist Democracy – IICA Data Cleaning
# Author: Alejandro Castillo Ardila
# Contact: a.castilloa2@uniandes.edu.co
# Date: May 21, 2025
# Description: This script cleans and harmonizes IICA data from
# 2002 to 2022 across three different formats and exports a unified dataset.
# ============================================================

# -------------------------------
# 1. Load Required Packages
# -------------------------------

pacman::p_load(tidyverse, dplyr, readxl, stringr, haven)

# -------------------------------
# 2. Load Raw IICA Data
# -------------------------------

IICA2002_2013 <- read_excel("Data/IICA_full.xlsx", sheet = 1)
IICA2014_16   <- read_excel("Data/IICA_full.xlsx", sheet = 2)
IICA2017_2022 <- read_excel("Data/IICA_full.xlsx", sheet = 3)

# -------------------------------
# 3. Clean IICA 2014–2016
# -------------------------------

# Convert categorical levels to numeric
IICA2014_16 <- IICA2014_16 %>%
  mutate(across(contains("_cat"), ~ case_when(
    . == "Bajo" ~ 1,
    . == "Medio bajo" ~ 2,
    . == "Medio" ~ 3,
    . == "Alto" ~ 4,
    . == "Muy alto" ~ 5
  )))

# Pivot IICA values and categories
IICA2014_16 <- IICA2014_16 %>%
  pivot_longer(cols = c(`2014`, `2015`, `2016`), names_to = "year", values_to = "IICA") %>%
  pivot_longer(cols = c(`2014_cat`, `2015_cat`, `2016_cat`), names_to = "year_cat", values_to = "IICA_cat") %>%
  select(-year_cat) %>%
  distinct(Municipio, year, .keep_all = TRUE) %>%
  rename(
    codmpio = Divipola,
    municipio = Municipio,
    departamento = Depto
  )

# -------------------------------
# 4. Clean IICA 2017–2022
# -------------------------------

IICA2017_2022 <- IICA2017_2022 %>%
  rename_with(~ gsub("\\.0$", "", .x), starts_with("20")) %>%
  rename(
    codmpio = Divipola,
    departamento = Depto,
    municipio = Municipio
  ) %>%
  rename_with(~ str_replace(.x, "IICA ", ""), starts_with("IICA")) %>%
  mutate(across(contains("Categoría"), ~ case_when(
    . == "Bajo" ~ 1,
    . == "Medio Bajo" ~ 2,
    . == "Medio" ~ 3,
    . == "Alto" ~ 4,
    . == "Muy Alto" ~ 5
  ))) %>%
  rename_with(~ str_replace_all(.x, "\\s*Categoría", "_cat"), contains("Categoría")) %>%
  rename_with(~ str_replace(.x, "IICA 2017-2022_cat", "2017_2022_cat"), contains("IICA 2017-2022_cat"))

# Pivot values and categories
IICA2017_2022 <- IICA2017_2022 %>%
  pivot_longer(cols = c(`2017`, `2018`, `2019`, `2020`, `2021`, `2022`), names_to = "year", values_to = "IICA") %>%
  pivot_longer(cols = c(`2017_cat`, `2018_cat`, `2019_cat`, `2020_cat`, `2021_cat`, `2022_cat`), names_to = "year_cat", values_to = "IICA_cat") %>%
  select(-year_cat, -`2017-2022_cat`, -`Divipola #`) %>%
  distinct(municipio, year, .keep_all = TRUE)

# -------------------------------
# 5. Clean IICA 2002–2013
# -------------------------------

colnames(IICA2002_2013)[1:6] <- c("codmpio", "departamento", "municipio", "year", "IICA", "IICA_cat")

IICA2002_2013 <- IICA2002_2013 %>%
  mutate(IICA_cat = case_when(
    IICA_cat == "Bajo" ~ 1,
    IICA_cat == "Medio Bajo" ~ 2,
    IICA_cat == "Medio" ~ 3,
    IICA_cat == "Alto" ~ 4,
    IICA_cat == "Muy Alto" ~ 5
  ))

# -------------------------------
# 6. Merge All IICA Datasets
# -------------------------------

# Ensure consistent types
IICA2002_2013 <- IICA2002_2013 %>%
  mutate(codmpio = as.numeric(codmpio), year = as.numeric(year))
IICA2014_16 <- IICA2014_16 %>%
  mutate(codmpio = as.numeric(codmpio), year = as.numeric(year))
IICA2017_2022 <- IICA2017_2022 %>%
  mutate(codmpio = as.numeric(codmpio), year = as.numeric(year))

# Combine all years
IICA <- bind_rows(IICA2002_2013, IICA2014_16, IICA2017_2022)

# Remove unnecessary columns
IICA <- IICA %>% select(-`2017-2022`)

# -------------------------------
# 7. Export Cleaned Dataset
# -------------------------------

write_csv(IICA, "Data/IICA_clean.csv")
write_dta(IICA, "Data/IICA_clean.dta")

# -------------------------------
# 8. Summary Table (Optional)
# -------------------------------
