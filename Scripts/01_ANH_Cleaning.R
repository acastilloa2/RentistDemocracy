# ============================================================
# Replication Script: Rentist Democracy – ANH Data Cleaning
# Author: Alejandro Castillo Ardila
# Contact: a.castilloa2@uniandes.edu.co
# Date: May 21, 2025
# Description: This script cleans and reshapes data from the
# Agencia Nacional de Hidrocarburos (ANH) for use in the analysis.
# ============================================================

# -------------------------------
# 1. Load Required Packages
# -------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, ggplot2, skimr, readxl, haven)

# -------------------------------
# 2. Load Raw ANH Data
# -------------------------------

anh_data <- read_excel("Data/ANH/1779158_ANEXO.xlsx", skip = 1)

# -------------------------------
# 3. Clean and Reshape Data
# -------------------------------

clean_anh_data <- anh_data %>% 
  select(Departamento, Municipio, `Codigo DANE`, TipoHdr, `2012`) %>%
  rename(valor = `2012`) %>% 
  group_by(Departamento, Municipio, `Codigo DANE`, TipoHdr) %>% 
  summarise(valor = first(valor), .groups = "drop") %>% 
  pivot_wider(names_from = TipoHdr, values_from = valor) %>%
  select(Departamento, Municipio, `Codigo DANE`, CRUDO, GAS) %>%
  mutate(
    CRUDO = as.numeric(CRUDO),
    GAS = as.numeric(GAS),
    
    # Binary indicators
    binary_CRUDO = ifelse(!is.na(CRUDO), 1, 0),
    binary_GAS = ifelse(!is.na(GAS), 1, 0),
    
    # Quartiles
    cuartil_CRUDO = ntile(CRUDO, 4),
    cuartil_GAS = ntile(GAS, 4),
    
    # Quintiles
    quintil_CRUDO = ntile(CRUDO, 5),
    quintil_GAS = ntile(GAS, 5),
    
    # Deciles
    decil_CRUDO = ntile(CRUDO, 10),
    decil_GAS = ntile(GAS, 10),
    
    # Percentiles (100 groups)
    percentil_CRUDO = ntile(CRUDO, 100),
    percentil_GAS = ntile(GAS, 100)
  )

# -------------------------------
# 4. Export Cleaned Dataset
# -------------------------------

anh_export <- clean_anh_data %>% 
  select(`Codigo DANE`, CRUDO, GAS, 
         binary_CRUDO, binary_GAS,
         cuartil_CRUDO, cuartil_GAS,
         quintil_CRUDO, quintil_GAS,
         decil_CRUDO, decil_GAS,
         percentil_CRUDO, percentil_GAS) %>% 
  rename(codmpio = `Codigo DANE`)

write_dta(
  anh_export,
  "Data/ANH/anh_clean.dta",
  version = 12,
  label = "ANH data cleaned and reshaped"
)
