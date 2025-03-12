# ---------------------------------------- #
#  Data Cleaning for Departmental elections 
# ---------------------------------------- #

# Author: Alejandro Castillo Ardila
# Date: 03-05-2025 

# Objective: Replicate DataCleaning for the departamental elections
# dataset

# Load required packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, skimr, haven, rio, readxl, tibble)

# ------------------------------ #
# Load Panel Data (CEDE, 2024)   ####
# ------------------------------ #
panel_bg <- read_dta("Data/PANEL_BUEN_GOBIERNO(2024).dta")
panel_cv <- read_dta("Data/PANEL_CONFLICTO_Y_VIOLENCIA(2024).dta")
panel_cg <- read_dta("Data/PANEL_CARACTERISTICAS_GENERALES(2024).dta")

panel <- panel_bg %>%
  left_join(panel_cv, by = c("codmpio", "ano")) %>%
  left_join(panel_cg, by = c("codmpio", "ano"))

remove(panel_bg, panel_cv, panel_cg) #Remove individual datasets

# Load GDP deflactor for Colombia

# Create the dataframe
gdp_deflactor <- tribble(
  ~country, ~anoCEDE, ~gdp_deflactor,
  "Colombia", 1999, 43.57820361,
  "Colombia", 2000, 48.43860008,
  "Colombia", 2001, 51.59604518,
  "Colombia", 2002, 54.67539548,
  "Colombia", 2003, 58.40918636,
  "Colombia", 2004, 62.66314691,
  "Colombia", 2005, 65.64164917,
  "Colombia", 2006, 69.45389355,
  "Colombia", 2007, 73.06690857,
  "Colombia", 2008, 78.67653493,
  "Colombia", 2009, 81.8741267,
  "Colombia", 2010, 84.98932283,
  "Colombia", 2011, 90.41742377,
  "Colombia", 2012, 93.6875101,
  "Colombia", 2013, 95.47476465,
  "Colombia", 2014, 97.6092294,
  "Colombia", 2015, 100,
  "Colombia", 2016, 105.1483343,
  "Colombia", 2017, 110.5463721,
  "Colombia", 2018, 115.6653099,
  "Colombia", 2019, 120.2949534,
  "Colombia", 2020, 122.0773933,
  "Colombia", 2021, 131.6020096,
  "Colombia", 2022, 151.1667205,
  "Colombia", 2023, 160.7447305
)


# --------------------------------------------------- #
# Clean relevant variables of CEDE data             ####
# --------------------------------------------------- #

panel_clean <- panel %>%  #Dataframe with relevant variables
  select(codmpio, ano, y_total, y_corr, y_transf, y_transf_nal, y_transf_otra,
         g_total, g_corr, g_func, y_cap_regalias, y_cap_transf, y_cap_cofinan,
         y_cap_otros, MDM_igpr, MDM, MDM_tasa_recaudo, indesarrollo_mun,
         SGP_propgeneral_li, SRAanh_regalias_productor, SGR_total,SGR_adirectas,
         SGR_adirectas_hidrocarburos, SRAanh_regalias_productor, homicidios) %>%
  filter(ano %in% c(1995:2018)) #Filter years 1995-2018


# --------------------------------------------------- #
# Load and celandata of Armed Conflict Incidence (DNP) #
# --------------------------------------------------- #

conflict <- read_excel("Data/IICA_Municipal_2002_2013.xlsx", sheet = 2)

#Clean conflict data


#Rename columns for compatibility
colnames(conflict)[c(1,4,5,6)] <- c("codmpio", "ano", "iica", "iica_cat") 


#Select relevant variable and transform variable typess
conflict <- conflict %>%
  select(codmpio, ano, iica, iica_cat) %>% 
  mutate(codmpio = as.numeric(codmpio),
         iica_cat = as.factor(iica_cat))


#--------------------------------------------------- #
# Merge Panel Data with Conflict Data --------------- #
# --------------------------------------------------- #


panel_clean <- panel_clean %>%
  left_join(conflict, by = c("codmpio", "ano")) %>%
  mutate(iica = ifelse(is.na(iica), 0, iica),
         iica_cat = ifelse(is.na(iica), 0, iica_cat))

#--------------------------------------------------- #
# Load and clean data of Departmental Elections ---- #
# --------------------------------------------------- #

elecciones2019 <- read_dta("Data/Gobernacion/2019_gobernacion.dta")
elecciones2015 <- read_dta("Data/Gobernacion/2015_gobernacion.dta")
elecciones2011 <- read_dta("Data/Gobernacion/2011_gobernacion.dta")
elecciones2007 <- read_dta("Data/Gobernacion/2007_gobernacion.dta")
elecciones2003 <- read_dta("Data/Gobernacion/2003_gobernacion.dta")
elecciones2000 <- read_dta("Data/Gobernacion/2000_gobernacion.dta")

elecciones2019 <- elecciones2019 %>% select(-circunscripcion)
elecciones2015 <- elecciones2015 %>% select(-circunscripcion)
elecciones2011 <- elecciones2011 %>% select(-circunscripcion)
elecciones2007 <- elecciones2007 %>% select(-circunscripcion)
elecciones2003 <- elecciones2003 %>% select(-circunscripcion)
elecciones2000 <- elecciones2000 %>% select(-circunscripcion)
elecciones <- bind_rows(elecciones2019,
                        elecciones2015,
                        elecciones2011,
                        elecciones2007,
                        elecciones2003,
                        elecciones2000)
remove(elecciones2019, elecciones2015, elecciones2011, elecciones2007,
       elecciones2003, elecciones2000)


#Winner Strenghts

eleccionesWS <- elecciones %>%
  filter(!nombres %in% c("TARJETAS NO MARCADAS", 
                         "VOTOS NULOS", 
                         "VOTOS EN BLANCO")) %>%
  group_by(ano, codmpio) %>%
  mutate(ganador = votos == max(votos, na.rm = TRUE)) %>%  # Marca al candidato más votado
  mutate(votosG = max(votos, na.rm = TRUE),  # Asigna el máximo número de votos
         total_votos = sum(votos, na.rm = TRUE)) %>%
  filter(ganador) %>%  # Filtra solo a los ganadores
  mutate(WS = votosG / total_votos,
         anoEle = ano) %>%
  ungroup()


#Margin of victory

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

# Join WS and MV

eleccionesWS <- eleccionesWS %>% 
  left_join(eleccionesMV, by = c("codmpio", "ano"))


# --------------------------------------------------- #
# Histogram of WS ---- #
# --------------------------------------------------- #

eleccionesWS %>% 
  ggplot(aes(x = WS)) +
  geom_histogram(binwidth = 0.05, fill = "royalblue", color = "black") +
  facet_wrap(~ano) +
  geom_density(position = "identity") +
  labs(title = "Winner Strength (WS) by Year", x = "Winner Strength (WF)", y = "Frequency") +
  theme_minimal()


# --------------------------------------------------- #
# Histogram of MV ---- #
# --------------------------------------------------- #

eleccionesWS %>% 
  ggplot(aes(x = MV)) +
  geom_histogram(binwidth = 5, fill = "royalblue", color = "black") +
  facet_wrap(~ano) +
  geom_density(position = "identity") +
  labs(title = "Margin of Victory (MV) by Year", x = "Margin of Victory (%)", y = "Frequency") +
  theme_minimal()


# --------------------------------------------------- #
# Histogram of WS for Casanare, Meta and Arauca #
# --------------------------------------------------- #

eleccionesWS %>%
  filter(departamento %in% c("CASANARE", "META", "ARAUCA")) %>%
  ggplot(aes(x = WS)) +
  geom_histogram(binwidth = 0.05, fill = "royalblue", color = "black") +
  facet_wrap(~ano) +
  labs(title = "Winner Strength (WS) by Year", x = "Winner Strength (WF)", y = "Frequency") +
  theme_minimal()


# ------------------------------------------------------------------ #
# Merge Panel Data with Elections and create treatment variable ####
# ------------------------------------------------------------------ #
panel2 <- panel_clean %>%
  filter(ano %in% c(1999, 2002, 2006, 2010, 2014, 2018)) %>% 
  mutate(anoEle = ano + 1) %>% 
  left_join(eleccionesWS, by = c("codmpio", "anoEle")) %>%
  mutate(treatment = ifelse(ano.x > 2012, 1, 0)) %>%
  rename(anoCEDE = ano.x, anoCEDE_Ele = ano.y) %>% 
  mutate(treatment = as.factor(treatment))

# The treatment variable is wether the observation of y_cap_regalias occurs
# after 2012. The following code creates a variable that assings that treatment

# Clean GDP deflactor

gdp_deflactor$gdp_deflactor <- gdp_deflactor$gdp_deflactor / 100

# Merge GDP deflactor with panel 1 by year

panel2 <- panel2 %>%
  left_join(gdp_deflactor, by = c("anoCEDE" = "anoCEDE"))

panel2 <- panel2 %>% 
  mutate(y_cap_regalias_cons = y_cap_regalias / gdp_deflactor) %>% 
  select(-gdp_deflactor)


#Export relevant dataset - Panel 1 as csv
write_dta(panel2, "Data/panel2.csv")

# Put missing labels andExport relevant dataset as DTA

write_dta(panel2, "Data/panel2.dta")



eleccionesWS %>%
  filter(departamento == "CASANARE",
         ano > 2010) %>% 
  select(municipio, nombres, primer_apellido, codigo_partido,
         votos, WS) %>%
  view("CasanareTablas")
  