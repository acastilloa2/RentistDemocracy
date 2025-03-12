### -----------------------------------------------------
#                     DATA CLEANING                     #
#                                                       #
#     Rentist Democracy: Distributive Institutions      #
#    and Political Competition in Colombian Llanos      #
#    Author:  Alejandro Castillo Ardila                 #
#    February 12th, 2024                                #
### -----------------------------------------------------

# Load required packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, skimr, haven, rio, readxl, tibble,
               janitor)

# ------------------------------ #
# Load Panel Data (CEDE, 2024)   ####
# ------------------------------ #
panel_bg <- read_dta("Data/PANEL_BUEN_GOBIERNO(2024).dta")
panel_sys <- read_dta("Data/PANEL_SALUD_Y_SERVICIOS.dta")
panel_cg <- read_dta("Data/PANEL_CARACTERISTICAS_GENERALES(2024).dta")

panel <- panel_bg %>%
  left_join(panel_sys, by = c("codmpio", "ano")) %>%
  left_join(panel_cg, by = c("codmpio", "ano"))

remove(panel_bg, panel_sys, panel_cg) #Remove individual datasets

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

panel_clean <- panel


# --------------------------------------------------- #
# Load and celandata of Armed Conflict Incidence (DNP) #
# --------------------------------------------------- #

conflict <- read_csv("Data/IICA_clean.csv")

conflict %>% 
  rename(ano = year,
         iica = IICA,
         iica_cat = IICA_cat) -> conflict
conflict %>% 
  select(codmpio, ano, iica,iica_cat ) -> conflict

conflict <- conflict %>%
  filter(ano %in% c(1999, 2002, 2006, 2010, 2014, 2018)) %>%
  filter(iica != 0)



#--------------------------------------------------- #
# Merge Panel Data with Conflict Data --------------- #
# --------------------------------------------------- #


panel_clean <- panel_clean %>%
  left_join(conflict, by = c("codmpio", "ano")) %>%
  mutate(iica = ifelse(is.na(iica), 0, iica),
         iica_cat = ifelse(is.na(iica), 0, iica_cat))


# ------------------------------ #
# Load Elections Data (2019) and before   ####
# ------------------------------ #
elecciones2019 <- read_dta("Data/2019_alcaldia.dta")
elecciones2015 <- read_dta("Data/2015_alcaldia.dta")
elecciones2011 <- read_dta("Data/2011_alcaldia.dta")
elecciones2007 <- read_dta("Data/2007_alcaldia.dta")
elecciones2003 <- read_dta("Data/2003_alcaldia.dta")
elecciones2000 <- read_dta("Data/2000_alcaldia.dta")

#Merge all sets, which have the same variables. First drop circunscripción from all datasets
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
# --------------------------------------------------- #
# Create Measures of Electoral Competitiveness ####
# --------------------------------------------------- #

# Winner Strenght

eleccionesWS <- elecciones %>%
  filter(!nombres %in% c("TARJETAS NO MARCADAS", 
                         "VOTOS NULOS", 
                         "VOTOS EN BLANCO")) %>%
  group_by(ano, codmpio) %>%
  filter(sum(elegido == 1) == 1) %>% # Filtrar grupos con un ganador
  mutate(votosG = votos[elegido == 1],
         total_votos = sum(votos, na.rm = TRUE)) %>%
  filter(elegido == 1) %>%
  mutate(WS = votosG / total_votos) %>% 
  filter(elegido == 1) %>% # Winner Strength (WF)
  mutate(anoEle = ano) %>% 
  ungroup()


# Calculate margin of victory
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

view(eleccionesMV)

# Left Join winner strenght and margin of victory by ano and codmpio

eleccionesWS <- eleccionesWS %>% 
  left_join(eleccionesMV, by = c("codmpio", "ano"))

# Histogram of Winner Strength (WF) with facet-wrap by year

eleccionesWS %>%
  filter(departamento %in% c("CASANARE", "META", "ARAUCA", "VICHADA")) %>% 
  ggplot(aes(x = WS)) +
  geom_histogram(binwidth = 0.1, fill = "royalblue", color = "black") +
  facet_wrap(~ano) +
  geom_density(position = "identity") +
  labs(title = "Winner Strength (WF) by Year", x = "Winner Strength (WF)", y = "Frequency") +
  theme_minimal()

#Histogram of Margin of Victory (MV) with facet-wrap by year

eleccionesWS %>%
  filter(departamento %in% c("CASANARE", "META", "ARAUCA", "VICHADA")) %>% 
  ggplot(aes(x = MV)) +
  geom_histogram(binwidth = 5, fill = "royalblue", color = "black") +
  facet_wrap(~ano) +
  geom_density() +
  labs(title = "Margin of Victory (MV) by Year", x = "Margin of Victory (MV)", y = "Frequency") +
  theme_minimal()

# ------------------------------------------------------------------ #
# Merge Panel Data with Elections and create treatment variable ####
# ------------------------------------------------------------------ #
panel1 <- panel_clean %>%
  filter(ano %in% c(1999, 2002, 2006, 2010, 2014, 2018)) %>% 
  mutate(anoEle = ano + 1) %>% 
  left_join(eleccionesWS, by = c("codmpio", "anoEle")) %>%
  mutate(treatment = ifelse(ano.x > 2012, 1, 0)) %>%
  rename(anoCEDE = ano.x, anoCEDE_Ele = ano.y) %>% 
  mutate(treatment = as.factor(treatment)) %>% 
  mutate(treatment_2 = ifelse(anoCEDE > 2012, anoCEDE - 2012, 0))

# The treatment variable is wether the observation of y_cap_regalias occurs
# after 2012. The following code creates a variable that assings that treatment

#EXPLAIN TREATMENT 2

# Clean GDP deflactor

gdp_deflactor$gdp_deflactor <- gdp_deflactor$gdp_deflactor / 100

# Merge GDP deflactor with panel 1 by year

panel1 <- panel1 %>%
  left_join(gdp_deflactor, by = c("anoCEDE" = "anoCEDE"))

panel1 <- panel1 %>% 
  mutate(y_cap_regalias_cons = y_cap_regalias / gdp_deflactor) %>% 
  select(-gdp_deflactor)

panel1 <- panel1 %>% clean_names()

panel1 <- panel1 %>%
  rename_with(~ substr(.x, 1, 32))  # Limita los nombres a 32 caracteres

write_dta(panel1, "Data/panel1.dta")


#Export relevant dataset - Panel 1 as csv
write_dta(panel1, "Data/panel1.csv")


#Histogram of IICA by year

panel1 %>% 
  select(ano_cede, ano_ele, iica, y_cap_regalias_cons) %>%
  view()

