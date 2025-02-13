### -----------------------------------------------------
#    Rentist Democracy: Distributive Institutions       #
#    and Political Competition in Colombian Llanos      #
#    Author:  Alejandro Castillo Ardila                 #
#    February 12th, 2024                                #
### -----------------------------------------------------



# Load required packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, skirm, haven, rio)

# ------------------------------ #
# Load Panel Data (CEDE, 2024)   ####
# ------------------------------ #
panel <- read_dta("Data/PANEL_BUEN_GOBIERNO(2024).dta")

# Summary statistics of MDM variable
summary(panel$MDM)

# Histogram of MDM
panel %>% 
  ggplot(aes(x = MDM)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of MDM", x = "Medición de Desempeño Municipal (MDM)", y = "Count") +
  theme_minimal()

# ------------------------------ #
# Load Elections Data (2019)     ####
# ------------------------------ #
elecciones2019 <- read_dta("Data/2019_alcaldia.dta")

# Create competitiveness indicator (Winner Strength - WF)
elecciones20191 <- elecciones2019 %>%
  filter(!nombres %in% c("TARJETAS NO MARCADAS", 
                         "VOTOS NULOS", 
                         "VOTOS EN BLANCO")) %>%
  group_by(codmpio) %>%
  mutate(votosG = votos[elegido == 1],  # Extract winner's votes
         total_votos = sum(votos, na.rm = TRUE)) %>%  # Sum total votes
  filter(elegido == 1) %>% 
  mutate(WF = votosG / total_votos) %>%  # Winner Strength
  ungroup()  # Ensure no grouped operations remain

# Histogram of Winner Strength (WF)
hist(elecciones20191$WF, 
     main = "Histogram of Winner Strength", 
     xlab = "Winner Strength", 
     col = "lightblue", 
     border = "black")


# ------------------------------- #
# Create Margin of Victory Measure #####
# ------------------------------- #

elecciones20192 <- elecciones2019 %>%
  filter(!nombres %in% c("TARJETAS NO MARCADAS", 
                         "VOTOS NULOS", 
                         "VOTOS EN BLANCO")) %>% 
  group_by(municipio) %>% 
  arrange(desc(votos)) %>%
  mutate(rank = row_number()) %>% 
  arrange(municipio) %>% #Arragne dataset by municipality
  group_by(municipio) %>% #Regroup by municipality
  filter(rank <= 2) %>%    # Keep top 2 candidates
  mutate(
    votosG = first(votos),  # First-place votes
    votosS = nth(votos, 2, default = 0), #Second place votes
    total_votos = sum(votos), na.rm = TRUE) %>%
  filter(rank == 1) %>%  # Keep only the winner's row
  mutate(MV = ((votosG / total_votos) * 100) - ((votosS / total_votos) * 100)) %>%  # Margin of Victory in percentage


# ------------------------------ #
# Merge Panel Data with Elections####
# ------------------------------ #
panel1 <- panel %>% 
  filter(ano == 2018) %>%
  left_join(elecciones20191, by = "codmpio")

panel2 <- panel %>% 
filter(ano == 2018) %>% 
  left_join(elecciones20192, by = "codmpio")

# Scatter plot of Winner Strength (WF) vs. MDM
panel1 %>% 
  ggplot(aes(x = WF, y = MDM)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Winner Strength vs. MDM", x = "Winner Strength (WF) 2019", y = "MDM 2018") +
  theme_minimal()

#Scatter plot of MV vs. MDM
panel2 %>%
  ggplot(aes(x = MV/100, y = MDM)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Margin of Victory vs. MDM", x = "Margin of Victory (MV) 2019", y = "MDM 2018") +
  theme_minimal()
