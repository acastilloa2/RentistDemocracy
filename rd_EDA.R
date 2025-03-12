### -----------------------------------------------------
#                 EXPLORATORY DATA ANALYSIS             #
#                                                       #
#     Rentist Democracy: Distributive Institutions      #
#    and Political Competition in Colombian Llanos      #
#    Author:  Alejandro Castillo Ardila                 #
#    February 17th, 2024                                #
### -----------------------------------------------------

pacman::p_load(tidyverse, skimr, haven, rio, fixest)


# ------------------------------ #
# Load data from data cleaning  ####
# ------------------------------ #

panel1 <- read_csv("Data/panel1.csv")

# Scatter plot of Winner Strength (WF) vs. MDM
panel1 %>%
  mutate(MDM_1 = MDM + 0.01) %>% 
  ggplot(aes(x = WS, y = MDM_1)) +
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre fuerza del ganador y medición de desempeño municipal",
       x = "Fuerza del ganador (t1)", y = "MDM (t0)", caption = "Fuente: Cálculos propios con base en Panel CEDE (2024)"
       ) +
  theme_bw()

#Scatter plot of MV vs. MDM

panel1 %>%
  ggplot(aes(x = MV/100, y = MDM)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el margen de victoria y la medición de desempeño municipal",
       x = "Margen de victoria (2019)", y = "MDM (2018") +
  theme_bw()

# ----------------------------------------- #
# Scatter plot of y_cap_regalias and WS #####
# ------------------------------------------#

panel1 %>% 
  #filter(anoEle == 2019) %>% 
  mutate(y_cap_regalias_1 = y_cap_regalias + 1) %>% 
  ggplot(aes(x = y_cap_regalias_1, y = WS)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre los ingresos por regalías y la fuerza del ganador",
       x = "Ingresos por regalías (pesos corrientes, t0)",
       y = "Fuerza del ganador (t1)") +
  theme_minimal()

#Now make it with logartihm of y_cap_regalias
panel1 %>% 
  #filter(anoEle == 2019) %>% 
  mutate(y_cap_regalias = ifelse(is.na(y_cap_regalias) | y_cap_regalias <= 0, 1, y_cap_regalias),  # Evita log(0) o log(NA)
         WS = ifelse(is.na(WS), 0, WS)) %>%  # Reemplaza NA en WS por 0
  ggplot(aes(x = log(y_cap_regalias), y = WS)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el logaritmo de los ingresos por regalías y la fuerza del ganador",
       x = "Logaritmo de los ingresos por regalías (t0)",
       y = "Fuerza del ganador (t1)") +
  theme_minimal()

# Scatter plot eliminating NAs and 0s

panel1 %>% 
  filter(!is.na(y_cap_regalias) & y_cap_regalias > 0 & !is.na(WS)) %>%  # Elimina NA y valores no válidos
  ggplot(aes(x = log(y_cap_regalias), y = WS)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el logaritmo de los ingresos por regalías y la fuerza del ganador",
       x = "Logaritmo de los ingresos por regalías (t0)",
       y = "Fuerza del ganador (t1)") +
  theme_minimal()


#Sumando + 1 y excluyendo NA

panel1 %>% 
  filter(!is.na(WS)) %>%  # Excluye solo los NA en WS
  ggplot(aes(x = log(y_cap_regalias + 1), y = WS)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el logaritmo (+1) de los ingresos por regalías y la fuerza del ganador",
       x = "Logaritmo (+1) de los ingresos por regalías (t0)",
       y = "Fuerza del ganador (t1)") +
  theme_minimal()


#Clean data

#Regression A: Fixed effects of municipality and treatment

regressionA <- feols(WS ~ log(y_cap_regalias + 1) + treatment, data = panel1)

summary(regressionA)


#Regressión B: Fixed effects of municipality of MV

regressionB <-feols(MV ~ log(y_cap_regalias + 1) +  | codmpio, data = panel1)

summary(regressionB)
# ------------------------------ #
# Scatter plot of y_cap_regalias and MV####
# ------------------------------ #

panel1 %>% 
  ggplot(aes(x = y_cap_regalias, y = MV/100)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el logaritmo de los ingresos por regalías y el margen de victoria",
       x = "Ingresos por regalías (2018)", y = "Margen de victoria") +
  theme_minimal()

#Now make it with logartihm of y_cap_regalias

panel2 %>% 
  ggplot(aes(x = log(y_cap_regalias), y = MV/100)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "log(y_cap_regalias) vs. Margin of Victory", x = "log(y_cap_regalias)", y = "Margin of Victory (MV)") +
  theme_minimal()
