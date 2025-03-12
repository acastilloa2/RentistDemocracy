### -----------------------------------------------------
#                  DATA ANALYSIS             #
#                                                       #
#     Rentist Democracy: Distributive Institutions      #
#    and Political Competition in Colombian Llanos      #
#    Author:  Alejandro Castillo Ardila                 #
#    March 3rd of 2025                                  #
### -----------------------------------------------------

pacman::p_load(tidyverse, skimr, haven, rio, fixest,
               modelsummary, sandwich, lmtest)


# ------------------------------ #
# Load data from data cleaning  ####
# ------------------------------ #

panel2 <- read_dta("Data/panel2.dta")

# Scatter plot of Winner Strength (WF) vs. MDM
panel2 %>%
  mutate(MDM_1 = MDM + 0.01) %>% 
  ggplot(aes(x = WS, y = MDM_1)) +
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre fuerza del ganador y medición de desempeño municipal",
       x = "Fuerza del ganador (t1)", y = "MDM (t0)", caption = "Fuente: Cálculos propios con base en Panel CEDE (2024)"
  ) +
  theme_bw()

#Scatter plot of MV vs. MDM

panel2 %>%
  ggplot(aes(x = MV/100, y = MDM)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el margen de victoria y la medición de desempeño municipal",
       x = "Margen de victoria (2019)", y = "MDM (2018") +
  theme_bw()

# ----------------------------------------- #
# Scatter plot of y_cap_regalias and WS #####
# ------------------------------------------#

panel2 %>% 
  #filter(anoEle == 2019) %>% 
  mutate(y_cap_regalias_1 = y_cap_regalias_cons + 1) %>% 
  ggplot(aes(x = y_cap_regalias_1, y = WS)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre los ingresos por regalías y la fuerza del ganador",
       x = "Ingresos por regalías (pesos corrientes, t0)",
       y = "Fuerza del ganador (t1)") +
  theme_minimal()

#Now make it with logartihm of y_cap_regalias
panel2 %>% 
  #filter(anoEle == 2019) %>% 
  mutate(y_cap_regalias = ifelse(is.na(y_cap_regalias_cons) | y_cap_regalias_cons <= 0, 1, y_cap_regalias_cons),  # Evita log(0) o log(NA)
         WS = ifelse(is.na(WS), 0, WS)) %>%  # Reemplaza NA en WS por 0
  ggplot(aes(x = log(y_cap_regalias_cons), y = WS)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el logaritmo de los ingresos por regalías y la fuerza del ganador",
       x = "Logaritmo de los ingresos por regalías (t0)",
       y = "Fuerza del ganador (t1)") +
  theme_minimal()

# Scatter plot eliminating NAs and 0s

panel2 %>% 
  filter(!is.na(y_cap_regalias_cons) & y_cap_regalias_cons > 0 & !is.na(WS)) %>%  # Elimina NA y valores no válidos
  ggplot(aes(x = log(y_cap_regalias_cons), y = WS)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el logaritmo de los ingresos por regalías y la fuerza del ganador",
       x = "Logaritmo de los ingresos por regalías (t0)",
       y = "Fuerza del ganador (t1)") +
  theme_minimal()


#Sumando + 1 y excluyendo NA

panel2 %>% 
  filter(!is.na(WS)) %>%  # Excluye solo los NA en WS
  ggplot(aes(x = log(y_cap_regalias_cons + 1), y = WS)) + 
  geom_point(color = "gray") + 
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlación entre el logaritmo (+1) de los ingresos por regalías y la fuerza del ganador",
       x = "Logaritmo (+1) de los ingresos por regalías (t0)",
       y = "Fuerza del ganador (t1)") +
  theme_minimal()


# Regressions

# Limit regression to municipalities with regalias > 0
panel2_1 <- panel2 %>% 
  filter(y_cap_regalias_cons > 1 ,
         !is.na(y_cap_regalias_cons))


#Replace al NAs by 0 in y_cap_regalias_cons in panel 1

panel2_2 <- panel2 %>% 
  mutate(y_cap_regalias_cons = ifelse(is.na(y_cap_regalias_cons), 0, y_cap_regalias_cons))
#Regression A: Simple regression without control

regressionA <- lm(WS ~ log(y_cap_regalias_cons) + treatment + log(y_cap_regalias_cons):treatment, data = panel2_1)

summary(regressionA)

coeftest(regressionA, vcov = vcovCL(regressionA, cluster = ~ codmpio))


#Regression B: Simple regression without control for Casanare, Meta and Vichada

regressionA1 <- panel2_1 %>% 
  filter(departamento %in% c("CASANARE", "META", "VICHADA")) %>% 
  lm(WS ~ log(y_cap_regalias_cons) + treatment + log(y_cap_regalias_cons):treatment, data = .)

summary(regressionA1)

coeftest(regressionA1, vcov = vcovCL(regressionA1, cluster = ~ codmpio))
