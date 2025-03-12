# --------------------- #
# IICA Cleaning ------- #
# --------------------- #


# Load packages

pacman::p_load(tidyverse, dplyr, readxl, stringr)

# Load the data
getwd()
IICA2002_2013 <- read_excel("Data/IICA_full.xlsx", sheet = 1)
IICA2014_16 <- read_excel("Data/IICA_full.xlsx", sheet = 2)
IICA2017_2022 <- read_excel("Data/IICA_full.xlsx", sheet = 3)

# Transform IICA 2014-16


#Name transformation

IICA2014_16 <- IICA2014_16 %>%
  mutate(across(contains("_cat"), ~ case_when(
    . == "Bajo" ~ 1,
    . == "Medio bajo" ~ 2,
    . == "Medio" ~ 3,
    . == "Alto" ~ 4,
    . == "Muy alto" ~ 5
  )))


# Pivoting by codmpio (pivot IICA and _cat)

IICA2014_16 %>% 
  pivot_longer(cols = c(`2014`, `2015`, `2016`), names_to = "year", values_to = "IICA") -> IICA2014_16

IICA2014_16 %>%
  pivot_longer(cols = c(`2014_cat`, `2015_cat`, `2016_cat`), names_to = "year_cat", values_to = "IICA_cat") -> IICA2014_16

IICA2014_16 %>% 
  select(-year_cat) -> IICA2014_16

IICA2014_16 <- IICA2014_16 %>% 
  distinct(Municipio, year, .keep_all = TRUE) %>% 
  rename(codmpio = Divipola,
         municipio = Municipio,
         departamento = Depto)


# Clean 2017-2022

IICA2017_2022 <- IICA2017_2022 %>%
  # Renombrar columnas de años automáticamente eliminando ".0"
  rename_with(~ gsub("\\.0$", "", .x), starts_with("20")) %>%
  # Renombrar variables específicas
  rename(
    codmpio = Divipola, 
    departamento = Depto, 
    municipio = Municipio
  ) %>%
  # Renombrar variables de IICA eliminando "IICA "
  rename_with(~ str_replace(.x, "IICA ", ""), starts_with("IICA")) %>%
  # Convertir las categorías en valores numéricos
  mutate(across(contains("Categoría"), ~ case_when(
    . == "Bajo" ~ 1,
    . == "Medio Bajo" ~ 2,
    . == "Medio" ~ 3,
    . == "Alto" ~ 4,
    . == "Muy Alto" ~ 5
  ))) %>%
  # Renombrar columnas de categoría con el formato correcto
  rename_with(~ str_replace_all(.x, "\\s*Categoría", "_cat"), contains("Categoría")) %>%
  # Manejar la columna especial "IICA 2017-2022 Categoría" si existe
  rename_with(~ str_replace(.x, "IICA 2017-2022_cat", "2017_2022_cat"), contains("IICA 2017-2022_cat"))



#Pivot IICA and _cat

IICA2017_2022 %>% 
  pivot_longer(cols = c(`2017`, `2018`, `2019`, `2020`, `2021`, `2022`), names_to = "year", values_to = "IICA") -> IICA2017_2022

IICA2017_2022 %>%
  pivot_longer(cols = c(`2017_cat`, `2018_cat`, `2019_cat`, `2020_cat`, `2021_cat`, `2022_cat`), names_to = "year_cat", values_to = "IICA_cat") -> IICA2017_2022

IICA2017_2022 %>%
  select(-year_cat) -> IICA2017_2022

IICA2017_2022 <- IICA2017_2022 %>%
  distinct(municipio, year, .keep_all = TRUE) %>%  # Elimina duplicados
  select(-`2017-2022`, -`2017-2022_cat`)  # Elimina las columnas

IICA2017_2022 %>% 
  select(-`Divipola #`) -> IICA2017_2022

#Clean 2002 2013

colnames(IICA2002_2013)[5] <- "IICA"
colnames(IICA2002_2013)[6] <- "IICA_cat"
colnames(IICA2002_2013)[1] <- "codmpio"
colnames(IICA2002_2013)[2] <- "departamento"
colnames(IICA2002_2013)[3] <- "municipio"
colnames(IICA2002_2013)[4] <- "year"

#Transform IICA_cat in numbers as the other ones

IICA2002_2013 <- IICA2002_2013 %>%
  mutate(
    IICA_cat = case_when(
      IICA_cat == "Bajo" ~ 1,
      IICA_cat == "Medio Bajo" ~ 2,
      IICA_cat == "Medio" ~ 3,
      IICA_cat == "Alto" ~ 4,
      IICA_cat == "Muy Alto" ~ 5
    )
  )

summary(IICA2002_2013$)

## Join the three datasets by year and codmpio

#Asegurate que todas las variables de codmpio y year sean numericas
IICA2002_2013 <- IICA2002_2013 %>%
  mutate(codmpio = as.numeric(codmpio),
         year = as.numeric(year))
IICA2014_16 <- IICA2014_16 %>%
  mutate(codmpio = as.numeric(codmpio),
         year = as.numeric(year))

IICA2017_2022 <- IICA2017_2022 %>%
  mutate(codmpio = as.numeric(codmpio),
         year = as.numeric(year))

IICA2002_2013 %>%
  bind_rows(IICA2014_16) %>%
  bind_rows(IICA2017_2022) -> IICA




summary(IICA2002_2013$IICA)
summary(IICA2014_16$IICA)
summary(IICA2017_2022$IICA)

# Save the data

write_csv(IICA, "Data/IICA_clean.csv")
