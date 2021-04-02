### 
# para pasar de archivos txt a conjunto de archivos .csv
#  en principio aplicable a la base de apertura de asambleas legislativas, ver si a otras

# librerias

library(tidyverse)
library(stringr)
library(lubridate)
library(janitor)

# primero prepararemos las bases raw de acuerdo con la estructura particular de cada base
# al fina las almacenaremos todas como .csvs

### 1 datos de apertura de sesiones legislativas #####


txts_apertura <- list.files(pattern = ".txt$", path = "original_data/apertura/") 
mds_cfk <- list.files(pattern = ".md$",path = "original_data/cfk/") 

setwd("original_data/apertura/") 

raw_apertura <- map_df(txts_apertura, ~ tibble(discurso = read_file(.x)) %>%
                      mutate(filename = basename(.x))) %>% 
  transmute(discurso = discurso, 
            presidente = str_sub(filename, 6, nchar(filename)-4), 
            año = as.numeric(str_sub(filename, 1, 4)),
            fuente = "apertura",
            fecha = NA,
            link = as.factor(NA)) %>% 
  mutate(titulo = paste0("Discurso de apertura de sesiones legislativas del período ", año))
  
setwd("../../")
## 2. bases presidenciales ####


## 2 A CFK 

# primer paso: lectura de archivos en un unico tibble

setwd("original_data/cfk/")

raw_cfk <- map_df(mds_cfk, ~ tibble(discurso = read_file(.x)) %>%
                         mutate(filename = basename(.x))) %>% 
  # agrego variables de id
  transmute(discurso = discurso, 
            presidente = "cristina_fernandez_de_kirchner", 
            fecha = as.Date(str_sub(filename, 6, 15)),
            fuente = "repo_mg",
            año = year(fecha),
            link = as.factor(NA))

setwd("../../")

# segundo paso: identificación del título a partir del cuerpo del texto

raw_cfk <- raw_cfk %>%  
  separate(
  .,
  discurso,
  into = c("titulo", "discurso"),
  sep = "--------",
  remove = TRUE,
  convert = FALSE,
  extra = "merge")

## B NK

nk_completos <- read.csv("original_data/nk/discursos_nk_completo.csv", encoding = "UTF-8")

raw_nk <- nk_completos %>% 
  mutate(fecha = stringr::word(as.character(nk_completos$texto), sep = "[[:upper:]]"),
         discurso = str_replace(texto, as.character(titulo), "")) %>% 
  mutate(discurso = str_replace(discurso, fecha, ""),
         presidente = "nestor_kirchner",
         fuente = "web_cfk",
         titulo = as.character(titulo)) %>% 
  select(-texto) %>% 
  rename(link = urls_nk) %>% 
  mutate(fecha = str_replace(fecha, 'enero', '01-'),
         fecha = str_replace(fecha, 'febrero', '02-'),
         fecha = str_replace(fecha, 'marzo', '03-'),
         fecha = str_replace(fecha, 'abril', '04-'),
         fecha = str_replace(fecha, 'mayo', '05-'),
         fecha = str_replace(fecha, 'junio', '06-'),
         fecha = str_replace(fecha, 'julio', '07-'),
         fecha = str_replace(fecha, 'agosto', '08-'),
         fecha = str_replace(fecha, 'septiembre', '09-'),
         fecha = str_replace(fecha, 'octubre', '10-'),
         fecha = str_replace(fecha, 'nviembre', '11-'),
         fecha = str_replace(fecha, 'diciembre', '12-'),
         fecha = str_replace(fecha, ', ', '-'),
         fecha = str_remove(fecha, ' '),
         fecha = lubridate::mdy(fecha)) %>% 
  mutate(año = lubridate::year(fecha))

## C ROSADA

af_completos <- read.csv("original_data/rosada/afernandez.csv", encoding = "UTF-8")

macri_completos <- read.csv("original_data/rosada/macri.csv", encoding = "UTF-8") 

raw_af <- af_completos %>% 
  mutate(fecha = as.Date(fecha),
         fuente = "rosada",
         presidente = "alberto_fernandez",
         año = year(fecha), 
         discurso = as.character(texto),
         titulo = as.character(titulo)) %>% 
  select(-texto)

raw_mm <- macri_completos %>% 
  mutate(fecha = as.Date(fecha),
         fuente = "rosada",
         presidente = "mauricio_macri",
         año = year(fecha), 
         discurso = as.character(texto),
         titulo = as.character(titulo)) %>% 
  select(-texto)


## Raul Alfonsin


ra_original  <- read.csv("original_data/ra/ra_data_tibble.csv", encoding = "UTF-8")

raw_ra  <- ra_original %>% 
  mutate(fecha = as.character(fecha),
         fecha = lubridate::dmy(fecha),
         #fecha = str_replace_all(as.character(fecha),".","-"),
         #año = str_sub(fecha, -4,-1),
         año = lubridate::year(fecha),
         fecha = as.factor(fecha),
         presidente = "raul_alfonsin",
         fuente = "web_ra",
         discurso = str_trim(texto)) %>% 
  rename(link = enlace) %>% 
  select(-texto)

str(ra_original)
# comparo bases antes de unirlas

janitor::compare_df_cols(
  raw_af, raw_mm, raw_nk,raw_apertura, raw_cfk, raw_ra,
  return = "all",
  bind_method = "rbind")

# todo listo, uniendo bases

all_raw <- rbind(raw_af, raw_mm, raw_nk, raw_apertura, raw_cfk, raw_ra) %>% 
  mutate(id = seq(1, nrow(.), 1))

# guardo bases por separado

raw_nk %>% 
  write_csv(path = "raw_data/raw_nk.csv")

raw_mm %>% 
  write_csv(path = "raw_data/raw_mm.csv")

raw_af %>% 
  write_csv(path = "raw_data/raw_af.csv")

raw_cfk %>% 
  write_csv(path = "raw_data/raw_cfk.csv")

raw_apertura %>% 
  write_csv(path = "raw_data/raw_apertura.csv")

raw_ra %>% 
  write_csv(path = "raw_data/raw_ra.csv")

# por si las moscas: para leer después

raw_nk  <- read.csv("raw_data/raw_nk.csv", encoding = "UTF-8")
raw_mm  <- read.csv("raw_data/raw_mm.csv", encoding = "UTF-8")
raw_af   <- read.csv("raw_data/raw_af.csv", encoding = "UTF-8")
raw_cfk   <- read.csv("raw_data/raw_cfk.csv", encoding = "UTF-8")
raw_apertura  <- read.csv("raw_data/raw_apertura.csv", encoding = "UTF-8")
raw_ra <- read.csv("raw_data/raw_ra.csv", encoding = "UTF-8")

# guardo base completa

all_raw %>% 
  write_csv(path = "raw_data/all_raw.csv")
