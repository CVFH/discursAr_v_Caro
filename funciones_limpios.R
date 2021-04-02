### HACIA UNA FORMULA DE LIMPIEZA GENERAL

## El objetivo de este script es crear una formula que
# reciba como input un .csv que contenga: fecha, nombre, titulo, texto
# devuelva un df con el texto convertido en tokens por palabras, 
# y limpio de sus palabras más frecuentes

# librerias 

library(tidyverse)
library(tidytext)
library(tm)


# función

clean_speech <- function(df_discurso){

  # input: un df con el discurso que tenga una columna "discurso" en la que se encuentre el texto a limpiar. se presume que está en español
  # output: un csv con el discurso expuesto de manera tidy: una palabra por fila, y limpio: libre de palabras comunes o insignificantes
  
# datos de trabajo preliminares
  
stopwords_es <- read_csv("stopwords/stopwords_es.csv")  #colgar en repo online?

palabras_comunes <- c("hoy","ciento","argentina","argentinos","hoy", ":","º","año", "años", "quiero", "sigue", "además","día","dia","días","días", "mil","millones", "honorable", "¿", '"', "•","●", "es", "|", "_", "pais","país",
                      "señores","senadores","diputados", "aplausos") 

clean_discurso <- df_discurso %>% 
  tidytext::unnest_tokens(word, discurso) %>% 
  anti_join(stopwords_es,by = c("word"="value")) %>% #saco los stopwords 
  anti_join(as_tibble(palabras_comunes), by = c("word"="value")) %>% # saco palabras comunes
  mutate(word= tm::removePunctuation(word),  # saco punctuation
         number = as.numeric(word),     # creo una nueva variable para sacar los números  
         ncharacters = nchar(word)) %>%  # creo una nueva variable para sacar las palabras con pocos caracteres
  filter(is.na(number)) %>%  # filtro los que no son números  
  filter(!ncharacters==1 ) %>% # filtro los que tienen un único caracter
  select(-number, -ncharacters) 

return(clean_discurso)

}