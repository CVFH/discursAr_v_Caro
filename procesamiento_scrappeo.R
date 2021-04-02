# este script contiene operaciones realizadas para scrappear data cruda


# ROSADA. BASADO MAYORMENTE EN DESARROLLO DE D. KOZAK #######

## script original de D.Kozak con algunas transformaciones para su uso
# actualicé enlaces, cambié formateo de fechas, guardé a .cvs en lugar de a .rds
# original en: https://github.com/DiegoKoz/discursos_presidenciales/blob/master/get_data.R

## librerias ####

library(rvest)
library(xml2)
library(tidyverse)
library(glue)

#### funciones #####

get_text <- function(link){
  tryCatch({
    
    pagina <- read_html(link)
    
    texto <- pagina %>% html_nodes('.jm-allpage-in') %>% 
      html_text(.)
    titulo <- pagina %>% html_nodes('strong') %>%
      html_text(.)
    
    df <- tibble(texto=texto, titulo=titulo)
    
    Sys.sleep(1)
    df
    
  },error = function(err) {glue::glue("error_in_link: {link}")})
}

get_new_links <- function(link){
  tryCatch({
    
    base <- link
    nodes <- read_html(link) %>% 
      rvest::html_nodes("a")  
    
    df <- tibble(link= html_attr(nodes, "href"), titulo = html_text(nodes)) %>% 
      filter(str_detect(link,"informacion/discursos/\\d"),
             link!="") %>% 
      na.omit(.) %>%
      mutate(link = gsub("#.*","",link),
             link= xml2::url_absolute(link, base= base)) %>% 
      distinct(link,.keep_all = TRUE )
    df
  },error = function(err) {glue::glue("error_in_link: {link}")})
}

### info preliminar ####

link <- 'https://www.casarosada.gob.ar/informacion/discursos'

paginas <- c(link, glue('{link}?start={seq(40,900, 40)}')) #originalmente decia 600 en lugar de 900
# HAY QUE AJUSTAR ESTE PARAMETRO SI SE QUIERE RECABAR TODOS LOS DISCURSOS DISPONIBLES A MEDIDA QUE SE VAN AGREGANDO
paginas_df <- tibble(paginas=paginas) %>% 
  mutate(new_links= map(paginas, get_new_links))

paginas_df <- paginas_df %>% 
  unnest(cols = c(new_links))

## guardo los links #####

paginas_df %>% 
  write_csv(path = 'original_data/rosada/links.csv')

### aplicación de funciones para extraer datos #####

paginas_df <- paginas_df %>% 
  mutate(cuerpo = map(link, get_text))


#elimino los fallidos
corpus <- paginas_df %>% 
  mutate(tipo = unlist(map(cuerpo,typeof))) %>% 
  filter(!tipo =='character')

#corpus %>% 
#  write_rds('rosada/download_data.rds')

### algunas transformaciones a la base obtenida ####

#corpus <- read_rds('rosada/download_data.rds')

corpus <- corpus %>% 
  mutate(fecha = str_extract(titulo,'(?<=\t[[:blank:]]{2,10})[[:upper:]].*20\\d\\d(?=[[:blank:]]{2,10})'),
         fecha_normalizada = str_extract(fecha, '\\d.*20\\d\\d'), #originalmente decia 201\\d
         fecha_normalizada = str_replace(fecha_normalizada, 'Enero', '01'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Febrero', '02'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Marzo', '03'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Abril', '04'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Mayo', '05'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Junio', '06'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Julio', '07'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Agosto', '08'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Septiembre', '09'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Octubre', '10'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Noviembre', '11'),
         fecha_normalizada = str_replace(fecha_normalizada, 'Diciembre', '12'),
         fecha_normalizada = str_replace_all(fecha_normalizada, 'del?', '-'),
         fecha_normalizada = str_remove_all(fecha_normalizada, ' '),
         fecha_normalizada = lubridate::dmy(fecha_normalizada)) %>% 
  select(link,fecha=fecha_normalizada, cuerpo) %>% 
  unnest()

# hay registros duplicados porque pusieron doble titulo
corpus <- corpus %>% 
  distinct(link,.keep_all = T)

#corpus %>% 
#  write_rds('data/corpus.rds')

# guardamos archivo completo #####
corpus %>% 
 write_csv(path = "original_data/rosada/corpus_completo.csv")

# corpus <- read.csv("original_data/rosada/corpus_completo.csv", encoding = "UTF-8")

# corpus$titulo[5]

## creo archivos separados #####


corpus %>%
  subset(str_detect(corpus$fecha,"2015") | 
           str_detect(corpus$fecha,"2016") | 
           str_detect(corpus$fecha,"2017") |
           str_detect(corpus$fecha,"2018") | 
           (str_detect(corpus$fecha,"2019")& str_detect(corpus$titulo,"Macri"))) %>% 
  write_csv(path = "original_data/rosada/macri.csv")

corpus %>%
  subset( (str_detect(corpus$fecha,"2019")& !str_detect(corpus$titulo,"Macri")) |
            str_detect(corpus$fecha,"2020") | 
            str_detect(corpus$fecha,"2021") ) %>% 
  write_csv(path = "original_data/rosada/afernandez.csv")

# SCRAPEO WEB DE CFK ########################

# datos fuente 
url_madre <- "https://www.cfkargentina.com/category/nestor/discursos-nestor-2/discursos-2003-2007/"
urls_madres <- tibble(c(url_madre, glue('{url_madre}page/{seq(2,4, 1)}'))) %>% 
  rename(urls_madres = colnames(.)[1])

# funciones 

# para extraer links

get_urls_hijos <- function(url_madre){
  # recibe enlace a una web , lee los hiperenlaces que contiene
  # y los devuelve en 
  pg <- read_html(url_madre)
  urls_hijos <- html_attr(html_nodes(pg, "a"), "href") 
  return(urls_hijos)
}

# para extraer texto

get_text_nk <- function(link_nk){
  tryCatch({
    
    pagina <- read_html(link_nk)
    
    texto <- pagina %>% html_nodes('.box_interna_lefy ') %>% 
      html_text(.)
    titulo <- pagina %>% html_nodes('.box_interna_lefy .box_interna_titulo') %>%
      html_text(.) 
    
    df <- tibble(texto=texto, titulo=titulo)
    
    Sys.sleep(1)
    df
    
  },error = function(err) {glue::glue("error_in_link: {link}")})
}

# primero obtenemos links

urls_hijos <- urls_madres %>% 
  mutate(urls_hijos = map(urls_madres, get_urls_hijos)) %>% 
  unnest(cols=(urls_hijos))

urls_nk2 <- urls_hijos %>%  
  group_by(urls_hijos) %>% 
  filter(n()==2) %>% # justo los enlaces a los discursos de nestor estan duplicados
  filter(!str_detect(urls_hijos, "category")) %>% # solamente queda eliminar estos enlaces residuales
  mutate(urls_hijos = as.factor(urls_hijos)) # los convertimos a factores para poder filtrarlos

urls_nk <- urls_nk2$urls_hijos %>% 
  fct_unique() %>% 
  as.character() %>%  # finalmente obtenemos un vector con los enlaces a los discursos de nk
  tibble() %>%  
  rename(urls_nk = colnames(.)[1]) 

# ahora extraemos textos 

nk_df <- urls_nk %>% 
  mutate(cuerpo = map(urls_nk, get_text_nk)) %>% 
  unnest(cols = c(cuerpo))

# guardamos en originales

nk_df %>% 
  write_csv(path = "original_data/nk/discursos_nk_completo.csv")


# SCRAPEO WEB DE ALFONSIN ######################## 

# datos fuente 
url_ra <- "https://www.alfonsin.org/discursos/"

# extraemos enlaces a discursos
pg_ra <- read_html(url_ra) #leo web

urls_ra <- html_attr(html_nodes(pg_ra, "a"), "href") %>% # extraigo enlaces
  tibble() %>% 
  rename(link_ra = colnames(.)[1]) %>% 
  filter(duplicated(urls_ra)) %>% # los enlaces a los discursos de estan duplicados, aprovechamos para filtrar
  filter(!is.na(urls_ra)) 

urls_ra %>% 
  write_csv(path= "original_data/ra/urls_ra.csv")

# para extraer texto. iteramos con ciclo
# porque estrategia de funcion no sirvio

# primero seteamos el tibble a llenar
enlace <- seq(1, 25, by=1) %>% as.character()
texto <- seq(1, 25, by=1) %>% as.character()
titulo <- seq(1, 25, by=1) %>% as.character()
fecha <- seq(1, 25, by=1) %>% as.character()
ra_data_tibble <- tibble(enlace, texto, titulo, fecha)

# iteramos sobre nuestros enlaces extrayendo info
for (i in seq(1, 25, by=1)) {
  
  ra_data_tibble$enlace[i] <- urls_ra$link_ra[i] 
  
  download.file(urls_ra$link_ra[i], destfile = "pg.html", quiet=TRUE)
  content <- read_html("pg.html")
  file.remove("pg.html")
  ra_data_tibble$texto[i] <- content %>% html_nodes('.areapost') %>% 
    html_text(.) 
  ra_data_tibble$titulo[i] <- content %>% html_nodes('.red') %>%
    html_text(.) 
  ra_data_tibble$fecha[i] <- content %>% html_nodes('.anionota') %>%
    html_text(.)

}  

# guardamos
ra_data_tibble %>% 
  write_csv(path = "original_data/ra/ra_data_tibble.csv")
