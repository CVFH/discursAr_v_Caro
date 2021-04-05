# v CARO 
# PRUEBO CON UN ABORDAJE DISTINTO AL VIEJO
# DIRECTAMENTE DEL ARCHIVO COMPILADO DE DISCURSOS
# Y CON FILTROS PARA SELECCIONAR CUÁLES MOSTRAR

#' Diccionario de discursos disponibles
#'  (\emph{Speeches collection})
#'  
#' @description
#' Función que devuelve un listado de discursos de apertura de sesiones emitidos por los presidentes de Argentina ante la Asamblea Legislativa.
#'  
#' @param viewer Por defecto es \code{FALSE}. Cuando \code{TRUE} devuelve una tabla en el \emph{Viewer} de \emph{RStudio} 
#'  (\emph{The default is \code{FALSE}. When \code{TRUE} it returns a table in \emph{RStudio Viewer}}).
#'  
#' @return El objeto de salida es un data set con los id dediscursos disponibles para usar como parámetro con
#'  con \code{\link{get_speech}}. Cuando el parámetro es \code{viewer = FALSE}, devuelve un tibble con \code{class "tbl_df","tbl","data.frame"}, y 
#'  cuando es \code{viewer = TRUE} devuelve un objeto con \code{class "datatables","htmlwidget"} 
#'  (\emph{The output is a data set with speeches id needed as parameters in \code{\link{get_speech}}. 
#'  When parameter is set to \code{viewer = FALSE} it returns a tibble and when it is \code{viewer = TRUE} it returns an 
#'  object of \code{class "datatables","htmlwidget"}}).
#'  
#' @examples 
#'  
#'  show_available_speech()
#'  
#'
#' @export

library(tidyverse)
library(stringr)

show_available_speech <- function(keyword = NULL, date = NULL, year = NULL, president = NULL, viewer = FALSE){
  
  # url
  
  url <- "https://raw.githubusercontent.com/CVFH/discursAr_v_Caro/main/raw_data/all_raw.csv"
  
  # Check for internet coection
  attempt::stop_if_not(.x = curl::has_internet(),  # from eph package
                       msg = "No se detecto acceso a internet. Por favor checkea tu conexion.")
  

  
## FAIL SAFELEY

check <- httr::GET(url)

httr::stop_for_status(x = check, 
                      task = "Fail to download data. Source is not available // La fuente de datos no esta disponible")


# Get list of files from github data repo

raw_data <- read.csv(url, encoding = "UTF-8") # poner enlace a .csv en git
list_raw <- raw_data %>% 
  select(-c(discurso, link, fuente))

if (!is.null(keyword)){
  
  list_raw <- list_raw %>% 
    subset(str_detect(titulo, keyword))
  
}

if (!is.null(president)){
  
  list_raw <- list_raw %>% 
  filter(presidente == president)
}

if (!is.null(date)){
  
  list_raw <- list_raw %>% 
    filter(fecha == date)
  
}

if (!is.null(year)){
  
  list_raw <- list_raw %>% 
    filter(año == year)
}

if(viewer == TRUE){
  
  x <-  list_raw %>%
    dplyr::rename(Feha = fecha, 
           Presidente = presidente,
           "Título" = titulo,
           Año = año,
           "ID del discurso" = id) %>% 
    dplyr::mutate(Presidente = stringr::str_replace_all(string = Presidente, pattern = "_", " "), 
                  Presidente = stringr::str_to_title(Presidente)) %>% 
    DT::datatable(options = list( 
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
  print(x)
  
  
} else {
  
  list_raw
}
}

show_available_speech(year=2019,president=alberto_fernandez)