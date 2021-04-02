# v CARO 
# PRUEBO
# esta parte quedo de lo anterior:
#' Descarga discursos
#'  (\emph{Download speeches})
#'
#' @description
#' Función que descarga los discursos presidenciales ante la Asamblea Legislativa desde 1854 hasta 2020
#'  (\emph{Function that downloads presidential speeches to de National Legislative Assembly from 1854 to 2020.})
#'
#' @param year integer con identificador de discurso que se quiere seleccionar. Se puede explorar un listado de discursos con \code{\link{show_available_speech}}
#' (\emph{integer id for a selected speech. Explore full list of speeches  with \code{\link{show_available_speech}}}).
#'
#' @param raw boleano que permite descargar discurso en formato \emph{tidy} cuando \code{raw = FALSE}  o crudo caso contrario
#' (\emph{boolean that sets if you want to download raw or \emph{tidy} formated speech data}).
#'
#'
#' @return Devuelve un tibble con clases \code{"spec_tbl_df" "tbl_df" "tbl" "data.frame"} con el contenido de un discurso presidencial en tres variables:
#' \code{discurso, presidente, year}.
#'  (\emph{it retruns a tibble with three variables (speech - \code{discurso} - president -\code{presidente} and \code{year}).
#' The object is of \code{class "tbl_df","tbl","data.frame"}}).
#'
#' @seealso  \code{\link{plot_speech}}
#'
#' @examples
#'
#'   get_speech(year = 1949)
#'
#'
#' @export

  get_speech <- function(keyword = NULL, date = NULL, year = NULL, president = NULL, speech_id = NULL,  raw = FALSE){

  ## Check for internet coection
  attempt::stop_if_not(.x = curl::has_internet(),
                       msg = "Internet access was not detected. Please check your connection //
                            No se detecto acceso a internet. Por favor chequear la conexion.")

  ## Check params
  # raw check

  assertthat::assert_that(is.logical(raw),
                          msg = "'raw' must be logical. Options = c(TRUE, FALSE) //
                             'raw' debe ser un boleano. Opciones = c(TRUE, FALSE)" )
 
  # priorizar id 
  
  if (!is.null(speech_id)){
    
    keyword <- NULL
    date <- NULL
    year <- NULL
    president <- NULL
    print ( "Beware that we will prioritize the required id. / Tenga en cuenta que prioriaremos la id ingresada como criterio de búsqueda")
  
  }
  
   # Search and check of id
  
  posible_speeches <-   discursAr::show_available_speech(keyword, date, year, president)
  id_possible <- posible_speeches$id
  
  if (!is.null(speech_id)){
    
    if (speech_id %in% id_possibe){
      
      search_id <- speech_id
    } else {
      
      print("We couldn't find your selected id / No pudimos encontrar el id ingresado ")
    }
  }
  
  else {
    
    if (len(id_possible == 1)){
      
      search_id <- id_possible
      
    } else{
      
      if (len(id_possible < 1)){
        
        print("No such discourse / No se ha encontrado ningún discurso acorde a los parámetros ingresados")
        
      } else {
        
        print("Too many discourses match your choices. Please be more precise.
               We strongly recommend you search for a discourse_id using show_available_speeches().
              / Su búsqueda arroja demasiados resultados. Por favor, sea más preciso.
               Sugerimos buscar el código de identificación del discurso (discourse_id) con la función show_available_speeches()")
      }
    }
  }

  # get RAW or Tidy (one token per row)

  if(raw == FALSE){ #Tidy version of speech

    return_speech <- posible_speeches %>% 
      filter(id == search_id) %>% 
      funciones_limpios() 
    
  } else {
    
    return_speech <- posible_speeches %>% 
      filter(id == search_id) %>% 
      select(titulo, discurso) 
  }
    
  return_speech 
  }