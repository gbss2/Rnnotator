#' Information about annotation data sets supported by PANTHER
#'
#' @author Michele Molina e Rafaella Ferraz
#' @export
#'
#' @return Returns information about annotation data sets supported by PANTHER
#' @return release_date
#' @return description
#' @return id
#' @return label
#' @return version
#'
#' @examples
#' sup <- support_annot()


support_annot <- function(){
  url <- "http://pantherdb.org/services/oai/pantherdb/supportedannotdatasets"
  support_json <- jsonlite::read_json(url,simplifyVector = T)
  support_ <- as.data.frame(support_json[["search"]][["annotation_data_sets"]][["annotation_data_type"]])
  return(support_)
}



