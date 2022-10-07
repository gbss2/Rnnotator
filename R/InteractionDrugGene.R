#' Show the interactions between a given set of drugs and a genes names
#’
#' @author Adriano Viegas
#' @param drug : (required) - A set of drugs
#' @param typeReturn : (optional) - "data_frame" (default) or "'table" (first: use library(gt) and gt(res) - res as function return).
#' @param interaction_sources : (optional) - Can be : "DrugBank","PharmGKB","TALC","TEND","TTD"
#' @param interaction_types : (optional) - Can be: "activator", "inhibitor", "unknown" - (For some researches, params like interaction_types don't work properly, you should use interaction_source instead)
#' @param anti_neoplastic : (optional) - Can be: "true"
#' @param clinically_actionable : (optional) - Can be: "true"
#' @param gene_categories : (optional) - Can be: "KINASE", "DNA REPAIR", "TUMOR SUPPRESSOR"
#'
#' @return A data frame or gt table.
#' @export
#’
#' @examples
#'
#' res <- interactionDrugGene(drug='FLT1,FLT3', interaction_sources='TALC', anti_neoplastic='true')
#'
interactionDrugGene <- function(drug=drug, typeReturn='data_frame', interaction_sources=NULL, interaction_types=NULL, anti_neoplastic=NULL, gene_categories=NULL, clinically_actionable=NULL) {
  url = (paste("https://dgidb.org/api/v2/interactions.json?drugs=", drug, sep=''))

  if(!is.null(interaction_sources)){
    url = paste(url, '&interaction_sources=', interaction_sources, sep='')
  }

  if(!is.null(interaction_types)){
    url = paste(url, '&interaction_types=', interaction_types, sep='')
  }

  if(!is.null(anti_neoplastic)){
    url = paste(url, '&anti_neoplastic=', anti_neoplastic, sep='')
  }

  if(!is.null(gene_categories)){
    url = paste(url, '&gene_categories=', gene_categories, sep='')
  }

  if(!is.null(clinically_actionable)){
    url = paste(url, '&clinically_actionable=', clinically_actionable, sep='')
  }
  response = httr::GET(url)

  rJs <- httr::content(response, as="text")
  out <- jsonlite::fromJSON(rJs)

  data <- list()

  for(i in 1:length(out$matchedTerms$interactions)){

    lInteract = length(out$matchedTerms$interactions[[i]]$geneName)

    drugName <- replicate(lInteract, out$matchedTerms$drugName[[i]])
    data$drugName <- c(data$drugName, drugName)
    conceptId <- replicate(lInteract, out$matchedTerms$conceptId[[i]])

    geneName <- c()
    interactionTypes <- c()

    geneName <- out$matchedTerms$interactions[[i]]$geneName
    interactionTypes <- out$matchedTerms$interactions[[i]]$interactionTypes

    data$geneName <- c(data$geneName, geneName)
    data$interactionTypes <- sapply(c(data$interactionTypes, interactionTypes), paste0, collapse=';')
    data$conceptId <- c(data$conceptId, conceptId)

  }

  if(typeReturn == 'data_frame'){
    dataFrame <- as.data.frame(data)
    return(dataFrame)
  }

  if(typeReturn == 'table'){
    link = sub('chembl:', 'https://www.ebi.ac.uk/chembl/compound_report_card/', data$conceptId)

    df <- tibble::tibble(
      drugName=data$drugName,
      geneName=data$geneName,
      interactionTypes=data$interactionTypes,
      link)

    # using html

      dplyr::mutate(df,
        link = glue::glue("[website]({link})"),
        link = purrr::map(link, gt::md)) -> df
  }
}
