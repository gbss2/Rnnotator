#' Show the interactions between a given set of genes and a drugs names
#’
#' @author Adriano Viegas
#' @param gene : (required) - A set of genes.
#' @param typeReturn : (optional) - "data_frame" (default) or "table" (first: use library(gt) and gt(res) - res as function return).
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
#' res <- interactionGeneDrug(gene='FLT1,FLT3', interaction_sources='TALC', anti_neoplastic='true')
#'
interactionGeneDrug <- function(gene=gene, typeReturn='data_frame', interaction_sources=NULL, interaction_types=NULL, anti_neoplastic=NULL, gene_categories=NULL, clinically_actionable=NULL) {
  url = (paste("https://dgidb.org/api/v2/interactions.json?genes=", gene, sep=''))

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

    lInteract = length(out$matchedTerms$interactions[[i]]$drugName)

    geneName <- replicate(lInteract, out$matchedTerms$geneName[[i]])
    data$geneName <- c(data$geneName, geneName)

    drugName <- c()
    interactionTypes <- c()

    drugName <- out$matchedTerms$interactions[[i]]$drugName
    interactionTypes <- out$matchedTerms$interactions[[i]]$interactionTypes
    drugConceptId <- out$matchedTerms$interactions[[i]]$drugConceptId

    data$drugName <- c(data$drugName, drugName)
    data$interactionTypes <- sapply(c(data$interactionTypes, interactionTypes), paste0, collapse=';')
    data$drugConceptId <- c(data$drugConceptId, drugConceptId)

  }

  if(typeReturn == 'data_frame'){
    dataFrame <- as.data.frame(data)
    return(dataFrame)
  }

  if(typeReturn == 'table'){
    link = sub('chembl:', 'https://www.ebi.ac.uk/chembl/compound_report_card/', data$drugConceptId)

    df <- tibble::tibble(
      nameGane=data$geneName,
      drugName=data$drugName,
      interactionTypes=data$interactionTypes,
      link)

    # using html

      dplyr::mutate(df,
        link = glue::glue("[website]({link})"),
        link = purrr::map(link, gt::md)) -> df
  }
}
