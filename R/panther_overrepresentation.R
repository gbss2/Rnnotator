#'Overrepresentation test
#'
#' @description Compare a input list with a reference list, and determine whether a particular functional class (e.g. molecular function, biological process, cellular component) is overrepresented or underrepresented in input list.
#' @description PANTHER tools for overrepresentation test using either Binomial or Fisher's Exact Test
#'
#' @author Michele Molina e Rafaella Ferraz
#'
#' @param genes  Genes identifier. Maximum of 1000 Identifiers. Examples: Ensemble gene identifier, Ensemble protein identifier, Ensemble transcript identifier, Entrez gene id, gene symbol, NCBI GI, HGNC Id, International protein index id, NCBI UniGene id, UniProt accession andUniProt id.
#' @param organism One taxon id required. Ex.: 9606
#' @param ref_list If not specified, the system will use all the genes for the specified organism. Each identifier to be delimited by comma i.e. ','. Maximum of 100000 Identifiers. Examples: Ensemble gene identifier, Ensemble protein identifier, Ensemble transcript identifier, Entrez gene id, gene symbol, NCBI GI, HGNC Id, International protein index id, NCBI UniGene id, UniProt accession andUniProt id.
#' @param refOrganism This parameter is only required if parameter 'ref_list' has been specified. Only one taxon id can be specified.
#' @param annotDataSet One of the supported PANTHER annotation data types. Use the 'support_annot' function to retrieve list of supported annotation data types. Ex: GO:0003674
#' @param testtype Fisher's Exact test will be used by default. Options: "FISHER" and "BINOMIAL"
#' @param correction correction of enrichment. Options: "FDR", "BONFERRONI" and "NONE"
#'
#' @return Return a dataframe:
#' @return number_in_list
#' @return fold_enrichment: proportion of term genes found in the input list compared to the. proportion of total term genes found in the background
#' @return fdr: correction
#' @return expected
#' @return number_in_reference
#' @return pValue
#' @return term.id: Gene Ontology ID
#' @return term.label
#' @return plus_minus
#'
#' @export
#'
#' @examples
#'
#' data <- panther_over(genes = c("BRCA1", "VDR", "HBB"), annotDataSet = "GO:0003674")
#' data2 <- panther_over(genes = c("BRCA1", "VDR", "HBB"), annotDataSet = "GO:0003674",ref_list = c("BRCA2", "APC"), refOrganism = "9606")

panther_over <- function(genes,
                         organism = "9606",
                         ref_list = NULL,
                         refOrganism = NULL,
                         annotDataSet,
                         testtype = "FISHER",
                         correction = "FDR"){

  copy <- paste(sapply(genes, paste), collapse=",%20")
  go <- stringr::str_replace(annotDataSet, ":", "%3A")

  if(is.null(ref_list)){
    url_over <- paste("http://pantherdb.org/services/oai/pantherdb/enrich/overrep?",
                      "geneInputList=",copy,
                      "&organism=",organism,
                      "&annotDataSet=", go,
                      "&enrichmentTestType=", testtype,
                      "&correction=", correction, sep = "")
  } else{
    ref_list_pant <- paste(sapply(ref_list, paste), collapse=",%20")
    url_over <- paste("http://pantherdb.org/services/oai/pantherdb/enrich/overrep?",
                      "geneInputList=",copy,
                      "&organism=",organism,
                      "&refInputList=",ref_list_pant,
                      "&refOrganism=",refOrganism,
                      "&annotDataSet=", go,
                      "&enrichmentTestType=", testtype,
                      "&correction=", correction, sep = "")
  }

  data_json_over <- jsonlite::read_json(url_over,simplifyVector = T)
  output_over <- NULL
  for (name in names(data_json_over[["results"]][["result"]])){
    table_over <- data_json_over[["results"]][["result"]][[name]]
    output_over <- cbind(output_over, table_over)
  }
  names(output_over) <- c("number_in_list", "fold_enrichment", "fdr",
                          "expected", "number_in_reference", "pValue", "term_id",
                          "term_label", "plus_minus")

  return(output_over)

}


