#' Mapping
#'
#' @description  Maps a list of genes for a specified organism and return gene ontology, PANTHER protein class annotation and Reactome pathways.
#'
#' @author Michele Molina e Rafaella Ferraz
#'
#' @param genes  Genes identifier. Maximum of 1000 Identifiers. Examples: Ensemble gene identifier, Ensemble protein identifier, Ensemble transcript identifier, Entrez gene id, gene symbol, NCBI GI, HGNC Id, International protein index id, NCBI UniGene id, UniProt accession and UniProt id.
#' @param organism Taxon id. Ex.: 9606
#'
#' @return Return a dataframe
#'
#' @return name: Pathways' name
#' @return id: Gene Ontology ID
#' @return panther_id: Annotation id from PANTHER
#' @return panther_label: Annotation label from PANTHER
#' @return gene_acession: Id of the gene from HGNC and UniProtKB
#'
#' @export
#'
#' @examples
#'
#' data <- panther_map(genes = c("BRCA1", "VDR", "HBB"))
#' unique <- panther_map(genes = c("AICDA"))

panther_map <- function(genes,
                        organism = "9606"){

  copy <- paste(sapply(genes, paste), collapse="%2C%20")
  url <- paste("http://pantherdb.org/services/oai/pantherdb/geneinfo?",
               "geneInputList=",copy,
               "&organism=",organism, sep = "")
  data_json <- jsonlite::read_json(url,simplifyVector = T)
  data_output <- NULL

  if ("mapped_genes" %in% names(data_json$search)){
    if (class(data_json[["search"]][["mapped_genes"]][["gene"]][["annotation_type_list"]]) == "list"){
      for (num_go in 1:length(data_json[["search"]][["mapped_genes"]][["gene"]][["annotation_type_list"]][["annotation_data_type"]][["annotation_list"]][["annotation"]])){
        table <- as.data.frame(data_json[["search"]][["mapped_genes"]][["gene"]][["annotation_type_list"]][["annotation_data_type"]][["annotation_list"]][["annotation"]][[num_go]])
        content_data <- data_json[["search"]][["mapped_genes"]][["gene"]][["annotation_type_list"]][["annotation_data_type"]][["content"]][num_go]
        table$panther_id <- content_data
        data_output <- rbind(data_output, table)
      }
    } else {
      for (num_gen in 1:length(data_json[["search"]][["mapped_genes"]][["gene"]][["persistent_id"]])){
        tables_by_gene <- NULL
        for (num_go in 1:length(data_json[["search"]][["mapped_genes"]][["gene"]][["annotation_type_list"]][["annotation_data_type"]][[num_gen]][["annotation_list"]][["annotation"]])){
          table <- as.data.frame(data_json[["search"]][["mapped_genes"]][["gene"]][["annotation_type_list"]][["annotation_data_type"]][[num_gen]][["annotation_list"]][["annotation"]][[num_go]])
          content_data <- data_json[["search"]][["mapped_genes"]][["gene"]][["annotation_type_list"]][["annotation_data_type"]][[num_gen]][["content"]][num_go]
          table$panther_id <- content_data
          tables_by_gene <- rbind(tables_by_gene, table)
        }
        tables_by_gene$gene_acession <- data_json[["search"]][["mapped_genes"]][["gene"]][["accession"]][num_gen]
        data_output <- rbind(data_output, tables_by_gene)
      }
    }
    data_output$panther_label <- data_output$panther_id
    data_output[data_output$panther_label == "GO:0003674","panther_label"] <- "molecular_function"
    data_output[data_output$panther_label == "GO:0008150","panther_label"] <- "biological_process"
    data_output[data_output$panther_label == "GO:0005575","panther_label"] <- "cellular_component"
    data_output[data_output$panther_label == "ANNOT_TYPE_ID_PANTHER_GO_SLIM_MF","panther_label"] <- "PANTHER GO Slim Molecular Function"
    data_output[data_output$panther_label == "ANNOT_TYPE_ID_PANTHER_GO_SLIM_BP","panther_label"] <- "PANTHER GO Slim Biological Process"
    data_output[data_output$panther_label == "ANNOT_TYPE_ID_PANTHER_GO_SLIM_CC","panther_label"] <- "PANTHER GO Slim Cellular Location"
    data_output[data_output$panther_label == "ANNOT_TYPE_ID_PANTHER_PC","panther_label"] <- "protein class"
    data_output[data_output$panther_label == "ANNOT_TYPE_ID_PANTHER_PATHWAY","panther_label"] <- "ANNOT_TYPE_PANTHER_PATHWAY"
    data_output[data_output$panther_label == "ANNOT_TYPE_ID_REACTOME_PATHWAY","panther_label"] <- "ANNOT_TYPE_REACTOME_PATHWAY"
    rownames(data_output) <- NULL
    return(data_output)

  } else {
    print("Unmapped genes:")
    print(data_json[["search"]][["unmapped_list"]][["unmapped"]])
  }
}
