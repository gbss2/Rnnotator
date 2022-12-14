#' Return phenotype annotations for a given gene
#’
#' @author Eric Baia
#' @param gene : A gene.
#' @param species species.
#'
#' @return A character list.
#' @export
#’
#' @examples
#' encode_rest("HBB")
#'
#'# source external_id            Gene           location
#'# 1  MIM morbid      613985 ENSG00000244734 11:5225464-5229395
#'# 2  MIM morbid      611162 ENSG00000244734 11:5225464-5229395
#'

encode_rest <- function (gene, species = "homo_sapiens") {

  link <- paste("https://rest.ensembl.org/phenotype/gene",
                species, gene, sep = "/")

  res <- httr::GET(link)
  httr::content_type("application/json")


  res1 <-jsonlite::fromJSON(jsonlite::toJSON(httr::content(res)))

  return(res1)

}
