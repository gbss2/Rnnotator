#' Show the interactions between a given set of gene and a drug names
#’
#' @author Adriano Viegas
#' @param genes : A set of genes (required)*.
#' @param itype : Interaction type - can be: inhibitor or activator (optional)*.
#’
#' @return A character list.
#' @export
#’
#' @examples
#'
#' res <- interactionGeneDrug("FLT1", "inhibitor")
#' res
#' FLT1 - DOVITINIB
#' FLT1 - LINIFANIB
#'
interactionGeneDrug = function(genes, itype) {
  if(missing(itype)){
    out = GET(paste("https://dgidb.org/api/v2/interactions.json?genes=", genes, sep=""), content_type("application/json"))
  }
  else{
    out = GET(paste("https://dgidb.org/api/v2/interactions.json?genes=", genes,"&interaction_types=", itype, sep=""), content_type("application/json"))
  }
  out = head(fromJSON(toJSON(content(out))))
  cnt = 1
  res = list()
  for(i in out$matchedTerms$geneName){
    res[i] = list(out$matchedTerms$interactions[[cnt]]$drugName)
    cnt = cnt + 1
  }
  return(res)
}
