#' Show the interactions between a given set of drugs and a gene names
#’
#' @author Adriano Viegas
#' @param drugs : A set of drugs (required)*.
#' @param itype : Interaction type - can be: inhibitor or activator (optional)*.
#’
#' @return A character list.
#' @export
#’
#' @examples
#'
#' res <- interactionDrugGene("VATALANIB", "inhibitor")
#' res
#' VATALANIB - KDR
#' VATALANIB - CSF1R

interactionDrugGene = function(drugs, itype) {
  if(missing(itype)){
    out = GET(paste("https://dgidb.org/api/v2/interactions.json?drugs=", drugs, sep=""), content_type("application/json"))
  }
  else{
    out = GET(paste("https://dgidb.org/api/v2/interactions.json?drugs=", drugs,"&interaction_types=", itype, sep=""), content_type("application/json"))
  }
  out = head(fromJSON(toJSON(content(out))))
  cnt = 1
  res = list()
  for(i in out$matchedTerms$drugName){
    res[i] = list(out$matchedTerms$interactions[[cnt]]$geneName)
    cnt = cnt + 1
  }
  return(res)
}
