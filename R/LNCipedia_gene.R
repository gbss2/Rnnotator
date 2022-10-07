#' Get long non-coding RNA (LNCRNAs) information
#'
#' @description This function makes it possible to search for Long non coding RNAs (LncRNAs) annotation information available in the public LNCipedia database. For this function, it is necessary to inform the gene symbol or the list of symbols of the lncRNAs to be searched. It returns a dataframe with the annotation data of the lncRNAs fetched.
#'
#' @param genes vector of lncRNAs symbols. Ex.: c("H19", "HOTAIR")
#'
#' @return A data frame object: chr, lncipediaTranscriptID,start, end, strand
#'
#' @export
#'
#' @examples
#' tt <- lncipedia_gene(c("H19", "HOTAIR", "MEG3", "SNG1", "MEG9"))

lncipedia_gene <- function(genes){

  output2 <- NULL

  for (gene in genes){
    url <- paste("https://lncipedia.org/api/gene/", gene, sep = "")
    lnc_gene <- tryCatch(jsonlite::read_json(url,simplifyVector = T),
                         error = function(e) e)

    if (inherits(lnc_gene, "error")){
      NULL
    } else {

      output <- lnc_gene[["transcripts"]]
      output$chr <- rep(lnc_gene[["chromosome"]], nrow(output))
      output$strand <- rep(lnc_gene[["strand"]], nrow(output))
      output2 <- rbind(output2,output)
    }

  }

  return(output2[,c(4,2,3,1,5)])
}





