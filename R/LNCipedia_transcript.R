#' Get long non-coding RNA (LNCRNAs) information
#'
#' @description This function makes it possible to search for Long non coding RNAs (LncRNAs) annotation information available in the public LNCipedia database. For this function, it is necessary to inform the symbol of the lncRNA, followed by “:” and the ID of the transcript to be searched. Ex. (“HOTAIR:1”). It is possible to fetch information from several lncRNAs by passing a vector as in the example: c(“HOTAIR:1”, “H19:3”, “H19:4”). It returns a dataframe with the annotation data of the lncRNAs fetched.
#'
#' @param transcripts vector of transcripts id. Ex.: c("H19:4", "HOTAIR:1")
#' @param elements Gene:transcript number. eg: ('H19:4')
#' @return A data frame object: refGenome, chr, TranscriptID, class, size, numExons,start, end, strand
#'
#' @export
#'
#' @examples
#' tt <- lncipedia_transcript(c("H19:1", "HOTAIR:4"))
#'

lncipedia_transcript <- function(transcripts){

  output <- NULL
  output2 <- NULL

  for (transcript in transcripts){
    url <- paste0("https://lncipedia.org/api/transcript/",transcript)
    lnc_transcript <- tryCatch(jsonlite::read_json(url,simplifyVector = T),
                               error = function(e) e)

    if (inherits(lnc_transcript, "error")){
      NULL
    } else {

      out <- lnc_transcript

      output$Exon_position <- paste0("exon",row.names(out$exons),": ",out$exons$start,"-",out$exons$end)
      output$Exon_position <- toString(output$Exon_position)

      output_frame <- as.data.frame(c(out$refGenome, out$chromosome,
                                      out$lncipediaTranscriptID, out$class,
                                      out$transcriptSize, out$nrExons, out$start,
                                      out$end, output$Exon_position))
      output2 <- rbind(output2,output_frame)

    }

  }

  return(output2)
}


