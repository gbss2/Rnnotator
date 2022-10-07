#' Get long non-coding RNA (LNCRNAs) information
#'
#'@description This function makes it possible to search for Long non coding RNAs (LncRNAs) annotation information available in the public LNCipedia database. For this function, it is necessary to inform the type of search and the specific elements related to the type of search. It allows searching from different data entered, such as chromosome, transcript class, id, start and end of the gene. It returns a dataframe with the annotation data of the lncRNAs fetched.
#'
#'@param type Specific type of information. eg: id, chromosome,start,end,high_confidence_set,keyword or class
#'@param elements eg: 'ENSG00000228630'
#'
#'@details id (optional): Any lncRNA identifier, internal or external. eg: 'ENSG00000228630'
#'@details chromosome (optional): Chromosome. eg: 'chr10'
#'@details start (optional): Start of region
#'@details end (optional): End of region
#'@details high_confidence_set (optional): Only retrieve lncRNAs in the high-confidence set. Default: false, set to 1 to enable
#'@details keyword (optional): A keyword in an abstract associated with the lncRNA. eg: 'cancer'
#'@details class (optional): Subclass. eg: 'intergenic'
#'
#'
#' @return A data frame object
#'
#' @export
#'
#' @examples
#' tt <- lncipedia_search(type = "id", elements = "ENSG00000228630")
#' aa <- lncipedia_search(type = "chromosome", elements = c("chr10", "chr11"))

lncipedia_search <- function(type,elements){

  output2 <- NULL

  for (id in elements){
    url <- paste("https://lncipedia.org/api/search?",type,"=", id, sep = "")
    lnc_id <- tryCatch(jsonlite::read_json(url,simplifyVector = T),
                         error = function(e) e)

    if (inherits(lnc_id, "error")){
      NULL
    } else {
      chr <- lnc_id[["transcripts"]][["chromosome"]]
      class <- lnc_id[["transcripts"]][["class"]]
      lncipediaGeneID <- lnc_id[["transcripts"]][["lncipediaGeneID"]]
      lncipediaTranscriptID <- lnc_id[["transcripts"]][["lncipediaTranscriptID"]]
      nrExons <- lnc_id[["transcripts"]][["nrExons"]]
      sequence <- lnc_id[["transcripts"]][["sequence"]]
      start <- lnc_id[["transcripts"]][["start"]]
      end <- lnc_id[["transcripts"]][["end"]]
      strand <- lnc_id[["transcripts"]][["strand"]]
      transcriptSize <- lnc_id[["transcripts"]][["transcriptSize"]]
      output <- data.frame(chr,class,lncipediaGeneID,lncipediaTranscriptID,
                           nrExons,sequence,start,end,strand,transcriptSize)
      output$transcriptAliases <- NA
      output$geneAliases <- NA

      for (num in 1:length(chr)){
        output$transcriptAliases[num] <- toString(lnc_id[["transcripts"]][["transcriptAliases"]][[num]], sep = ",")
        output$geneAliases[num] <- toString(lnc_id[["transcripts"]][["geneAliases"]][[num]], sep = ",")
        exon <- c()

        for (num_exon in 1:nrow(lnc_id[["transcripts"]][["exons"]][[num]])){
          ee <- paste(paste("exon", num_exon, sep = ""), lnc_id[["transcripts"]][["exons"]][[num]][["start"]][num_exon], lnc_id[["transcripts"]][["exons"]][[num]][["end"]][num_exon], sep = ":")
          exon <- append(exon, ee)
        }

        output$exon_start_end[num] <- toString(exon)
      }
      output2 <- rbind(output2,output)
    }
  }
  return(output2)
}

