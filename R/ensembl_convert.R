
#' Convert the co-ordinates of one assembly to another
#’
#' @author Eric Baia
#' @param asm_one		Version of the input assembly	-	(Ex: GRCh37)
#' @param asm_two		Version of the output assembly	-	(Ex: GRCh38)
#' @param chr	type cromossomo	X
#' @param coord_start Coordinate start
#' @param coord_end Coordinate End
#' @param strand DNA strand
#' @return A data frame.
#'# assembly seq_region_name     end coord_system   start strand
#'# 1   GRCh37               X 1000100   chromosome 1000000      1
#'# 2   GRCh38               X 1039365   chromosome 1039265      1
#' @export
#’
#' @examples
#'
#' fg <- ensembl_convert(asm_one ="GRCh37",
#'chr = "x",
#'coord_start ="1000000",
#'coord_end ="1000100",
#'asm_two ="GRCH38",
#'strand = "1")
#'
#'# /map/human/GRCh37/X:1000000..1000100:1/GRCh38?content-type=application/json
#'
#'
#'
#'
#'



ensembl_convert <- function(asm_one, asm_two, chr, coord_start, coord_end, strand){
  server <- "https://rest.ensembl.org"
  ext <- paste("/map/human/", asm_one,
               "/",chr,":",coord_start,"..",coord_end, ":",strand,"/", asm_two, "?", sep ="")

  r <- httr::GET(paste0(server, ext), httr::content_type("application/json"))
  ens<- jsonlite::fromJSON(jsonlite::toJSON(httr::content(r)))
  ens_final <- rbind(ens$mappings$original, ens$mappings$mapped[, names(ens$mappings$original)])

  return(ens_final)

}






