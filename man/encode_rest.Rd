% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enconde_rest.r
\name{encode_rest}
\alias{encode_rest}
\title{Return phenotype annotations for a given gene}
\usage{
encode_rest(gene, species = "homo_sapiens")
}
\arguments{
\item{gene}{: A gene.}

\item{species}{species.}
}
\value{
A character list.
}
\description{
Return phenotype annotations for a given gene
}
\examples{
encode_rest("HBB")

# source external_id            Gene           location
# 1  MIM morbid      613985 ENSG00000244734 11:5225464-5229395
# 2  MIM morbid      611162 ENSG00000244734 11:5225464-5229395

}
\author{
Eric Baia
}
