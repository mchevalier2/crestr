% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbGetTaxonomy.R
\name{getTaxonomy}
\alias{getTaxonomy}
\title{Extract taxonID(s) corresponding to the taxonomic description}
\usage{
getTaxonomy(
  family = "",
  genus = "",
  species = "",
  taxaType = 1,
  depth.out = 8,
  dbname = "gbif4crest_02",
  crest = NA
)
}
\arguments{
\item{family}{The name of the family.}

\item{genus}{The name of the genus.}

\item{species}{The name of the species.}

\item{taxaType}{A numerical index (between 1 and 5) to define the type of
palaeoproxy used: 1 for plants, 2 for beetles, 3 for chironomids,
4 for foraminifers, 5 for diatoms and 6 for rodents.}

\item{depth.out}{The taxonomic resolution of the output table. 1 for Kingdom,
2 for phylum, 3 for class_name, 4 for order_name, 5 for family, 6 for genus,
7 for species and 8 to also include the taxonID.}

\item{dbname}{The name of the data source database.}

\item{crest}{A crestObj to be used to refine the selection to a specific study
area. Set to \code{NA} by default (global search).}
}
\value{
A vector of unique taxonIDs.
}
\description{
Extract all possible TaxonIDs corresponding to the provided taxonomical
description, which can be at the family, the genus or the species levels.
}
\examples{
\dontrun{
  getTaxonomy("Zamiaceae    ")
  getTaxonomy(genus="Ceratozamia", depth.out=6)
  ## \code{taxaType = 2} searches for beetles and not plants, so the next line returns nothing.
  getTaxonomy("Zamiaceae", "Ceratozamia", taxaType = 2)
}

}
