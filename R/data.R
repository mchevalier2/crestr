#' Example dataset to run the CREST method for the first time.
#'
#' A dataset containing 20 randomly generated pollen samples for 7 pollen taxa.
#'
#' @format A data frame with 20 rows (samples) and 8 columns (1 column for the
#'         age and one for each of the 7 taxa):
#' \describe{
#'   \item{Age:}{Age of each sample}
#'   \item{Taxon1:}{Percentage of Taxon1 in each sample.}
#'   \item{Taxon2:}{Percentage of Taxon2 in each sample.}
#'   \item{Taxon3:}{Percentage of Taxon3 in each sample.}
#'   \item{Taxon4:}{Percentage of Taxon4 in each sample.}
#'   \item{Taxon5:}{Percentage of Taxon5 in each sample.}
#'   \item{Taxon6:}{Percentage of Taxon6 in each sample.}
#'   \item{Taxon7:}{Percentage of Taxon7 in each sample.}
#' }
"crest_ex"



#' Example dataset to associate taxa with climate variables.
#'
#' A data frame indicating the taxa that should be used to reconstruct each
#' climate variable (1s in the matrix) and those who should be excluded (0s).
#'
#' @format A data frame with 7 rows (taxa) and 2 columns (climate variables):
#' \describe{
#'   \item{bio1:}{The first variable to reconstruct (mean annual temperature)}
#'   \item{bio12:}{The second variable to reconstruct (annual precipitation)}
#' }
"crest_ex_selection"



#' Example dataset to Extract data from the example database.
#'
#' A database indicating the taxonomy of the example proxies.
#'
#' @format A data frame with 7 rows (taxa) and 5 columns (taxonomy description):
#' \describe{
#'   \item{Level:}{An integer indicating the taxonomic resolution
#'                 (1 family, 2 genus, 3 species, 4 or higher ignore taxon)}
#'   \item{Family:}{The family corresponding to the ProxyName}
#'   \item{Genus:}{The genus corresponding to the ProxyName}
#'   \item{Species:}{The species corresponding to the ProxyName}
#'   \item{ProxyName:}{The names of the observed proxies, as reported in the
#'                     main data file}
#' }
"crest_ex_pse"

#' A shapefile of the world's country borders.
#'
#' A shapefile of the world's country borders.
#'
"M1"

#' A \code{\link{crestObj}} ran with the example dataset.
#'
#' A \code{\link{crestObj}} ran with the example dataset. Useful to illustrate
#' many functions of the package.
#'
"reconstr"
