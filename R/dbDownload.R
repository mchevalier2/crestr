
#' Download the gbif4crest_02 dataset from figShare.
#'
#' Download the gbif4crest_02 dataset from figShare.
#'
#' @param filename The path and name of the file where the database should be saved.
#' @param lite A boolean (default \code{TRUE}) to indicate if the full database
#'        should be downloaded (including the raw presences from GBIF;
#'        \code{lite = FALSE}) or only the curated data (\code{lite = TRUE}).
#' @export
#' @seealso The full SQLite3 database can be downloaded from \url{https://figshare.com/articles/dataset/GBIF_for_CREST_database/6743207}.
#' @examples
#' \dontrun{
#'   dbDownload() ## This will download the database in your working directory.
#' }
#'
dbDownload <- function( filename = "gbif4crest_02.zip", lite=TRUE ) {
    if(tools::file_ext(filename) != 'zip') filename <- paste0(filename, ".zip")
    if(lite) {
        utils::download.file("https://figshare.com/ndownloader/files/25071872", filename)
    } else {
        utils::download.file("https://figshare.com/ndownloader/files/36126908", filename)
    }
    cat("File downloaded and saved at:", tools::file_path_as_absolute(filename), "\n\n")
}
