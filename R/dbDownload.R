
#' Download the gbif4crest_02 dataset from figShare.
#'
#' Download the gbif4crest_02 dataset from figShare.
#'
#' @param filename The path and name of the file where the database should be saved.
#' @param lite Deprecated since v3. A boolean (default \code{TRUE}) to indicate
#'        if the full database should be downloaded (including the raw presences
#'        from GBIF; \code{lite = FALSE}) or only the curated data (\code{lite =
#'        TRUE}).
#' @param version The version of the gbif4crest dataset desired. Either 2 or 3.
#' @param res The spatial resolution of the dataset. Either 15min or 5min. Since
#'        v3, 5min is the only version available.
#' @param timeout Maximum duration in seconds of the download. If the file is
#'        not fully downloaded after 'timeout' seconds, it will be interrupted.
#' @export
#' @seealso The full SQLite3 database can be downloaded from \url{https://figshare.com/articles/dataset/GBIF_for_CREST_database/6743207}.
#' @examples
#' \dontrun{
#'   dbDownload() ## This will download the latest version of the database in
#'   your working directory.
#' }
#'
dbDownload <- function( filename = "gbif4crest_03.zip", version=3, lite=TRUE, res='5min', timeout=10000 ) {
    oldtimeout <- getOption('timeout')
    options(timeout=timeout)
    if(tools::file_ext(filename) != 'zip') filename <- paste0(filename, ".zip")
    if(version == 2) {
        if(res == '15min') {
            if(lite) {
                filename <- ifelse(filename=="gbif4crest_03.zip", "gbif4crest_02_15min_lite.zip", filename)
                utils::download.file("https://figshare.com/ndownloader/files/25071872", filename, method='libcurl', mode='wb',     headers  = c("User-Agent" = "Mozilla/5.0", "Referer" = "https://figshare.com/" ))
            } else {
                filename <- ifelse(filename=="gbif4crest_03.zip", "gbif4crest_02_15min.zip", filename, method='libcurl', mode='wb',     headers  = c("User-Agent" = "Mozilla/5.0", "Referer" = "https://figshare.com/" ))
                utils::download.file("https://figshare.com/ndownloader/files/36126908", filename, method='libcurl', mode='wb',     headers  = c("User-Agent" = "Mozilla/5.0", "Referer" = "https://figshare.com/" ))
            }
        } else if(res == '5min') {
            filename <- ifelse(filename=="gbif4crest_03.zip", "gbif4crest_02_5min.zip", filename)
            utils::download.file("https://figshare.com/ndownloader/files/42606571", filename, method='libcurl', mode='wb',     headers  = c("User-Agent" = "Mozilla/5.0", "Referer" = "https://figshare.com/" ))
        } else {
            stop("This resolution is not available. Pick res='15min' or '5min'.")
        }
    } else if(version == 3) {
        if(res == '5min') {
            filename <- ifelse(filename=="gbif4crest_03.zip", "gbif4crest_03_5min.zip", filename)
            utils::download.file("https://figshare.com/ndownloader/files/51983723", filename, method='libcurl', mode='wb',     headers  = c("User-Agent" = "Mozilla/5.0", "Referer" = "https://figshare.com/" ))
        } else {
            stop("This resolution is not available. Pick res='5min'.")
        }
    }
    cat("File downloaded and saved at:", tools::file_path_as_absolute(filename), "\n\n")
    options(timeout=oldtimeout)
}
