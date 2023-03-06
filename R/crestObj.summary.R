
#' @export
summary.crestObj <- function(x, as=x$misc$stage, ...) {
    if(base::missing(x)) x

    name <- find.original.name(x)
    is_formatted <- is_fitted <- is_reconstructed <- is_looed <- FALSE
    if(as == 'data_extracted' | as == 'data_inserted') {
        is_formatted <- TRUE
    } else if (as == 'PDFs_fitted') {
        is_formatted <- is_fitted <- TRUE
    }else if (as == 'climate_reconstructed') {
        is_formatted <- is_fitted <- is_reconstructed <- TRUE
    } else if (as == 'leave_one_out') {
        is_formatted <- is_fitted <- is_reconstructed <- is_looed <- TRUE
    }

    cat('*\n')
    cat(paste0('* Summary of the crestObj named `',name,'`:\n'))
    cat(paste0('*   x Calibration data formatted .. ', is_formatted,'\n'))
    cat(paste0('*   x PDFs fitted ................. ', is_fitted,'\n'))
    cat(paste0('*   x Climate reconstructed ....... ', is_reconstructed,'\n'))
    cat(paste0('*   x Leave-One-Out analysis ...... ', is_looed,'\n'))
    cat('*\n')
}
