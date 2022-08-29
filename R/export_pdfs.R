#' Export the pdfs fitted for the different taxa.
#'
#' Export the pdfs fitted for the different taxa.
#'
#' @inheritParams crestObj
#' @inheritParams export
#' @param taxa The names of the taxa of interest. All the pdfs are saved by
#'        default.
#' @return No return value, function called to export the PDFs as files.
#' @export
#' @examples
#' \dontrun{
#'   data(crest_ex)
#'   data(crest_ex_pse)
#'   data(crest_ex_selection)
#'   reconstr <- crest(
#'     df = crest_ex, pse = crest_ex_pse, taxaType = 0,
#'     site_info = c(7.5, 7.5),
#'     climate = c("bio1", "bio12"), bin_width = c(2, 20),
#'     shape = c("normal", "lognormal"),
#'     selectedTaxa = crest_ex_selection, dbname = "crest_example",
#'     leave_one_out = TRUE
#'   )
#'   #> Replace 'tempdir()' by the location where the sample should be saved (e.g. 'getwd()')
#'   export_pdfs(reconstr,
#'               dataname='crest_example',
#'               loc=tempdir()
#'   )
#' }
#'
export_pdfs <- function( x, dataname = x$misc$site_info$site_name,
                    climate = x$parameters$climate, taxa = x$inputs$taxa.name,
                    loc = getwd(), as.csv = FALSE ) {

    if(base::missing(x)) x

    if (methods::is(x)[1] == 'crestObj') {
        if (length(x$modelling$pdfs) == 0) {
            stop("\nNo pdfs available for export. You need to run crest.calibrate() first.\n\n")
        }

        if(!as.csv) {
            if (!requireNamespace("openxlsx", quietly = TRUE)) {
                as.csv <- TRUE
                warning("The package 'openxlsx' is required to export the data as excel spreadsheets. The data have been exported as csv.\n\n")
            }
        }

        if (sum(! climate %in% x$parameters$climate) > 0) {
            stop(paste0("\nNot all climate parameters provided are accepted values. Climate values must be one or more of the following: '",paste(x$parameters$climate, collapse="', '"),"'.\n\n"))
        }

        if (sum(! taxa %in% x$inputs$taxa.name) > 0) {
            stop(paste0("\nNot all the taxa names provided are accepted values. Taxa names must be one or more of the following: '",paste(x$inputs$taxa.name, collapse="', '"),"'.\n\n"))
        }


        if(is.na(dataname)) dataname <- 'crest_outputs'
        if (!base::file.exists(base::file.path(loc, dataname))){
            base::dir.create(base::file.path(loc, dataname), showWarnings = TRUE)
        }

        idx <- 1
        for(clim in climate) {
            df <- x$modelling$xrange[[clim]]
            for(tax in taxa) {
                df <- cbind(df, x$modelling$pdfs[[tax]][[clim]]$pdfpol)
            }
            colnames(df) <- c(clim, taxa)

            if(as.csv) {
                utils::write.table(df, base::file.path(loc, dataname, paste0(clim,'.csv')), col.names=TRUE, row.names=FALSE, quote=FALSE, na='', sep=',')
            } else {
                if(idx == 1) {
                    wb <- openxlsx::createWorkbook()
                }
                openxlsx::addWorksheet(wb, clim)
                openxlsx::writeData(wb, sheet = clim, x = df, colNames=TRUE)
            }
            idx <- idx + 1
        }

        if(!as.csv) openxlsx::saveWorkbook(wb, file.path(loc, dataname, paste0('taxa_pdfs.xlsx')), overwrite = TRUE)

    } else {
        stop("'\ncrestr::export()' is only availble for crestObj objects.\n\n")
    }
    invisible(x)
}
