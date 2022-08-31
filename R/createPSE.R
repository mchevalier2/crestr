#' Creates a spreadsheet with the format required for a PSE.
#'
#' Creates a spreadsheet with the format required for a PSE from a list of taxa.
#'
#' @param taxa A list of taxa to include in the PSE file.
#' @param loc An absolute or relative path that indicates where the spreadsheet
#'        should be saved. Also used to specify the name of the file. Default:
#'        the file is saved in the working directory under the name
#'        \code{proxy_species_equivalency.xlsx}.
#' @return No return value, called to create a PSE file.
#' @export
#' @examples
#' data(crest_ex)
#' createPSE(taxa = colnames(crest_ex)[-1],
#'           loc=file.path(tempdir(), 'pse.xlsx')
#' )
#'
createPSE <- function(taxa, loc='proxy_species_equivalency.xlsx') {
    if(base::missing(taxa)) taxa

    if (!requireNamespace("openxlsx", quietly = TRUE)) {
        cat("This function requires the package 'openxlsx'. Install it using 'install.packages(\"openxlsx\")' to continue.\n\n")
    } else {
        wb <- openxlsx::createWorkbook()

        forms <- c()
        for(tax in 1:length(taxa)) {
            forms <- c(forms, paste0("IF(ISBLANK(D", tax+1, "),IF(ISBLANK(C", tax+1, "),IF(ISBLANK(B", tax+1, "),4,1),2),3)"))
        }

        df <- data.frame(
          Level = forms,
          Family = rep(NA, length(taxa)),
          Genus = rep(NA, length(taxa)),
          Species = rep(NA, length(taxa)),
          ProxyName = taxa,
          stringsAsFactors = FALSE
        )

        class(df$Level) <- c(class(df$Level), "formula")

        openxlsx::addWorksheet(wb, "proxy_species_equivalency")
        openxlsx::writeData(wb, sheet = 1, x = df)
        openxlsx::saveWorkbook(wb, loc, overwrite = TRUE)
    }

    invisible()
}
