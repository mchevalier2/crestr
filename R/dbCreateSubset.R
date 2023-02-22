
#' Create a subset of the global calibration dataset
#'
#' Create a subset of the global calibration dataset
#'
#' @inheritParams crestObj
#' @inheritParams crest
#' @param dbname The name of the database. Default is \code{'gbif4crest_02'} and
#'        data will be extracted from the online database. The SQLite3 version
#'        of the database can also be used here by providing the complete path
#'        to a file ending by \code{.sqlite3}, e.g. \code{/path/to/file/gbif4crest_02.sqlite3}
#' @param out The name or path of the new dataset
#' @param verbose A boolean to print non-essential comments on the terminal
#'        (default \code{TRUE}).
#' @export
#' @seealso The full SQLite3 database can be downloaded from \url{https://figshare.com/articles/dataset/GBIF_for_CREST_database/6743207}.
#' @examples
#' \dontrun{
#'   dbSubset(2, xmn=0, xmx=15, ymn=0, ymx=15, out='example.sqlite3')
#' }
#'
dbSubset <- function( taxaType,
                      xmn = NA, xmx = NA, ymn = NA, ymx = NA,
                      dbname = "gbif4crest_02",
                      out = "gbif4crest_reduced",
                      verbose=TRUE) {


    if(base::missing(taxaType)) taxaType

    if(verbose) cat('\n## Prepping data for database extraction\n')

    if(verbose) cat('  <> Checking database connection .......... ')
    # If the 'crest_example' database is selected, we modify the parameter to
    # point to the sqlite3() database created in tmp()
    if (dbname == 'crest_example') {
        dbname <- .exampleDB()
    }
    if(!testConnection(dbname)) return(NA)

    if(verbose) cat('[OK]\n  <> Checking taxaType ..................... ')
    if(taxaType > 6 | taxaType < 0) {
        cat("[FAILED]\n\n")
        stop("'taxaType' should be an integer between 0 and 6. See ?crest.get_modern_data for more information.\n")
    }

    if(verbose) cat('[OK]\n  <> Checking coordinates .................. ')
    coords        <- check_coordinates(xmn, xmx, ymn, ymx)
    xmn           <- coords[1]
    xmx           <- coords[2]
    ymn           <- coords[3]
    ymx           <- coords[4]
    estimate_xlim <- coords[5]
    estimate_ylim <- coords[6]


    if(verbose) cat('[OK]\n  <> Extracting climate .................... ')

    if(tools::file_ext(out) != 'sqlite3') out <- paste(out, 'sqlite3', sep='.')

    if(base::file.exists(base::file.path(base::tempdir(), base::basename(out)))) base::file.remove(base::file.path(base::tempdir(), base::basename(out)))
    mydb <- RSQLite::dbConnect(RSQLite::SQLite(), base::file.path(base::tempdir(), base::basename(out)))

    req <- paste0("SELECT * FROM data_qdgc WHERE longitude >= ", xmn, " AND longitude <= ", xmx, " AND latitude >= ", ymn, " AND latitude <= ", ymx)
    tmp <- dbRequest(req, dbname)
    RSQLite::dbWriteTable(mydb, "data_qdgc", tmp, field.types=c('locid'='INTEGER', 'longitude'='REAL', 'latitude'='REAL', 'countryid'='INTEGER', 'oceanid'='INTEGER', 'terr_ecoid'='INTEGER', 'mari_ecoid'='INTEGER', 'elevation'='REAL', 'elev_min'='REAL', 'elev_max'='REAL', 'elev_range'='REAL', 'bio1'='REAL', 'bio2'='REAL', 'bio3'='REAL', 'bio4'='REAL', 'bio5'='REAL', 'bio6'='REAL', 'bio7'='REAL', 'bio8'='REAL', 'bio9'='REAL', 'bio10'='REAL', 'bio11'='REAL', 'bio12'='REAL', 'bio13'='REAL', 'bio14'='REAL', 'bio15'='REAL', 'bio16'='REAL', 'bio17'='REAL', 'bio18'='REAL', 'bio19'='REAL', 'ai'='REAL', 'sst_ann'='REAL', 'sst_jfm'='REAL', 'sst_amj'='REAL', 'sst_jas'='REAL', 'sst_ond'='REAL', 'sss_ann'='REAL', 'sss_jfm'='REAL', 'sss_amj'='REAL', 'sss_jas'='REAL', 'sss_ond'='REAL',  'diss_oxy'='REAL', 'nitrate'='REAL', 'phosphate'='REAL', 'silicate'='REAL', 'icec_ann'='REAL', 'icec_jfm'='REAL', 'icec_amj'='REAL', 'icec_jas'='REAL', 'icec_ond'='REAL'))

    LOCIDS <- unique(tmp$locid)
    GEOPOIDS <- c(unique(tmp$countryid), unique(tmp$oceanid))
    GEOPOIDS <- GEOPOIDS[!is.na(GEOPOIDS)]
    ECOIDS <- c(unique(tmp$'terr_ecoid'), unique(tmp$'mari_ecoid'))
    ECOIDS <- ECOIDS[!is.na(ECOIDS)]

    if(verbose) cat('[OK]\n  <> Extracting distributions .............. ')

    req <- paste0("SELECT * FROM distrib_qdgc WHERE locid IN (", paste(LOCIDS, collapse=', '), ") AND taxonid >= ", taxaType*1000000, " AND taxonid < ", (taxaType+1)*1000000)
    tmp <- dbRequest(req, dbname)
    RSQLite::dbWriteTable(mydb, "distrib_qdgc", tmp)

    TAXONIDS <- unique(tmp$taxonid)

    if(verbose) cat('[OK]\n  <> Extracting biogeography ............... ')

    req <- paste0("SELECT * FROM biogeography WHERE ecoid IN (", paste(ECOIDS, collapse=', '), ")")
    tmp <- dbRequest(req, dbname)
    RSQLite::dbWriteTable(mydb, "biogeography", tmp)

    if(verbose) cat('[OK]\n  <> Extracting geopolitical_units ......... ')

    req <- paste0("SELECT * FROM geopolitical_units WHERE geopoid IN (", paste(GEOPOIDS, collapse=', '), ")")
    tmp <- dbRequest(req, dbname)
    RSQLite::dbWriteTable(mydb, "geopolitical_units", tmp)

    if(verbose) cat('[OK]\n  <> Extracting typeofobservations ......... ')

    req <- paste0("SELECT * FROM typeofobservations")
    tmp <- dbRequest(req, dbname)
    RSQLite::dbWriteTable(mydb, "typeofobservations", tmp)

    if(verbose) cat('[OK]\n  <> Extracting taxa ....................... ')

    req <- paste0("SELECT * FROM taxa WHERE taxonid IN (", paste(TAXONIDS, collapse=', '), ")")
    tmp <- dbRequest(req, dbname)
    RSQLite::dbWriteTable(mydb, "taxa", tmp)

    RSQLite::dbDisconnect(mydb)

    base::file.copy(base::file.path(base::tempdir(), base::basename(out)), base::dirname(out))
    base::file.remove(base::file.path(base::tempdir(), base::basename(out)))

    if(verbose) {
      cat('[OK]\n')
      cat(paste0('## Data extraction completed.\n\n'))
    }

    return(invisible(base::file.path(base::dirname(out), base::basename(out))))
}
