
#' @export
.sqlite <- function(){
    return('/Users/mchevali1/Research/GBIF/gbif4crest_02.sqlite3')
}



#' @export
.exampleDB <- function(){

    if(base::file.exists(base::file.path(base::tempdir(), 'exampleDB.sqlite3'))) base::file.remove(base::file.path(base::tempdir(), 'exampleDB.sqlite3'))
    mydb <- RSQLite::dbConnect(RSQLite::SQLite(), base::file.path(base::tempdir(), 'exampleDB.sqlite3'))

    RSQLite::dbWriteTable(mydb, 'data_qdgc', dbEx[['data_qdgc']])
    RSQLite::dbWriteTable(mydb, 'geopolitical_units', dbEx[['geopolitical_units']])
    RSQLite::dbWriteTable(mydb, 'distrib_qdgc', dbEx[['distrib_qdgc']])
    RSQLite::dbWriteTable(mydb, 'biogeography', dbEx[['biogeography']])
    RSQLite::dbWriteTable(mydb, 'taxa', dbEx[['taxa']])
    RSQLite::dbWriteTable(mydb, 'typeofobservations', dbEx[['typeofobs']])

    RSQLite::dbDisconnect(mydb)

    return(base::file.path(base::tempdir(), 'exampleDB.sqlite3'))
}



#' @export
.ifExampleDB <- function(dbname){
    return(base::basename(dbname) == 'exampleDB.sqlite3')
}
