#' Convert abundance data into percentage data.
#'
#' Convert abundance data into percentage data.
#'
#' @param df The dataframe containing the data to convert.
#' @param col2convert A vector of the columns to convert. Default is all the
#'    columns but the first, which contains an age, a depth or a sampleID.
#' @return A vector of unique taxonIDs.
#' @export
#' @examples
#' df = data.frame(matrix(1:25, ncol = 5))
#' colnames(df) = paste( rep("col", 5), 1:5, sep = '')
#' convert2percentages(df)
#' convert2percentages(df, col2convert = 3:5)

convert2percentages <- function (df, col2convert = 2:ncol(df) ) {
    df2 <- cbind( df[, -col2convert],
                  100 * df[, col2convert] / apply(df[, col2convert], 1, sum)
                 )
    colnames(df2) <- colnames(df)
    rownames(df2) <- rownames(df)
    df2
}
