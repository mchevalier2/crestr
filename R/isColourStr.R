
#' Test if R can interpret a string as a colour
#'
#' Test if R can interpret a string as a colour
#'
#' @param col The string to be tested.
#' @return A boolean value, TRUE if col is a valid colour, FALSE otherwise
#' @export
#' @examples
#' isColourStr('black')
#' isColourStr('blakc')
#'
isColourStr <- function(col) {
  sapply(col,
         function(X) {
            tryCatch(is.matrix(grDevices::col2rgb(X)),
                     error = function(e) FALSE)
          }
        )
}
