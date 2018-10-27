#' Create a 2-column matrix from two vectors
#' @param X one array of length n
#' @param Y another array of length n
#' @examples
#' createPos(c(1,2,3), c(1,4,9))
#' @return A 2-column numerical matrix
#' @export
#' @importFrom stats na.omit 

# Helper to create a 2-column matrix from two vectors
createPos <- function(X, Y) {
    return( 
        cbind(matrix(na.omit(X)), na.omit(Y))
    );
}
