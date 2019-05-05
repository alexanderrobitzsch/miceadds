## File Name: vector2matrix.R
## File Version: 0.11


#-- converts a vector into a matrix
vector2matrix <- function( index1, index2, val, empty_val=NA )
{
    index1 <- paste(index1)
    index2 <- paste(index2)
    vars1 <- unique( c( index1, index2 ) )
    VV <- length(vars1)
    m1 <- matrix( empty_val, nrow=VV, ncol=VV)
    colnames(m1) <- rownames(m1) <- vars1
    h1 <- cbind( match( index1, vars1 ), match( index2, vars1 ) )
    m1[ h1 ] <- val
    return(m1)
}
