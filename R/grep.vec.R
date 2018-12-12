## File Name: grep.vec.R
## File Version: 1.13

##################################################
# vector version of grep
grep.vec <- function( pattern.vec, x, operator="AND", ...)
{
    x0 <- x
    xv <- NULL
    for (vv in seq_len( length(pattern.vec) ) ){
        if (operator=="AND"){
            x <- x[ grep( pattern.vec[vv], x, ... ) ]
        } else {
            xv <- union( xv,x0[ grep( pattern.vec[vv], x0, ... ) ] )
        }
    }
    if (operator !="AND"){ x <- xv }
    index.x <- which( x0 %in% x )
    res <- list( "x"=x, "index.x"=index.x )
    return(res)
}
##################################################
