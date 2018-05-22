## File Name: min0.R
## File Version: 0.05


min0 <- function(x, na.rm=TRUE){
    sd1 <- stats0( x=x, FUN=min, na.rm=na.rm )
    return(sd1)
}
