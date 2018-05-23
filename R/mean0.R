## File Name: mean0.R
## File Version: 0.07


mean0 <- function(x, na.rm=TRUE){
    sd1 <- stats0( x=x, FUN=mean, na.rm=na.rm )
    return(sd1)
}
