## File Name: grepvec_leading.R
## File Version: 0.02


grepvec_leading <- function( patternvec, x, value=FALSE )
{
    NP <- length(patternvec)
    ind <- NULL
    for (vv in 1:NP){
        ind1 <- grep_leading( pattern=patternvec[vv] , x )
        ind <- c( ind, ind1 )
    }
    ind <- sort(unique(ind))
    if (value){
        ind <- x[ind]
    }
    return(ind)
}
