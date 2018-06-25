## File Name: mice.impute.eap.R
## File Version: 2.07

mice.impute.eap <- function (y, ry, x, eap, ...)
{
    pos <- parent.frame(n=1)
    res <- mice_imputation_get_states(pos=pos)
    vname <- res$vname
    newstate <- res$newstate
    M.scale <- eap[[ vname ]][[ "M" ]]
    SE.scale <- eap[[ vname ]][[ "SE" ]]
    N <- length(M.scale)
    ximp <- stats::rnorm( N, mean=M.scale, sd=SE.scale )
    return(ximp)
}
