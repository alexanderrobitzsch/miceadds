## File Name: miceadds_weighted_centering.R
## File Version: 0.04

miceadds_weighted_centering <- function(x, w)
{
    if (is.matrix(x)){
        n <- nrow(x)
        cx <- colSums(x*w) / sum(w)
        xs <- x - sirt::sirt_matrix2(cx, nrow=n)
    } else {
        xs <- x - stats::weighted.mean(x, w=w)
    }
    return(xs)
}
