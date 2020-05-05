## File Name: subset_datlist_modify_factor_levels.R
## File Version: 0.03


subset_datlist_modify_factor_levels <- function(dat)
{
    V <- ncol(dat)
    for (vv in 1:V){
        v1 <- dat[[vv]]
        if (is.factor(v1)){
            m1 <- sort(unique(v1))
            ind <- match(m1, levels(v1))
            h1 <- levels(v1)[ind]
            h2 <- as.numeric(v1)
            v2 <- as.factor(h2)
            levels(v2) <- h1
            dat[[vv]] <- v2
        }
    }
    return(dat)
}
