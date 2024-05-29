## File Name: miceadds_weighted_scaling_y.R
## File Version: 0.03

miceadds_weighted_scaling_y <- function(y, w)
{
    require_namespace('TAM')
    y <- miceadds_weighted_centering(x=y, w=w)
    y <- y / TAM::weighted_sd(y, w=w)
    return(y)
}
