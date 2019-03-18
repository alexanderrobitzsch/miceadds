## File Name: inverse_logit.R
## File Version: 0.05

inverse_logit <- function(p)
{
    # y <-  1 / ( 1 + exp(-p) )
    y <- stats::plogis(p)
    return(y)
}
