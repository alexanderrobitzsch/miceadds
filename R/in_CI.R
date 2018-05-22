## File Name: in_CI.R
## File Version: 0.05

in_CI <- function( est, se, true, level=.95, df=Inf)
{
    quant <- - stats::qt( (1-level)/2, df=df)
    in_ci <- ( est - quant * se < true ) & ( est + quant * se > true )
    return(in_ci)
}
