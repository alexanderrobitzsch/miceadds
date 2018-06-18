## File Name: plot.ml_mcmc.R
## File Version: 0.02

plot.ml_mcmc <- function( x, ask=TRUE, ...)
{
    plot( x$sampled_values, ask=ask, ...)
}
