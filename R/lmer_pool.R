## File Name: lmer_pool.R
## File Version: 0.07


lmer_pool <- function( models, level=.95, ...)
{
    lmer_pool_wrapper(models=models, level=level, FUN=lmer_vcov, ...)
}
