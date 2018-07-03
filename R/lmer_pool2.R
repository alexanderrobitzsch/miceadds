## File Name: lmer_pool2.R
## File Version: 0.03

lmer_pool2 <- function( models, level=.95, ...)
{
    lmer_pool_wrapper(models=models, level=level, FUN=lmer_vcov2, ...)
}
