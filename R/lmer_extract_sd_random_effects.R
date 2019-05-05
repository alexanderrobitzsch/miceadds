## File Name: lmer_extract_sd_random_effects.R
## File Version: 0.04

lmer_extract_sd_random_effects <- function(fit)
{
    require_namespace("lme4")
    object <- lme4::VarCorr(fit)
    vdd <- as.data.frame(object, order="lower.tri")
    pars <- vdd[,"sdcor"]
    return(pars)
}
