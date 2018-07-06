## File Name: lmer_extract_sd_random_effects.R
## File Version: 0.03

lmer_extract_sd_random_effects <- function(fit)
{
    object <- lme4::VarCorr(fit)
    vdd <- as.data.frame(object, order="lower.tri")
    pars <- vdd[,"sdcor"]
    return(pars)
}
