## File Name: mice_imputation_multilevel_lmerControl_define_optimizer.R
## File Version: 0.08

mice_imputation_multilevel_lmerControl_define_optimizer <- function(model,
            optimizer="bobyqa", ...)
{
    require_namespace("lme4")
    args <- list(...)
    if ( ! ( "control" %in% names(args) ) ){
        if (model=="binary"){
            control <- lme4::glmerControl(optimizer=optimizer)
        } else {
            control <- lme4::lmerControl(optimizer=optimizer)
        }
    }
    return(control)
}
