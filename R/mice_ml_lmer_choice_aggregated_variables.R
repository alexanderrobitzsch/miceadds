## File Name: mice_ml_lmer_choice_aggregated_variables.R
## File Version: 0.07


mice_ml_lmer_choice_aggregated_variables <- function( x_sel, clus, eps=1E-5)
{
    sd_sel <- GroupSD(data=x_sel, group=clus, extend=FALSE)
    sd0_sel <- ma.wtd.sdNA(data=x_sel )
    # variables which should be aggregated to a higher level
    vars_aggr <- which( colMeans(sd_sel[,-1, drop=FALSE]) / sd0_sel > eps )
    #---- output
    return(vars_aggr)
}
