## File Name: mice_ml_lmer_extend_imputed_values_lower_level.R
## File Version: 0.03

mice_ml_lmer_extend_imputed_values_lower_level <- function( imp_upper ,
    ry_lower, ry_upper, level_ids_lower , level_ids_upper , extend=TRUE)
{
    if (extend){
        cly2 <- level_ids_upper[ ! ry_upper ]
        i1 <- match( level_ids_lower[ ! ry_lower ] , cly2 )
        imp <- imp_upper[ i1 ]
    } else {
        imp <- imp_upper
    }
    return(imp)
}
