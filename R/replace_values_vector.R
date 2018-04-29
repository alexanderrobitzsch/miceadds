## File Name: replace_values_vector.R
## File Version: 0.01

replace_values_vector <- function(vec, vec_names, value)
{
    if ( length(vec_names) > 0 ){
        vec[ vec_names ] <- value
    }
    return(vec)
}
