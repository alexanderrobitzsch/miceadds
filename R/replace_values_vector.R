## File Name: replace_values_vector.R
## File Version: 0.01
## File Last Change: 2017-08-24 10:02:00

replace_values_vector <- function(vec, vec_names, value)
{
	if ( length(vec_names) > 0 ){
		vec[ vec_names ] <- value
	}
	return(vec)
}
