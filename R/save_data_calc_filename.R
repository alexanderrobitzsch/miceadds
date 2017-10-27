## File Name: save_data_calc_filename.R
## File Version: 0.01


save_data_calc_filename <- function( file , type)
{
    type2 <- type 
	if ( type == "csv2" ){ 
		type2 <- "csv" 
	}
	if ( type == "table" ){ 
		type2 <- "dat" 
	}
	i1 <- grep( type2 , file )
	i1 <- grep( paste0("\\.",type2) , file , fixed=TRUE)	
	if ( length(i1) == 0 ){
		file <- paste0( file , "." , type2 )
	}
	return(file)
}


calc_filename <- save_data_calc_filename
