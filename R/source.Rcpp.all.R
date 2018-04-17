## File Name: source.Rcpp.all.R
## File Version: 0.03

# function for sourcing Rcpp files
source.Rcpp.all <- function( path, file_names=NULL, ext = "\\.cpp", excl = "RcppExports" )
{
    files <- list.files( path, ext )
	ind <- grep( excl, files )
	if (length(ind)>0){
		files <- files[ - ind ]
	}
	
	if (is.null(file_names)){
		file_names <- strsplit( files, split="__")
		file_names <- unlist( lapply( file_names , FUN = function(ll){ ll[[1]][1] } ) )
	}
	
	#*** search corresponding files
	NF <- length(file_names)
	file_source <- rep("" , NF )
	for (ff in 1:NF){
        file_name <- file_names[ff]
		file <- max( list.files( path , file_name ) )
		file_source[ff] <- file
	}
	nmax <- length(file_source)
	file_source <- file_source[ ! is.na( file_source )  ]
		
	#*** source Rcpp files
	NF <- length(file_source)
	for ( nn in 1:NF){
		file <- file_source[nn]
		cat( paste0( "\n**** source " , file , " \n\n") )
		utils::flush.console()
		Rcpp::sourceCpp( file.path( path , file ) , showOutput = TRUE )
		utils::flush.console()
	}
}




