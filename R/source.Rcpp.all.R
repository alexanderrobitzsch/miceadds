## File Name: source.Rcpp.all.R
## File Version: 0.13

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
    file_source <- NULL
    for (ff in 1:NF){
        file_name <- file_names[ff]
        file <- list.files( path , file_name ) 
        file_source <- c( file_source, file )
    }
    s1 <- lapply( strsplit( file_source, split = "__" ), FUN = function(ll){ ll[[1]][1] } )    
    file_source <- unique(unlist(s1))
    
    nmax <- length(file_source)
    file_source <- file_source[ ! is.na( file_source )  ]
    file_source <- paste0( file_source, "__")    
        
    #*** source Rcpp files
    NF <- length(file_source)
    for ( nn in 1:NF){
        file1 <- file_source[nn]
        file <- max( grep( file1 , files , value=TRUE) )
        cat( paste0( "\n**** source " , file , " \n\n") )
        utils::flush.console()
        Rcpp::sourceCpp( file.path( path , file ) , showOutput = TRUE )
        utils::flush.console()
    }
}




