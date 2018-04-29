## File Name: source.all.R
## File Version: 1.02

#**********************************************************
# function for sourcing all files within a directory
source.all <- function( path , grepstring= "\\.R" , print.source=TRUE ){
    files <- list.files( path  ) 
    files <- grep.vec(  grepstring , files , "OR")$x
    for ( ff in files ){ 
        source( file.path( path , ff ) ) 
        if ( print.source ){ 
            cat( paste( "*** source" , ff ) , "\n") 
            utils::flush.console()
        }
    }
}
#**********************************************************
