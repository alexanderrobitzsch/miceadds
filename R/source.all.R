## File Name: source.all.R
## File Version: 1.14

#**********************************************************
# function for sourcing all files within a directory
source.all <- function( path, grepstring="\\.R", print.source=TRUE, file_sep="__" )
{
    files <- list.files(path)
    files <- grep.vec( grepstring, files, "OR")$x
    dfr <- filename_split_vec( file_names=files, file_sep=file_sep, file_ext="\\.R")
    dfr <- dfr[ ! duplicated(dfr$stem, fromLast=TRUE), ]
    files <- paste(dfr$file_name)
    for ( ff in files ){
        source( file.path( path, ff ) )
        if ( print.source ){
            cat( paste( "*** source", ff ), "\n")
            utils::flush.console()
        }
    }
}
#**********************************************************
