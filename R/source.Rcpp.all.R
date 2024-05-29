## File Name: source.Rcpp.all.R
## File Version: 0.401

# function for sourcing Rcpp files
source.Rcpp.all <- function( path, file_names=NULL, ext="\\.cpp", excl="RcppExports",
    remove_temp_file=FALSE)
{
    path00 <- getwd()
    setwd(path)
    files0 <- files <- list.files( path, ext )
    ind <- grep( excl, files )
    if (length(ind)>0){
        files <- files[ - ind ]
    }
    if (is.null(file_names)){
        file_names <- strsplit( files, split='__')
        file_names <- unlist( lapply( file_names, FUN=function(ll){ ll[[1]][1] } ) )
    }
    #*** search corresponding files
    NF <- length(file_names)
    file_source <- NULL
    for (ff in 1L:NF){
        file_name <- file_names[ff]
        file <- grep( file_name, files0, value=TRUE, fixed=TRUE)
        file_source <- c( file_source, file )
    }
    file_source0 <- file_source

    s1 <- lapply( strsplit( file_source, split='__' ), FUN=function(ll){ ll[[1]][1] } )
    file_source <- unique(unlist(s1))

    nmax <- length(file_source)
    file_source <- file_source[ ! is.na( file_source )  ]
    file_source <- paste0( file_source, '__')

    #*** source Rcpp files
    NF <- length(file_source)
    string_header <- '[include_header_file]'

    for ( nn in seq_len(NF) ){
        file1 <- file_source[nn]
        file <- max( grep( file1, files, value=TRUE) )
        file0 <- file
        cat( paste0( '\n**** source ', file, ' \n\n') )
        utils::flush.console()
        out_ff <- readLines( file.path( path, file ) )
        ind_ff <- grep( string_header, out_ff, fixed=TRUE)
        rem <- FALSE
        for (ii in ind_ff){
            entry <- strsplit( out_ff[ii+1], split='\'' )[[1]]
            entry[2] <- gsub( '\\.h', '', entry[2] )
            files_ii <- list.files(path, '\\.h')
            e2 <- max( grep(entry[2], files_ii, value=TRUE) )
            entry2 <- paste0( entry[1], ' \'', e2, '\'' )
            out_ff[ii+1] <- entry2
            file0 <- '_temp.cpp'
            file00 <- list.files(path, file0)
            change <- TRUE
            if ( length(file00) > 0 ){
                out_ff00 <- readLines( file.path( path, file00 ) )
                if ( mean( out_ff00==out_ff )==1 ){
                    change <- FALSE
                }
            }
            if (change){
                writeLines(out_ff, file.path(path,file0) )
            }
            rem <- TRUE
        }
        Rcpp::sourceCpp( file.path( path, file0 ), showOutput=TRUE )
        if (rem & remove_temp_file){
            file.remove( file.path( path, file0 ) )
        }
        utils::flush.console()
    }
    setwd(path00)
}
