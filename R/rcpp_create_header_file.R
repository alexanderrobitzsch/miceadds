## File Name: rcpp_create_header_file.R
## File Version: 0.17

rcpp_create_header_file <- function(file_name, pack=NULL, path=getwd() )
{
    files <- list.files( path, "\\.cpp")
    file_name1 <- max( grep( file_name, files, value=TRUE ) )

    out <- readLines( file.path( path, file_name1 ) )
    out <- out[ substring(out,1,2) !="//" ]
    ind1 <- grep_leading( "#include", out )
    ind2 <- grep_leading( "using namespace", out )
    ind <- c( ind1, ind2 )
    res1 <- out[ind]
    #*** output types for functions
    vec <- c("double", "bool", "int", "arma::", "Rcpp::", "char",
                "Integer", "Numeric", "List")
    ind <- grepvec_leading(vec, out )
    paren <- grep( ")", out, fixed=TRUE )
    NF <- length(ind)
    res2 <- NULL
    for (ff in seq_len(NF) ){
        ind_ff <- ind[ff]
        seq_ff <- seq( ind_ff, min( paren[ paren >=ind_ff ] ) )
        res_ff <- out[ seq_ff ]
        res_ff <- gsub( ")", ");", res_ff, fixed=TRUE )
        res2 <- c( res2, "", res_ff )
    }
    file_name0 <- strsplit( file_name1, split="__" )[[1]][1]
    pack1 <- paste0( "_", pack )
    if ( is.null(pack) ){
        pack1 <- ""
    }
    header_name <- gsub("\\.", "", toupper( paste0( pack1, "_", file_name0, "_H" ) ) )
    res0 <- paste0( "#ifndef ", header_name )
    res0[2] <- paste0( "#define ", header_name )
    res3 <- c( "", res0, " ", res1, res2, "", paste0( "#endif // ", header_name ) )
    file_name2 <- gsub("\\.cpp", "\\.h", file_name1)
    writeLines( res3, file.path( path, file_name2) )
}
