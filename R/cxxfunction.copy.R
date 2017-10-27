## File Name: cxxfunction.copy.R
## File Version: 1.03


cxxfunction.copy <- function( cppfct , name ){
    TAM::require_namespace_msg("inline")
    g1 <- inline::getDynLib(cppfct)
    cppname <- gsub( "\\.dll" , "\\.cpp" ,  g1[["path"]] )
    h1 <- readLines( cppname )
    tempname <- g1[["name"]]
    h1 <- gsub( tempname , name , h1 )
	h1 <- c( paste0( "//  Code created: " , Sys.time() ) , "" , h1 )
	name1 <- paste0( tolower(name) , ".cpp" )
    writeLines( h1 , name1 )
	crlrem( filename1=name1 , filename2=name1 )
}
