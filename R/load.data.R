## File Name: load.data.R
## File Version: 0.453



#--- miceadds::load.data: load conveniently R objects of different data formats
load.data <- function( filename, type=NULL, path=getwd(), spss.default=TRUE, ...)
{
    #*** the resulting object is dat4!
    dir <- path
    file <- filename
    i1 <- grep.vec( c("Rdata", "RData", "csv", "csv2", "table", "sav" ), file, "OR" )$x
    if ( length(i1)==0 ){
        files <- list.files( dir, filename )
        files <- grep.vec( filename, files, "AND")$x
    } else {
        files <- file
    }
    if (is.null(type)){
        s1 <- strsplit(files, split=".", fixed=TRUE)[[1]]
        type <- s1[ length(s1) ]
    }
    if (type=="sav"){
        TAM::require_namespace_msg("foreign")
    }
    type1 <- type
    if ( type=="table" ){
        files <- grep.vec( c("dat","txt"), files, "OR" )$x
        type1 <- "dat"
    }
    files <- grep( gsub("csv2","csv", type1), files, value=TRUE)
    file <- max(files)
    cat( paste0( "*** Load ", file, "\n"))

    #*** Rdata objects
    if (type %in% c("Rdata","RData") ){
        dat4 <- load.Rdata2( filename=file, path=dir )
    }
    #*** csv2 objects
    if (type %in% c("csv2","CSV2")){
        dat4 <- utils::read.csv2( file=file_path(dir,file), ... )
    }
    #*** csv objects
    if (type %in% c("csv","CSV")){
        dat4 <- utils::read.csv( file=file_path(dir,file), ... )
    }
    #*** table objects
    if (type=="table"){
        dat4 <- utils::read.table( file=file_path(dir,file), header=TRUE, ... )
    }
    #*** sav objects (SPSS objects)
    if (type %in% c("sav","SAV") ){
        if ( ! spss.default){
            dat4 <- foreign::read.spss( file=file_path(dir,file), ... )
        } else {
            dat4 <- foreign::read.spss( file=file_path(dir,file), to.data.frame=TRUE,
                        use.value.labels=FALSE, ... )
        }
    }
    return(dat4)
}
