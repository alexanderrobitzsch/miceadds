## File Name: load.data.R
## File Version: 0.492



#--- miceadds::load.data: load conveniently R objects of different data formats
load.data <- function( filename, type=NULL, path=getwd(), load_fun=NULL,
        spss.default=TRUE, ...)
{
    dir <- path
    file <- filename
    file0 <- NULL
    type0 <- type
    if (! is.null(load_fun)){
        type <- "user_"
        file0 <- file
    }
    i1 <- grep.vec( c("RDS", "Rdata", "RData",
                        "csv", "csv2", "table", "sav", "xls",
                    "xlsx", type0 ), x=file, operator="OR" )$x
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
        require_namespace("foreign")
    }
    if (type %in% c("xls", "xlsx")){
        require_namespace("readxl")
    }
    type1 <- type
    if ( type=="table" ){
        files <- grep.vec( c("dat","txt"), files, "OR" )$x
        type1 <- "dat"
    }
    if (type %in% c("csv2","csv", "CSV", "CSV2")){
        files <- grep( gsub("csv2","csv", type1), files, value=TRUE)
        type1 <- "csv"
    }
    file1 <- max( grep( type1, files, value=TRUE) )
    if ( is.na(file1) ){
        file <- files
    } else {
        file <- file1
    }
    file <- max(file)
    cat( paste0( "*** Load ", file, "\n"))

    #*** Rdata objects
    if (type %in% c("Rdata","RData") ){
        dat4 <- load.Rdata2( filename=file, path=dir )
    }
    #*** Rdata objects
    if (type %in% c("RDS") ){
        dat4 <- load.Rdata2( filename=file, path=dir, RDS=TRUE )
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
    #*** xlsx objects
    if (type %in% c("xls","XLS","xlsx","XLSX")){
        dat4 <- readxl::read_excel( path=file_path(dir,file), ... )
        dat4 <- as.data.frame(dat4)
    }
    #*** user-defined loading function
    if (type %in% c("user_")){
        args <- list(file_path(dir,file), ...)
        dat4 <- do.call( what=load_fun, args=args)
    }
    #--- output
    return(dat4)
}
