## File Name: save.data.R
## File Version: 0.384


#--- miceadds: saving data
save.data <- function( data, filename, type="Rdata", path=getwd(),
                row.names=FALSE, na=NULL, suffix=NULL, suffix_space="__",
                index=FALSE, systime=FALSE,  ...)
{
    if ("sav" %in% type){
        require_namespace("sjlabelled")
    }

    #***
    dir <- path
    file <- filename
    if ( ! is.null(suffix) ){
        file <- paste0( file, suffix_space, suffix )
    }
    #*** add index in data frame if requested
    if ( index){
        data <- index.dataframe(data, systime=systime)
    }

    file0 <- file
    #*** Rdata objects
    if ( "Rdata" %in% type  ){
        file <- save_data_calc_filename( file=file0, type="Rdata")
        save( data, file=file.path( dir, file ) )
    }
    #*** RDS objects
    if ( "RDS" %in% type  ){
        file <- save_data_calc_filename( file=file0, type="RDS")
        saveRDS( object=data, file=file.path( dir, file ) )
    }


    #*** csv2 objects
    if ("csv2" %in% type  ){
        if ( is.null(na)){ na <- "" }
        file <- save_data_calc_filename( file=file0, type="csv2")
        utils::write.csv2( x=data, file=file.path(dir,file),
                                row.names=row.names, na=na,... )
    }
    #*** csv objects
    if ("csv" %in% type ){
        if ( is.null(na)){ na <- "" }
        file <- save_data_calc_filename( file=file0, type="csv")
        utils::write.csv( x=data, file=file.path(dir,file),
                                row.names=row.names, na=na,  ... )
    }
    #*** table objects
    if ("table" %in% type ){
        if ( is.null(na)){ na <- "." }
        file <- save_data_calc_filename( file=file0, type="table")
        utils::write.table( x=data, file=file.path(dir,file), na=na,
                                row.names=row.names, ... )
    }
    #*** sav objects (SPSS objects)
    if ( "sav" %in% type ){
        file <- save_data_calc_filename( file=file0, type="sav")
        data <- sjlabelled::set_label( x=data, lab=attr(data, "variable.labels") )
        sjlabelled::write_spss( x=data, path=file.path( dir, file ) )
    }
}
