## File Name: summary.lmer_pool.R
## File Version: 0.12


summary.lmer_pool <- function( object, digits=4, file=NULL, ...)
{
    require_namespace(pkg="CDM")

    # open sink
    CDM::osink( file=file, suffix=paste0( "__SUMMARY.Rout") )
    x <- object

    table <- data.frame( est=x$qbar )
    table$se <- sqrt( diag(x$Tm) )
    table$t <- table[, 1]/table[, 2]
    table$df <- x$df

    table$p <- 2 * (1 - stats::pt(abs(table$t), x$df))
    table$`lo 95` <- table$est - stats::qt(0.975, x$df) * table$se
    table$`hi 95` <- table$est + stats::qt(0.975, x$df) * table$se
    table$fmi <- x$lambda
    table$fmi_Betw <- x$lambda_Between
    table$fmi_Within <- x$lambda_Within
    table <- as.data.frame(table)
    if ( is.na(table$se)[1] ){
            table$df <- NA
                        }
    if ( ! is.null( object$u_NULL ) ){
        if ( object$u_NULL){
            table <- table[, "est", drop=FALSE ]
        }
    }
    table0 <- table
    for (vv in seq(1, ncol(table) ) ){
        table[,vv] <- round( table[,vv], digits=digits )
    }
    print(table)
    return(table0)

    # close sink
    CDM::csink( file=file )
}
#*******************************************************
