## File Name: summary.lmer_pool.R
## File Version: 0.05


summary.lmer_pool <- function( object, digits=4, file=NULL, ...)
{
    # open sink
    CDM::osink( file=file, suffix=paste0( "__SUMMARY.Rout") )
    
#    class(object) <- "mipo.nmi"
#    res <- summary.mipo.nmi(object, ...)    
#    return(res)

    x <- object
    # table <- array( x$qbar, dim=c(length(x$qbar), 10) )
    # dimnames(table) <- list( labels(x$qbar),
    #        c("est", "se", "t", "df", "Pr(>|t|)", "lo 95", "hi 95",
    #                "fmi", "fmi_Betw", "fmi_Within"))
                    
    table <- data.frame( est=x$qbar )                
    table$se <- sqrt( diag(x$Tm) )
    table$t <- table[, 1]/table[, 2]        
    table$df <- x$df
    

    
    table$p <- 2 * (1 - stats::pt(abs(table$t), x$df))
    table$`lo 95` <- table$est - stats::qt(0.975, x$df) * table$se
    table$`hi 95` <- table$est + stats::qt(0.975, x$df) * table$se    
    
#    if (is.null(x$nmis) | is.null(names(x$qbar)))
#        table[, 8] <- NA else table[, 8] <- x$nmis[names(x$qbar)]
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
