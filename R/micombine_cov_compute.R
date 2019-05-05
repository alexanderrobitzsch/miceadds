## File Name: micombine_cov_compute.R
## File Version: 1.21


#**** subroutine for combining covariances for multiply imputed data
micombine_cov_compute <- function( cor.list, N, conf.level, nested=FALSE, Nimp=NULL )
{
    # convert correlations to Fisher transformed values
    # combination of point estimators according Rubin's formula
    if ( ! nested ){
        qhat <- lapply( cor.list, FUN=function(cc){ cc$est } )
        u <- lapply( cor.list, FUN=function(cc){ matrix(cc$se^2,1,1) } )
        if (Nimp>1){
            fisher.cor.combine <- mitools::MIcombine( qhat, u )
            fisher.cor.combine$variance <- fisher.cor.combine$variance[1,1]
            fisher.cor.combine$fmi <- fisher.cor.combine$missinfo
        } else {
            fisher.cor.combine <- list(coef=qhat[[1]])
            fisher.cor.combine$variance <- u[[1]][1,1]
            fisher.cor.combine$fmi <- 0
        }
    }

    if ( nested ){
        qhat <- lapply( cor.list, FUN=function(cc){
                    lapply( cc, FUN=function(dd){ dd$est } ) } )
        se <- lapply( cor.list, FUN=function(cc){
                    lapply( cc, FUN=function(dd){ dd$se } ) } )
        fisher.cor.combine <- NMIcombine( qhat=qhat, se=se )
        fisher.cor.combine$coef <- fisher.cor.combine$qbar
        fisher.cor.combine$variance <- fisher.cor.combine$Tm[1,1]
        fisher.cor.combine$fmi <- fisher.cor.combine$lambda
    }
    zr <- fisher.cor.combine$coef
    zr.se <- sqrt( fisher.cor.combine$variance )
    t.zr <- zr / zr.se
    res <- c( cov=zr, cov_se=zr.se, fmi=fisher.cor.combine$fmi,
                t=t.zr, p=2*stats::pnorm( abs(t.zr), lower.tail=FALSE ),
                zr + stats::qnorm( ( 1 - conf.level ) / 2 ) * zr.se,
                zr - stats::qnorm( ( 1 - conf.level ) / 2 ) * zr.se  )
    names(res)[6] <- paste( "lower", round(100*conf.level,2),sep="")
    names(res)[7] <- paste( "upper", round(100*conf.level,2),sep="")
    return(res)
}

