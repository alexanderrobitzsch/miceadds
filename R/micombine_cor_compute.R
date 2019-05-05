## File Name: micombine_cor_compute.R
## File Version: 0.51


# subroutine for combining correlations for multiply imputed data
micombine_cor_compute <- function( cor.list, N, conf.level, nested=FALSE, Nimp=NULL,
        N_partial=0 )
{
    # convert correlations to Fisher transformed values
    # combination of point estimators according Rubin's formula
    if ( ! nested ){
        fisher.cor.list <- as.list( cor2fisher( cor.list) )
        var.fisher <- as.list( 1/( unlist(N) - N_partial -3) )
        if (Nimp>1){
            fisher.cor.combine <- mitools::MIcombine( fisher.cor.list, var.fisher)
            fisher.cor.combine$variance <- fisher.cor.combine$variance[1,1]
            fisher.cor.combine$fmi <- fisher.cor.combine$missinfo
        } else {
            fisher.cor.combine <- list("coef"=fisher.cor.list[[1]])
            fisher.cor.combine$variance <- var.fisher[[1]]
            fisher.cor.combine$fmi <- 0
        }
    }
    if ( nested ){
        cor.list <- unlist(cor.list)
        fisher.cor.list <- as.list( cor2fisher( cor.list) )
        fisher.cor.list <- List2nestedList(List=fisher.cor.list,
                        N_between=Nimp[1], N_within=Nimp[2] )
        var.fisher <- as.list( sqrt(1/( unlist(N) -3 - N_partial)), prod(Nimp)  )
        var.fisher <- List2nestedList(List=var.fisher,
                        N_between=Nimp[1], N_within=Nimp[2] )
        fisher.cor.combine <- NMIcombine( qhat=fisher.cor.list, se=var.fisher)
        fisher.cor.combine$coef <- fisher.cor.combine$qbar
        fisher.cor.combine$variance <- fisher.cor.combine$Tm[1,1]
        fisher.cor.combine$fmi <- fisher.cor.combine$lambda
    }

    zr <- fisher.cor.combine$coef
    zr.se <- sqrt( fisher.cor.combine$variance )
    t.zr <- zr / zr.se
    res <- c( "r"=fisher2cor(zr), "fisher_r"=zr,
                "fisher_rse"=zr.se, "fmi"=fisher.cor.combine$fmi,
                "t"=t.zr, "p"=2 * stats::pnorm( abs(t.zr), lower.tail=FALSE ),
                fisher2cor( zr + stats::qnorm( ( 1 - conf.level ) / 2 ) * zr.se ),
                fisher2cor( zr - stats::qnorm( ( 1 - conf.level ) / 2 ) * zr.se ) )
    names(res)[7] <- paste( "lower", round(100*conf.level,2),sep="")
    names(res)[8] <- paste( "upper", round(100*conf.level,2),sep="")
    res <- c( res, NA  )
    names(res)[9] <- "rse"
    res <- res[ c(1,9,2:8) ]
    res["rse"] <- fisher2cor.D1(zr)*zr.se
    return(res)
}

