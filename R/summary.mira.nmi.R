## File Name: summary.mira.nmi.R
## File Version: 0.071


#-- summary function for mira.nmi object
summary.mira.nmi <- function(object, ...)
{
    B <- object$Nimp['between']
    W <- object$Nimp['within']
    for (bb in 1L:B){
        for (i in 1L:W){
            cat('\n', '## summary of imputation: between',
                    bb, '- within', i, ':\n')
            print(summary(object$analyses[[bb]][[i]], ...), ...)
        }
    }
}

