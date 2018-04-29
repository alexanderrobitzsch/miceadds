## File Name: mice_imputation_pls_do_impute.R
## File Version: 0.02

mice_imputation_pls_do_impute <- function( x , y , ry , imputationWeights , 
    use_weights , pls.impMethod , pls.print.progress ,
    pls.impMethodArgs , type , ... )
{
    #*** logical whether an imputation should be conducted
    do_imputation <- ( pls.impMethod != "xplsfacs" )
    
    if ( do_imputation ){           
        if ( use_weights ){   # if there exists a real sample weight vector
            x <- cbind(1, as.matrix(x) )
            xobs <- x[ry,]
            yobs <- y[ry]
            weights.obs <- imputationWeights[ ry   ]
            weights.obs <- normalize_vector( weights.obs )
            # check appropriate imputation method
            if ( ! pls.impMethod %in% c( "norm" , "pmm" ) ){
                stop( paste0( "Only imputation methods 'norm' and 'pmm' can be " ,
                        "applied when weights are provided.\n") )                    
            }
            
            # draw regression coefficients     
            parm <- mice_imputation_weighted_norm_draw( yobs = yobs , xobs = xobs , 
                        ry = ry , y = y , x = x , weights.obs = weights.obs , ... )                     
            if (pls.impMethod == "norm" ){ 
                x1 <- x[  ! ry, ] %*% parm$beta + stats::rnorm(sum(!ry)) * parm$sigma
            }        
            if (pls.impMethod == "pmm" ){ 
                yhatobs <- x[ry, ] %*% parm$coef
                yhatmis <- x[!ry, ] %*% parm$beta
                x1 <- apply(as.array(yhatmis), 1, .pmm.match, 
                            yhat = yhatobs, y = y[ry], ... )
            }
            do_imputation <- FALSE
        }
    }
    
    # do Imputation
    if( pls.print.progress  ){  
        cat( "\n" , paste( "Imputation Method " , pls.impMethod , sep="") , "\n" ) 
    }
    
    #-- apply imputation routine            
    if ( do_imputation){
        args <- pls.impMethodArgs
        args$x <- x
        args$y <- y
        args$ry <- ry
        args$type <- type
        impMethod <- paste0("mice.impute." , pls.impMethod)
        x1 <- do.call( impMethod , args )    
    }
        
    #-- no imputation
    if ( pls.impMethod == "xplsfacs" ){ 
        x1 <- x   
    }
        
    return(x1)            
}        
