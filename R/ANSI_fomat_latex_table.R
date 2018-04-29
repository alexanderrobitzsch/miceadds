## File Name: ANSI_fomat_latex_table.R
## File Version: 0.11

ANSI_format_latex_table <- function(dat, round=NULL,
    phantom_minus = FALSE, phantom_digit = -Inf, 
    first_cols = NULL, last_col = "\\\\" ,
    bold_lower = NULL , bold_upper = NULL , 
    latex_first = NULL , latex_last = NULL )
{
    # round ... digits for rounding
    NR <- nrow(dat)
    NC <- ncol(dat)    
        
    if ( length(phantom_digit) < NC){
        phantom_digit <- rep( phantom_digit , NC )
    }        
        
    if (! is.null(first_cols) ){
        NFC <- length(first_cols)        
    } else {
        NFC <- 0
    }
    if (is.null(bold_lower) ){
        bold_lower <- rep(-Inf,NC)
    }
    if (is.null(bold_upper) ){
        bold_upper <- rep(Inf,NC)
    }    
    v1 <- latex_first
    for (rr in 1:NR){ #rr <- 2            
        v2 <- NULL        
        if (NFC > 0){
            for (ff in 1:NFC){
                v2 <- paste0( v2 , first_cols[[ff]][[rr]] , " " , " & " )        
            }
        }                        
        for (cc in 1:NC){    # cc <- 1        


        
            if ( ! is.null(round) ){
                dat_rr_cc <- round( dat[rr,cc] , round[cc] )
                h1 <- sprintf( paste0("%." , round[cc] , "f" ) , dat_rr_cc )
                if (h1 == "-0"){
                    h1 <- "0"
                }
            }                        
                        
            z1 <- dat_rr_cc            
            z2 <- abs(dat_rr_cc)
            g1 <- log( z2 , 10 )
        
        
#            if ( ! phantom_digit[cc] ){
                if ( g1 < phantom_digit[cc] ){
                    h1 <- paste0("\\phantom{1}",h1)        
                }
#            }    
            # include phantom symbol for minus sign                                
            if (phantom_minus[cc] & ( dat_rr_cc >= 0 ) ){
                h1 <- paste0("\\phantom{-}",h1)        
            }        
                        
            # bold
            if ( dat_rr_cc < bold_lower[cc]){
                h1 <- paste0("\\mathbf{" , h1 , "}")
            }
            if ( dat_rr_cc > bold_upper[cc]){
                h1 <- paste0("\\mathbf{" , h1 , "}")
            }                        
            # math environment
            h1 <- paste0("$" , h1 , "$" )
            if (cc < NC){
                g1 <- "&"
            } else {
                g1 <- " "
            }                    
            v2 <- paste0( v2 , " " ,  h1 , " " , g1)
            
        }
        v2 <- paste0( v2 , " " , last_col[rr] )
        v1 <- c( v1 , v2)
    }    
    if ( ! is.null(latex_last) ){
        v1 <- c( v1 , latex_last)
    }    
    return(v1)
}
