## File Name: mice_imputation_pls_print_progress3.R
## File Version: 0.02
## File Last Change: 2017-02-06 11:05:49

mice_imputation_pls_print_progress3 <- function(pls.print.progress , time1 , time2 )
{
    if( pls.print.progress ){  
        cat( "\nMissing Data Draws finished " , substring(Sys.time(),1) ,"\n" ) 
		utils::flush.console() 
		cat( "Time elapsed:" , print(time2 - time1 ) , "|" , time1 , "|" , time2 )
        cat( "\n...................PLS....................................\n")
		utils::flush.console() 
    }
}
