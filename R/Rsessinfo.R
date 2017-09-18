## File Name: Rsessinfo.R
## File Version: 0.03
## File Last Change: 2017-02-06 11:05:50


####################################################
# R session info
Rsessinfo <- function(){
    si <- Sys.info()
    si2 <- utils::sessionInfo()
    paste0( si2$R.version$version.string , " " , si2$R.version$system 
             , " | nodename = " , si["nodename"] , " | login = " , si["login"] )
            }
####################################################
