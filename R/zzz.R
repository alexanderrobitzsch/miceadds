## File Name: zzz.R
## File Version: 0.10
## File Last Change: 2017-02-06 11:05:51
#  zzz.R
#
# This function is simply copied from mice package.

version <- function(pkg="miceadds"){
  lib <- dirname(system.file(package = pkg))
  d <- utils::packageDescription(pkg)
  return(paste(d$Package,d$Version,d$Date,lib))
}

# on attach TAM
.onAttach <- function(libname,pkgname){
  d <- utils::packageDescription("miceadds")
  packageStartupMessage( 
			paste0( "* " , d$Package," " , d$Version," (",d$Date,")") 
								)
}
