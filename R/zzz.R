## File Name: zzz.R
## File Version: 0.161

#  zzz.R
#
version <- function(pkg="miceadds")
{
    lib <- dirname(system.file(package=pkg))
    d <- utils::packageDescription(pkg)
    return(paste(d$Package,d$Version,d$Date,lib))
}

.onAttach <- function(libname,pkgname)
{
    d <- utils::packageDescription('miceadds')
    v1 <- paste0( '* ', d$Package,' ', d$Version,' (',d$Date,')')
    packageStartupMessage(v1)
}
