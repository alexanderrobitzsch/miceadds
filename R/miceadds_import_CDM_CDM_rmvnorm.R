## File Name: miceadds_import_CDM_CDM_rmvnorm.R
## File Version: 0.04

miceadds_import_CDM_CDM_rmvnorm <- function(...)
{
    require_namespace(pkg='CDM')
    res <- CDM::CDM_rmvnorm(...)
    return(res)
}
