##### Baseline function for reading in CPS Morg data
read_cps_morg <- function(years){
  data('morg.link.table')
  if (missing(years)) years <- morg.link.table[nrow(morg.link.table), Year]
  
  morg.return <- lapply(X = morg.link.table[Year %in% years, url],
                        FUN = function(x){
                          as.data.table(haven::read_dta(file = x))
                        }
  )
  names(morg.return) <- paste0('cps.morg', years)
  return(morg.return)
}
