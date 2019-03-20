##### Baseline function for reading in CPS Morg data
read_cps_morg <- function(years){
  data('morg.link.table')
  # default to most recent year of data
  if (missing(years)) years <- morg.link.table[nrow(morg.link.table), Year]
  
  # allow user to input character vector or numeric vector of years
  if (class(years) != 'numeric') years <- as.numeric(years)
  
  # Throw error if user passes invalid years
  if (sum(!years %in% morg.link.table[ , Year]) > 0) {
    failed.years <- years[!years %in% morg.link.table[ , Year]]
    failed.years <- paste(failed.years, collapse = ', ')
    stop(paste0("Sorry, you've asked for CPS MORG data for year(s): ", failed.years, '.', 
                " These are not available from the National Bureau of",
                " Economic Researchers."))
    
  }
  years.ordered <- years[order(years)]
  
  morg.return <- lapply(X = morg.link.table[Year %in% years, url],
                        FUN = function(x){
                          as.data.table(haven::read_dta(file = x))
                        }
  )
  names(morg.return) <- paste0('cps.morg', years.ordered)
  return(morg.return)
}
