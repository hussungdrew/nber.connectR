###### Function for reading in FARS data from NBER
read_fars <- function(years, types){
  
  data('fars.link.table')
  ## years, types default to most recent year's accident dataset
  if (missing(years)) years <- fars.link.table[ , max(Year)]
  if (missing(types)) types <- rep('accident', times = length(years))
  
  ## Exception handling
  # allow user to pass one 'types' argument to return it for all years
  if (length(types) == 1 & length(years) != 1) types <- rep(types, times = length(years))
  # handle numeric or character 'years' vector
  if (class(years) != 'numeric') years <- as.numeric(years)
  # Throw error if user passes invalid years
  if (sum(!years %in% fars.link.table[ , Year]) > 0) {
    failed.years <- years[!years %in% fars.link.table[ , Year]]
    failed.years <- paste(failed.years, collapse = ', ')
    stop(paste0("Sorry, you've asked for FARS data for year(s): ", failed.years, '.', 
                " These are not available from the National Bureau of",
                " Economic Researchers."))
    
  }
  # Throw error if user passes invalid types
  if (sum(!types %in% fars.link.table[ , dataset]) > 0) {
    failed.types <- types[!types %in% fars.link.table[ , dataset]]
    failed.types <- paste(failed.types, collapse = ', ')
    stop(paste0("Sorry, you've asked for the FARS dataset(s): ", failed.types, '.',
                " This is not a valid format for FARS data."))
  }
  
  # order years/types according to ordering in fars.link.table
  years.ordered <- years[order(years)]
  types.ordered <- types[order(years)]
  
  # unique keys to subset fars.link.table for later lapply()
  link.list <- paste0(years.ordered, types.ordered)
  
  # subset for lapply()ing
  link.subset <- fars.link.table %>% copy %>% 
    .[ , concat := paste0(Year, dataset)] %>%
    .[concat %in% link.list] 
  
  fars.return <- lapply(link.subset[ , url],
                        FUN = function(url){
                          
                          haven::read_dta(url) %>% as.data.table
                          
                        })
  # set informative names on fars.return
  names(fars.return) <- paste0('fars', years.ordered, '.', types.ordered)
  
  return(fars.return)
}