####### Baseline function for downloading supporting CPS Morg documentation from NBER
get_morg_docs <- function(years, save.locally, save.path){
  data('morg.link.table')
  
  if (missing(years)) years <- morg.link.table[!is.na(description.link), max(Year)]
  if (missing(save.locally)) save.locally <- FALSE
  if (missing(save.path) & save.locally == TRUE) {
    warning('No path given to save CPS MORG documentation. Defaulting to current working directory.')
    save.path <- getwd()
  }
  
  # allow user to input character vector or numeric vector of years
  if (class(years) != 'numeric') years <- as.numeric(years)
  # order years properly
  years.ordered <- years[order(years)]
  
  # Throw error if user passes invalid years
  if (sum(!years %in% morg.link.table[ , Year]) > 0) {
    failed.years <- years[!years %in% morg.link.table[ , Year]]
    failed.years <- paste(failed.years, collapse = ', ')
    stop(paste0("Sorry, you've asked for CPS MORG documentation of data for year(s): ", failed.years, '.', 
                " These are not available from the National Bureau of",
                " Economic Researchers."))
    
  }
  
  desc.table <- morg.link.table[Year %in% years]
  lapply(desc.table[ , description.link],
         FUN = function(url){
           if (save.locally){
             save.path <- paste0(save.path, '/',
                                 'cps.morg.doc_', 
                                 desc.table$Year[which(desc.table[, description.link] == url)],
                                 '.txt')
             download.file(url, destfile = save.path)
           }
           else url.show(url)
         })
}
