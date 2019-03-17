####### Function for reading in FARS supporting documentation from NBER
get_fars_docs <- function(years, types, save.locally, save.path){
  
  data('fars.link.table')
  ## years, types default to most recent year's accident dataset
  if (missing(years)) years <- fars.link.table[ , max(Year)]
  if (missing(types)) types <- rep('accident', times = length(years))
  ## default to not save file locally
  if (missing(save.locally)) save.locally <- FALSE
  if (missing(save.path) & save.locally){
    warning('No path given to save FARS documentation. Defaulting to current working directory.')
    save.path <- getwd()
  }
  
  
  ## Exception handling
  # allow user to pass one 'types' argument to return it for all years
  if (length(types) == 1 & length(years) != 1) types <- rep(types, times = length(years))
  # handle numeric or character 'years' vector
  if (class(years) != 'numeric') years <- as.numeric(years)
  # Throw error if user passes invalid years
  if (sum(!years %in% fars.link.table[ , Year]) > 0) {
    failed.years <- years[!years %in% fars.link.table[ , Year]]
    failed.years <- paste(failed.years, collapse = ', ')
    stop(paste0("Sorry, you've asked for documentation for FARS data for year(s): ", failed.years, '.', 
                " These are not available from the National Bureau of",
                " Economic Researchers."))
    
  }
  # Throw error if user passes invalid types
  if (sum(!types %in% fars.link.table[ , dataset]) > 0) {
    failed.types <- types[!types %in% fars.link.table[ , dataset]]
    failed.types <- paste(failed.types, collapse = ', ')
    stop(paste0("Sorry, you've asked for documentation for the FARS dataset(s): ", failed.types, '.',
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
  
  lapply(link.subset$description.url,
         FUN = function(url){
           
           if (save.locally){
             
             save.path <- paste0(save.path, '/',
                                 'fars_doc.',
                                 link.subset$Year[which(link.subset[ , description.url] == url)],
                                 '.',
                                 link.subset$dataset[which(link.subset[ , description.url] == url)],
                                 '.txt')
             download.file(url, destfile = save.path)
           }
           else url.show(url)
         })
  
  
}