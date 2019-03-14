####### Baseline function for downloading supporting CPS Morg documentation from NBER
get_morg_docs <- function(years, save.locally, save.path){
  data('morg.link.table')
  
  if (missing(years)) years <- morg.link.table[!is.na(description.link), max(Year)]
  if (missing(save.locally)) save.locally <- FALSE
  if (missing(save.path) & save.locally == TRUE) {
    warning('No path given to save CPS MORG documentation. Defaulting to current working directory.')
    save.path <- getwd()
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
