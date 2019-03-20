###### R script for master data.table for calling separate read functions
master <- data.table(type = c('read', 'read', 'document', 'document'),
                     fcn = c('read_cps_morg', 'read_fars', 'get_morg_docs', 'get_fars_docs')) %>%
  setkey(., 'fcn')

master_read <- function(func, ...){
  
  do.call(master[func, fcn], args = list(...))
  
}

test <- master_read('read_fars', years = c(1997, 2011), types = 'accident')
