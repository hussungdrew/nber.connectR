##### R script for the creation of the morg.link.table
library(data.table)
library(rvest)
morg.url <- 'https://www.nber.org/morg/annual/'

######### Get links to .dta files located on morg page
morg.links <- read_html(morg.url) %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  .[grepl(pattern = '.dta', x = ., ignore.case = T)]

morg.names <- morg.links

## Give these files a proper name for display purposes
morg.names[substr(morg.links, start = 5, stop = 5) %in% c('7', '8', '9')] <- paste0(substr(morg.links[substr(morg.links, start = 5, stop = 5) %in% c('7', '8', '9')], 
                                                                              start = 1, stop = 4),
                                                                       '19', substr(morg.links[substr(morg.links, start = 5, stop = 5) %in% c('7', '8', '9')], 
                                                                                    start = 5, stop = nchar(morg.links)))
morg.names[substr(morg.names, start = 5, stop = 6) != "19"] <- paste0(substr(morg.names[substr(morg.names, start = 5, stop = 6) != '19'],
                                                                            start = 1, stop = 4),
                                                                     '20', substr(morg.names[substr(morg.names, start = 5, stop = 6) != '19'],
                                                                                  start = 5, stop = nchar(morg.names)))

##Morg.links now ready to be made into data.table
morg.link.table <- data.table(File = morg.names,
                              url = paste0(morg.url, morg.links)) %>%
  .[order(File)] %>%
  .[ , Year := as.numeric(substr(File, start = 5, stop = 8))] %>%
  .[!grepl('oldweights', x = url, fixed = T)]

##Description file header
desc.head <- paste0(morg.url, 'desc/')

##Grab description file links
desc.local.links <- read_html(desc.head) %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  .[grepl(pattern = 'morg\\d', x = ., fixed = F)] 

desc.full.links <- paste0(desc.head, desc.local.links, 'desc.txt')

year <- paste0(ifelse(substr(desc.local.links, 5, 5) %in% c('7', '8', '9'), '19', '20'), 
               substr(desc.local.links, 5, 6)) 

desc.link.table <- data.table(Year = as.numeric(year),
                              description.link = desc.full.links)

## Master data.table with Year, filename of dataset, the url to the dataset, and a link to the description on NBER
morg.link.table <- merge(morg.link.table,
                         desc.link.table,
                         by = "Year", all.x = T)

save(morg.link.table, file = 'morg.link.table.rda')


