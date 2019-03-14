##### R script for the creation of the fars.link.table
library(data.table)
library(rvest)
library(stringr)
fars.url <- 'https://www.nber.org/data/fars.html'

##### Get links to .dta files on fars.url

### FARS General Description: A nationwide census of fatal accidents in the US
  # F atal
  # A ccident
  # R eporting
  # S ystem

### 3 types of FARS data 
 # 1: Accident-Level : one record per crash
 # 2: Vehicle-Level : one record per vehicle involved in crash
 # 3: Person-Level  : one record per person involved in crash

# Function to grab links from FARS section of NBER pased on regexpr
get_fars_links <- function(pattern){
  
  read_html(fars.url) %>%
           html_nodes('a') %>%
           html_attr('href') %>%
           .[grepl(pattern = pattern, x = ., ignore.case = T)]
  
}

# Creation of a table of links for accident, vehicle, and person-level data
fars_links <- lapply(c('accident.dta', 'vehicle.dta', 'person.dta'), 
                     FUN = function(x) get_fars_links(x)) 

names(fars_links) <- c('accident.url', 'vehicle.url', 'person.url')

fars_table <- as.data.table(fars_links) %>%
  .[ , Year := stringr::str_extract(pattern = '[0-9][0-9][0-9][0-9]', string = accident.url)] %>%
  .[order(Year)] %>%
  .[ , c('Year', 'accident.url', 'vehicle.url', 'person.url')] %>%
  .[ , 
     c('accident.url', 'vehicle.url', 'person.url') := lapply(X = .[ , c('accident.url', 'vehicle.url', 'person.url'), with = F],
                                                              FUN = function(x) paste0('https://www.nber.org', x))]
# Get accompanying documentation 
get_fars_desc <- function(pattern){
  
  read_html(fars.url) %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    .[grepl(pattern = paste0('desc/', pattern), x = ., ignore.case = T)]
  
}

fars_desc <- lapply(c('accident', 'vehicle', 'person'), 
                                  FUN = function(x) get_fars_desc(x)) 

names(fars_desc) <- c('accident.desc', 'vehicle.desc', 'person.desc')

fars_desc <- as.data.table(fars_desc) %>%
  .[ , Year := stringr::str_extract(pattern = '[0-9][0-9][0-9][0-9]', string = accident.desc)] %>%
  .[order(Year)] %>%
  .[ , c('Year', 'accident.desc', 'vehicle.desc', 'person.desc')] %>%
  .[ , 
     c('accident.desc', 'vehicle.desc', 'person.desc') := lapply(X = .[ , c('accident.desc', 'vehicle.desc', 'person.desc'), with = F],
                                                              FUN = function(x) paste0('https://www.nber.org', x, '/desc.txt'))]

# Now we have a table of links to data and links to accompanying documentation
 # -- combine them!

fars_melted_table <- fars_table %>%
  melt(., id.vars = c('Year'), measure.vars = setdiff(names(.), 'Year'),
       variable.name = 'dataset', value.name = 'url') %>%
  .[ , dataset := substr(x = as.character(dataset), start = 1, stop = (nchar(as.character(dataset)) - 4))]

fars_melted_desc <- fars_desc %>%
  melt(., id.vars = c('Year'), measure.vars = setdiff(names(.), 'Year'),
       variable.name = 'dataset', value.name = 'description.url') %>%
  .[ , dataset := substr(x = as.character(dataset), start = 1, stop = (nchar(as.character(dataset)) - 5))]

fars.link.table <- merge(fars_melted_table,
                         fars_melted_desc,
                         by = c('Year', 'dataset'))
