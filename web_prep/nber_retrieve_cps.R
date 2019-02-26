###### Function for retrieving CPS data off of the NBER site

## Five types of CPS data
# 1: Basic Monthly Data -- primary source of labor force stats in the US
# 2: Supplements -- same data as Basic Monthly but with supplemental info
# 3: Merged Outgoing Rotation Groups (MORG) -- extracts of Basic Monthly Data but when usual hours/earnings q's are asked
# 4: Mare-Winship Files -- subsets of 168 vars from March Annual Demographic Supplement files 1964-1992
# 5: May Extracts -- standardized extracts of 200 vars made from CPS May Supplement files. Cover 1967-1987

############################################## MORG Data ########################################################
library(data.table)
library(rvest)
library(foreign)
library(readstata13)

## Head of MORG section
morg.url <- 'https://www.nber.org/morg/annual/'

######### Get links to .dta files located on morg page
morg.links <- read_html(morg.url) %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  .[grepl(pattern = '.dta', x = ., ignore.case = T)]

morg.names <- morg.links
morg.names[substr(morg.links, start = 5, stop = 5) == '0' | 
             substr(morg.links, start = 5, stop = 5) == '1'] <- paste0(substr(morg.links[substr(morg.links, start = 5, stop = 5) == '0' | 
                                                                                           substr(morg.links, start = 5, stop = 5) == '1'], 
                                                                              start = 1, stop = 4),
                                                                     '20', substr(morg.links[substr(morg.links, start = 5, stop = 5) == '0' | 
                                                                                               substr(morg.links, start = 5, stop = 5) == '1'], 
                                                                                  start = 5, stop = nchar(morg.links)))
morg.names[substr(morg.names, start = 5, stop = 5) != "2"] <- paste0(substr(morg.names[substr(morg.names, start = 5, stop = 5) != '2'],
                                                                     start = 1, stop = 4),
                                                                     '19', substr(morg.names[substr(morg.names, start = 5, stop = 5) != '2'],
                                                                                  start = 5, stop = nchar(morg.names)))

##Morg.links now ready to be made into data.table
morg.link.table <- data.table(File = morg.names,
                              link = paste0(morg.url, morg.links)) %>%
  .[order(File)] %>%
  .[ , Year := as.numeric(substr(File, start = 5, stop = 8))] %>%
  .[!grepl('oldweights', x = link, fixed = T)]

#test load in .dta file
test <- read.dta(file = morg.link.table[23, link]) ##success!

## baseline function for reading in data
read_cps_morg <- function(start.year, end.year){
  morg.return <- lapply(X = morg.link.table[Year %in% start.year:end.year, link],
         FUN = function(x){
           read.dta(file = x)
         }
      )
  names(morg.return) <- paste0('cps.morg', start.year:end.year)
  return(morg.return)
}

test <- read_cps_morg(1979, 1983)

######### Get links to description files
desc.head <- paste0(morg.url, 'desc/')

desc.local.links <- read_html(desc.head) %>%
  html_nodes('a') %>%
  html_attr('href') %>%
  .[2:length(.)] 

desc.full.links <- paste0(desc.head, desc.local.links, 'desc.txt')

year <- paste0(ifelse(substr(desc.local.links, 5, 5) == '0' | substr(desc.local.links, 5, 5) == '1', '20', '19'), 
               substr(desc.local.links, 5, 6)) 

desc.link.table <- data.table(Year = as.numeric(year),
                              desc.link = desc.full.links)

morg.link.table <- merge(morg.link.table,
                         desc.link.table,
                         by = "Year", all.x = T)
  
