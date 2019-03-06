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
library(haven)

## baseline function for reading in data
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

get_morg_docs <- function(years, save.locally, save.path){
  data('morg.link.table')
  
  if (missing(years)) {
    stop('Please pass a numeric vector of years for which you wish to retrieve CPS MORG documentation.')
  }
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
             download.file(description.link, destfile = save.path)
           }
           else url.show(url)
         })
}

# collapse_morg_pop <- function(x, group.vars){
#   x[ , earn.weight := earnwt/12]
#   x[ , prop.earn := earn.weight/sum(earn.weight), by = group.vars]
#   return(x[ , .(Population = sum(weight/3, na.rm = T),
#                 Year = mean(as.integer(year)),
#                 Avg.Week.Earn = weighted.mean(x = uearnwk, w = prop.earn, na.rm = T)), by = group.vars])
# }
# earn.pop <- lapply(test,
#                    collapse_morg_pop, group.vars = 'state')
