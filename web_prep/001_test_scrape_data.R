library(rvest)
library(magrittr)
library(data.table)
library(stringr)

# Scrape Sections and Links -----------------------------------------------
dp <- read_html("https://www.nber.org/data")

# get the names of the datasets and their associated links
d_nodes <- html_nodes(dp, "td a")

dsets <- d_nodes %>%
  html_text()

dsets_links <- d_nodes %>% 
  html_attr("href")

dst <- data.table("section" = dsets, 
                  "link" = dsets_links)

# Drop all instances of \n
dst[, section := gsub(pattern = "\n", replacement = "", x = section)]
dst[, link := gsub(pattern = "\n", replacement = "", x = link)]

# Drop links up until "Macro Data" header
drp_head <- which(is.na(dst$link))[1]
dst <- dst[-(1:drp_head)]

# Drop links after email
drp_tail <- grep("@", dst$link)
dst <- dst[-(drp_tail:nrow(dst))]

# Drop headers, and names with empty links
dst <- dst[!(link=="/" | is.na(link))]

# Paste "https://www.nber.org" if the first character is a "/"
dst[, link := ifelse(substr(link, 1, 1) == "/" | substr(link, 1, 2) == "./" ,
                     paste0("https://nber.org", link), link)]

# Now catch those instances where neither "./" or "/" is true. 
dst[, link := ifelse(!(substr(link, 1, 4) == "http"),
                     paste0("https://nber.org/", link), link)]

# Get links of data for each section link ---------------------------------

get_links <- function(link) {
  site <- html_nodes(read_html(link), "td a")
  links <- html_attr(site, "href")
  links <- links[str_sub(links, start = -4L) %in% c(".csv", ".xls", "xlsx", ".dta", "bdat")]
  links <- ifelse(substring(links, 1, 1) == "/", paste0("https://nber.org", links), 
                  paste0("https://nber.org/", links))
  return(links)
}

# These links could be used directly for downloading data. 
## Examples
get_links(dst$link[2])
get_links(dst$link[58])

examp_dat <- fread("https://nber.org/SSPACO/2013/aco2013.csv")

# Add Data Links to dst ---------------------------------------------------
# Only do it for nber links
nber_links <- dst[grep("nber", link, ignore.case = TRUE)]
nber_links[, data_links := lapply(link, get_links)]

# We can see that not all of the links have usable data links at the first level. 
