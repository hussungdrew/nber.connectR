##### Function for "Retrieving Vital Statistics Births Data"

## Data Sections
# 1.) Birth Cohort Linked Birth/Infant Death Data
#     Has CSV formats.
# 2.) Natality Detail Data
#     Has CSV formats.
# 3.) Linked Birth Infant Death Period Data
#     Has CSV formats, many different data sections here, 
#     be cautious the correct data is being imported. 
# 4.) 1995-2002 Perinatal Mortality Data
#     Has SAS datasets.
# 5.) 1995-1998 Matched Multiple Births Data
#     Has SAS files
# 6.) Vital Statistics Books 
#     These are PDFs of pre 1968 Stats, I am researching possible methods
#     for getting some of this data in a usable format. 


# nber.connectR Package Details ---------------------------------------------------------
# Packages to add to Depends: field of DESCRIPTION file
#  - haven
#  - stringr
# Packages to add to Imports: field of DESCRIPTION file
#  - data.table
#  - magrittr

# Link Helper Functions and Packages --------------------------------------
library(data.table)
library(magrittr)
library(rvest)

get_links <- function(link, 
                      # File extentions as the function currently is must be 
                      # 4 characters. 
                      file_extension = c(".csv", ".xls", "xlsx", ".dta", "bdat")) {
  site <- html_nodes(read_html(link), "td a")
  links <- html_attr(site, "href")
  # Allow for no file name extensions to be required. 
  if (!is.null(file_extension)) {
    links <- links[stringr::str_sub(links, start = -4L) %in% file_extension]
  }
  links <- ifelse(substring(links, 1, 1) == "/", paste0("https://nber.org", links), 
                  paste0("https://nber.org/", links))
  return(links)
}

# Link Prep ---------------------------------------------------------------
# This section will not be in the final function, rather it will be added
# to the lookup table code depending on how we decide to integrate the lookup
# tables.

# Link of section 
vts_birth_link <- "https://www.nber.org/data/births.html"

# Get Sub Section Links
vts_sub_links <- get_links(link = vts_birth_link, file_extension = NULL)

# Automate this later...
vts_sub_links <- vts_sub_links[4:9]


# Reading in Data for Section 1 -------------------------------------------
# Create function for reading in Birth Cohort Linked Birth/Infant Death Data

# Get data links
sec1_dls <- get_links(link = vts_sub_links[[1]], file_extension = ".zip")

# Only keep CSVs for now. 
sec1_dls <- sec1_dls[stringr::str_sub(sec1_dls, start = -8) == ".csv.zip"]

# Function for reading in zipped CSVs
# The function creates a temporary file and directory, 
# then it deletes the temporary file once it has been 
# loaded into memory. The tempory directory is deleted 
# if R closes in a standard fashion.
read_ziplink <- function(link, format = "csv") {
  temp <- tempfile()
  download.file(link, temp, mode = "wb")
  temp <- unzip(temp, exdir = tempdir())
  if (format == "csv") {
    dt <- fread(temp)
  } else if (format == "sas") {
    dt <- as.data.table(haven::read_sas(temp))
  } else if (format == "stata") {
    dt <- as.data.table(haven::read_dta(temp))
  }
  unlink(temp)
  return(dt)
}

# Funtion works for SAS, Stata, and csv data types as demonstrated below.  
test <- read_ziplink(link = sec1_dls[[1]])
test2 <- read_ziplink(link = "https://www.nber.org/lbid/2013/linkco2013us_num.sas7bdat.zip", 
                      format = "sas")
test3 <- read_ziplink(link = "https://www.nber.org/lbid/2013/linkco2013us_num.dta.zip", 
                      format = "stata")

## The types of data in section 1
# 1.1 Linked Deaths: format linkcoYYYYus_num.csv.zip
# 1.2 Births: format linkcoYYYYus_den.csv.zip
# 1.3 Unliked Deaths: format linkcoYYYYus_unl.csv.zip
# 1.4 US Territories Linked Deaths: format linkcoYYYYps_num.csv.zip
# 1.5 US Territories Births: format linkcoYYYYps_den.csv.zip
# 1.6 US Territories Unliked Deaths: format linkcoYYYYps_unl.csv.zip

### WARNINGs: Many of the 1.6 section data pages are missing, or broken,
#   need to figure out how to just have it throw a generic warning like, 
#   "Data for the given year doesn't exist"
#   Also: Some of this data is very big, resulting in a lot of large data
#         so we will have to be cautious as we read in large data sets.
#         It seems section 1.2 is rather large.

## Read Birth Cohort Linked Birth/Infant Death Data Function

read_vtsb_bcl <- function(years, 
                          section,
                          links = sec1_dls) {
  # Name list 
  names(years) <- paste0("d_", years, "_", 
                         gsub("[:space:]", 
                              x = section, 
                              replacement = "_"))
  # subset years
  links <- lapply(years, function(year) {grep(pattern = year, 
                              x = links, 
                              value = TRUE)})
  links <- unlist(links)
  
  # subset data.section
  if (section %in% c("linked deaths", "1.1")) {
    links <- grep(pattern = "us_num", x = links, value = TRUE)
    
  } else if (section %in% c("births", "1.2")) {
    links <- grep(pattern = "us_den", x = links, value = TRUE)
    
  } else if (section %in% c("unliked deaths", "1.3")) {
    links <- grep(pattern = "us_unl", x = links, value = TRUE)
    
  } else if (section %in% c("linked deaths us terr", "1.4")) {
    links <- grep(pattern = "ps_unl", x = links, value = TRUE)
    
  } else if (section %in% c("births us terr", "1.5")) {
    links <- grep(pattern = "ps_den", x = links, value = TRUE)
    
  } else if (section %in% c("unliked deaths us terr", "1.6")) {
    links <- grep(pattern = "ps_unl", x = links, value = TRUE)
  }
  
  # read list of tables
  links <- unlist(links)
  dt <- lapply(links, read_ziplink)
  return(dt)
}

# This way the user can request select years
sec1_1_dat <- read_vtsb_bcl(years = c(1995, 2013), 
                          section = "1.1")

# Or a vector of sequential years
# This also shows how if a data year does not exist, the 
# function will not return a warning, but rather will just
# return the data for the years that are present.
sec1_2_dat <- read_vtsb_bcl(years = 1990:1995, 
                          section = "1.1")

# Because we named the list we can call tables by name
sec1_1_dat$d_1995_1.11[1:10, 1:5]


