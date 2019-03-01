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
# WARNING: As is this function downloads file into the working directory 
# and deletes it... Probably need to create a better solution. Or have it 
# download into a temporary directory somewhere.
read_ziplink <- function(link) {
  temp <- tempfile()
  download.file(link, temp)
  temp_unzp <- unzip(temp)
  data <- fread(temp_unzp)
  unlink(temp)
  file.remove(temp_unzp)
  return(data)
}

# Can be slow for large datasets. 
test <- read_ziplink(sec1_dls[[1]])

