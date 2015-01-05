#
# Utility function
#
# scrape a direcory structure web page for file/folder links
getFiles <- function(url) {
  rawtxt <- content(GET(url), "text")
  # strip out file name links
  rawtxt <- sapply(strsplit(rawtxt, "<A "), gsub, pattern = "(<([a-z]*|/[a-z]*)>|HREF=|\")", replacement = "")
  rawtxt <- unname(sapply(sapply(rawtxt, strsplit, split = ">"), "[[", 1))
  # drop the first two which are title and back link
  files <- rawtxt[-(1:2)]
  # paste on http location
  paste0("http://ec.gc.ca", files)
}


#---------------------------------------------------------------
#
#  Extract data from web page
#
#---------------------------------------------------------------
library(httr)
library(devtools)

url <- "http://open.canada.ca/data/api/action/package_show?id=9ec91c92-22f8-4520-8b2c-0f1cce663e18"
# get url locations of data from webpage
r <- GET(url)
# extract the locations of the resources
urls <- sapply(content(r) $ result $ resources, "[[", "url")



# read in look up tables
# -----------------------

variableLU <- read.csv(urls[2])
siteLU <- read.csv(urls[3])
descriptionLU <- read.csv(urls[4])


# read in data
# ------------

# read in folder names (one for each site)
siteFiles <- getFiles(urls[1])
# loop over these reading all file names
allFiles <- unname(unlist(sapply(siteFiles, getFiles)))
# read each file, store as a list (takes a wee while...)
dataList <- lapply(seq_along(allFiles), function(i) {
    fnameshort <- tail(strsplit(allFiles[i], "/")[[1]], 1)
    cat("reading file :", fnameshort, "...");flush.console()
    out <- read.csv(allFiles[i])
    cat(" done", length(allFiles) - i, "to go!\n")
    out
  })
# rbind into one data.frame
dataFull <- do.call(rbind, dataList)

# the raw data
head(dataFull)

# look up tables
head(variableLU)
head(siteLU)
head(descriptionLU)
# could add these in at a later date
# this would sort out some issues that we clear up by hand anyway such as missing lat long and missing station name

waterq <- dataFull

# clean datetime
# get correct time zone ?
waterq $ sample_datetime <-  strptime(as.character(waterq $ sample_datetime), "%Y-%m-%dT%H:%M:%S")

# remove white space
waterq $ method_detect_limit <- as.numeric(gsub("MG/L", "", as.character(waterq $ method_detect_limit)))

# change NULL and blank to NA
waterq $ flag <- as.character(waterq $ flag)
waterq $ flag[waterq $ flag %in% c("", "NULL")] <- NA

# fill in blank station_name on one entry
waterq $ station_name[waterq $ station_name == ""] <- "North Alouette River at 132nd Ave and Edge Street"
waterq $ station_name <- waterq $ station_name[drop = TRUE]

# remove white space and replace blank with NA
waterq $ status <- gsub(" ", "", as.character(waterq $ status))
waterq $ status[waterq $ status == ""] <- NA

write.csv(waterq_raw, "data-raw/waterq.csv", row.names = FALSE)
