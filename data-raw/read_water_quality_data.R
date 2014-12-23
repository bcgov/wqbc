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
library(rjson) 
library(XML)


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


save(dataFull, variableLU, siteLU, descriptionLU, file = "~/consulting/wqbc/data-raw/dataFull.rda")
# DONE

