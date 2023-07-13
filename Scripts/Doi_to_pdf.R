# T1.2: Script to open url for papers from DOI #

################################################################################

#### Set up ####

# load packages

library(tidyverse)
library(httr)

# load DOI

#DOI <- read.csv("./Data files/DOIs.csv", header = TRUE)
DOI <- read.csv("./Data files/DOIs_2010.csv", header = TRUE)

#### Make DOI a url ####

headers <- lapply(DOI[-44,4], HEAD)
# entry 44 has an error but nothing wrong so do that one manually

# open the urls
map(.x = headers, ~{
  browseURL(.x$url, browser = getOption("browser"),
            encodeIfNeeded = FALSE)
})


