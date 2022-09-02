# SCRIPT TO DOWNLOAD PAPERS FROM DOI #

#### Set up ####

# load packages

library(tidyverse)
library(httr)

# load DOI

DOI <- read.csv("./Data files/DOIs.csv", header = TRUE)

DOI_flagged <- read.csv("./Data files/Flagged_dois.csv", header = TRUE)

# remove all DOI in flagged from full list

DOI_reduced <- DOI[!DOI$DOI_ISBN %in% DOI_flagged$flagged_DOIs,]

# add http: at the beginning

DOI_reduced <- DOI_reduced %>% mutate(DOI = paste0("http://doi.org/", 
                                                   DOI_ISBN))

DOI_flagged <- DOI_flagged %>% mutate(DOI = paste0("http://doi.org/", 
                                                   flagged_DOIs))

#### Make DOI a url ####

headers <- lapply(DOI_reduced[-44,4], HEAD)
# entry 44 has an error but nothing wrong so do that one manually

headers2 <- lapply(DOI_flagged[,3], HEAD)

# open the urls
map(.x = headers, ~{
  browseURL(.x$url, browser = getOption("browser"),
            encodeIfNeeded = FALSE)
})

# open the urls
map(.x = headers2, ~{
  browseURL(.x$url, browser = getOption("browser"),
            encodeIfNeeded = FALSE)
})
