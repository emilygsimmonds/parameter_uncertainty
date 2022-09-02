# SCRIPT TO DOWNLOAD PAPERS FROM DOI #

#### Set up ####

# load packages

library(tidyverse)
library(httr)

# load DOI

DOI <- read.csv("./Data files/DOIs.csv", header = TRUE)

DOI_short <- read.csv("./Data files/DOIs_short_studies.csv", header = TRUE)

DOI_flagged <- read.csv("./Data files/Flagged_dois.csv", header = TRUE)

# add http: at the beginning

DOI <- DOI %>% mutate(DOI = paste0("http://doi.org/", DOI_ISBN))

DOI_short <- DOI_short %>% mutate(DOI = paste0("http://doi.org/", DOI_ISBN))

DOI_flagged <- DOI_flagged %>% mutate(DOI = paste0("http://doi.org/", flagged_DOIs))

#### Make DOI a url ####

headers <- lapply(DOI[,4], HEAD)

# entry 20 has an error but nothing wrong so do that one manually
headers2 <- lapply(DOI_short[-20,4], HEAD) 

headers3 <- lapply(DOI_flagged[,3], HEAD)

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

# open the urls
map(.x = headers3, ~{
  browseURL(.x$url, browser = getOption("browser"),
            encodeIfNeeded = FALSE)
})
