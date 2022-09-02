# SCRIPT TO ORGANISE DOIs AND CHECK FOR EXCLUSIONS #

#### Set up ####

# load datafiles

DOIs_long <- read.csv("./Data files/DOIs.csv", header = TRUE)
  
DOIs_short <- read.csv("./Data files/DOIs_short_studies.csv", header = TRUE)
  
DOIs_Kendall <- read.csv("./Data files/DOIs_Kendall.csv", header = TRUE)
  
#### Check how many DOIs overlap ####

flagged_DOIs <- c(DOIs_long$DOI_ISBN[DOIs_long$DOI_ISBN %in% DOIs_Kendall$DOI.ISBN],
                  DOIs_short$DOI_ISBN[DOIs_short$DOI_ISBN %in% DOIs_Kendall$DOI.ISBN])

write.csv(as.data.frame(flagged_DOIs), file = "./Data files/Flagged_DOIs.csv")
