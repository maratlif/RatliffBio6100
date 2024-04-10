# Homework 11
# 10 April 2024
# MAR

#### Libraries ----
library(log4r)
library(TeachingDemos)
library(tidyverse)
library(pracma)
library(ggmosaic)

#### Source function files ----
source("barracudar/DataTableTemplate.R")
source("barracudar/AddFolder.R")
source("barracudar/BuildFunction.R")
source("barracudar/MetaDataTemplate.R")
source("barracudar/CreatePaddedLabel.R")
source("barracudar/InitiateSeed.R")
source("barracudar/SetUpLog.R")
source("barracudar/SourceBatch.R")
source("barracudar/QBox.R")
source("barracudar/QCon1.R")
source("barracudar/QCon2.R")
source("barracudar/QHist.R")
source("barracudar/QLogis.R")
source("barracudar/QScat.R")
source("barracudar/QBub.R")
source("barracudar/QContour.R")

setwd("/Users/Molly/Desktop/UVM/Courses/Spring2024/Bio6100/Homework11")

filelist <- list.files("/Users/Molly/Desktop/UVM/Courses/Spring2024/Bio6100/Homework11/OriginalData")
filelist

paste0("Here is", " the ", "filepath: ", filelist[1])
filenames <- c()

for (i in 1:8){
  setwd(paste0("/Users/Molly/Desktop/UVM/Courses/Spring2024/Bio6100/Homework11/OriginalData","/",filelist[i]))
  filenames[i] <- list.files(pattern="countdata")
}
print(filenames)
str(filenames)

# all_data <- list()
# for (i in seq_along(filenames)){
#   setwd(paste0("/Users/Molly/Desktop/UVM/Courses/Spring2024/Bio6100/Homework11/OriginalData","/",filelist[i]))
#  data_i <- read.csv(filenames[i])
#  all_data[[i]] <- data_i
# }
# 
# data <- do.call(rbind,all_data)
# str(data)
# 
# myVars <- c("startDate", "scientificName")
# dataBird <- data[, myVars]
# str(dataBird)
# 
# pattern <- "\\d{4}"
# dataBird$year <- ifelse(grepl(pattern, dataBird$startDate), 
#                     str_extract(dataBird$startDate, pattern), NA)
# 
# summaryStats <- dataBird %>%
#   group_by(year) %>%
#   summarise(abundance = n(), species = n_distinct(scientificName))
# summaryStats <- data.frame(summaryStats)
# print(summaryStats)  
# 
# stats <- cbind(filenames, summaryStats)
# str(stats)




# Function to process and summarize bird data
birdStats <- function(filenames, myVars, pattern) {
  all_data <- list()
  
  for (i in seq_along(filenames)) {
    setwd(paste0("/Users/Molly/Desktop/UVM/Courses/Spring2024/Bio6100/Homework11/OriginalData/", filelist[i]))
    data_i <- read.csv(filenames[i])
    all_data[[i]] <- data_i
  }
  
  data <- do.call(rbind, all_data)
  
  dataBird <- data[, myVars]
  
  dataBird$year <- ifelse(grepl(pattern, dataBird$startDate), 
                          str_extract(dataBird$startDate, pattern), NA)
  
  summaryStats <- dataBird %>%
    group_by(year) %>%
    summarise(abundance = n(), species = n_distinct(scientificName)) %>%
    data.frame()
  
  stats <- cbind(filenames, summaryStats)
  
  return(stats)
}

stats <- birdStats(filenames, myVars, pattern)
print(stats)
