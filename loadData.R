# Script to 
# (1) take the analyst database as a QA'd csv file compiled and load it
# (2) take the lookup excel file and load it in
# (3) to add in columns as needed from the lookup file
# (4) write data here for now

library(aws.s3)
library(aws.ec2metadata)
library(readxl)
library(magrittr)
library(dplyr)


#### Load compiled analyst database that has been QA'd -------------------------
# Start of each month

read_offshoreWindData <- function(filename, foldername = "bronze/process_mapping/offshore_wind_data/"){
  
  fileread <-  aws.s3::s3read_using(
    FUN = readxl::read_xlsx,
    object = paste0(foldername, filename),  
    bucket = "s3-ranch-004",
    sheet = "metaData"
  )
  
  return(fileread)
}


# Select latest dated csv file to upload from compiled database
offshoreWindData <- read_offshoreWindData("visDataPre.xlsx")














#### Load organogram lookup sheets from excel file -----------------------------

# function to read in all sheets of excel database into a list
read_xlsAllsheets <- function(filename, tibble = FALSE) {
  # if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

# Use the function above to read in the xls lookup data from the bucket
read_xlsData<- function(filename, foldername = "bronze/organograms/qaData/"){
  
  fileread<-  aws.s3::s3read_using(
    read_xlsAllsheets,
    object= paste0("/bronze/organograms/qaData/", filename),  
    bucket= "s3-ranch-004"
  )
  
  return(fileread)
}


# Read in lookups
lookupsList <- read_xlsData("organogram_lookups.xlsx")
lookupsListNames <- names(lookupsList)

#Loop through the sheetNames to output each element of the list as a named datafile
for(i in 1:length(lookupsListNames)){
  x <- lookupsList[[i]]
  assign(paste(lookupsListNames[i]), x)
  rm(x)
}







#### Write data ----------------------------------------------------------------

write_nonQaData <- function(outputData, filename, foldername = "/bronze/organograms/nonQaData/"){
  
  # Global settings
  bucket <- "s3-ranch-004"
  Sys.setenv("AWS_REGION_KEY" = "eu-west-1",
             "AWS_DEFAULT_REGION" = "eu-west-1")
  file_path <- paste0(foldername, filename)
  aws.s3::s3write_using(
    x = dataset,
    FUN = write.csv,
    object = file_path,
    bucket = bucket,
    opts = list( "headers" = 
                   list("x-amz-acl" = "bucket-owner-full-control"))
  )
  
}

#write_nonQaData(outputData = mysurvey_trial, filename = "AID_trial_20230323.csv")

