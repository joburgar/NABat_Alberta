######################################################################################################
# NABat_rawdata_management.R
# created by Joanna Burgar, 19-Dec-2020
# script for summarising NABat raw data
######################################################################################################

######################################################################################################
# DIRECTORIES (BEGINNING)
######################################################################################################
getwd()
RawFileDir = paste(getwd(),"NABat_RawFiles",sep="/")
InputDir = paste(getwd(),"Input", sep="/")
OutputDir = paste(getwd(),"Output",sep="/")
######################################################################################################
# DIRECTORIES (END)
######################################################################################################

######################################################################################################
# LOAD PACKAGES (BEGINNING)
######################################################################################################
library(tidyverse)    # data manipulation, plotting
library(Cairo)   # printing maps for report quality output
library(R.utils) # for moving file directory

######################################################################################################
# LOAD PACKAGES (END)
######################################################################################################

######################################################################################################
# CREATE DATAFRAME OF RAW CALL DATA FILES (BEGINNING)
######################################################################################################
# set working directory for map output

# Create vector, and then dataframe, of raw data file names
rawdata.names <- list.files(path="./NABat_2022_Alberta_eBat_output", recursive = TRUE)
glimpse(rawdata.names)

RawFile.df <- as.data.frame(rawdata.names)
head(RawFile.df)
nrow(RawFile.df)

# Populate dataframe with data structure as columns
# Need GRTS.Cell.ID, Location.Name, Deployment.ID, Filename 
RawFile.df$GRTS.Cell.ID <- word(RawFile.df$rawdata.names,1,sep = "\\/")
RawFile.df$Location.Name <- word(RawFile.df$rawdata.names,2,sep = "\\/")
RawFile.df$Deployment.ID <- word(RawFile.df$rawdata.names,3,sep = "\\/")
RawFile.df$Filename <- word(RawFile.df$rawdata.names,-1,sep = "\\/")

# Create a column with the file types
RawFile.df <- mutate(RawFile.df, FileType = ifelse(grepl ('#', Filename), "zc", 
                                             rawdata.names %>% word(-1,sep = "\\.")))
# Summarise rawfile data
RawFile.df %>% count(GRTS.Cell.ID) # 56 NABat grid cells

RawFile.df %>% count(Location.Name) # 106 sites surveyed

RawFile.df %>% count(FileType) # 18 files types, need to filter out call files
RawFile.df %>% filter(FileType %in% c("w4v", "wav", "zc", "zca")) %>% count(FileType)
# FileType      n
# 1 w4v       32200
# 2 wav       38041
# 3 zc       209175
# 4 zca         421

# Create a dataframe with just the call files, exclude the pathway column
RawCall.df <- RawFile.df %>% filter(FileType %in% c("w4v", "wav", "zc", "zca")) %>% select(-rawdata.names)
glimpse(RawCall.df)

# Housekeeping, remove RawFile.df
rm(RawFile.df, rawdata.names)

setwd(InputDir)
# save.image("NABat_RawCall.RData")
# load("NABat_RawCall.RData")
######################################################################################################
# CREATE DATAFRAME OF RAW CALL DATA FILES (END)
######################################################################################################

######################################################################################################
# LINK RAW CALL DATAFRAME WITH STATION DATA (END)
######################################################################################################
setwd(InputDir)
NABat_deploy <- read.csv("NABat_Deployment_Data.csv", header = TRUE, stringsAsFactors = NA) 
glimpse(NABat_deploy)

NABat_deploy %>% group_by(Land.Unit.Code, GRTS.Cell.ID) %>% count(Deployment.ID)
NABat_deploy$Location.Name_Year <- as.factor(paste(NABat_deploy$Location.Name, NABat_deploy$Deployment.ID, sep="_"))

glimpse(RawCall.df)
RawCall.df %>% group_by(GRTS.Cell.ID, Location.Name, FileType) %>% count(Deployment.ID)
Files_per_Location <- RawCall.df %>% group_by(GRTS.Cell.ID, Location.Name, FileType) %>% count(Deployment.ID)
Files_per_Location$Location.Name_Year <- as.factor(paste(Files_per_Location$Location.Name, Files_per_Location$Deployment.ID, sep="_"))

### need to change to factor (I think) so dplyr select will work
NABat_deploy <- left_join(NABat_deploy, Files_per_Location %>% select(Location.Name_Year:n), by="Location.Name_Year")
glimpse(NABat_deploy)
head(NABat_deploy)

getwd()
sapply(new_folder_path, dir.create)
