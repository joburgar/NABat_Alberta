######################################################################################################
# NABat_rawdata_management.R
# created by Joanna Burgar, 19-Dec-2020
# script for summarising NABat raw data
# revised 3-Apr-2023 to assign GRTS to desktop Alberta eBat output files
######################################################################################################

######################################################################################################
# DIRECTORIES (BEGINNING)
######################################################################################################
# getwd()
# # RawFileDir = paste(getwd(),"NABat_RawFiles",sep="/")
# InputDir = paste(getwd(),"Input", sep="/")
# OutputDir = paste(getwd(),"Output",sep="/")
# open file as part of NABat_Alberta project
# have output from Alberta eBat in a folder called "NABat_YEAR_Alberta_eBat_output"

######################################################################################################
# DIRECTORIES (END)
######################################################################################################

######################################################################################################
# LOAD PACKAGES (BEGINNING)
######################################################################################################
library(tidyverse)    # data manipulation, plotting
library(R.utils) # for moving file directory
library(filesstrings)

######################################################################################################
# LOAD PACKAGES (END)
######################################################################################################

######################################################################################################
# CREATE DATAFRAME OF SUMMARY FILES, EXPORTED BY ALBERTA EBAT SCRIPT
######################################################################################################

# Import summary files
YEAR <- "2023"
dat_summary <- fs::dir_ls(path=paste0("./NABat_",YEAR,"_Alberta_eBat_output/"), 
                          regexp = "\\_summary.csv$", recurse = TRUE) %>%
  map_dfr(~read_csv(.x, col_types = cols(.default = "c")), .id="source") %>% # issues with min_wind so read in as character
  type_convert()  # convert back to proper formats

dat_summary$Pfolder <- word(dat_summary$source,3, sep="/") %>% str_remove("_summary.csv")
dat_summary %>% count(Pfolder)

# fs::dir_ls(path=paste("/Volumes/LaCie_5TB/NABat_from_FTP"), recurse = 1)

sta <- read.csv("Input/NABat_Station_Covariates.csv") %>% filter(X2023==1)
sta %>% select(Orig_Name, Orig.Name)

sta.sub <- sta[c("GRTSCellID","LocName","Orig_Name")] %>% as_tibble() 
sta.sub %>% count(Orig_Name) %>% print(n=100)
dat_summary %>% count(Site) %>% print(n=100)

sta.sub <- sta.sub %>% mutate(match = case_when(Orig_Name %in% unique(dat_summary$Site) ~ "yes",
                                     TRUE ~ "no"))

sta.sub %>% count(match)
sta.sub %>% filter(match=="yes") %>% print(n=7)
sta.sub %>% filter(match=="no") %>% print(n=56)

dat_summary <- left_join(dat_summary, sta.sub %>% select(-match), by=c("Site"="Orig_Name"))
dat_summary <- dat_summary %>% rename(GRTS.Cell.ID = GRTSCellID, Location.Name=LocName)

dat_summary %>% count(Location.Name) %>% print(n=100)
dat_summary %>% count(Pfolder)

dat_summary %>% count(Site) %>% print(n=100)
Pfolder.names <- unique(dat_summary$Pfolder)
dat_summary %>% filter(Pfolder %in% Pfolder.names[9]) %>% count(Site) %>% print(n=50)

# add in the correct Location Names.
dat_summary <- dat_summary %>% mutate(Location.Name = case_when(Pfolder=="ACA" &  Site == "ACA-BATFS-01" ~ "72071_SE_01",
                                                                Pfolder=="ACA" &  Site == "ACA-BATFS-02" ~ "203655_SW_01",
                                                                
                                                                Pfolder=="Bayne" &  Site == "BP-96N" ~ "219731_SW_01",
                                                                
                                                                Pfolder=="BNP" &  Site == "UpperHotsprings" ~ "148842_SW_01",
                                                                Pfolder=="BNP" &  Site == "Fenlands" ~ "148842_NW_01",
                                                                Pfolder=="BNP" &  Site == "GolfCourse" ~ "148842_NE_01",
                                                                Pfolder=="BNP" &  Site == "Bryant" ~ "293385_NW_01",
                                                                Pfolder=="BNP" &  Site == "Divide" ~ "41322_SE_01",
                                                                Pfolder=="BNP" &  Site == "Flints" ~ "280601_NE_01",                                                                Pfolder=="BNP" &  Site == "Flints" ~ "280601_NE_01",
                                                                Pfolder=="BNP" &  Site == "FishLakes" ~ "2410_NW_01",
                                                                Pfolder=="BNP" &  Site == "Indianhead" ~ "90474_NW_01",
                                                                Pfolder=="BNP" &  Site == "MooseMeadows" ~ "296805_SE_02",
                                                                Pfolder=="BNP" &  Site == "Paliser" ~ "235690_SW_01",
                                                                Pfolder=="BNP" &  Site == "Scotch" ~ "156010_SE_01",
                                                                Pfolder=="BNP" &  Site == "Sunshine" ~ "311483_SE_01",
                                                                Pfolder=="BNP" &  Site == "Windy" ~ "127338_NW_01",
                                                                
                                                                Pfolder=="CWS" &  Site == "MENWA-BAT-01" ~ "172883_NW_01",
                                                                Pfolder=="CWS" &  Site == "MENWA-BAT-02" ~ "172883_NW_02",
                                                                Pfolder=="CWS" &  Site == "MENWA-BAT-03" ~ "172883_NW_03",
                                                                
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-NE" ~ "264037_SE_01",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-SH" ~ "264037_NE_01",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-ST" ~ "264037_NW_01",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-TR" ~ "264037_SW_01",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-D1" ~ "EINP-D1",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-D2" ~ "EINP-D2",
                                                                
                                                                Pfolder=="Edson_Switzer" &  Site == "EdsonUnit1LH" ~ "89754_SE_01",
                                                                Pfolder=="Edson_Switzer" &  Site == "EdsonUnit2KB" ~ "89754_NW_01",
                                                                Pfolder=="Edson_Switzer" &  Site == "EdsonUnit3TB" ~ "89754_SE_02",
                                                                Pfolder=="Edson_Switzer" &  Site == "SwitzerUnit2North" ~ "254362_NE_01",
                                                                Pfolder=="Edson_Switzer" &  Site == "SwitzerUnit3VC" ~ "254362_SE_02",

                                                                Pfolder=="JNP" &  Site == "BRULE-TUNNEL" ~ "267463_NE_01",
                                                                Pfolder=="JNP" &  Site == "BUFFALO-PRAI" ~ "23146_SE_01",
                                                                Pfolder=="JNP" &  Site == "MIETTE" ~ "267463_NW_01",
                                                                Pfolder=="JNP" &  Site == "TEKARRA" ~ "23146_NE_01",
                                                                Pfolder=="JNP" &  Site == "VALLEYOF5" ~ "23146_NE_02",
                                                                Pfolder=="JNP" &  Site == "WABASSO" ~ "23146_SE_02",
                                                                Pfolder=="JNP" &  Site == "WHIRLPOOL" ~ "309417_SW_01",
                                                                
                                                                Pfolder=="JNP_Mobile" &  Site == "JNP-D1-MIETTE" ~ "JNP-D1",
                                                                Pfolder=="JNP_Mobile" &  Site == "JNP-D2-TOWN" ~ "JNP-D2",
                                                                Pfolder=="JNP_Mobile" &  Site == "JNP-D3-TOWN" ~ "JNP-D3",
                                                                
                                                                
                                                                Pfolder=="MH_All" &  Site == "BIPGRNE01" ~ "273363_NE_01",
                                                                Pfolder=="MH_All" &  Site == "BIPGRSE01" ~ "273363_SE_01",
                                                                Pfolder=="MH_All" &  Site == "HargravesSE01" ~ "66719_SE_01",
                                                                Pfolder=="MH_All" &  Site == "HargravesSE02" ~ "66719_SE_02",
                                                                Pfolder=="MH_All" &  Site == "MRNAcoulee" ~ "322807_SE_02",  # correct site name is MRNA Coulee top
                                                                Pfolder=="MH_All" &  Site == "MRNAdugout" ~ "322807_SE_01", 
                                                                Pfolder=="MH_All" &  Site == "MRNAlake" ~ "199047_NW_01",
                                                                Pfolder=="MH_All" &  Site == "OnefourCoulee" ~ "224135_SE_05",
                                                                Pfolder=="MH_All" &  Site == "OnefourLostRiver" ~ "224135_SE_04",                                                                Pfolder=="MH_All" &  Site == "MRNAdugout" ~ "322807_SE_02",
                                                                Pfolder=="MH_All" &  Site == "OnefourPlayground" ~ "39815_SW_01",
                                                                Pfolder=="MH_All" &  Site == "OnefourReservoir" ~ "39815_SW_02",
                                                                
                                                                Pfolder=="MRWCC" &  Site == "MRWCC" ~ "134339_NW_01",
                                                                
                                                                Pfolder=="Peace1" &  Site == "SHEEP1" ~ "221850_SE_01",
                                                                Pfolder=="Peace1" &  Site == "SHEEP2" ~ "221850_SW_01",
                                                                Pfolder=="Peace1" &  Site == "299-SULPHUR" ~ "197018_NW_01",
                                                                Pfolder=="Peace1" &  Site == "FIREMANSPIT" ~ "197018_NE_01",
                                                                Pfolder=="Peace1" &  Site == "316-MUSKEG1" ~ "9626_NW_01",

                                                                Pfolder=="WLNP_mobile" &  Site == "2023-07-05" ~ "WLNP-D1",
                                                                Pfolder=="WLNP_mobile" &  Site == "2023-07-06" ~ "WLNP-D2",
                                                                Pfolder=="WLNP_stationary" &  Site == "BLAK-NABAT" ~ "139715_NE_01",
                                                                Pfolder=="WLNP_stationary" &  Site == "CAM-NABAT" ~ "139715_SW_01",
                                                                Pfolder=="WLNP_stationary" &  Site == "POW-NABAT"  ~ "205251_SW_01",

                                                                Pfolder=="Wabasca" &  Site == "UNIT1" ~ "3667_NW_01",
                                                                Pfolder=="Wabasca" &  Site == "UNIT2" ~ "3667_NE_01",
                                                                Pfolder=="Wabasca" &  Site == "UNIT3" ~ "3667_SW_01",
                                                                Pfolder=="Wabasca" &  Site == "UNIT4" ~ "3667_SE_01",
                                                                
                                                                Pfolder=="MRWCC" &  Site == "Mcintyre" ~ "134339_NW_01",

                                                                # Pfolder=="WCS" &  grepl("ACBPHampshireClose",Filename) ~ "30403_SW_01",
                                                                # Pfolder=="WCS" &  grepl("ACBPTopaz",Filename) ~ "38595_SE_01",
                                                                # Pfolder=="WCS" &  grepl("ACBPCEI",Filename) ~ "42883_SE_01",
                                                                # Pfolder=="WCS" &  grepl("ACBPWARanches",Filename) ~ "120707_NE_01",
                                                                # Pfolder=="WCS" &  grepl("ACBPOldmanSH",Filename) ~ "163011_SW_01",
                                                                # Pfolder=="WCS" &  grepl("NCCFlemingNW",Filename) ~ "326009_NE_01",
                                                                # Pfolder=="WCS" &  grepl("NCCKerrSW",Filename) ~ "326009_NW_01",
                                                                # Pfolder=="WCS" &  grepl("GoldenRanchesNorth",Filename) ~ "99523_SW_01",
                                                                # Pfolder=="WCS" &  grepl("GoldenRanchesSouth",Filename) ~ "99523_SW_02",
                                                                # Pfolder=="WCS" &  grepl("Hicks",Filename) ~ "115907_NW_01",
                                                                # Pfolder=="WCS" &  grepl("GamblingLake",Filename) ~ "115907_SE_01",
                                                                # Pfolder=="WCS" &  grepl("TomahawkEastAndex",Filename) ~ "171651_NE_01",
                                                                # Pfolder=="WCS" &  grepl("TomahawkWestAndex",Filename) ~ "171651_NE_02",
                                                                
                                                                TRUE ~ Location.Name))

dat_summary %>% filter(is.na(Location.Name)) %>% group_by(Pfolder) %>% count(Site) %>% print(n=30)
# Bayne = metadata not yet available
# good to filter out all NA files and proceed

dat_summary %>% filter(!is.na(Location.Name)) %>% count(Pfolder) %>% print(n=13)
glimpse(dat_summary)
dat_summary$GRTS.Cell.ID <- word(dat_summary$Location.Name, 1,1,sep="_")
dat_summary <- dat_summary %>% mutate(GRTS.Cell.ID = case_when(grepl("D",GRTS.Cell.ID) ~ "Mobile",
                                                                TRUE ~ GRTS.Cell.ID))

dat_summary <- dat_summary %>% filter(!is.na(Location.Name))

dat_summary %>% filter(GRTS.Cell.ID!="Mobile") %>% count(GRTS.Cell.ID) # 33 GRTS cells (up to 62 GRTS cells)
dat_summary %>% filter(GRTS.Cell.ID!="Mobile") %>% count(Location.Name) # 58 stations (+28 from Bayne's lab) 86 stations

dat_summary %>% filter(Date > "2023-01-01") %>% summarise(min(Date), max(Date), mean(Date))
# min date = Feb 7, 2023; max date = Oct 19, 2023; mean date = July 17, 2023
dat_summary %>% filter(Date < "2023-01-01") %>% summarise(min(Date), max(Date), mean(Date))

# dat_summary %>% filter(Date < "2021-01-01") %>% count(Filename)
# dat_summary %>% filter(Site =="MH005") %>% count(Filename) # some files with 2017 date? Ask Sandi but proceed without them

unique(dat_summary$Location.Name)
dat_summary %>% filter(grepl("D",Location.Name)) %>% count(Location.Name) # 7 mobile transects (EINP 2, JNP 3, WLNP 2)
dat_summary %>% filter(Date < "2024-01-01") %>% count(Location.Name)

dat_summary$Location.Name.Yr <- paste(dat_summary$Location.Name, substr(as.character(dat_summary$Date),1,4), sep="_")
to_file_dat_summary <- unique(dat_summary$Location.Name.Yr)
# dat_summary %>% filter(Location.Name=="139715_NE_01")

for(i in 1:length(to_file_dat_summary)){
  write.csv(dat_summary %>% filter(Location.Name.Yr == to_file_dat_summary[i]),
            paste0("./Input/NABat_ProcessedFiles/To_Be_Sorted/",to_file_dat_summary[i],"_summary.csv"))
}


######################################################################################################
# CREATE DATAFRAME OF RAW CALL DATA FILES (BEGINNING)
######################################################################################################
# set working directory for map output

# Create vector, and then dataframe, of raw data file names
rawdata.names <- list.files(path="./NABat_2023_Alberta_eBat_output", recursive = TRUE)
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
Files_per_Location <- Files_per_Location %>% ungroup()

### need to change to factor (I think) so dplyr select will work
NABat_deploy <- left_join(NABat_deploy, Files_per_Location %>% select(n:Location.Name_Year), by="Location.Name_Year")
glimpse(NABat_deploy)
head(NABat_deploy)

######################################################################################################
# MOVE FILES INTO APPROPRIATE FOLDERS #
######################################################################################################
getwd()

files_to_move <- list.files("./NABat_ProcessedFiles_DT/To_Be_Sorted")
GRTS_cells <- word(files_to_move,1,sep="_")
GRTS_cells <- GRTS_cells[grepl("M",GRTS_cells)] # remove the "mobile" grts cells

GRTS_cells

current_GRTS_cells <- list.files("./Input/NABat_ProcessedFiles")
current_GRTS_cells <- current_GRTS_cells[!grepl("To_Be",current_GRTS_cells)] # remove the to be sorted folder

setwd("./Input/NABat_ProcessedFiles")
for(i in 1:length(GRTS_cells)){
  ifelse(!dir.exists(GRTS_cells[i]), dir.create(GRTS_cells[i]), "Folder exists already")
}

NABat_stations <- word(files_to_move,1,3,sep="_")
NABat_stations <- NABat_stations[!grepl("M",NABat_stations)] # remove the "mobile" grts cells

for(i in 1:length(NABat_stations)){
  setwd(paste0("/Volumes/LaCie_2TB/NABat_Alberta/Input/NABat_ProcessedFiles/",word(NABat_stations[i],1,sep="_")))
  ifelse(!dir.exists(NABat_stations[i]), dir.create(NABat_stations[i]), "Folder exists already")
}

for(i in 1:length(NABat_stations)){
  setwd(paste0("/Volumes/LaCie_2TB/NABat_Alberta/Input/NABat_ProcessedFiles/",word(NABat_stations[i],1,sep="_"),"/",NABat_stations[i]))
  ifelse(!dir.exists("2022"), dir.create("2022"), "Folder exists already")
}

for(i in 1:length(NABat_stations)){
  file_to_copy <- files_to_move[grepl(NABat_stations[i], NABat_stations)]
  copy_to_folder<- paste0("/Volumes/LaCie_2TB/NABat_Alberta/Input/NABat_ProcessedFiles/",word(NABat_stations[i],1,sep="_"),"/",NABat_stations[i],"/2022")
  file.copy(paste0("/Volumes/LaCie_2TB/NABat_Alberta/Input/NABat_ProcessedFiles/To_Be_Sorted/",file_to_copy), copy_to_folder)
  }



######################################################################################################
# MOVE FILES INTO APPROPRIATE FOLDERS #
######################################################################################################
NABat_deploy <- read.csv("Input/NABat_Deployment_Data.csv", header = TRUE, stringsAsFactors = NA) 
glimpse(NABat_deploy)

setwd("/Volumes/LaCie_5TB/NABat_from_FTP/")

Pfolder.names <- list.files()

# old_names <- fs::dir_ls(recurse = 1)
# old_names <- old_names[!grepl(".csv",old_names)] # remove the csv files
# old_names <- old_names[!grepl("xlsx",old_names)] # remove the xlsx files
# old_names <- old_names[!grepl("doc",old_names)] # remove the doc files
# old_names <- old_names[!grepl("wav",old_names)] # remove the wav files
# old_names <- old_names[!grepl("txt",old_names)] # remove the txt files
# 
# old_names <- as.data.frame(as.character(old_names))
# colnames(old_names) <- "raw"
# old_names$Pfolder <- word(old_names$raw, 1, sep="/")
# old_names$Site <- word(old_names$raw, 2, sep="/")
# old_names %>% as_tibble
# old_names <- old_names %>% filter(!is.na(Site))
# unique(old_names$Pfolder)

glimpse(NABat_deploy)
unique(NABat_deploy$Contact)

rename_folders_to_NABat <- function(contact=contact, Pfolder_to_rename="EINP"){
  names <- NABat_deploy %>% filter(Contact == contact) %>% 
    select(Location.Name, Orig.Name)
  # names$Orig.Name <- word(names$Orig.Name,1)
  names$Orig.Name <- str_replace_all(names$Orig.Name,"_"," ")
  list.files(paste0("./",Pfolder_to_rename))
  file.rename(from=paste0("./",Pfolder_to_rename,"/",names$Orig.Name), to=paste0("./",Pfolder_to_rename,"/",names$Location.Name))
}

rename_folders_to_NABat(contact = "allison@mrwcc.ca", Pfolder_to_rename = "MRWCC")

list.files()
names <- NABat_deploy %>% filter(Contact == "barb.johnston@pc.gc.ca" | Contact == "barb.johnston@canada.ca; geoffrey.prophet@canada.ca" ) %>% 
  select(Location.Name, Orig.Name)
list.files("./2021_Banff NP")
file.rename(from=paste0("./2021_Banff NP/",Banff_names$Orig.Name), to=paste0("./2021_Banff NP/",Banff_names$Location.Name))


# add in the correct Location Names.
MRWCC_old_names <- list.files("./MRWCC")
MRWCC_old_names <- word(MRWCC_old_names, 1)
MRWCC_old_names <- MRWCC_old_names[!grepl("Landowner", MRWCC_old_names)]
MRWCC_old_names <- MRWCC_old_names[!grepl("locations", MRWCC_old_names)]
MRWCC_old_names <- as.data.frame(MRWCC_old_names)
colnames(MRWCC_old_names) <- "Site"
MRWCC_old_names <- MRWCC_old_names %>% mutate(new_names = case_when(
  Site == "Audet" ~ "99719_SE_01",
  Site == "Bakke" ~ "94599_NE_02",
  Site == "Balog" ~ "45447_SE_01",
  Site == "Bird" ~ "123587_SE_01",
  Site == "BobWillis" ~ "128391_NW_02",
  Site == "Buchanan" ~ "198023_NW_01",
  Site == "Cody" ~ "25283_SE_01",
  Site == "Cunningham" ~ "45447_NW_04",
  Site == "Ellertgarber" ~ "45447_NW_03",
  Site == "Finstad" ~ "219527_NE_01",
  Site == "Foggin" ~ "215687_NW_01",
  Site == "Ford" ~ "214407_NE_01",
  Site == "Galt" ~ "306599_NE_01",
  Site == "Hillmer" ~ "150723_SE_01",
  Site == "Joyce" ~ "45447_NW_02",
  Site == "King" ~ "62855_SE_01",
  Site == "Lee" ~ "126403_NE_01",
  Site == "Lindeman" ~ "160135_SE_02",
  Site == "Losey" ~ "17799_NE_01",
  Site == "MacCallum" ~ "84615_SW_01",
  Site == "Mccolloch" ~ "306599_NE_02",
  Site == "MccollochNorth" ~ "83335_SE_01",
  Site == "Obbay" ~ "225671_NW_01",
  Site == "Pimm" ~ "306599_SW_01",
  Site == "Ross" ~ "302012_NE_01",
  Site == "Russell" ~ "34183_NW_01",
  Site == "Shamber" ~ "25283_SE_02",
  Site == "Smith" ~ "94599_NE_01",
  Site == "Sommerfeldt" ~ "215687_NE_01",
  Site == "Stronski" ~ "83335_SW_01",
  Site == "Suzanne" ~ "34183_SW_01",
  Site == "Waldy" ~ "52419_SW_01",
  Site == "Walker" ~ "45447_SE_01",
  Site == "Wills" ~ "128391_NW_01"))

MRWCC_old_names
Pfolder_to_rename = "MRWCC"
names <- MRWCC_old_names
names$Orig.Name <- paste(names$Site, "Property")
names$Location.Name <- names$new_names
list.files(paste0("./",Pfolder_to_rename))
file.rename(from=paste0("./",Pfolder_to_rename,"/",names$Orig.Name), to=paste0("./",Pfolder_to_rename,"/",names$Location.Name))
rename_folders_to_NABat(contact = "allison@mrwcc.ca", )


list.files()
names <- NABat_deploy %>% filter(Contact == "barb.johnston@pc.gc.ca" | Contact == "barb.johnston@canada.ca; geoffrey.prophet@canada.ca" ) %>% 
  select(Location.Name, Orig.Name)
list.files("./2021_Banff NP")
file.rename(from=paste0("./2021_Banff NP/",Banff_names$Orig.Name), to=paste0("./2021_Banff NP/",Banff_names$Location.Name))
