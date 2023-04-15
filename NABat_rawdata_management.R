######################################################################################################
# NABat_rawdata_management.R
# created by Joanna Burgar, 19-Dec-2020
# script for summarising NABat raw data
# revised 3-Apr-2023 to assign GRTS to desktop Alberta eBat output files
######################################################################################################

######################################################################################################
# DIRECTORIES (BEGINNING)
######################################################################################################
getwd()
# RawFileDir = paste(getwd(),"NABat_RawFiles",sep="/")
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
# CREATE DATAFRAME OF SUMMARY FILES, EXPORTED BY ALBERTA EBAT SCRIPT
######################################################################################################

# Import summary files
dat_summary <- fs::dir_ls(path=paste("./NABat_2022_Alberta_eBat_output/"), 
                          regexp = "\\_summary.csv$", recurse = TRUE) %>%
  map_dfr(~read_csv(.x, col_types = cols(.default = "c")), .id="source") %>% # issues with min_wind so read in as character
  type_convert()  # convert back to proper formats

dat_summary$Pfolder <- word(dat_summary$source,3, sep="/") %>% str_remove("_summary.csv")
dat_summary %>% count(Pfolder)

# fs::dir_ls(path=paste("/Volumes/LaCie_5TB/NABat_from_FTP"), recurse = 1)

sta <- read.csv("NABat_Station_Covariates.csv") %>% filter(X2022==1)
sta %>% select(Orig_Name, Orig.Name)

sta.sub <- sta[c("GRTSCellID","LocName","Orig_Name")] %>% as_tibble() 
sta.sub %>% count(Orig_Name) %>% print(n=100)
dat_summary %>% count(Site) %>% print(n=100)

sta.sub <- sta.sub %>% mutate(match = case_when(Orig_Name %in% unique(dat_summary$Site) ~ "yes",
                                     TRUE ~ "no"))

sta.sub %>% count(match)
sta.sub %>% filter(match=="yes") %>% print(n=61) # all Bayne folders
sta.sub %>% filter(match=="no") %>% print(n=103)

dat_summary <- left_join(dat_summary, sta.sub %>% select(-match), by=c("Site"="Orig_Name"))
dat_summary <- dat_summary %>% rename(GRTS.Cell.ID = GRTSCellID, Location.Name=LocName)

dat_summary %>% count(Location.Name) %>% print(n=100)

dat_summary %>% select(source, Filename) %>% filter(grepl("WCS", source))

dat_summary %>% count(Site) %>% print(n=100)
Pfolder.names <- unique(dat_summary$Pfolder)
dat_summary %>% filter(Pfolder %in% Pfolder.names[9]) %>% count(Site) %>% print(n=50)
dat_summary %>% filter(Pfolder %in% Pfolder.names[6])

# add in the correct Location Names.
dat_summary <- dat_summary %>% mutate(Location.Name = case_when(Pfolder=="3667_Sarchuk" &  Site == "UNIT2" ~ "3667_NE_01",
                                                                Pfolder=="3667_Sarchuk" &  Site == "UNIT3" ~ "3667_SW_01",
                                                                Pfolder=="3667_Sarchuk" &  Site == "UNIT4" ~ "3667_SE_01",
                                                                
                                                                Pfolder=="ABMI_Bayne" &  Site == "BP-96N" ~ "219731_SW_01",

                                                                Pfolder=="BNP_2021_Fenlands" ~ "148842_NW_01",
                                                                Pfolder=="BNP_2021_Healy" ~ "165226_NE_01",
                                                                Pfolder=="BNP_2021_NABat" &  Site == "BNPmobile2021" ~ "Mobile_2021",
                                                                Pfolder=="BNP_2021_NABat" &  Site == "Fenlands2021" ~ "148842_NW_01",
                                                                Pfolder=="BNP_2021_NABat" &  Site == "GolfCourse2021" ~ "148842_NE_01",
                                                                Pfolder=="BNP_2021_NABat" &  Site == "UpperHotsprings2021" ~ "148842_SW_01",
                                                                Pfolder=="BNP_2022_NABat" &  Site == "BNPmobile2022" ~ "Mobile_2021",
                                                                Pfolder=="BNP_2022_NABat" &  Site == "Fenlands2022" ~ "148842_NW_01",
                                                                Pfolder=="BNP_2022_NABat" &  Site == "GolfCourse2022" ~ "148842_NE_01",
                                                                Pfolder=="BNP_2022_NABat" &  Site == "UpperHotsprings2022" ~ "148842_SW_01",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "Bryant2022" ~ "293385_NW_01",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "Divide2022" ~ "41322_SE_01",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "Flints2022" ~ "280601_NE_01",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "Indianhead2022" ~ "90474_NW_01",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "MooseMeadows2022" ~ "296805_SE_02",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "Paliser2022" ~ "235690_SW_01",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "Scotch2022" ~ "156010_SE_01",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "Sunshine2022" ~ "311483_SE_01",
                                                                Pfolder=="BNP_2022_Baseline" &  grepl("2022-", Site) ~ "311483_SE_01",
                                                                Pfolder=="BNP_2022_Baseline" &  Site == "Windy2022" ~ "127338_NW_01",
                                                                Pfolder=="BNP_Fenlands" ~ "148842_NW_01",
                                                                Pfolder=="BNP_Healy" ~ "165226_NE_01",
                                                                
                                                                Pfolder=="Copton" &  Site == "SHEEP1" ~ "221850_SE_01",
                                                                Pfolder=="Copton" &  Site == "SHEEP2" ~ "221850_SW_01",
                                                                Pfolder=="Sulphur" &  Site == "299-SULPHUR" ~ "197018_NW_01",
                                                                Pfolder=="Sulphur" &  Site == "FIREMANSPIT" ~ "197018_NE_01",
                                                                Pfolder=="Muskeg" &  Site == "316-MUSKEG1" ~ "9626_NW_01",
                                                                Pfolder=="Muskeg" &  Site == "MUSKEG2" ~ "9626_NW_01",
                                                                
                                                                Pfolder=="EINP" &  Site == "20220719" ~ "Mobile",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-NE" ~ "264037_SE_01",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-SH" ~ "264037_NE_01",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-ST" ~ "264037_NW_01",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-TR" ~ "264037_SW_01",
                                                                Pfolder=="EINP" &  Site == "EI-MINBAT-D1" ~ "MINBAT-D1",
                                                                Pfolder=="EINP" &  Site == "EI-MINBAT-D2" ~ "MINBAT-D2",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-D1" ~ "SM4BAT-D1",
                                                                Pfolder=="EINP" &  Site == "EI-SM4BAT-D2" ~ "SM4BAT-D2",
                                                                
                                                                Pfolder=="Edson_Switzer" &  Site == "204442NE01" ~ "204442_NE_01",
                                                                Pfolder=="Edson_Switzer" &  Site == "24218SW01" ~ "24218_SW_01",
                                                                Pfolder=="Edson_Switzer" &  Site == "254362NE01" ~ "254362_NE_01",
                                                                Pfolder=="Edson_Switzer" &  Site == "254362SE02" ~ "254362_SE_02",
                                                                Pfolder=="Edson_Switzer" &  Site == "89754SE01" ~ "89754_SE_01",
                                                                Pfolder=="Edson_Switzer" &  Site == "89754SE02" ~ "89754_SE_02",
                                                                
                                                                Pfolder=="JNP_Poco" ~ "267463_NW_01",
                                                                                                                              
                                                                Pfolder=="JNP" &  Site == "568028" ~ "Mobile",
                                                                Pfolder=="JNP" &  Site == "BUFFALO-PRAI" ~ "23146_SE_01",
                                                                Pfolder=="JNP" &  Site == "DECOIGNE" ~ "105066_SW_01",
                                                                Pfolder=="JNP" &  Site == "TEKARRA" ~ "23146_NE_01",
                                                                Pfolder=="JNP" &  Site == "TONQUIN" ~ "27242_NE_01",
                                                                Pfolder=="JNP" &  Site == "VALLEYOF5" ~ "23146_NE_02",
                                                                Pfolder=="JNP" &  Site == "WABASSO" ~ "23146_SE_02",
                                                                Pfolder=="JNP" &  Site == "WHIRLPOOL" ~ "309417_SW_01",
                                                                Pfolder=="JNP" &  Site == "WILLOW" ~ "209306_SW_01",
                                                                
                                                                Pfolder=="MRWCC" &  Site == "Audet" ~ "99719_SE_01",
                                                                Pfolder=="MRWCC" &  Site == "Bakke" ~ "94599_NE_02",
                                                                Pfolder=="MRWCC" &  Site == "Balog" ~ "45447_SE_01",
                                                                Pfolder=="MRWCC" &  Site == "Bird" ~ "123587_SE_01",
                                                                Pfolder=="MRWCC" &  Site == "BobWillis" ~ "128391_NW_02",
                                                                Pfolder=="MRWCC" &  Site == "Buchanan" ~ "198023_NW_01",
                                                                Pfolder=="MRWCC" &  Site == "Cody" ~ "25283_SE_01",
                                                                Pfolder=="MRWCC" &  Site == "Cunningham" ~ "45447_NW_04",
                                                                Pfolder=="MRWCC" &  Site == "Ellertgarber" ~ "45447_NW_03",
                                                                Pfolder=="MRWCC" &  Site == "Finstad" ~ "219527_NE_01",
                                                                Pfolder=="MRWCC" &  Site == "Foggin" ~ "215687_NW_01",
                                                                Pfolder=="MRWCC" &  Site == "Ford" ~ "214407_NE_01",
                                                                Pfolder=="MRWCC" &  Site == "Galt" ~ "306599_NE_01",
                                                                Pfolder=="MRWCC" &  Site == "Hillmer" ~ "150723_SE_01",
                                                                Pfolder=="MRWCC" &  Site == "Joyce" ~ "45447_NW_02",
                                                                Pfolder=="MRWCC" &  Site == "King" ~ "62855_SE_01",
                                                                Pfolder=="MRWCC" &  Site == "Lee" ~ "126403_NE_01",
                                                                Pfolder=="MRWCC" &  Site == "Lindeman" ~ "160135_SE_02",
                                                                Pfolder=="MRWCC" &  Site == "Losey" ~ "17799_NE_01",
                                                                Pfolder=="MRWCC" &  Site == "MacCallum" ~ "84615_SW_01",
                                                                Pfolder=="MRWCC" &  Site == "Mccolloch" ~ "306599_NE_02",
                                                                Pfolder=="MRWCC" &  Site == "MccollochNorth" ~ "83335_SE_01",
                                                                Pfolder=="MRWCC" &  Site == "Obbay" ~ "225671_NW_01",
                                                                Pfolder=="MRWCC" &  Site == "Pimm" ~ "306599_SW_01",
                                                                Pfolder=="MRWCC" &  Site == "Ross" ~ "302012_NE_01",
                                                                Pfolder=="MRWCC" &  Site == "Russell" ~ "34183_NW_01",
                                                                Pfolder=="MRWCC" &  Site == "Shamber" ~ "25283_SE_02",
                                                                Pfolder=="MRWCC" &  Site == "Smith" ~ "94599_NE_01",
                                                                Pfolder=="MRWCC" &  Site == "Sommerfeldt" ~ "215687_NE_01",
                                                                Pfolder=="MRWCC" &  Site == "Stronski" ~ "83335_SW_01",
                                                                Pfolder=="MRWCC" &  Site == "Suzanne" ~ "34183_SW_01",
                                                                Pfolder=="MRWCC" &  Site == "Waldy" ~ "52419_SW_01",
                                                                Pfolder=="MRWCC" &  Site == "Walker" ~ "45447_SE_01",
                                                                Pfolder=="MRWCC" &  Site == "Wills" ~ "128391_NW_01",
                                                                
                                                                Pfolder=="MHAC" ~ "252355_SE_01",
                                                                
                                                                Pfolder=="Medicine_Hat" &  Site == "MH005" ~ "329075_NW_02",
                                                                Pfolder=="Medicine_Hat" &  Site == "MH1001" ~ "322807_SE_01",
                                                                Pfolder=="Medicine_Hat" &  Site == "MH1002" ~ "199047_NW_01",
                                                                Pfolder=="Medicine_Hat" &  Site == "MH1004" ~ "203143_SW_01",
                                                                Pfolder=="Medicine_Hat" &  Site == "MHBBB" ~ "224135_SE_05",
                                                                Pfolder=="Medicine_Hat" &  Site == "MHPA7" ~ "322807_SE_02",
                                                                
                                                                Pfolder=="WBNP" &  Site == "NW-PATROLCBN" ~ "336731_NW_01",
                                                                Pfolder=="WBNP" &  Site == "PINELRD35" ~ "303706_NW_01",
                                                                Pfolder=="WBNP" &  Site == "RAINBOW" ~ "303706_NE_01",
                                                                Pfolder=="WBNP" &  Site == "SW--HUBBY" ~ "336731_SW_01",
                                                                Pfolder=="WBNP" &  Site == "SW-KETTLE" ~ "336731_SE_01",
                                                                Pfolder=="WBNP" &  Site == "SW-PARSONS" ~ "303706_SW_01",
                                                                Pfolder=="WBNP" &  Site == "WBNP" ~ "Mobile",
                                                                
                                                                Pfolder=="WCS" &  grepl("ACBPHampshireClose",Filename) ~ "30403_SW_01",
                                                                Pfolder=="WCS" &  grepl("ACBPTopaz",Filename) ~ "38595_SE_01",
                                                                Pfolder=="WCS" &  grepl("ACBPCEI",Filename) ~ "42883_SE_01",
                                                                Pfolder=="WCS" &  grepl("ACBPWARanches",Filename) ~ "120707_NE_01",
                                                                Pfolder=="WCS" &  grepl("ACBPOldmanSH",Filename) ~ "163011_SW_01",
                                                                Pfolder=="WCS" &  grepl("NCCFlemingNW",Filename) ~ "326009_NE_01",
                                                                Pfolder=="WCS" &  grepl("NCCKerrSW",Filename) ~ "326009_NW_01",
                                                                Pfolder=="WCS" &  grepl("GoldenRanchesNorth",Filename) ~ "99523_SW_01",
                                                                Pfolder=="WCS" &  grepl("GoldenRanchesSouth",Filename) ~ "99523_SW_02",
                                                                Pfolder=="WCS" &  grepl("Hicks",Filename) ~ "115907_NW_01",
                                                                Pfolder=="WCS" &  grepl("GamblingLake",Filename) ~ "115907_SE_01",
                                                                Pfolder=="WCS" &  grepl("TomahawkEastAndex",Filename) ~ "171651_NE_01",
                                                                Pfolder=="WCS" &  grepl("TomahawkWestAndex",Filename) ~ "171651_NE_02",
                                                                
                                                                Pfolder=="WLNP" &  grepl("Mobile",Filename) ~ "Mobile",
                                                                Pfolder=="WLNP_Stationary" &  grepl("BLAK",Filename) ~ "139715_NE_01",
                                                                Pfolder=="WLNP_Stationary" &  grepl("CAM",Filename) ~ "139715_SW_01",
                                                                Pfolder=="WLNP_Stationary" &  grepl("POW",Filename) ~ "205251_SW_01",
                                                                
                                                                TRUE ~ Location.Name))

dat_summary %>% filter(is.na(Location.Name)) %>% group_by(Pfolder) %>% count(Site)
# EINP = already included in other files? (these are kaleidoscope outputs)
# ABMI_Bayne = site outside of AB
# MRWCC = Cartier just noise calls and can't seem to get GRTS Cell Id for coordinates, other are random files not sure what site
# good to filter out all NA files and proceed

dat_summary %>% filter(!is.na(Location.Name)) %>% count(Pfolder) %>% print(n=33)
glimpse(dat_summary)
dat_summary$GRTS.Cell.ID <- word(dat_summary$Location.Name, 1,1,sep="_")
dat_summary <- dat_summary %>% mutate(GRTS.Cell.ID = case_when(grepl("M",GRTS.Cell.ID) ~ "Mobile",
                                                                TRUE ~ GRTS.Cell.ID))

dat_summary <- dat_summary %>% filter(!is.na(Location.Name))

dat_summary %>% filter(GRTS.Cell.ID!="Mobile") %>% count(GRTS.Cell.ID) # 121 GRTS cells
dat_summary %>% filter(GRTS.Cell.ID!="Mobile") %>% count(Location.Name) # 164 stations

dat_summary %>% filter(Date > "2022-01-01") %>% summarise(min(Date), max(Date), mean(Date))
dat_summary %>% filter(Date < "2022-01-01") %>% summarise(min(Date), max(Date), mean(Date))

dat_summary %>% filter(Date < "2021-01-01") %>% count(Filename)
dat_summary %>% filter(Site =="MH005") %>% count(Filename) # some files with 2017 date? Ask Sandi but proceed without them

unique(dat_summary$Location.Name)
dat_summary %>% filter(grepl("M",GRTS.Cell.ID)) %>% count(Location.Name)
dat_summary %>% filter(Date < "2022-01-01") %>% count(Location.Name)

dat_summary$Location.Name.Yr <- paste(dat_summary$Location.Name, substr(as.character(dat_summary$Date),1,4), sep="_")
to_file_dat_summary <- unique(dat_summary$Location.Name.Yr)

for(i in 1:length(to_file_dat_summary)){
  write.csv(dat_summary %>% filter(Location.Name.Yr == to_file_dat_summary[i]),
            paste0("./Input/NABat_ProcessedFiles/To_Be_Sorted/",to_file_dat_summary[i],"_summary.csv"))
}


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
