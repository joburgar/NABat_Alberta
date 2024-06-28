#Load Packages
list.of.packages <- c("data.table", "leaflet", "tidyverse", "lunar", "zoo", "colortools", "lubridate", "camtrapR", "circular", "RColorBrewer", "Cairo", "viridis", "knitr", "sf","osmdata", "ggspatial", "ggmap","gridExtra", "grid")
# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
source("NABat_sum_to_count_function.R") 

lapply(list.of.packages, require, character.only = TRUE)

# Timezone 
tz <- "UTC"

# Set a single catagorical variable of interest from station covariates (`sta`) for summary graphs. If you do not have and appropriate category use "Project.ID".
category <- "GRTS.Cell.ID"

# Define a colour from the R options to base the colourscheme
colour <- "lightseagreen"

# # Define the GRTS.Cell.ID and Year of interest if subsetting for year, studyarea
# GRTS_interest <- "922"
Year_interest <- year(as.Date("2023-01-01"))

# # Import count files
# dat_count <- fs::dir_ls(path="./Input/NABat_ProcessedFiles_DT",regexp = "\\counts.csv$", recurse = TRUE) %>%
#   map_dfr(read_csv, col_types = cols(.default = 'c'), .id = "source") %>% 
#   type_convert() %>%
#   mutate(SurveyNight = ymd(Date, truncated = 1)) %>%
#   mutate(Year = year(SurveyNight), Month = month(SurveyNight, label = T), jDay = yday(SurveyNight)) %>%
#   filter(Year==Year_interest)
# 
# #View(dat_count)
# dat_count$Orig.Name <- as.factor(paste(dat_count$Site, dat_count$Detector, sep="_"))
# dat_count$GRTS.Cell.ID <- as.factor(word(dat_count$source,4,sep = "\\/"))
# dat_count$Location.Name <- as.factor(word(dat_count$Detector,-1))
# dat_count$Deployment.ID <- as.factor(word(dat_count$source,5,sep = "\\/"))
# dat_count$Classification <- as.factor(dat_count$Classification %>% 
#                                         recode(EPFU.LANO = "EPFU-LANO", LABO.MYLU = "LABO-MYLU",
#                                                My_40k = "Myotis 40k", MYEV.MYSE = "MYEV-MYSE"))
# dat_count$Classification <- factor(dat_count$Classification,
#                                    levels = c("EPFU", "EPFU-LANO", "LANO", "LACI", "LABO", "LABO-MYLU", "MYLU",
#                                               "MYCA", "MYCI", "MYEV","MYEV-MYSE", "MYSE", "MYVO", "Myotis 40k",
#                                               "unknown", "noise"))
# summary(dat_count)
# dat_count <- dat_count%>% arrange(GRTS.Cell.ID)
# dat_count %>% group_by(GRTS.Cell.ID) %>% count(Site, Detector)
# 
# # put WLNP in wrong GRTS cell for analysis
# dat_count <- dat_count %>% mutate(GRTS.Cell.ID = case_when(grepl("WLNP",Orig.Name) ~ as.character("25027"),
#                      TRUE ~ as.character(GRTS.Cell.ID)))
# # finding unique surveys
# dat_count$Cell_Date <- paste(dat_count$GRTS.Cell.ID, dat_count$SurveyNight, sep="_")

# Import summary files
dat_summary <- fs::dir_ls(path=paste("./Input/NABat_ProcessedFiles_DT/"), 
                          regexp = "\\_summary.csv$", recurse = TRUE) %>%
  map_dfr(~read_csv(.x, col_types = cols(.default = "c")), .id="source") %>% # issues with min_wind so read in as character # not necessary if using desktop version
  type_convert() %>% # convert back to proper formats, min_wind still problematic so only use max or mean wind if available
  mutate(SurveyNight = ymd(Date, truncated = 1)) %>%
  mutate(Year = as.factor(year(SurveyNight)), Month = month(SurveyNight, label = T), jDay = yday(SurveyNight)) %>%
  filter(Year==Year_interest) %>% 
  select(-Date)%>%
  rename(Orig.Name=Site)

# dat_summary$min_wind <- as.numeric(dat_summary$min_wind)
glimpse(dat_summary)
dat_summary <- dat_summary[c("Orig.Name","Filename","n_calls","prob1","sp1","prob2","sp2","prob3","sp3","source","GRTS.Cell.ID","SurveyNight","Year","Month","jDay")]
dat_summary$GRTS.Cell.ID <- as.factor(word(dat_summary$source,4,sep = "\\/"))
dat_summary$Deployment.ID <- as.factor(word(dat_summary$source,5,sep = "\\/"))
dat_summary$Location.Name <- as.factor(paste(dat_summary$GRTS.Cell.ID, "Mobile", sep="_"))
# dat_summary$Location.Name <- as.factor(word(dat_summary$source,6,sep = "\\/"))
# dat_summary$Location.Name <- str_replace(dat_summary$Location.Name, "_summary.csv","")

dat_summary %>% count(Location.Name)

# finding unique surveys
dat_summary$Cell_Date <- paste(dat_summary$GRTS.Cell.ID, dat_summary$SurveyNight, sep="_")
# check that dates are unique surveys not just going past midnight
glimpse(dat_summary)
dat_summary %>% count(GRTS.Cell.ID) # 139715; 203370; 264037; 267463

dat_summary %>% filter(GRTS.Cell.ID==267463) %>%
  select(GRTS.Cell.ID,Orig.Name, Filename, SurveyNight) %>% print(n=160)

dat_summary <- dat_summary %>% mutate(SurveyNight = case_when(Orig.Name=="JNP-D2-TOWN" ~ as.Date("2023-06-16"),
                                                              Orig.Name=="JNP-D3-TOWN" ~ as.Date("2023-06-23"),
                                                                TRUE ~ as.Date(SurveyNight)))

dat_summary$Cell_Date <- paste(dat_summary$GRTS.Cell.ID, dat_summary$SurveyNight, sep="_")

dat_summary %>% group_by(Location.Name, Orig.Name) %>% count(Cell_Date)
dat_summary %>% group_by(Location.Name, Orig.Name) %>% count(SurveyNight)

# Create file type to differentiate zc from wav
dat_summary$File.Type <- str_sub(dat_summary$Filename,-2,-1) %>% recode("0#" = "zc", "av"="wav")
dat_summary %>% group_by(File.Type) %>% count(Orig.Name)
nrow(dat_summary)
# Extract the time from filename and put in date format
dat_summary$Time.temp1 <- str_extract(dat_summary$Filename,"_[0-9]{6}.wav") %>% str_sub(2,7)
dat_summary$Time.temp2 <- str_extract(dat_summary$Filename,"[0-9]{2}\\-[0-9]{2}\\-[0-9]{2} [0-9]{2}\\-[0-9]{2}\\-[0-9]{2}") %>% str_sub(-8,-1)
dat_summary$Time.temp3 <- str_extract(dat_summary$Filename,"_[0-9]{6}_") %>% str_sub(2,7)

dat_summary$Time.temp1p <- as.POSIXct(strptime(dat_summary$Time.temp1, "%H%M%S", tz))
dat_summary$Time.temp2p <- as.POSIXct(strptime(dat_summary$Time.temp2, "%H-%M-%S", tz))
dat_summary$Time.temp3p <- as.POSIXct(strptime(dat_summary$Time.temp3, "%H%M%S", tz))
dat_summary <- dat_summary %>% mutate(Timep = coalesce(Time.temp1p, Time.temp2p, Time.temp3p)) %>%
  select(-c(Time.temp1, Time.temp2, Time.temp3, Time.temp1p, Time.temp2p, Time.temp3p))


# Read deployment data csv for station covariates
eff <- read.csv("Input/NABat_Deployment_Data_DT_2023.csv", header=T) %>%
  mutate(Survey.Start.Time = ymd(Survey.Start.Time), Survey.End.Time = ymd(Survey.End.Time))
eff$Survey.End.Time - eff$Survey.Start.Time
eff <- eff %>% filter(Deployment.ID>as.numeric(Year_interest)-1)
as.data.frame(eff %>% group_by(Deployment.ID) %>% count(GRTS.Cell.ID)) # 6 NABat cells surveyed
eff


# finding unique surveys
eff$Cell_Date <- paste(eff$GRTS.Cell.ID, eff$Survey.Start.Time, sep="_")

eff.names <- eff %>% group_by(Location.Name) %>% count(GRTS.Cell.ID)
eff.names$GRTS.Cell.ID <- as.factor(eff.names$GRTS.Cell.ID)
dat.names.sum <- dat_summary %>% group_by(Orig.Name) %>% count(GRTS.Cell.ID)
names.join <- as.data.frame(full_join(eff.names, dat.names.sum, by="GRTS.Cell.ID"))
names.join[is.na(names.join$n.y),] # will show missing deployment data
names.join[is.na(names.join$n.x),] # will show missing deployment data
 
NABat.stns.to.use <- unique(eff$Location.Name)

###--- add covariates to dat objects
eff <- eff %>% mutate(NP = case_when(grepl("NP",Land.Unit.Code) ~ "In",
                                     !grepl("NP",Land.Unit.Code) ~ "Out"))
eff$Location.Name <- paste0(eff$Location.Name, eff$SurveyNum)

dat_summary$NP <- eff$NP[match(dat_summary$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]
dat_summary$Natural.Region <- eff$NRNAME[match(dat_summary$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]
dat_summary$Land.Unit.Code <- eff$Land.Unit.Code[match(dat_summary$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]
# dat_count$Land.Use.Type <- eff$Land.Use.Type[match(dat_count$GRTS.Cell.ID, sta$GRTS.Cell.ID,)]

dat_summaryT <- dat_summary[complete.cases(dat_summary$Timep),]

# now prep to use Alberta eBat criteria to classify species
dat_sum_sub <- dat_summaryT[c("GRTS.Cell.ID","Location.Name","SurveyNight","Year","Month","jDay","Timep",
                              "Filename","n_calls","prob1","sp1","prob2","sp2","prob3","sp3")]

# check for erroneous time stamps and remove entire survey
Timepdate <- date(Sys.time())
Timepdatetime1 <- as.POSIXct(paste(Timepdate,"08:00:00"), tz)
Timepdatetime2 <- as.POSIXct(paste(Timepdate,"18:00:00"),tz)

timestamp.error <- as.data.frame(dat_sum_sub %>% filter(Timep >Timepdatetime1 & Timep<Timepdatetime2) %>% filter(sp1!="noise") %>% group_by(Location.Name) %>% select(Filename))
unique(timestamp.error$Location.Name)
unique(timestamp.error$Filename)

dat_sum_sub %>% filter(Location.Name %in% timestamp.error$Location.Name) %>% count(GRTS.Cell.ID)
dat_sum_sub <- dat_sum_sub %>% filter(!Filename%in%timestamp.error$Filename)

# create thresholds for noise and bat classifications
threshold_noise <- 0.8; threshold_bat <- 0.5
# start with all call sequences as "unknown"
dat_sum_sub$category <- "unknown"

# Index cases to be categorized as noise
index_noise <- with(dat_sum_sub, (sp1 == "noise") & (prob1 > threshold_noise)) 
# Set the indexed categories to "noise" 
dat_sum_sub$category[index_noise] <- "noise"

# Index the noise values to be filtered out
index_remove_noise <- with(dat_sum_sub, (category == "unknown") & (sp1 == "noise")) 

# Note that there are 126 `NA` values
sum(is.na(index_remove_noise)) 
# These `NA` values are due to an `NA` in the `sp1` column which is the result of a tie in the random forest probabilities
dat_sum_sub[is.na(index_remove_noise),]

# Set any `NA` value to `FALSE` (i.e. not a noise value to be filtered out)
index_remove_noise[is.na(index_remove_noise)] <- FALSE 
# For the noise values to be filtered out, get the corresponding probabilities
prob_noise <- dat_sum_sub[index_remove_noise, "prob1"] 
# Record the state of the data frame before the filtering out of noise
dat_sum_sub_before_noise_removal <- dat_sum_sub

dat_sum_sub[index_remove_noise, "sp1"] <- dat_sum_sub[index_remove_noise, "sp2"]
dat_sum_sub[index_remove_noise,"sp2"] <- dat_sum_sub[index_remove_noise, "sp3"] 
dat_sum_sub[index_remove_noise, "prob1"] <- dat_sum_sub[index_remove_noise, "prob2"] 
dat_sum_sub[index_remove_noise,"prob2"] <- dat_sum_sub[index_remove_noise, "prob3"] 

# Now that the `prob3` value has been shifted to the `prob2` column, replace the `prob3` column with `NA` values, for the cases where noise has been filtered out
dat_sum_sub[index_remove_noise, "prob3"] <- NA 
dat_sum_sub[index_remove_noise, "sp3"] <- NA

dat_sum_sub[index_remove_noise, "prob1"] <- dat_sum_sub[index_remove_noise, "prob1"] / (1 - prob_noise) 
dat_sum_sub[index_remove_noise, "prob2"] <- dat_sum_sub[index_remove_noise, "prob2"] / (1 - prob_noise) 
# Compare "sp1" columns before and after filtering out noise
table(dat_sum_sub_before_noise_removal$sp1)
table(dat_sum_sub$sp1)

index_bat_species <- with(dat_sum_sub, (category == "unknown") & (n_calls >= 3) & (prob2 / prob1 <= 0.80)) 

index_bat_species[is.na(index_bat_species)] <- FALSE # to deal with the NA values

dat_sum_sub$category[index_bat_species] <- dat_sum_sub$sp1[index_bat_species]

eBat_param <- read.csv("./Input/eBat_param.csv")
dim(eBat_param)
tmp <- as.data.frame(lapply(eBat_param[,3:13], function(y) gsub("±.*$", "", y)))
param <- eBat_param$Parameter
eBat_param <- tmp %>% mutate_at(vars(1:11), as.numeric) # make sure all call data is numeric
eBat_param <- as.data.frame(t(eBat_param))
colnames(eBat_param) <- param
eBat_param %>% arrange(Fmin) %>% select(Fmin)
eBat_param$sp <- rownames(eBat_param)
eBat_param <- eBat_param %>% filter(sp!="noise")

# https://pubs.usgs.gov/of/2018/1068/ofr20181068.pdf
# HighF = min freq >30
# 40k = min freq 35-45
# Myotis40k = Myotis 35-40 min freq
# LowF = min freq <30
# 25k = min freq 15-25

Q25_sp <- as.character(eBat_param %>% filter(Fmin<25) %>% select(sp))
Q40_sp <- as.character(unlist(eBat_param %>% filter(Fmin>=35 & Fmin<=45) %>% select(sp)))
LowF_sp <- as.character(unlist(eBat_param %>% filter(Fmin<=30) %>% select(sp)))
HighF_sp <- as.character(unlist(eBat_param %>% filter(Fmin>30) %>% select(sp)))

Q25_index <- (dat_sum_sub$sp1 %in% Q25_sp) & (dat_sum_sub$prob1 < 0.4) & (dat_sum_sub$prob1 > 0.2)
Q40_index <- (dat_sum_sub$sp1 %in% Q40_sp) & (dat_sum_sub$sp2 %in% Q40_sp) & (dat_sum_sub$prob1 < 0.4) & (dat_sum_sub$prob2 > 0.2)
LowF_index <- (dat_sum_sub$sp1 %in% LowF_sp) & (dat_sum_sub$sp2 %in% LowF_sp) & (dat_sum_sub$prob1 < 0.4) & (dat_sum_sub$prob2 > 0.2)
HighF_index <- (dat_sum_sub$sp1 %in% HighF_sp) & (dat_sum_sub$sp2 %in% HighF_sp) & (dat_sum_sub$prob1 < 0.4) & (dat_sum_sub$prob2 > 0.2)

LABO.MYLU_index <- (dat_sum_sub$sp1 %in% c("LABO", "MYLU")) & (dat_sum_sub$sp2 %in% c("LABO", "MYLU")) & (dat_sum_sub$prob1 < 0.4) & (dat_sum_sub$prob2 > 0.2)

EPFU.LANO_index <- (dat_sum_sub$sp1 %in% c("EPFU", "LANO")) & (dat_sum_sub$sp2 %in% c("EPFU", "LANO")) & (dat_sum_sub$prob1 < 0.4) & (dat_sum_sub$prob2 > 0.2)

MYEV.MYSE_index <- (dat_sum_sub$sp1 %in% c("MYEV", "MYSE")) & (dat_sum_sub$sp2 %in% c("MYEV", "MYSE")) & (dat_sum_sub$prob1 < 0.4) & (dat_sum_sub$prob2 > 0.2)

My_40k_index <- (dat_sum_sub$sp1 %in% c("MYCI", "MYLU", "MYVO")) & (dat_sum_sub$sp2 %in% c("MYCI", "MYLU", "MYVO")) & (dat_sum_sub$prob1 < 0.4) & (dat_sum_sub$prob2 > 0.2)

#' Check the number of call sequences belonging to each category:
sum(HighF_index, na.rm=T)
sum(Q40_index, na.rm=T)
sum(My_40k_index, na.rm=T)
sum(LABO.MYLU_index, na.rm=T)
sum(MYEV.MYSE_index, na.rm=T)

sum(LowF_index, na.rm=T)
sum(Q25_index, na.rm=T)
sum(EPFU.LANO_index, na.rm=T)

#' Define a new data frame:  
dat_sum_sub_df <- dat_sum_sub
dat_sum_sub_df$category <- dat_sum_sub_df$sp1

# for all species with the "unknown" classification
# reclassify going from coarser to finer resolution of ID
dat_sum_sub_df %>% count(category)

dat_sum_sub_df$category[dat_sum_sub_df$n_calls < 3 | 
                          dat_sum_sub_df$prob1 < threshold_bat & dat_sum_sub_df$sp1 != "noise"] <- "unknown"

dat_sum_sub_df$category[HighF_index] <- "HighF"
dat_sum_sub_df$category[Q40_index] <- "40k"
dat_sum_sub_df$category[My_40k_index] <- "My_40k"

dat_sum_sub_df$category[LABO.MYLU_index] <- "LABO.MYLU"
dat_sum_sub_df$category[MYEV.MYSE_index] <- "MYEV.MYSE"

dat_sum_sub_df$category[LowF_index] <- "LowF"
dat_sum_sub_df$category[Q25_index] <- "25k"
dat_sum_sub_df$category[EPFU.LANO_index] <- "EPFU.LANO"

# dat_sum_sub_df <- dat_sum_sub_df %>% filter(n_calls<3)

dat_time <- dat_sum_sub_df %>% filter(n_calls>2) %>% select(-n_calls:-sp3) %>% filter(category!="noise")
# unique(dat_time$category)
# dat_time %>% count(category)

dat_time$Classification <- as.factor(dat_time$category %>% 
                                       recode(EPFU.LANO = "EPFU-LANO", LABO.MYLU = "LABO-MYLU",
                                              My_40k = "Myotis 40k", MYEV.MYSE = "MYEV-MYSE"))

# levels(dat_time$Classification)
dat_time$Classification <- factor(dat_time$Classification,
                                  levels = c("EPFU", "EPFU-LANO", "LANO", "LACI", "LABO", "LABO-MYLU", "MYLU",
                                             "MYCA", "MYCI", "MYEV","MYEV-MYSE", "MYSE", "MYVO", "Myotis 40k",
                                             "40k","25k","HighF","LowF","unknown"))

dat_time <- as.data.frame(dat_time) 
dat_time$Time <- format(dat_time$Timep, format = "%H:%M:%S")

# covariates for overlap
dat_time$NP <- eff$NP[match(dat_time$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]
dat_time$Land.Use.Type <- eff$Land.Cover[match(dat_time$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]
dat_time$Natural.Region <- eff$Natural.Region[match(dat_time$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]


dat_time %>% count(Classification)
nrow(dat_time) # 311
# difference is that dat_time has filtered out all "calls" with <3 pulses
# inclined to go with dat_time as splits unknown down a bit

dat_time_agg <- dat_time %>% group_by(GRTS.Cell.ID, Location.Name, SurveyNight) %>% count(Classification)
colnames(dat_time_agg)[5] <- "Count"
dat_time_agg$Count <- as.numeric(dat_time_agg$Count)
dat_time_agg$Year <- as.factor(year(dat_time_agg$SurveyNight))
dat_time_agg$Month <- month(dat_time_agg$SurveyNight)
dat_time_agg$jDay <- yday(dat_time_agg$SurveyNight)
dat_time_agg$NP <- eff$NP[match(dat_time_agg$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]
dat_time_agg$Land.Use.Type <- eff$Land.Cover[match(dat_time_agg$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]
dat_time_agg$Natural.Region <- eff$Natural.Region[match(dat_time_agg$GRTS.Cell.ID, eff$GRTS.Cell.ID,)]

dat_time_agg %>% ungroup() %>% summarise(min(SurveyNight), max(SurveyNight))
dat_time_agg %>% ungroup() %>% count(Classification)
dat_time_agg %>% ungroup() %>% summarise(sum(Count))
dat_time_agg %>% ungroup() %>% count(GRTS.Cell.ID)
unique(dat_time_agg$Location.Name)
length(unique(dat_time_agg$SurveyNight))

# Call df without the noise files
call_count <- dat_time_agg %>% ungroup () # no noise files but keeping this to keep the same code below, to use with dat_tmime_agg

nrow(dat_sum_sub) # 331 files
nrow(call_count) # 36

call_count %>% as_tibble()

call_count$GRTS.Cell.ID.SurveyNight <- paste(call_count$GRTS.Cell.ID, call_count$Location.Name, sep="_")
call_count %>% group_by(GRTS.Cell.ID.SurveyNight) %>% summarise(total.calls = sum(Count)) %>% arrange(total.calls)
call_count %>% group_by(GRTS.Cell.ID.SurveyNight,Classification) %>% summarise(total.calls = sum(Count)) %>% arrange(desc(total.calls))
unknown.calls <- call_count %>% filter(Classification=="unknown") %>% summarise(sum(Count)) / sum(call_count$Count)

# 0.2508039 % unknown calls

call_count %>% group_by(GRTS.Cell.ID.SurveyNight) %>% summarise(sum(Count))
names(call_count2)
# subset data for overall mean nightly bat calls by year for each GRTS
call_count2 <- call_count %>% group_by(GRTS.Cell.ID, SurveyNight)

# Reorder GRTS to be NP, Land.Unit.Code then GRTS
GRTS.Order <- call_count %>% arrange(GRTS.Cell.ID) 
GRTS.Order$Order <- row.names(GRTS.Order)
GRTS.Order <- fct_reorder(GRTS.Order$GRTS.Cell.ID, GRTS.Order$Order, min)

se <- function(x) sqrt(var(x)/length(x))

GRTS.Calls <- call_count %>% group_by(GRTS.Cell.ID) %>% 
  summarise(Mean = mean(Count), SE = se(Count))

calls.GRTS <- GRTS.Calls %>%
  ggplot(aes(x = GRTS.Cell.ID, Mean), na.rm=T)+
  geom_point(colour="white", shape=21, size=4,aes(fill=GRTS.Cell.ID))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Mean ± 1 SE Nightly Bat Calls"))) +
  xlab(expression("NABat GRTS Cell ID"))+
  geom_linerange(aes(GRTS.Cell.ID, ymin = Mean-SE, ymax = Mean+SE)) +
  theme_classic()+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 8)) +
  theme(legend.position = "none")

Cairo(file="Output/calls.2023.GRTS.plot.PNG", 
      type="png",
      width=2000, 
      height=1500, 
      pointsize=14,
      bg="white",
      dpi=300)
calls.GRTS
dev.off()

GRTS.Calls.SN <- call_count %>% group_by(GRTS.Cell.ID, SurveyNight) %>% 
  summarise(Mean = mean(Count), SE = se(Count))
GRTS.Calls.SN$SurveyNum <- c(1,2,1,2,1,2,1)
GRTS.Calls.SN$Unique <- paste(GRTS.Calls.SN$GRTS.Cell.ID, GRTS.Calls.SN$SurveyNum, sep="_")
call_count %>%  group_by(GRTS.Cell.ID, SurveyNight) %>% summarise(Count = sum(Count)) %>% arrange(Count) %>% print(n=59)

calls_per_night <- dat_summaryT %>% group_by(SurveyNight) %>% count(GRTS.Cell.ID) %>% arrange(n)
calls_per_night %>% ungroup() %>% summarise(mean(n), min(n), max(n), se(n))
summary(calls_per_night$n)
dat_summaryT %>% group_by(GRTS.Cell.ID, SurveyNight) %>% summarise(max(Timep))

fcalls.GRTS <- GRTS.Calls.SN %>%
  ggplot(aes(x = Unique, Mean), na.rm=T)+
  geom_point(colour="white", shape=21, size=4,aes(fill=GRTS.Cell.ID))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab(expression(paste("Mean ± 1 SE Nightly Bat Calls"))) +
  xlab(expression("NABat GRTS Cell ID"))+
  geom_linerange(aes(Unique, ymin = Mean-SE, ymax = Mean+SE)) +
  theme_classic()+
  theme(axis.text.y = element_text(size=12))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 8)) +
  theme(legend.position = "none")

Cairo(file="Output/fcalls.2023.GRTS.plot.PNG", 
      type="png",
      width=2000, 
      height=1500, 
      pointsize=14,
      bg="white",
      dpi=300)
fcalls.GRTS
dev.off()


# Generate colours to display the species levels
call_count$Classification <- droplevels(call_count$Classification)
call_count$GRTS.Cell.ID.SurveyNight <- paste(call_count$GRTS.Cell.ID, call_count$SurveyNight, sep="_")

eff$GRTS.Cell.ID.SurveyNight <- paste(eff$GRTS.Cell.ID, eff$Survey.Start.Time, sep="_")
call_count$SurveyNum <- eff$SurveyNum[match(call_count$GRTS.Cell.ID.SurveyNight, eff$GRTS.Cell.ID.SurveyNight)]

Sp.hist.mob.data <- call_count %>% group_by(GRTS.Cell.ID, Location.Name, SurveyNum, SurveyNight, Classification) %>% summarise(sum=Count)
Sp.hist.mob <- ggplot(data = Sp.hist.mob.data, aes(x = Classification, y = sum, fill=as.factor(SurveyNum))) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette = "Set1")+
  theme_classic() + xlab("Species / Species Group") + ylab("Total Bat Calls") + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 8)) +
  theme(axis.title.y = element_text(size = 14)) +   theme(axis.title.x = element_blank())+
  facet_wrap(~GRTS.Cell.ID, ncol = 4, scales = "free_y")

Cairo(file="Output/Sp.hist.2023.mob.plot.PNG", 
      type="png",
      width=2000, 
      height=1200, 
      pointsize=14,
      bg="white",
      dpi=300)
Sp.hist.mob
dev.off()

# Determine total effort (survey nights)
total.effort <- call_count %>% group_by(Location.Name, SurveyNum) %>% summarise_at(c("SurveyNight"), list(Min = min, Max=max))
total.effort$Diff <- (total.effort$Max - total.effort$Min)+1
sum(total.effort$Diff) # 7 survey nights

# Determine total calls
# call_count %>% filter(grepl("swift|SM2", Detector)) %>% group_by(Classification) %>% summarise(sum(Count))

call_count %>% group_by(Classification) %>% summarise(sum(Count))
Table.Calls <- call_count %>% group_by(Classification) %>% summarise(Call.Count = sum(Count))
Table.Calls$Per.Known <- as.data.frame(round(Table.Calls$Call.Count/sum(Table.Calls$Call.Count)*100,0))[,1]
Table.Calls$Occupancy <- as.data.frame(round(Table.Calls$Call.Count/sum(as.numeric(total.effort$Diff)),2))[,1] 
colnames(Table.Calls) <- c("Species / Species Group", "Call Count", "% of Calls","Calls per Night")
as.data.frame(Table.Calls)

knitr::kable(Table.Calls, 
             caption=paste("Overall bat call count, percentage and call per night for mobile transects surveyed in 2023"),
             align = "lrrr")
write.csv(Table.Calls, "Mobile_Table_Calls_2023.csv")

call_count %>% count(GRTS.Cell.ID)
call_count %>% count(GRTS.Cell.ID.SurveyNight)

call_count %>% summarise(min(SurveyNight), max(SurveyNight))
dat_summary %>% count(Land.Unit.Code)
call_count %>% group_by (GRTS.Cell.ID.SurveyNight) %>% summarise(sum(Count))


##################################################
# reformat for NABat submission

dat_time <- NABat_sum_to_submit(dat_sum_sub = dat_sum_sub) # Creates a dat_time df with data ready to reformat for NABat
dat_time <- dat_time %>% rename(Orig.Name = Location.Name)
dat_time %>% as_tibble()

eff_sub <- eff %>% select(Orig.Name, Location.Name, Survey.Start.Time, Survey.End.Time, Latitude, Longitude, Cell_Date)
dat_time$Cell_Date <- paste(dat_time$GRTS.Cell.ID, dat_time$SurveyNight, sep="_")
dat_time %>% as_tibble()

mobile_data <- left_join(dat_time, eff_sub %>% select(-Orig.Name)) %>% as_tibble()
mobile_data <- mobile_data %>% mutate(Survey.Start.Time = case_when(Timep <= ymd_hms(paste(Sys.Date(),"20:00:00"),tz=tz) ~ as.Date(Survey.Start.Time-1),
                                                            TRUE ~ as.Date(Survey.Start.Time)))


mobile_data$RecordingTime <- paste(mobile_data$SurveyNight, mobile_data$Time)
Bulk_call_data <- mobile_data %>% select(Location.Name, Survey.Start.Time, Survey.End.Time, Latitude, Longitude, RecordingTime, Filename, Classification)
Bulk_call_data <- Bulk_call_data %>% rename('| Location Name'= Location.Name, 'Survey Start Time'=Survey.Start.Time, 'Survey End Time'=Survey.End.Time, 
                                            'Audio Recording Name' = Filename, 'Audio Recording Time' = RecordingTime, 'Auto Id' = Classification)

# add in null columns (NABat template)
xx <- c("Event Low Temperature", "Event High Temperature", "Event Low Relative Humidity",
        "Event High Relative Humidity", "Event Low Weather Event", "Event High Weather Event", "Event Low Wind Speed", "Event High Wind Speed",
        "Event Low Cloud Cover", "Event High Cloud Cover", "Software Type", "Manual Id","Species List","Manual Vetter")

Bulk_call_data[xx] <- NA

names(Bulk_call_data)

Bulk_call_data <- Bulk_call_data[c("| Location Name", "Survey Start Time", "Survey End Time", "Event Low Temperature", "Event High Temperature", "Event Low Relative Humidity",
                                   "Event High Relative Humidity", "Event Low Weather Event", "Event High Weather Event", "Event Low Wind Speed", "Event High Wind Speed",
                                   "Event Low Cloud Cover", "Event High Cloud Cover", "Audio Recording Name", "Audio Recording Time", "Latitude", "Longitude","Software Type","Auto Id", "Manual Id","Species List","Manual Vetter" )]

Bulk_call_data$`Software Type` <- "Alberta eBat"
Bulk_call_data$`Species List` <- "Alberta_01"

# NABat submission requires that the following fields are numeric (i.e., float) 
# Event Low Temperature	Event High Temperature	Event Low Relative Humidity	Event High Relative Humidity
# Event Low Wind Speed	Event High Wind Speed	Event Low Cloud Cover	Event High Cloud Cover
glimpse(Bulk_call_data)
cols.as.numeric <- c("Event Low Temperature",	"Event High Temperature",	"Event Low Relative Humidity",	"Event High Relative Humidity",
                     "Event Low Wind Speed",	"Event High Wind Speed",	"Event Low Cloud Cover",	"Event High Cloud Cover")

Bulk_call_data[cols.as.numeric]<- sapply(Bulk_call_data[cols.as.numeric],as.numeric)

cols.as.character <- c("| Location Name","Event High Weather Event",	"Event Low Weather Event",	"Auto Id", "Manual Id", "Manual Vetter")
Bulk_call_data[cols.as.character]<- sapply(Bulk_call_data[cols.as.character],as.character)

sapply(Bulk_call_data, class)
glimpse(Bulk_call_data)

# need to format dates to mm/dd/YYYY
Bulk_call_data$`Survey Start Time` <- paste(format(as.Date(Bulk_call_data$`Survey Start Time`), '%m/%d/%Y'), "12:00:00")
Bulk_call_data$`Survey End Time` <- paste(format(as.Date(Bulk_call_data$`Survey End Time`), '%m/%d/%Y'), "12:00:00")
Bulk_call_data %>% as_tibble()

getwd()
write.csv(Bulk_call_data, paste0("NABat_submit/NABat_mobile_call_",Year_interest,".csv"), row.names = FALSE)

