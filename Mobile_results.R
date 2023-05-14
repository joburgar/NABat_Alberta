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
Year_interest <- year(as.Date("2022-01-01"))

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
dat_summary <- dat_summary %>% mutate(Location.Name = case_when(grepl("D1",Orig.Name) ~ as.character("264037_D1_Mobile"),
                                                            grepl("D2",Orig.Name) ~ as.character("264037_D2_Mobile"),
                                                            TRUE ~ as.character(Location.Name)))


dat_summary <- dat_summary %>% mutate(Cell_Date = case_when(grepl("264037_2022-07-16",Cell_Date) ~ as.character("264037_2022-07-15"),
                                                                TRUE ~ as.character(Cell_Date)))
dat_summary %>% group_by(Location.Name, Orig.Name) %>% count(Cell_Date)


glimpse(dat_summary)

# Create file type to differentiate zc from wav
dat_summary$File.Type <- str_sub(dat_summary$Filename,-2,-1) %>% recode("0#" = "zc", "av"="wav")
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
# 
# # Create environmental covariate
# nightly.max.env.cov <- dat_summary %>% group_by(Location.Name,SurveyNight) %>% dplyr::summarise_at(c("max_temp","max_hum","max_wind"), list(Mean = mean))
# nightly.max.env.cov <- nightly.max.env.cov %>% rename(Max.Temp = "max_temp_Mean", Max.Hum = "max_hum_Mean", Max.Wind = "max_wind_Mean")
# 
# Read deployment data csv for station covariates
eff <- read.csv("Input/NABat_Deployment_Data_DT_2022.csv", header=T) %>%
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

# now use Alberta eBat criteria to classify species
dat_sum_sub <- dat_summaryT[c("GRTS.Cell.ID","Location.Name","SurveyNight","Year","Month","jDay","Timep",
                              "Filename","n_calls","prob1","sp1","prob2","sp2","prob3","sp3")]

call_count <- NABat_sum_to_count(dat_sum_sub = dat_sum_sub) # Creates a call df without the noise files and aggregates into 'count' style df

nrow(dat_sum_sub) # 1441 files
nrow(call_count) # 68

call_count %>% as_tibble()

call_count$GRTS.Cell.ID.SurveyNight <- paste(call_count$GRTS.Cell.ID, call_count$Location.Name, sep="_")
call_count %>% group_by(GRTS.Cell.ID.SurveyNight) %>% summarise(total.calls = sum(Count)) %>% arrange(total.calls)
call_count %>% group_by(GRTS.Cell.ID.SurveyNight,Classification) %>% summarise(total.calls = sum(Count)) %>% arrange(desc(total.calls))
unknown.calls <- call_count %>% filter(Classification=="unknown") %>% summarise(sum(Count)) / sum(call_count$Count)

# 0.1728395 % unknown calls

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

Cairo(file="Output/calls.2022.GRTS.plot.PNG", 
      type="png",
      width=2000, 
      height=1500, 
      pointsize=14,
      bg="white",
      dpi=300)
calls.GRTS
dev.off()


GRTS.Calls.SN <- call_count %>% group_by(GRTS.Cell.ID, Location.Name, SurveyNight) %>% 
  summarise(Mean = mean(Count), SE = se(Count))
GRTS.Calls.SN$SurveyNum <- c(1,2,1,2,3,1,2,3,1,2,1,2,1,1,2,3)
GRTS.Calls.SN$Unique <- paste(GRTS.Calls.SN$GRTS.Cell.ID, GRTS.Calls.SN$SurveyNum, sep="_")

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

Cairo(file="Output/fcalls.2022.GRTS.plot.PNG", 
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

Cairo(file="Output/Sp.hist.2022.mob.plot.PNG", 
      type="png",
      width=2000, 
      height=2000, 
      pointsize=14,
      bg="white",
      dpi=300)
Sp.hist.mob
dev.off()

# Determine total effort (survey nights)
total.effort <- call_count %>% group_by(Location.Name, SurveyNum) %>% summarise_at(c("SurveyNight"), list(Min = min, Max=max))
total.effort$Diff <- (total.effort$Max - total.effort$Min)+1
sum(total.effort$Diff) # 16 survey nights

# Determine total calls
# call_count %>% filter(grepl("swift|SM2", Detector)) %>% group_by(Classification) %>% summarise(sum(Count))

call_count %>% group_by(Classification) %>% summarise(sum(Count))
Table.Calls <- call_count %>% group_by(Classification) %>% summarise(Call.Count = sum(Count))
Table.Calls$Per.Known <- as.data.frame(round(Table.Calls$Call.Count/sum(Table.Calls$Call.Count)*100,0))[,1]
Table.Calls$Occupancy <- as.data.frame(round(Table.Calls$Call.Count/sum(as.numeric(total.effort$Diff)),2))[,1] 
colnames(Table.Calls) <- c("Species / Species Group", "Call Count", "% of Calls","Calls per Night")
as.data.frame(Table.Calls)

knitr::kable(Table.Calls, 
             caption=paste("Overall bat call count, percentage and call per night for mobile transects surveyed in 2022"),
             align = "lrrr")
write.csv(Table.Calls, "Mobile_Table_Calls_2022.csv")
