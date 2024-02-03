### run classification code
NABat_sum_to_count <- function(dat_sum_sub = dat_sum_sub, threshold_noise = 0.8, threshold_bat =  0.5){
  
  # create thresholds for noise and bat classifications
  
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
  # dat_sum_sub$sp1 <- dat_sum_sub$sp1 %>% replace_na("unknown")
  # dat_sum_sub$sp2 <- dat_sum_sub$sp2 %>% replace_na("unknown")
  
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
  
  dat_time <- dat_sum_sub_df %>% filter(n_calls>2) %>% select(-n_calls:-sp3) %>% filter(category!="noise")
  
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
  
  dat_time %>% count(Classification)
  nrow(dat_time) # 242994
  # difference is that dat_time has filtered out all "calls" with <3 pulses
  # inclined to go with dat_time as splits unknown down a bit
  
  dat_time_agg <- dat_time %>% group_by(GRTS.Cell.ID, Location.Name, SurveyNight) %>% count(Classification)
  colnames(dat_time_agg)[5] <- "Count"
  dat_time_agg$Count <- as.numeric(dat_time_agg$Count)
  dat_time_agg$Year <- as.factor(year(dat_time_agg$SurveyNight))
  dat_time_agg$Month <- month(dat_time_agg$SurveyNight)
  dat_time_agg$jDay <- yday(dat_time_agg$SurveyNight)
  dat_time_agg$NP <- eff$NP[match(dat_time_agg$Location.Name, eff$Orig.Name,)]
  dat_time_agg$Natural.Region <- eff$NRNAME[match(dat_time_agg$Location.Name, eff$Orig.Name,)]
  dat_time_agg$Land.Unit.Code <- eff$Land.Unit.Code[match(dat_time_agg$Location.Name, eff$Orig.Name,)]

  
  dat_time_agg %>% ungroup() %>% summarise(min(SurveyNight), max(SurveyNight))
  dat_time_agg %>% ungroup() %>% count(Classification)
  dat_time_agg %>% ungroup() %>% summarise(sum(Count))
  dat_time_agg %>% ungroup() %>% count(GRTS.Cell.ID)
  unique(dat_time_agg$Location.Name)
  length(unique(dat_time_agg$SurveyNight))
  
  # Create a call df without the noise files
  call_count <- dat_time_agg %>% ungroup () # no noise files but keeping this to keep the same code below, to use with dat_tmime_agg
  
  return(call_count)
  
}
    

#######################################################################################################
#######################################################################################################
### run classification code for submission to NABat
NABat_sum_to_submit <- function(dat_sum_sub = dat_sum_sub, threshold_noise = 0.8, threshold_bat =  0.5){
  
  # create thresholds for noise and bat classifications
  
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
  # dat_sum_sub$sp1 <- dat_sum_sub$sp1 %>% replace_na("unknown")
  # dat_sum_sub$sp2 <- dat_sum_sub$sp2 %>% replace_na("unknown")
  
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
  
  dat_time <- dat_sum_sub_df %>% filter(n_calls>2) %>% select(-n_calls:-sp3) %>% filter(category!="noise")
  
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
  
  dat_time %>% count(Classification)
  nrow(dat_time)
  # difference is that dat_time has filtered out all "calls" with <3 pulses
  # inclined to go with dat_time as splits unknown down a bit
  
  
  # add in null columns (NABat template)
  xx <- c("Nightly Low Weather Event", "Nightly High Weather Event", "Nightly Low Cloud Cover", "Nightly High Cloud Cover", "Software Type", "Manual Id","Species List", "Manual Vetter")
  Bulk_call_data[xx] <- NA
  
  names(Bulk_call_data)
  Bulk_call_data <- Bulk_call_data[c("| Location Name", "Survey Start Time", "Survey End Time", "Nightly Low Temperature", "Nightly High Temperature", "Nightly Low Relative Humidity",
                                     "Nightly High Relative Humidity", "Nightly Low Weather Event", "Nightly High Weather Event", "Nightly Low Wind Speed", "Nightly High Wind Speed",
                                     "Nightly Low Cloud Cover", "Nightly High Cloud Cover", "Audio Recording Name", "Audio Recording Time", "Software Type","Auto Id", "Manual Id","Species List","Manual Vetter" )]
  
  Bulk_call_data$`Software Type` <- "Alberta eBat"
  Bulk_call_data$`Species List` <- "Alberta_01"
  
  # NABat submission requires that the following fields are numeric (i.e., float) 
  # Nightly Low Temperature	Nightly High Temperature	Nightly Low Relative Humidity	Nightly High Relative Humidity
  # Nightly Low Wind Speed	Nightly High Wind Speed	Nightly Low Cloud Cover	Nightly High Cloud Cover
  glimpse(Bulk_call_data)
  cols.as.numeric <- c("Nightly Low Temperature",	"Nightly High Temperature",	"Nightly Low Relative Humidity",	"Nightly High Relative Humidity",
                       "Nightly Low Wind Speed",	"Nightly High Wind Speed",	"Nightly Low Cloud Cover",	"Nightly High Cloud Cover")
  
  Bulk_call_data[cols.as.numeric]<- sapply(Bulk_call_data[cols.as.numeric],as.numeric)
  
  cols.as.character <- c("| Location Name","Nightly High Weather Event",	"Nightly Low Weather Event",	"Auto Id", "Manual Id", "Manual Vetter")
  Bulk_call_data[cols.as.character]<- sapply(Bulk_call_data[cols.as.character],as.character)
  
  sapply(Bulk_call_data, class)
  glimpse(Bulk_call_data)
  
  # need to format dates to mm/dd/YYYY
  Bulk_call_data$`Survey Start Time` <- paste(format(as.Date(Bulk_call_data$`Survey Start Time`), '%m/%d/%Y'), "12:00:00")
  Bulk_call_data$`Survey End Time` <- paste(format(as.Date(Bulk_call_data$`Survey End Time`), '%m/%d/%Y'), "12:00:00")
  Bulk_call_data %>% as_tibble()
  
  
  return(call_count)
  
}

  

