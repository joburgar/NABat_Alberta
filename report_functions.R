#### LOGGING FUNCTIONS ----

logError <- function(error, file) {
  print(error)
  write(paste0(Sys.time(), " ERROR with ", file, ": ", toString(error)), 
        file = here::here(FOLDER_REPORT, "build_errors.log"), append = TRUE)
}

logWarning <- function(warning, file) {
  print(warning)
  write(paste0(Sys.time(), " WARNING with ", file, ": ", toString(warning)), 
        file = here::here(FOLDER_REPORT, "build_errors.log"), append = TRUE)
}

logSuccess <- function(file) {
  message <- paste0(Sys.time(), " SUCCESS with ", file)
  print(message)
  write(message, file = here::here(FOLDER_REPORT, "build_success.log"), append = TRUE)
}


#### REPORT FUNCTIONS ----

### * write footer
writeFooter <- function(doc, file) {
  for (i in seq_along(file$var)) {
    officer::footers_replace_all_text(x = doc,
                                      old_value = file$var[i],
                                      new_value = as.character(file$value[i]),
                                      fixed = TRUE)
  }
}


### * replace results
writeResults <- function(doc, file) {
  for (i in seq_along(file$var)) {
    officer::body_replace_all_text(x = doc,
                                   old_value = file$var[i],
                                   new_value = as.character(file$value[i]),
                                   fixed = TRUE)
  }
}


### * write an image to doc
writeImage <- function(doc, bookmark, imagePath, width, height, style = "Normal") {
  officer::cursor_bookmark(x = doc, id = bookmark)
  officer::body_add_img(x = doc,
                        src = paste0(imagePath),
                        width = width,    ## inches
                        height = height,  ## inches
                        style = style,
                        pos = "on")       ## adds element "on" cursor
}


### * bats flextable theme
theme_bats <- function(ftab) {
  ftab %>%
    # flextable::color(color = "#404040", part = "all") %>%   ## font color
    flextable::font(fontname = "Calibri", part = "all") %>% 
    flextable::fontsize(size = 10.5, part = "header") %>%
    flextable::fontsize(size = 10.5, part = "body")
}


### * make and add Appendix Figure 1: Map with inset
AppFig1 <- function(doc, grts_cell_id, bookmark) {
  
  ## this function is untested and probably doesn't work as is, is the resulting plot one object or two?
  
  ## make map
  grid.newpage()
  vpb_ <- viewport(width = 1, height = 1, x = 0.4, y = 0.5)       ## the larger map
  vpa_ <- viewport(width = 0.6, height = 0.5, x = 0.85, y = 0.8)  ## the inset in upper right
  GRTS.map <- GRTS.plot(GRTS.Cell.ID = grts_cell_id, h.just = 0)
  
  map.inset <- ggplot() + 
            geom_sf(data = Alberta) +
            geom_sf(data = NR, mapping = aes(fill = NR), lwd = 0) +
            scale_fill_manual(name = "Natural Regions",
                              values = c("#669933","cadetblue3","#CCFF99","#FFCC66","chocolate1","#CC3333")) +
            geom_sf(data = NABat_grid_3857, col = "azure2", lwd = 0.8) +
            geom_sf(data = NABat_grid %>% dplyr::filter(GRTS_ID == grts_cell_id), col="black", lwd=1) +
            coord_sf() +
            theme(axis.text.x = element_text(size = 4),
                  axis.text.y = element_text(size = 5),
                  legend.position = "none")
  
  Cairo(file=paste("Appendices/Maps/",grts_cell_id,"_map.png", sep=""),
        type="png",
        width=2500,
        height=1800,
        pointsize=14,
        bg="white",
        dpi=300)

  ## Create a graphical object g here
  print(GRTS.map, vp = vpb_)
  print(map.inset, vp=vpa_)
  
  ## Stop writing to the PDF file
  dev.off()
 
  
  
  ## add map to doc
  # officer::cursor_bookmark(x = doc, id = bookmark)
  # officer::body_add_gg(x = doc, value = map_with_inset, width = 5.9, height = 5.25)
  # 
}

# tab <- Appendix.Table1
# grts_cell_id <- "42650"
# rm(tab, grts_cell_id,distanceVars, checKMultiples, newNames, i, j)
### * make and add Appendix Table 1: NABat station meta data
AppTab1 <- function(doc, tab, grts_cell_id, bookmark) {
  
  ## name of vars that need to be rounded to whole numbers
  distanceVars <- c("Clutter.Distance", "Water.Distance", "Road.Distance", "Human.Footprint.Distance")
  
  ## complete table width = 6.65 inches; reserve 1.33 for first col, split rest (5.32) evenly
  width <- 5.32/(tab %>% dplyr::filter(GRTS.Cell.ID == grts_cell_id) %>% tally() %>% pull())
  
  ## some GRTS.Cell.ID's have Location.Names with more than one survey date
  checKMultiples <- tab %>% dplyr::filter(GRTS.Cell.ID == grts_cell_id) %>% group_by(Location.Name) %>% tally()
  newNames <- vector(length = 0)
  if(any(checKMultiples$n > 1)) {
    for(i in 1:dim(checKMultiples)[1]) {
      temp <- rep(checKMultiples$Location.Name[i], times = checKMultiples$n[i])
      for(j in 1:length(temp)) {
        temp[j] <- paste0(temp[j], "_", j)
      }
      newNames <- c(newNames, temp)
      rm(temp)
    }
  }
  
  ## wrangle data
  tab <- tab %>% 
    dplyr::filter(GRTS.Cell.ID == grts_cell_id) %>% select(-GRTS.Cell.ID) %>%
    select(Station = Location.Name, Orig.Name:Clutter.Type, Percent.Clutter, Water.Distance, 
           Water.Type, Road.Distance:Unusual.Occurrences) %>%          ## move Percent.Clutter
    mutate(across(all_of(distanceVars), ~ janitor::round_half_up(as.numeric(.), digits = 0))) %>%
    mutate(across(everything(), as.character)) %>%
    # tidyr::pivot_longer(-Location.Name, names_to = "Station", values_to = "val") %>%
    # tidyr::pivot_wider(names_from = "Location.Name", values_from = "val") %>%
    t() %>%  ## instead of pivot_longer then pivot_wider b/c some (e.g., 58010) have multiple survey dates
    data.frame(stringsAsFactors = FALSE) %>% rownames_to_column(var = "Station") %>%
    mutate(Station = str_replace_all(Station, "[.]", " "),
           Station = str_replace(Station, "Orig ", "Original "))
  ## get unique column names
  if(length(newNames) > 0) {
    names(tab)[-1] <- newNames
    tab <- tab %>% dplyr::filter(Station != "Station")
  } else {
    tab <- tab %>% janitor::row_to_names(1)
  }
  ## create and format flextable
  tab <- tab %>% 
    flextable::regulartable() %>%
    flextable::bold(part = "header") %>%
    theme_bats() %>%
    flextable::bg(i = c(1:5, 12:23), bg = "#F2F2F2") %>%
    flextable::align(j = -1, align = "right", part = "all") %>%
    flextable::width(j = 1, width = 1.33) %>%              ## width is in inches (3.38 cm = 1.33 in)
    flextable::width(j = -1, width = width)
  
  ## add flextable to doc
  flextable::body_replace_flextable_at_bkm(doc, bookmark, value = tab)
}


### * make and add Appendix Table 2: Minimum and maximum nightly survey weather conditions
AppTab2 <- function(doc, tab, bookmark, grts_cell_id) {
  ## create and format flextable
  tab <- tab %>% 
    filter(str_detect(Location.Name, grts_cell_id)) %>% select(-Location.Name) %>% unique() %>%
    flextable::regulartable() %>%
    flextable::set_header_labels(SurveyNight = "Survey Night",
                                 Min.Tmp = "Min Temp",
                                 Max.Tmp = "Max Temp",
                                 Min.RH = "Min Hum",
                                 Max.RH = "Max Hum",
                                 Min.WS = "Min Wind",
                                 Max.WS = "Max Wind") %>%
    flextable::bold(part = "header") %>%
    theme_bats() %>%
    flextable::align(j = -1, align = "right", part = "all") %>%
    flextable::width(j = 1, width = 0.96) %>%              ## width is in inches (2.45 cm = 0.96 in)
    flextable::width(j = -1, width = 0.83)                 ## width is in inches (2.11 cm = 0.83 in)
  
  ## add flextable to doc
  flextable::body_replace_flextable_at_bkm(doc, bookmark, value = tab)
}


### * make and add Appendix Figure 2: Mean (+- 1SE) nightly bat calls
AppFig2 <- function(doc, grts_cell_id, bookmark, alt_text) {
  
  ## only graph species / species groups with >100 calls
  sp.to.use <- call_count.Sp %>% 
    group_by(GRTS.Cell.ID, Classification) %>% 
    summarise(Sum = sum(Count), .groups = "drop_last") %>% 
    filter(Sum>100)
  
  ## don't run if grts_cell_id NOT in call_count.Sp
  if(dim(sp.to.use %>% filter(GRTS.Cell.ID == grts_cell_id))[1] > 0) {
    
    ## group data for overall mean nightly bat calls by year with volancy, and create plot
    app.calls.Sp <- call_count.Sp %>% 
      group_by(GRTS.Cell.ID, Classification, Deployment.ID) %>% 
      summarise(Mean = mean(Count), SE = se(Count), .groups = "drop_last") %>%
      dplyr::filter(GRTS.Cell.ID == grts_cell_id) %>% 
      filter(Classification %in% sp.to.use$Classification) %>%
      ## start ggplot here
      ggplot(aes(x = Classification, y = Mean, fill = "GRTS.Cell.ID")) +
      geom_point(colour = "white", shape = 21, size = 4) +
      scale_fill_manual(values = unique(col.catVol[1])) +
      ylab(expression(paste("Mean ± 1 SE Nightly Bat Calls"))) +
      geom_linerange(aes(Classification, ymin = Mean-SE, ymax = Mean+SE)) +
      ## above warns when 1 or more have NA se: `Removed X rows containing missing values`
      theme_classic() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"),
            axis.text.y = element_text(size=12),
            axis.title.x = element_blank(),
            axis.text.x = element_text(colour = "black", size = 8),
            legend.position = "none",
            legend.title = element_blank()) +
      facet_wrap(~Deployment.ID)
    
    ## add ggplot to doc 
    officer::cursor_bookmark(x = doc, id = bookmark)
    officer::body_add_gg(x = doc, value = app.calls.Sp,
                         width = 5, height = 3.5)  ## 4 in = 10 cm; 9 cm = 3.5 in
  } else {
    ## instead, add text to doc
    officer::cursor_bookmark(x = doc, id = bookmark)
    officer::slip_in_text(x = doc, str = as.character(alt_text), pos = "after")
  }
}


### * make and add Appendix Figure 3: Total bat calls
AppFig3 <- function(doc, grts_cell_id, bookmark, alt_text) {
  
  ## don't run if grts_cell_id NOT in call_count.Sp
  if(dim(call_count.Sp %>% filter(GRTS.Cell.ID == grts_cell_id))[1] > 0) {
    
    ## create ggplot
    app.hist.calls <- ggplot(data = call_count.Sp %>% dplyr::filter(GRTS.Cell.ID == grts_cell_id),
                             aes(x = Classification, y = Count, fill = Location.Name)) + 
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Paired") +
      theme_classic() + 
      xlab("Species / Species Group") + 
      ylab("Total Bat Calls") +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, colour = "black", size = 8),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_blank()) +
      facet_wrap(~Deployment.ID)
    
    ## add ggplot to doc
    officer::cursor_bookmark(x = doc, id = bookmark)
    officer::body_add_gg(x = doc, value = app.hist.calls, width = 5, height = 3.7)  ## 4.2 in = 10.69 cm; 9.5 cm = 3.7 in
  } else {
    ## instead, add text to doc
    officer::cursor_bookmark(x = doc, id = bookmark)
    officer::slip_in_text(x = doc, str = as.character(alt_text), pos = "after")
  }
}


### * make and add Appendix Table 3: Nightly call counts
AppTab3 <- function(doc, grts_cell_id, bookmark, alt_text) {
  
  ## don't run if grts_cell_id NOT in call_count.Sp
  if(dim(dat_count %>% filter(GRTS.Cell.ID == grts_cell_id))[1] > 0) {
  
    ## get only odd instances of all Location.Names for this grts_cell_id (i.e., 1, 3, 5, ...)
    shadeRows <- dat_count %>% dplyr::filter(GRTS.Cell.ID == grts_cell_id) %>% select(Location.Name) %>%
      unique() %>% pull() %>% as.character()
    if(length(shadeRows) > 1) {
      shadeRows <- shadeRows[seq(from = 1, to = length(shadeRows), by = 2)]
    }
    
    ## create and format flextable
    tab <- dat_count %>% 
      group_by(Location.Name, SurveyNight, Classification) %>%
      summarise(Call.Count = sum(Count), .groups = "drop_last") %>% 
      pivot_wider(names_from = Classification, values_from = Call.Count) %>%
      select(Station = Location.Name, Date = SurveyNight, EPFU, `EPFU LANO` = `EPFU-LANO`, LANO, 
             LACI, LABO, `LABO MYLU` = `LABO-MYLU`, MYLU, MYCA, MYCI, MYEV, `MYEV MYSE` = `MYEV-MYSE`, 
             MYSE, MYVO, `My 40k` = `Myotis 40k`, Unk = unknown, Noise = noise) %>%
      dplyr::filter(grepl(grts_cell_id, Station)) %>% 
      mutate(across(everything(), ~ replace_na(., replace = list(.x ="")))) #%>%
    
    sRows <- which(tab$Station %in% shadeRows)
    
    tab <- flextable::regulartable(tab) %>%
      flextable::bold(part = "header") %>%
      theme_bats() %>%
      flextable::fontsize(size = 9, part = "header") %>%
      flextable::fontsize(part = "header", j = 15, size = 8.5) %>%
      flextable::fontsize(j = 2, size = 9) %>%
      flextable::fontsize(j = -(1:2), size = 10) %>%
      flextable::bg(i = sRows, bg = "#F2F2F2") %>%
      flextable::align(j = (1:2), align = "left", part = "all") %>%
      flextable::align(j = -(1:2), align = "right", part = "all") %>%
      flextable::width(j = 1, width = 1.06) %>%              ## width is in inches (2.7 cm = 1.06 in)
      flextable::width(j = 2, width = 0.76) %>%              ## width is in inches (1.93 cm = 0.76 in)
      flextable::width(j = -(1:2), width = 0.47)             ## width is in inches (1.19 cm = 0.47 in)
    
    ## add flextable to doc
    flextable::body_replace_flextable_at_bkm(doc, bookmark, value = tab)
  } else {
    ## instead, add text to doc
    officer::cursor_bookmark(x = doc, id = bookmark)
    officer::slip_in_text(x = doc, str = as.character(alt_text), pos = "after")
  }
}

