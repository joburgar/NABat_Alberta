#### NABat Annual Report Appendices Builder -----
## Author: Joanna Burgar
## Date:   August 2021


#### load packages, set values, source files ----

### * additional packages
library(flextable)
library(officer)
# library(png)

# run through Appendix_maps.R first to ensure maps are ready to be included in report

### * set values
FOLDER_REPORT = here::here("Appendices")
FILE_OUT = "Appendix_"

footer_date <- "31 March 2023"
footer_title <- "North American Bat Monitoring Program - Alberta 2022"
footer_copyright <- "2023 Government of Alberta"

alt_text <- "not surveyed"


### * generate individual annual GRTS specific appendices
source("Appendix_tables.R")  ##, local = knitr::knit_global())


### * source building functions
source("report_functions.R")


## !! NO NEED TO EDIT BEYOND THIS POINT !! ##


#### prep ----

unique.GRTS.Cell.ID = unique(Appendix.Table1$GRTS.Cell.ID)
Deployment.ID <- unique(Year_interest) %>% as.character()

# vals <- data.frame(stringsAsFactors = FALSE,
#                    var = c("grts_cell_id", "Deployment.ID"),
#                    value = c(grts_cell_id, Deployment.ID) )

footer_vals <- data.frame(stringsAsFactors = FALSE,
                          var = c("footer_date", "footer_title", "footer_copyright"),
                          value = c(footer_date, footer_title,  footer_copyright)
                          )


#### build ----
 generateAppendix <- function(grts_cell_id) {
  
   # grts_cell_id <- '3667'
  ## let user know which grts_cell_id is generating
  print(paste0("Generating Appendix for: ", grts_cell_id))
  
  ## load the template file
  doc <- read_docx(here::here(FOLDER_REPORT, "Appendix_template.docx"))
  
  ## define a list of functions that will be sequentially applied to the template
  functionList <- list(
    function(doc) { body_replace_all_text(doc, old_value = "grts_cell_id",
                                   new_value = as.character(grts_cell_id),
                                   fixed = TRUE); NULL},
    function(doc) { body_replace_all_text(doc, old_value = "Deployment.ID",
                                          new_value = as.character(Deployment.ID),
                                          fixed = TRUE); NULL},
    function(doc) {  AppTab3(doc, grts_cell_id = grts_cell_id, bookmark = "bkm_table3", alt_text); NULL },
    
    # function(doc) {  writeResults(doc, vals); NULL },  ## example of writing multiple different results
    function(doc) {  writeFooter(doc, footer_vals); NULL },
    # function(doc) {  AppFig1(doc, grts_cell_id = grts_cell_id, bookmark = "bkm_fig1_map"); NULL  },
    ## Use the writeImage code instead of AppFig1, b/c need to write maps first
    function(doc) {  writeImage(doc, bookmark = "bkm_fig1_map",
                                imagePath = paste("Appendices/Maps/AppFig1_",grts_cell_id,".png", sep=""),
                                width = 6.6, height = 4.7); NULL },
    function(doc) {  AppTab1(doc, tab = Appendix.Table1, grts_cell_id = grts_cell_id,
                             bookmark = "bkm_table1"); NULL },
    function(doc) {  AppTab2(doc, tab = Appendix.Table2, bookmark = "bkm_table2", grts_cell_id=grts_cell_id); NULL },
    function(doc) {  AppFig2(doc, grts_cell_id = grts_cell_id, bookmark = "bkm_fig2", alt_text); NULL  },
    function(doc) {  AppFig3(doc, grts_cell_id = grts_cell_id, bookmark = "bkm_fig3", alt_text); NULL  }
  )
  

  tmp <- lapply(functionList, function(f) f(doc))
  
  ## write the template
  outputFile <- here::here(FOLDER_REPORT, paste0(FILE_OUT, grts_cell_id, ".docx"))
  print(paste0("Writing to ", outputFile))
  print(doc, target = outputFile)
  
  logSuccess(paste("Appendix:",grts_cell_id))
}

generateAppendixSafe <- function(grts_cell_id) {
  tryCatch(withCallingHandlers(generateAppendix(grts_cell_id),
                               warning = function(w) logWarning(w, grts_cell_id)),
           error = function(e) logError(e, grts_cell_id))
  return()
}

controlFile <- unique.GRTS.Cell.ID %>% as.character()
# controlFile <- "3667" # need to redo for 2017, 2018 and 2019 because typo in sta
tmp <- lapply(controlFile, function(f) generateAppendixSafe(f))
