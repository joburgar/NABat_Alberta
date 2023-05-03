

###---recode Classification to "AutoID" and use NABat 4-6 letter codes for NABat submission
#25k|40k|40kMyo|ANPA|ANPAEPFU|ANTPAL|ARJA|ARTJAM|BRACAV|BRCA|CHME|CHOMEX|CORA|CORRAF|CORTO|COTO|COTOVI|DIEC|DIPECA|
#EPFU|EPFULABO|EPFULANO|EPFUMYLU|EPTFUS|EUDMAC|EUFL|EUMA|EUMFLO|EUMPER|EUMUND|EUPE|EUUN|HiF|HighF|IDIPHY|IDPH|LABL|
#LABLPAHE|LABO|LABOLASE|LABOMYLU|LABONYHU|LABOPESU|LACI|LACILANO|LACITABR|LAEG|LAIN|LAMI|LANO|LANOTABR|LASBLO|LASBOR|
#LASCIN|LASE|LASEGA|LASINT|LASMIN|LASNOC|LASSEM|LASXAN|LAXA|LEMY|LENI|LEPNIV|LEPYER|LESP|LEYE|LUSO|LoF|LowF|MACA|MACCAL|
#MOLMOL|MOME|MOMO|MORMEG|MYAR|MYAU|MYCA|MYCAMYCI|MYCAMYYU|MYCI|MYCIMYVO|MYEV|MYEVMYTH|MYGR|MYKE|MYLE|MYLU|MYLUMYCI|
#MYLUMYSE|MYLUMYVO|MYOAUR|MYOAUS|MYOC|MYOCAL|MYOCIL|MYOEVO|MYOGRI|MYOKEE|MYOLEI|MYOLUC|MYOOCC|MYOSEP|MYOSOD|MYOTHY|MYOVEL|
#MYOVOL|MYOYUM|MYSE|MYSO|MYTH|MYVE|MYVO|MYYU|NOCLEP|NOISE|NOLE|NOTBAT|NYCFEM|NYCHUM|NYCMAC|NYFE|NYHU|NYMA|NYSP|NoID|PAHE|PARHES|PERSUB|PESU|STERUF|STRU|TABR|TADBRA|HiLo

NABat_AppendixMap_sf = readRDS("NABat_AppendixMap_sf.RDS")


render_one <- function(GRTS_ID) {
  rmarkdown::render(
    'NABat_Annual_Report_Appendices_formatted.Rmd',
    output_file = paste0('Appendices/Appendix_',GRTS.Cell.ID,'.docx'),
    params = list(GRTS.Cell.ID = GRTS.Cell.ID),
    envir = parent.frame()
  )
}

library("rmarkdown")
source("Appendix_maps.R", local = knitr::knit_global())

unique.GRTS.Cell.ID = unique(sta$GRTS.Cell.ID)

# for(GRTS.Cell.ID in unique.GRTS.Cell.ID){
#   rmarkdown::render("NABat_Annual_Report_Appendices.Rmd",
#                     params=list(GRTS.Cell.ID = GRTS.Cell.ID),
#                     output_file=paste0("Appendices/Appendix_", GRTS.Cell.ID, ".docx"))
# }

unique.GRTS.Cell.ID
# Year_interest <- 2021
# unique.GRTS.Cell.ID <- 267463
for(GRTS.Cell.ID in unique.GRTS.Cell.ID) {
  # render_one(GRTS.Cell.ID)
  doc <- read_docx(paste0('Appendices/Appendix_',GRTS.Cell.ID,'.docx')) %>%
    body_replace_all_text(old_value = "ZZ_YEAR", new_value = paste0(Year_interest)) %>%
    ## block_section applies to everything BEFORE it is called, so apply portrait_pages section 
    ## until Table3, then move the cursor to the end and apply landscape_pages section
    cursor_reach(keyword = "Table 3") %>% cursor_backward() %>%
    body_end_section_portrait() %>%
    cursor_end() %>% body_end_section_landscape() %>% 
    footers_replace_all_text(old_value = "ZZ_DATE", new_value = format(Sys.time(), '%d %B %Y')) %>%
    footers_replace_all_text(old_value = "ZZ_PLACE", new_value = paste0("Alberta")) %>%
    footers_replace_all_text(old_value = "ZZ_YEAR", new_value = paste0(Year_interest))
  print(doc, target = paste0('Appendices/Appendix_',GRTS.Cell.ID,'.docx'))
}

