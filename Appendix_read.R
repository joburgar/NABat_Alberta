

###---recode Classification to "AutoID" and use NABat 4-6 letter codes for NABat submission
#25k|40k|40kMyo|ANPA|ANPAEPFU|ANTPAL|ARJA|ARTJAM|BRACAV|BRCA|CHME|CHOMEX|CORA|CORRAF|CORTO|COTO|COTOVI|DIEC|DIPECA|
#EPFU|EPFULABO|EPFULANO|EPFUMYLU|EPTFUS|EUDMAC|EUFL|EUMA|EUMFLO|EUMPER|EUMUND|EUPE|EUUN|HiF|HighF|IDIPHY|IDPH|LABL|
#LABLPAHE|LABO|LABOLASE|LABOMYLU|LABONYHU|LABOPESU|LACI|LACILANO|LACITABR|LAEG|LAIN|LAMI|LANO|LANOTABR|LASBLO|LASBOR|
#LASCIN|LASE|LASEGA|LASINT|LASMIN|LASNOC|LASSEM|LASXAN|LAXA|LEMY|LENI|LEPNIV|LEPYER|LESP|LEYE|LUSO|LoF|LowF|MACA|MACCAL|
#MOLMOL|MOME|MOMO|MORMEG|MYAR|MYAU|MYCA|MYCAMYCI|MYCAMYYU|MYCI|MYCIMYVO|MYEV|MYEVMYTH|MYGR|MYKE|MYLE|MYLU|MYLUMYCI|
#MYLUMYSE|MYLUMYVO|MYOAUR|MYOAUS|MYOC|MYOCAL|MYOCIL|MYOEVO|MYOGRI|MYOKEE|MYOLEI|MYOLUC|MYOOCC|MYOSEP|MYOSOD|MYOTHY|MYOVEL|
#MYOVOL|MYOYUM|MYSE|MYSO|MYTH|MYVE|MYVO|MYYU|NOCLEP|NOISE|NOLE|NOTBAT|NYCFEM|NYCHUM|NYCMAC|NYFE|NYHU|NYMA|NYSP|NoID|PAHE|PARHES|PERSUB|PESU|STERUF|STRU|TABR|TADBRA|HiLo


render_one <- function(GRTS_ID) {
  rmarkdown::render(
    'NABat_Annual_Report_Appendices.Rmd',
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


for(GRTS.Cell.ID in unique.GRTS.Cell.ID){
  render_one(GRTS.Cell.ID)  
}


