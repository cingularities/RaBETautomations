#' This function filteres data SHR plant growth forms and attached canopy code and % cover species data from excel workbook sheets.
#' @param rawdata Raw Data.
#' @return Filtered SHR data with attached canopy code and % cover by species.
#' @export
#' @examples
#' shr_filter(rawdata)

#Function for filtering SHR
shr_filter <- function(rawdata) {
  allsheets <- rawdata %>%
    excel_sheets() %>% #import as sheets
    set_names() %>% #use sheet names
    map(read_excel, path = sampledata)%>% #read excel
    map(pivot_longer,cols = -OBSERVATION, names_to = "observation") %>%
    map(pivot_wider, names_from = OBSERVATION, values_from = value)
  topcanopycode <- allsheets$`TOP CANOPY CODES` %>% gather(Point, topcanopycode, -observation)
  plantgrowthform <- allsheets$`PLANT GROWTH FORMS` %>% gather(Point, plantgrowthform, -observation)
  coverbyspecies <- allsheets$`PCT COVER BY SPECIES` %>% gather(Point, coverbyspecies, -observation)
  finaldataset <- cbind(topcanopycode,plantgrowthform$plantgrowthform,coverbyspecies$coverbyspecies)
  SHR <- finaldataset %>%
    filter(`plantgrowthform$plantgrowthform` == "SHR")
  SHR = SHR[,c(2,3,4,5)]
  names(SHR) <- c("location", "topcanopycode" , "plantgrowthform" , "coverbyspecies")
  return(SHR)
}

