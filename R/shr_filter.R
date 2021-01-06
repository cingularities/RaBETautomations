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
    map(read_excel, path = rawdata)%>% #read excel
    map(pivot_longer,cols = -OBSERVATION, names_to = "observation") %>% # Moving  rows to column, excluding observation and renaming it
    map(pivot_wider, names_from = OBSERVATION, values_from = value)  #transposing data rows to column
  topcanopycode <- allsheets$`TOP CANOPY CODES` %>% gather(Point, topcanopycode, -observation) #gathers data each column for topcanopycode and binds them on top of each other
  plantgrowthform <- allsheets$`PLANT GROWTH FORMS` %>% gather(Point, plantgrowthform, -observation) #gathers data each column for plantgrowthform and binds them on top of each other
  coverbyspecies <- allsheets$`PCT COVER BY SPECIES` %>% gather(Point, coverbyspecies, -observation) #gathers data each column for coverbyspecies and binds them on top of each other
  finaldataset <- cbind(topcanopycode,plantgrowthform$plantgrowthform,coverbyspecies$coverbyspecies) #binds location and all three sheets in one sheet
  SHR <- finaldataset %>% #filters SHR
    filter(`plantgrowthform$plantgrowthform` == "SHR")
  SHR = SHR[,c(2,3,4,5)] #selects specific columns
  names(SHR) <- c("location", "topcanopycode" , "plantgrowthform" , "coverbyspecies") #renames columns
  return(SHR)
}

