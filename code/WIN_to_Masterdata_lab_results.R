
#Loading packages----
library(readr)
library(readxl)
library(openxlsx)
library(dplyr)
library(lubridate)
library(arsenal)

#Loading in AEL WIN data and GTMNERR Masterdata
win <- xlsx::read.xlsx("C:/Users/schirmer_n/Documents/Data/Guana_data/WIN_files/02.2025.WIN.xlsx", 1)
field <- xlsx::read.xlsx("C:/Users/schirmer_n/Documents/Data/Guana_data/2025.Field.xlsx", 1)

###RUN FIRST: check if there are any Result Comments, StationCodes, or Results Qualifiers not recognized in the code----
#StationCodes = GTMGRNUT, GTMRNNUT, GTMLSNUT, GTMLMNUT, GTMGL2NUT, GTMMKNUT. Any misspellings or inclusions of other sites may generate errors
#Filters out non-Sample location IDs 
win_gtm <- win %>% 
  filter(substr(Monitoring.Location.ID, 1, 1) == "G")
#Displays all StationCodes
unique(win_gtm$Monitoring.Location.ID)

#Results Comments = "J4" - The RPD in the replicate sample duplicate was outside control criteria. Estimated Result / "Q" - Exceeded sample time 
print(unique(win$Reslt.Comments))

#Results Qualifiers = "U" - Below MDL/ "I" - Below PQL, above PQL
print(unique(win$Value.Qualifier))
#if any other values are printed consult the lab results .pdf sheet and update code/metadata with new comments
###----


#Renaming columns in WIN data to names used in Masterdata and filtering non-sample data from lab data----
nut_sites <- unique(field$StationCode[!field$StationCode %in% c("GTMGR1NUT", "GTMGL4NUT")])

win2 <- win %>% 
  rename(StationCode = Monitoring.Location.ID,
         ComponentLong = Org.Analyte.Name,
         Result = Org.Result.Value,
         Unit = Org.Result.Unit,
         MDL = Org.MDL,
         PQL = Org.PQL,
         ResultsQualifier = Value.Qualifier,
         AnalysisMethod = Analysis.Method,
         ActivityType = Activity.Type, 
         SampleDate = Activity.Start.Date.Time, 
         DateAnalyzed = Analysis.Date.Time
  ) %>% 
  filter(StationCode %in% nut_sites)

###----

#Replacing WIN ComponentLong names w/ Masterdata names to make mapping values to Masterdata file easier and applying comments and flags----
win3 <- win2 %>% 
  mutate(
         ComponentLong = case_when(
                                   ComponentLong == "Ammonia (N)" ~ "Ammonium, Filtered", 
                                   ComponentLong == "Enterococci (MPN)" ~ "Enterococcus", 
                                   ComponentLong == "Nitrogen- Total Kjeldahl" & Sample.Fraction == "Dissolved" ~ "Total Kjeldahl Nitrogen Filtered", 
                                   ComponentLong == "Nitrogen- Total Kjeldahl" & Sample.Fraction == "Total" ~ "Total Kjeldahl Nitrogen", 
                                   ComponentLong == "Chlorophyll a- uncorrected" ~ "Chlorophyll a, Uncorrected (Trichromatic)", 
                                   ComponentLong == "Chlorophyll a- corrected" ~ "Chlorophyll a, Corrected (Monochromatic)", 
                                   ComponentLong == "Orthophosphate (P)" ~ "Orthophosphate", 
                                   ComponentLong == "Pheophytin-a" ~ "Pheophytin a", 
                                   ComponentLong == "Phosphorus- Total" ~ "Total Phosphorus"
                                   ), 
        
         ComponentShort = case_when(ComponentLong == "Ammonium, Filtered" ~ "NH4F", 
                                    ComponentLong == "Enterococcus" ~ "ENTERO", 
                                    ComponentLong == "Total Kjeldahl Nitrogen Filtered" & Sample.Fraction == "Dissolved" ~ "TKNF", 
                                    ComponentLong == "Total Kjeldahl Nitrogen" & Sample.Fraction == "Total" ~ "TKN", 
                                    ComponentLong == "Chlorophyll a, Uncorrected (Trichromatic)" ~ "CHLa_UnC", 
                                    ComponentLong == "Chlorophyll a, Corrected (Monochromatic)" ~ "CHLa_C", 
                                    ComponentLong == "Orthophosphate" ~ "PO4", 
                                    ComponentLong == "Pheophytin a" ~ "PHEa", 
                                    ComponentLong == "Total Phosphorus" ~ "TP"
                                    ), 
         
         SampleDate = openxlsx::convertToDateTime(SampleDate, tz = "EST"), 
         
         Laboratory = "AEL", 
         
         F_Record = "", 
         
         FieldComments = "", 
         
         MRL = "", 
         
         DateReceived = "", 
         
         "RQ#" = "", 
         
         QAQC = "", 
         
         QAQC2 = "", 
         
         UNID = "", 
         
         ActivityType = case_when(ActivityType == "SAMPLE" ~ "Sample")
         
  )

###----

#Merging Result.Comments with ResultsQualifier----
##If there are any other values in the Result.Comments column copy the below code and replace "" values with the code
win3$ResultsQualifier[win3$Result.Comments == "J4"] <- "J4"
win3$ResultsQualifier[win3$Result.Comments == "Q"] <- "Q"

win4 <- win3 %>% 
  mutate(
    Flag = case_when(
                     ResultsQualifier == "U" ~ "<-4>[SBL]", 
                     ResultsQualifier == "I" ~ "<0>", 
                     ResultsQualifier == "" ~ "<0>", 
                     ResultsQualifier == "Q" ~ "<1>(CHB)", 
                     ResultsQualifier == "J4" ~ "<1>[SRD]"), 
    
    
    TestComments = case_when(ResultsQualifier == "U" ~ "The compound was analyzed for but not detected", 
                             ResultsQualifier == "I" ~ "The reported value is between the laboratory method detection limit and the laboratory practical quantitation limit.", 
                             ResultsQualifier == "J4" ~ "The RPD in the replicate sample duplicate was outside control criteria. Estimated result",
                             ResultsQualifier == "Q" ~ "Exceeded sample hold time")
    
    )
###----

#Adding Columns needed to bind field and WIN----

field_ready <- field %>% 
  rename(
    Lab.Accreditation.Authority = Lab.Accredidation.Authority
  )

###----

#Selecting and ordering columns so that windata4 is in the same format as MasterData file----
win_final <- win4 %>% 
  select(UNID, 
         F_Record,
         StationCode, 
         SampleDate, 
         ActivityType, 
         ComponentShort, 
         ComponentLong, 
         Result, 
         Unit, 
         FieldComments, 
         MRL, 
         MDL, 
         PQL, 
         Dilution, 
         ResultsQualifier, 
         TestComments, 
         Flag, 
         DateReceived, 
         DateAnalyzed, 
         AnalysisMethod, 
         Lab.ID, 
         Lab.Accreditation.Authority, 
         Laboratory, 
         'RQ#', 
         QAQC, 
         QAQC2
         ) 

###----

#Converting columns into POSIXct format to allow for binding of field_ready and win_final dataframes----
win_format <- win_final %>% 
  mutate(
    DateReceived = parse_date_time2(DateReceived, "m/d/Y H:M", tz = "EST"),
    SampleDate = lubridate::force_tz(SampleDate, tzone = "EST"), 
    UNID = as.double(UNID), 
    Result = as.character(Result)
  ) 


field_format <- field_ready %>% 
  mutate(
    F_Record = as.character(F_Record)
  ) %>% 
  rename(
    'RQ#' = RQ.
  )

###----

#moving data from WIN to MD----
win_field <- dplyr::bind_rows(win_format, field_format)
win_field$StationCode <- as.character(win_field$StationCode)

#arranging rows to conform with previous formatting
win_field_format <- win_field %>% 
  arrange(StationCode, SampleDate)
View(win_field_format)

###----

#Updating old Masterdata with updated data----
wb <- loadWorkbook("C:/Users/schirmer_n/Documents/Data/Guana_data/2025_Guana_masterdata.xlsx")
writeData(wb, sheet = "2025", win_field_format, colNames = T)
saveWorkbook(wb, "C:/Users/schirmer_n/Documents/Data/Guana_data/2025_Guana_masterdata.xlsx", overwrite = T)

#Used to compare formatting before binding
formats <- summary(arsenal::comparedf(win_field_format, field))
formats$vars.nc.table
