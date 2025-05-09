#Loading in AEL WIN data and GTMNERR Masterdata
mstr_input <- "C:/Users/schirmer_n/Documents/Data/Guana_data/2025_Guana_masterdata_NS(1).xlsx"
mstr_output <- "C:/Users/schirmer_n/Documents/Data/Guana_data/2025_Guana_masterdata_NS_output.xlsx"
##Renaming columns in WIN data to names used in Masterdata and filtering non-sample data from lab data----
#input 
nut_sites

win2 <- win %>% 
  #rename function, with replacement name on the left side, and old name on the right side
  rename(StationCode = 'Monitoring Location ID',
         ComponentLong = 'Org Analyte Name',
         Result = 'Org Result Value',
         Unit = 'Org Result Unit',
         MDL = 'Org MDL',
         PQL = 'Org PQL',
         ResultsQualifier = 'Value Qualifier',
         AnalysisMethod = 'Analysis Method',
         ActivityType = 'Activity Type', 
         SampleDate = 'Activity Start Date Time', 
         DateAnalyzed = 'Analysis Date Time', 
         SampleFraction = 'Sample Fraction'
  ) %>% 
  #filter function to remove all sites not found on the nut_sites list 
  filter(StationCode %in% nut_sites)

###----

#Replacing WIN ComponentLong names w/ Masterdata names to make mapping values to Masterdata file easier and applying comments and flags----
win3 <- win2 %>% 
  mutate(
         ComponentLong = case_when(
                                   ComponentLong == "Ammonia (N)" ~ "Ammonium, Filtered", 
                                   ComponentLong == "Enterococci (MPN)" ~ "Enterococcus", 
                                   ComponentLong == "Nitrogen- Total Kjeldahl" & SampleFraction == "Dissolved" ~ "Total Kjeldahl Nitrogen Filtered", 
                                   ComponentLong == "Nitrogen- Total Kjeldahl" & SampleFraction == "Total" ~ "Total Kjeldahl Nitrogen", 
                                   ComponentLong == "Chlorophyll a- uncorrected" ~ "Chlorophyll a, Uncorrected (Trichromatic)", 
                                   ComponentLong == "Chlorophyll a- corrected" ~ "Chlorophyll a, Corrected (Monochromatic)", 
                                   ComponentLong == "Orthophosphate (P)" ~ "Orthophosphate", 
                                   ComponentLong == "Pheophytin-a" ~ "Pheophytin a", 
                                   ComponentLong == "Phosphorus- Total" ~ "Total Phosphorus"
                                   ), 
        
         ComponentShort = case_when(ComponentLong == "Ammonium, Filtered" ~ "NH4F", 
                                    ComponentLong == "Enterococcus" ~ "ENTERO", 
                                    ComponentLong == "Total Kjeldahl Nitrogen Filtered" & SampleFraction == "Dissolved" ~ "TKNF", 
                                    ComponentLong == "Total Kjeldahl Nitrogen" & SampleFraction == "Total" ~ "TKN", 
                                    ComponentLong == "Chlorophyll a, Uncorrected (Trichromatic)" ~ "CHLa_UnC", 
                                    ComponentLong == "Chlorophyll a, Corrected (Monochromatic)" ~ "CHLa_C", 
                                    ComponentLong == "Orthophosphate" ~ "PO4", 
                                    ComponentLong == "Pheophytin a" ~ "PHEa", 
                                    ComponentLong == "Total Phosphorus" ~ "TP"
                                    ), 
         
         SampleDate = lubridate::force_tz(SampleDate, tzone = "America/New_York"), 
         
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

###----s

#Merging Result.Comments with ResultsQualifier----
##If there are any other values in the Result.Comments column copy the below code and add the new qualifier in the brackets and to the right of the <-
win3$ResultsQualifier[win3$'Result Comments' == "J4"] <- "J4"
win3$ResultsQualifier[win3$'Result Comments' == "Q"] <- "Q"

win4 <- win3 %>% 
  mutate(
    #using case_when statements to generate a Flag column to generate CDMO style flags based on the lab generated ResultsQualifier column
    Flag = case_when(
                     ResultsQualifier == "U" ~ "<-4>[SBL]", 
                     ResultsQualifier == "I" ~ "<0>", 
                     ResultsQualifier == "Q" ~ "<1>(CHB)", 
                     ResultsQualifier == "J4" ~ "<1>[SRD]"), 
    
    Flag = replace(Flag, is.na(ResultsQualifier), "<0>"), 
    
    #using case_when statements to generate comments explaining the use of the code
    TestComments = case_when(ResultsQualifier == "U" ~ "The compound was analyzed for but not detected", 
                             ResultsQualifier == "I" ~ "The reported value is between the laboratory method detection limit and the laboratory practical quantitation limit.", 
                             ResultsQualifier == "J4" ~ "The RPD in the replicate sample duplicate was outside control criteria. Estimated result",
                             ResultsQualifier == "Q" ~ "Exceeded sample hold time")
    
    )
###----
print(win$`Analysis Date Time`[1])
print(lubridate::force_tz(win$`Analysis Date Time`[1], tzone = "America/New_York"))
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
         'Lab ID', 
         'Lab Accreditation Authority', 
         Laboratory, 
         'RQ#', 
         QAQC, 
         QAQC2
         ) 

###----

#Converting columns into POSIXct format to allow for binding of field_ready and win_final dataframes----
win_format <- win_final %>% 
  mutate(
    UNID = as.double(UNID), 
    Result = as.character(Result), 
    DateReceived = as_date(DateReceived)
  ) 

field_format <- field %>% 
  mutate(
    F_Record = as.character(F_Record)
  )

###----

#moving data from WIN to MD----
win_field <- dplyr::bind_rows(win_format, field_format)
win_field$StationCode <- as.character(win_field$StationCode)

#arranging rows to conform with previous formatting
#using sprintf() to manually assign formatting to all numerical values within the Results column to correct for floating point precision problems

win_field_format <- win_field %>% 
 
  arrange(SampleDate, StationCode, ActivityType) %>% 
  
  mutate(
    
    UNID = seq.int(nrow(win_field)), 
    
    Result = ifelse(
      suppressWarnings(!is.na(as.numeric(Result))), 
      sprintf("%.2f", as.numeric(Result)), 
      Result
      )
  )   

print(win_field_format$SampleDate[703])
###----

#Updating old Masterdata with updated data----
wb <- loadWorkbook(mstr_output)
writeData(wb, sheet = "2025", win_field_format, colNames = T)
saveWorkbook(wb, here::here("data", "test123.xlsx"), overwrite = T)
file.rename(here::here("data", "test123.xlsx"), mstr_output)
##Used to compare formatting before binding
#generates a table displaying all differences between two data frames x = win_format y = field_format
formats <- summary(arsenal::comparedf(win_format, field_format))
print(formats)
#specifies the component of the table containing information about column type formatting

