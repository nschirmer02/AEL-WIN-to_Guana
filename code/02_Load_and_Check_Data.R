#Loading in AEL WIN data and GTMNERR Masterdata
win <- readxl::read_excel(here::here("data", "02.2025.WIN.xlsx"), 1)
field <- readxl::read_excel(here::here("data", "2025.Field.xlsx"), 1)

colnames(win_gtm)
###RUN FIRST: check if there are any Result Comments, StationCodes, or Results Qualifiers not recognized in the code----
#StationCodes = GTMGRNUT, GTMRNNUT, GTMLSNUT, GTMLMNUT, GTMGL2NUT, GTMMKNUT. Any misspellings or inclusions of other sites may generate errors
#Filters out non-Sample location IDs 
win_gtm <- win %>% 
  filter(substr('Monitoring Location ID', 1, 3) == "GTM" & substr('Monitoring Location ID', nchar('Monitoring Location ID') - 2, nchar('Monitoring Location ID')) == "NUT")
#Displays all StationCodes
unique(win_gtm$'Monitoring Location ID')
unique(win_gtm$'Monitoring Location ID')
View()
#Results Comments = "J4" - The RPD in the replicate sample duplicate was outside control criteria. Estimated Result / "Q" - Exceeded sample time 
print(unique(win$'Reslt Comments'))

#Results Qualifiers = "U" - Below MDL/ "I" - Below PQL, above PQL
print(unique(win$'Value Qualifier'))
#if any other values are printed consult the lab results .pdf sheet and update code/metadata with new comments
###----

