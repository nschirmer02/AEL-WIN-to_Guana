#Loading in AEL WIN data and GTMNERR Masterdata into Rproj
win <- readxl::read_excel("C:/Users/schirmer_n/Documents/Data/Guana_data/2025.04_WIN.xlsx", 1)
field <- readxl::read_excel("C:/Users/schirmer_n/Documents/Data/Guana_data/2025_Guana_masterdata_NS(1).xlsx", "2025")
#StationCodes = GTMGRNUT, GTMRNNUT, GTMLSNUT, GTMLMNUT, GTMGL2NUT, GTMMKNUT. Any misspellings or inclusions of other sites may generate errors
#Filters out non-Sample location IDs 

win_gtm <- win %>% 
  filter(substr(win$`Monitoring Location ID`, 1, 3) == "GTM" & substr(win$`Monitoring Location ID`, nchar(win$`Monitoring Location ID`) - 2, nchar(win$`Monitoring Location ID`)) == "NUT")

#Displays all StationCodes
nut_sites <- unique(win_gtm$'Monitoring Location ID')
print(nut_sites)

#Results Comments = "J4" - The RPD in the replicate sample duplicate was outside control criteria. Estimated Result / "Q" - Exceeded sample time 
print(unique(win$'Result Comments'))

#Results Qualifiers = "U" - Below MDL/ "I" - Below PQL, above PQL
print(unique(win$'Value Qualifier'))
#if any other values are printed consult the lab results .pdf sheet and update code/metadata with new comments
###----

