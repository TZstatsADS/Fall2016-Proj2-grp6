setwd("/Users/jiwenyou/Desktop")

# Import housing data and restaurant inspection datasets
building <- fread("ManhattanHousing.csv")
food <- fread("DOHMH_New_York_City_Restaurant_Inspection_Results.csv")
theater <- fread("DOITT_THEATER_01_13SEPT2010.csv")

# We are going to create five categories to describe the location
# Public Facility, Entertainment, Residential Area, Restaurant/Cafe and Bar
distinct(building, CATEGORY)
public <- list("35  INDOOR PUBLIC AND CULTURAL FACILITIES","32  HOSPITAL AND HEALTH FACILITIES","27  FACTORIES",
               "37  RELIGIOUS FACILITIES","40  SELECTED GOVERNMENTAL FACILITIES","33  EDUCATIONAL FACILITIES",
               "30  WAREHOUSES","29  COMMERCIAL GARAGES","28  COMMERCIAL CONDOS","21  OFFICE BUILDINGS","23  LOFT BUILDINGS",
               "16  CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT","31  COMMERCIAL VACANT LAND","22  STORE BUILDINGS")

entertainment <- list("36  OUTDOOR RECREATIONAL FACILITIES","26  OTHER HOTELS","25  LUXURY HOTELS","34  THEATRES")

residential <- list("38  ASYLUMS AND HOMES","11A CONDO-RENTALS","04  TAX CLASS 1 CONDOS","41  TAX CLASS 4 - OTHER",
                 "14  RENTALS - 4-10 UNIT"," 03  THREE FAMILY HOMES","02  TWO FAMILY HOMES","01  ONE FAMILY HOMES",
                 "17  CONDOPS","15  CONDOS - 2-10 UNIT RESIDENTIAL","13  CONDOS - ELEVATOR APARTMENTS",
                 "12  CONDOS - WALKUP APARTMENTS","10  COOPS - ELEVATOR APARTMENTS","09  COOPS - WALKUP APARTMENTS",
                 "08  RENTALS - ELEVATOR APARTMENTS","07  RENTALS - WALKUP APARTMENTS")

building <- building %>% 
  dplyr::select(NEIGHBORHOOD,CATEGORY = `BUILDING CLASS CATEGORY`,ADDRESS,ZIPCODE = `ZIP CODE`) %>%
  filter(ZIPCODE > 0) %>%
  mutate(region=as.character(ZIPCODE)) %>%
  mutate(NEW_CATEGORY = ifelse(CATEGORY %in% public, "PUBLIC FACILITY", 
                               ifelse(CATEGORY %in% entertainment, "ENTERTAINMENT", "RESIDENTIAL AREA")))
theater <- theater %>% 
  dplyr::select(NEIGHBORHOOD = CITY, ADDRESS = ADDRESS1, ZIPCODE = ZIP) %>%
  filter(ZIPCODE > 0) %>%
  mutate(region=as.character(ZIPCODE),CATEGORY = "THEATER") %>%
  mutate(NEW_CATEGORY = "ENTERTAINMENT")

building <- rbind(building,theater)

food <- food %>%
  dplyr::select(DBA,BORO,BUILDING,STREET,ZIPCODE) %>%
  filter(ZIPCODE > 0) %>%
  mutate(region=as.character(ZIPCODE)) %>%
  mutate(NEW_CATEGORY = ifelse(grepl("BAR", food$DBA), "BAR", "RESTAURANT/CAFE"))

# Each observation represents one place
# We only keep zipcode and category in the dataset
public_whole <- select(building,NEW_CATEGORY,region) %>%
  rbind(select(food,NEW_CATEGORY,region))

# Count the number of each kinds of facilities within different zipcode region
public_count <- public_whole %>%
  group_by(region,NEW_CATEGORY) %>%
  dplyr::summarise(value = n())
  
public_count <- public_count[!(public_count$NEW_CATEGORY == "ENTERTAINMENT" & public_count$value > 1000),]
public_count <- public_count[!(public_count$NEW_CATEGORY == "PUBLIC FACILITY" & public_count$value > 1000),]       
save(building, file="./Fall2016-Proj2-grp6/data/building.RData")
save(food,file="./Fall2016-Proj2-grp6/data/food.RData")
save(public_whole,file="./Fall2016-Proj2-grp6/data/public_whole.RData")
save(public_count,file="./Fall2016-Proj2-grp6/data/public_count.RData")



