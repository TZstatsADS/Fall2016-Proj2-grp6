# convert longitude and latitude to zipcode
library(zipcode)

# dataset zipcode contains the longtitude and latitude of the center point within each zipcode area
data(zipcode)
unique(crime_data$Borough)
names(crime_data)

# seperate the datasets according to different counties
brooklyn <- crime_data %>% filter(Borough == "BROOKLYN")
manhattan <- crime_data %>% filter(Borough == "MANHATTAN")
queens <- crime_data %>% filter(Borough == "QUEENS")
bronx <- crime_data %>% filter(Borough == "BRONX")
si <- crime_data %>% filter(Borough == "STATEN ISLAND")
other <- crime_data %>% filter(Borough %in% c("(null)" , ""))

# zipcodes in different counties
Bronx1 <- c(10453, 10457, 10460,10458, 10467, 10468,10451, 10452, 10456,10454, 10455, 10459, 10474,10463,10471,10466, 10469, 10470, 10475,10461, 10462,10464, 10465,10472, 10473)

Brooklyn1 <- c(11212, 11213, 11216, 11233, 11238,11209, 11214, 11228,11204, 11218, 11219, 11230,11234, 11236, 11239,11223, 11224, 11229, 11235,11201, 11205, 11215, 11217, 11231,11203, 11210, 11225, 11226,11207, 11208,11211, 11222,11220, 11232,11206, 11221, 11237)

Manhattan1 <- c(10026, 10027, 10030, 10037, 10039,10001, 10011, 10018, 10019, 10020, 10036,10029, 10035,10010, 10016, 10017, 10022,10012, 10013, 10014,10004, 10005, 10006, 10007, 10038, 10280,10002, 10003, 10009,10021, 10028, 10044, 10065, 10075, 10128,10023, 10024, 10025,10031, 10032, 10033, 10034, 10040)

Queens1 <- c(11361, 11362, 11363, 11364,11354, 11355, 11356, 11357, 11358, 11359, 11360,11365, 11366, 11367,11412, 11423, 11432, 11433, 11434, 11435, 11436,11101, 11102, 11103, 11104, 11105, 11106,11374, 11375, 11379, 11385,11691, 11692, 11693, 11694, 11695, 11697,11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429,11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421,11368, 11369, 11370, 11372, 11373, 11377, 11378)

Si1 <-	c(10302, 10303, 10310,10306, 10307, 10308, 10309, 10312,10301, 10304, 10305,10314)

Bronx1 <- as.character(Bronx1)
Brooklyn1 <- as.character(Brooklyn1)
Manhattan1 <- as.character(Manhattan1)
Queens1 <- as.character(Queens1)
Si1 <- as.character(Si1)
other1 <- c(Bronx1,Brooklyn1,Manhattan1,Queens1,Si1)

Bronxmap <- zipcode %>% filter(zip %in% Bronx1)
Brooklynmap <- zipcode %>% filter(zip %in% Brooklyn1)
Manhattanmap <- zipcode %>% filter(zip %in% Manhattan1)
Queensmap <- zipcode %>% filter(zip %in% Queens1)
Simap <- zipcode %>% filter(zip %in% Si1)
othermap <- zipcode %>% filter(zip %in% other1)

# find the closest zipcode center point
cal <- function(data,y){
  table <- y[,4:5]
  table[,1] <- as.numeric(data[22])
  table[,2] <- as.numeric(data[23])
  dif <- y[,4:5] - table
  differ <- (dif^2)[,1] + (dif^2)[,2]
  return(y[which.min(differ),1])
}
si$zip <- apply(si,1,cal,y=Simap)
bronx$zip <- apply(bronx,1,cal,y=Bronxmap)
brooklyn$zip <- apply(brooklyn,1,cal,y=Brooklynmap)
manhattan$zip <- apply(manhattan,1,cal,y=Manhattanmap)
queens$zip <- apply(queens,1,cal,y=Queensmap)
other$zip <- apply(other,1,cal,y=othermap)

# combine them together and generate a new dataset
crime.new <- rbind(bronx,brooklyn,manhattan,queens,si,other)
save(crime.new,file="./Fall2016-Proj2-grp6/data/crime.new/crime.new.RData")
save(si,file="./Fall2016-Proj2-grp6/data/crime.new/si.RData")
save(bronx,file="./Fall2016-Proj2-grp6/data/crime.new/bronx.RData")
save(brooklyn,file="./Fall2016-Proj2-grp6/data/crime.new/brooklyn.RData")
save(queens,file="./Fall2016-Proj2-grp6/data/crime.new/queens.RData")
save(manhattan,file="./Fall2016-Proj2-grp6/data/crime.new/manhattan.RData")
save(other,file="./Fall2016-Proj2-grp6/data/crime.new/other.RData")
