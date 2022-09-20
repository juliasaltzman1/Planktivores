# CODED BY JULIA SALTZMAN 
# LAST UPDATED SEPTEMBER 14 2022 
# READ IN DATA ------------------------------------------------------------
Cocos_Final <- read.csv("~/Downloads/Cocos_Final.csv")
COCOS_UNCLEAN<- Cocos_Final
HADLEY_SALINITY <- read_excel("~/Desktop/Graduate_Research/Environmental Data/Data_Set_1/HADLEY_SALINITY.xls")
CHLORO <- read_excel("~/Desktop/Graduate_Research/Environmental Data/Data_Set_1/CHLORO.xls")



# LUNAR CYCLE  ------------------------------------------------------------

library(lunar)

Format_Date <- as.Date(Cocos_Final$Date,format = "%Y-%m-%d")

Cocos_Final$DateNEW = Format_Date

Cocos_Final$Year_Month <- format(Cocos_Final$DateNEW, "%Y/%m")


Cocos_Final$LunarDistance= lunar.distance(as.Date(Cocos_Final$DateNEW), shift = 6)

Cocos_Final$LunarIlluminationMean = lunar.illumination.mean(as.Date(Cocos_Final$DateNEW),shift= 6)

Cocos_Final$LunarPhase4 = lunar.phase(as.Date(Cocos_Final$DateNEW), shift= 6, name = TRUE)


Cocos_Final$LunarPhase8 = lunar.phase(as.Date(Cocos_Final$DateNEW), shift= 6, name = 8)

# JOIN IN SALINITY AND CHLA ------------------------------------------------------------
library(dplyr)

CHLA_Format_Date <- as.Date(CHLORO$DATE, format = "%Y-%m-%d")

CHLORO$Formatted_Date= CHLA_Format_Date

CHLORO$Month <- format(CHLORO$Formatted_Date, "%m")

CHLORO$Year <- format(CHLORO$Formatted_Date, "%Y")

CHLORO$Year_Month <- format(CHLORO$Formatted_Date, "%Y/%m")

SAL_CHL = CHLORO %>% right_join(HADLEY_SALINITY, by = "Year_Month")

cocos_environmental = SAL_CHL %>% inner_join(Cocos_Final, by = "Year_Month")
write.csv(cocos_environmental, "cocos_environmental.csv")




# CLEAN DATA --------------------------------------------------------------
# first lets summarize 

table(cocos_environmental$WhaleSharks)
table(cocos_environmental$MantaRays)
table(cocos_environmental$MobulaRays)

# The negative 1s in the dataset indicate that a diver saw a given species, but they did not indicate how many they saw. For this reason, -1s become a 1 to indicate presence. Because, the divers saw atleast 1 of an individual, so this should not be a 0 in models. 

cocos_environmental$WhaleSharks[cocos_environmental$WhaleSharks < 0 ] <- 1
cocos_environmental$MantaRays[cocos_environmental$MantaRays < 0 ] <- 1
cocos_environmental$MobulaRays[cocos_environmental$MobulaRays < 0 ] <- 1

write.csv(cocos_environmental)



