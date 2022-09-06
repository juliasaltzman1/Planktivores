#Created by: Julia Saltzman 
#Last edited: 20- July- 2022 

# Here I add and integrate environmental and cycle data 

# Packages ----------------------------------------------------------------
library(lubridate)
library(lunar)
library(dplyr)

# Lunar Cycle Data  -------------------------------------------------------

library(lunar)
Format_Date <- as.Date(Cocos_Final$Date,format = "%Y-%m-%d")

Cocos_Final$DateNEW = Format_Date

Cocos_Final$Year_Month <- format(Cocos_Final$DateNEW, "%Y/%m")


Cocos_Final$LunarDistance= lunar.distance(as.Date(Cocos_Final$DateNEW), shift = 6)

Cocos_Final$LunarIlluminationMean = lunar.illumination.mean(as.Date(Cocos_Final$DateNEW),shift= 6)

Cocos_Final$LunarPhase4 = lunar.phase(as.Date(Cocos_Final$DateNEW), shift= 6, name = TRUE)


Cocos_Final$LunarPhase8 = lunar.phase(as.Date(Cocos_Final$DateNEW), shift= 6, name = 8)



# Environmental Data  -----------------------------------------------------


## Sea Surface Temperature -------------------------------------------------
SST_Format_Date <- as.Date(REYONDS_R$DATE, format = "%Y-%m-%d")

REYONDS_R$Formatted_Date= SST_Format_Date

REYONDS_R$Month <- format(REYONDS_R$Formatted_Date, "%m")

REYONDS_R$Year <- format(REYONDS_R$Formatted_Date, "%Y")

REYONDS_R$Year_Month <- format(REYONDS_R$Formatted_Date, "%Y/%m")



## Salinity ----------------------------------------------------------------

  
SALINITY_Format_Date <- as.Date(HADLEY_SALINITY$DATE, format = "%Y-%m-%d")

HADLEY_SALINITY$Formatted_Date= SALINITY_Format_Date

HADLEY_SALINITY$Month <- format(HADLEY_SALINITY$Formatted_Date, "%m")

HADLEY_SALINITY$Year <- format(HADLEY_SALINITY$Formatted_Date, "%Y")

HADLEY_SALINITY$Year_Month <- format(HADLEY_SALINITY$Formatted_Date, "%Y/%m")


## Chlorophyll A ------------------------------------------------------------

CHLA_Format_Date <- as.Date(CHLORO$DATE, format = "%Y-%m-%d")

CHLORO$Formatted_Date= CHLA_Format_Date

CHLORO$Month <- format(CHLORO$Formatted_Date, "%m")

CHLORO$Year <- format(CHLORO$Formatted_Date, "%Y")

CHLORO$Year_Month <- format(CHLORO$Formatted_Date, "%Y/%m")



## Joining Environmental Data ----------------------------------------------


## Mean Monthly Data -------------------------------------------------------

library(dplyr)
SAL_TEMP = REYONDS_R %>% right_join(HADLEY_SALINITY, by = "Year_Month")

SAL_TEMP_CHL = CHLORO %>% right_join(SAL_TEMP, by = "Year_Month")

EDATA = subset(SAL_TEMP_CHL, select = c(Month, Year, Year_Month, CHLA, TEMP, SALINITY))


# EDATA is the full environmental dataset 

cocos_environmental = EDATA %>% inner_join(Cocos_Final, by = "Year_Month")
write.csv(cocos_environmental, "cocos_environmental.csv")

 cocos_environmental$TEMP = 


