#Created by: Julia Saltzman 
#Last edited: 24- July- 2022 

# CORRELATIONS  -----------------------------------------------------------

# here I explore the correlations between the temperature and ONI data 


cor(cocos_environmental$SST, cocos_environmental$Temperature,  use = "complete.obs")
cor(cocos_environmental$SST, cocos_environmental$ONI,  use = "complete.obs")
cor(cocos_environmental$Temperature, cocos_environmental$ONI,  use = "complete.obs")
