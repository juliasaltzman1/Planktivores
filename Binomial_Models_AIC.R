# Set Up and Required Packages --------------------------------------------
install.packages(lme4)
library(lme4) # for binomial glmm 
install.packages(AICcmodavg)
library(AICcmodavg) # for AIC 
install.packages(sjPlot)
library(sjPlot) # for visualization 

# Note: The name of the data file in this code is "GCE" 

# You can read more about 'lme4' here: https://cran.r-project.org/web/packages/lme4/lme4.pdf 
# You can read more about 'glmer' (the function we used in LME4) here: https://www.rdocumentation.org/packages/lme4/versions/1.1-29/topics/glmer  
# You can read more about 'AICcmodavg' here: https://cran.r-project.org/web/packages/AICcmodavg/AICcmodavg.pdf 
# You can read more about 'sjPlot' here: https://strengejacke.github.io/sjPlot/ 

# Whale Sharks  -----------------------------------------------------------

WSAA <- glmer(Whale.Shark ~ TEMP +  ElNinoIndex + CHLA +  SALINITY + LunarIlluminationMean + VisibilityMeters + CurrentCode  + Year.x + SIN_JULIAN + COS_JULIAN + SeaTempCelsius + LunarPhase + LunarDistance + (1|SiteCode) + (1|DiverCode), data = GCE, family = binomial)
summary(WSAA)

WSBB <- glmer(Whale.Shark ~ TEMP + ElNinoIndex + CHLA +  SALINITY + LunarIlluminationMean + VisibilityMeters + CurrentCode  + Year.x + SIN_JULIAN + COS_JULIAN + SeaTempCelsius + (1|SiteCode)+(1|DiverCode), data = GCE, family = binomial) 
summary(WSBB)

WSCC <- glmer(Whale.Shark ~ TEMP + ElNinoIndex + CHLA +  SALINITY + VisibilityMeters + CurrentCode  + Year.x + SIN_JULIAN + COS_JULIAN + SeaTempCelsius + (1|SiteCode)+(1|DiverCode), data = GCE, family = binomial)
summary(WSCC)

WSDD <- glmer(Whale.Shark ~ TEMP +  ElNinoIndex + CHLA + LunarIlluminationMean + VisibilityMeters + CurrentCode  + Year.x + SIN_JULIAN + COS_JULIAN + SeaTempCelsius + LunarPhase + LunarDistance + (1|SiteCode) + (1|DiverCode), data = GCE, family = binomial)
summary(WSDD)

WSEE <- glmer(Whale.Shark ~ TEMP +  ElNinoIndex +  SALINITY + LunarIlluminationMean + VisibilityMeters + CurrentCode  + Year.x + SIN_JULIAN + COS_JULIAN + SeaTempCelsius + LunarPhase + LunarDistance + (1|SiteCode) + (1|DiverCode), data = GCE, family = binomial)
summary(WSEE)

WSFF <- glmer(Whale.Shark ~  ElNinoIndex + CHLA +  SALINITY + LunarIlluminationMean + VisibilityMeters + CurrentCode  + Year.x + SIN_JULIAN + COS_JULIAN + SeaTempCelsius + LunarPhase + LunarDistance + (1|SiteCode) + (1|DiverCode), data = GCE,family = binomial )
summary(WSFF)

WSGG <- glmer(Whale.Shark ~ TEMP +  ElNinoIndex + CHLA +  SALINITY + LunarIlluminationMean + VisibilityMeters + CurrentCode  + Year.x + SIN_JULIAN + COS_JULIAN +  LunarPhase + LunarDistance + (1|SiteCode) + (1|DiverCode), data = GCE, family = binomial)
summary(WSGG)

WSHH <- glmer(Whale.Shark ~ TEMP +  ElNinoIndex + CHLA +  SALINITY + LunarIlluminationMean + Year.x + SIN_JULIAN + COS_JULIAN + SeaTempCelsius + LunarPhase + LunarDistance + (1|SiteCode) + (1|DiverCode), data = GCE, family = binomial)
summary(WSHH)


# Visualization of Model  -------------------------------------------------

plot_model(WSCC, show.values=TRUE, show.p=TRUE)
tab_model(WSCC, show.se  = "TRUE")
tab_model(AA, show.se  = "TRUE")