# CODED BY: JULIA SALTZMAN 

# LAST UPDATED SEPTEMBER 3 2022 


# SET UP AND REQUIRED PACKAGES --------------------------------------------
library(glmmTMB)
library(AICcmodavg)
library(sjPlot) 
library(MuMIn)
library(ggpubr)
library(ggplot2)
# MOBULA MODELS  ----------------------------------------------------------

GLOBAL_MOBULAS_ZINB <- glmmTMB(MobulaRays ~ SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)


GLOBAL_MOBULAS_ZINB_NO_LUNAR <- glmmTMB(MobulaRays ~ SST + Temperature  + SALINITY + CHLA + ONI + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)


GLOBAL_MOBULAS_ZINB_NO_SALINITY <- glmmTMB(MobulaRays ~ SST + Temperature  +  CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MOBULAS_ZINB_NO_CHLA  <- glmmTMB(MobulaRays ~ SST + Temperature  + SALINITY +  ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MOBULAS_ZINB_NO_SST <- glmmTMB(MobulaRays ~ Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MOBULAS_ZINB_NO_TEMPDEPTH <- glmmTMB(MobulaRays ~ SST + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MOBULAS_NO_CURRENTVIS <- glmmTMB(MobulaRays ~ SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MOBULAS_YEAR_ONLY <- glmmTMB(MobulaRays ~ Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

# MOBULA AIC 

list <- c("GLOBAL_MOBULAS_ZINB ","GLOBAL_MOBULAS_ZINB_NO_LUNAR", "GLOBAL_MOBULAS_ZINB_NO_SALINITY", "GLOBAL_MOBULAS_ZINB_NO_CHLA", "GLOBAL_MOBULAS_ZINB_NO_SST ", "GLOBAL_MOBULAS_ZINB_NO_TEMPDEPTH ", "GLOBAL_MOBULAS_NO_CURRENTVIS")


cand.set <- list(GLOBAL_MOBULAS_ZINB,GLOBAL_MOBULAS_ZINB_NO_LUNAR, GLOBAL_MOBULAS_ZINB_NO_SALINITY, GLOBAL_MOBULAS_ZINB_NO_CHLA, GLOBAL_MOBULAS_ZINB_NO_SST , GLOBAL_MOBULAS_ZINB_NO_TEMPDEPTH , GLOBAL_MOBULAS_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)

# RESIUDALS 

resmobs <- resid(GLOBAL_MOBULAS_ZINB_NO_LUNAR)
plot(fitted(GLOBAL_MOBULAS_ZINB_NO_LUNAR), resmobs)
qqnorm(resmobs)
qqline(resmobs) 
plot(density(resmobs))

resmobs2 <- resid(GLOBAL_MOBULAS_ZINB_NO_SALINITY)
plot(fitted(GLOBAL_MOBULAS_ZINB_NO_SALINITY), resmobs2)
qqnorm(resmobs2)
qqline(resmobs2) 
plot(density(resmobs2))

# MOBULA MODEL AVERAGE 

MOBULA_MODEL_AVERAGE_ZINB = model.avg(GLOBAL_MOBULAS_ZINB_NO_LUNAR, GLOBAL_MOBULAS_ZINB_NO_SALINITY)

tab_model(MOBULA_MODEL_AVERAGE_ZINB)

# COMPARE MODELS ACCOUNTING FOR AND NOT ACCOUNTING FOR ENVIRONMENTAL VARIABLITY 

tab_model(MOBULA_MODEL_AVERAGE_ZINB)
tab_model(GLOBAL_MOBULAS_YEAR_ONLY)
# MANTA MODELS  -----------------------------------------------------------


GLOBAL_MANTAS_ZINB <- glmmTMB(MantaRays ~ SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)


GLOBAL_MANTAS_ZINB_NO_LUNAR <- glmmTMB(MantaRays ~ SST + Temperature  + SALINITY + CHLA + ONI + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)


GLOBAL_MANTAS_ZINB_NO_SALINITY <- glmmTMB(MantaRays ~ SST + Temperature  +  CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MANTAS_ZINB_NO_CHLA  <- glmmTMB(MantaRays ~ SST + Temperature  + SALINITY +  ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MANTAS_ZINB_NO_SST <- glmmTMB(MantaRays ~ Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MANTAS_ZINB_NO_TEMPDEPTH <- glmmTMB(MantaRays ~ SST + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MANTAS_NO_CURRENTVIS <- glmmTMB(MantaRays ~ SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_MANTAS_YEAR_ONLY <- glmmTMB(MantaRays ~ Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

# AIC 

list <- c("GLOBAL_MANTAS_ZINB ","GLOBAL_MANTAS_ZINB_NO_LUNAR", "GLOBAL_MANTAS_ZINB_NO_SALINITY", "GLOBAL_MANTAS_ZINB_NO_CHLA", "GLOBAL_MANTAS_ZINB_NO_SST ", "GLOBAL_MANTAS_ZINB_NO_TEMPDEPTH ", "GLOBAL_MANTAS_NO_CURRENTVIS")


cand.set <- list(GLOBAL_MANTAS_ZINB,GLOBAL_MANTAS_ZINB_NO_LUNAR, GLOBAL_MANTAS_ZINB_NO_SALINITY, GLOBAL_MANTAS_ZINB_NO_CHLA, GLOBAL_MANTAS_ZINB_NO_SST , GLOBAL_MANTAS_ZINB_NO_TEMPDEPTH , GLOBAL_MANTAS_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)

# COMPARE MODELS ACCOUNTING FOR AND NOT ACCOUNTING FOR ENVIRONMENTAL VARIABLITY 


tab_model(GLOBAL_MANTAS_ZINB_NO_LUNAR)
tab_model(GLOBAL_MANTAS_YEAR_ONLY )
# WHALE SHARK MODELS  -----------------------------------------------------
GLOBAL_WHALESHARKS_ZINB <- glmmTMB(WhaleSharks ~ SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)


GLOBAL_WHALESHARKS_ZINB_NO_LUNAR <- glmmTMB(WhaleSharks ~ SST + Temperature  + SALINITY + CHLA + ONI + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)


GLOBAL_WHALESHARKS_ZINB_NO_SALINITY <- glmmTMB(WhaleSharks ~ SST + Temperature  +  CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_WHALESHARKS_ZINB_NO_CHLA  <- glmmTMB(WhaleSharks ~ SST + Temperature  + SALINITY +  ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_WHALESHARKS_ZINB_NO_SST <- glmmTMB(WhaleSharks ~ Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_WHALESHARKS_ZINB_NO_TEMPDEPTH <- glmmTMB(WhaleSharks ~ SST + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + CurrentCode + Visibility + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_WHALESHARKS_NO_CURRENTVIS <- glmmTMB(WhaleSharks ~ SST + Temperature  + SALINITY + CHLA + ONI + LunarDistance + LunarPhase8 + SIN_TIME + COS_TIME +Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

GLOBAL_WHALESHARKS_YEAR_ONLY <- glmmTMB(WhaleSharks ~ Year.y + (1|DiverCode)+(1|SiteCode), data= cocos_environmental, family="nbinom2",ziformula=~1, REML=TRUE)

# AIC 
list <- c("GLOBAL_WHALESHARKS_ZINB ","GLOBAL_WHALESHARKS_ZINB_NO_LUNAR", "GLOBAL_WHALESHARKS_ZINB_NO_SALINITY", "GLOBAL_WHALESHARKS_ZINB_NO_CHLA", "GLOBAL_WHALESHARKS_ZINB_NO_SST ", "GLOBAL_WHALESHARKS_ZINB_NO_TEMPDEPTH ", "GLOBAL_WHALESHARKS_NO_CURRENTVIS")


cand.set <- list(GLOBAL_WHALESHARKS_ZINB,GLOBAL_WHALESHARKS_ZINB_NO_LUNAR, GLOBAL_WHALESHARKS_ZINB_NO_SALINITY, GLOBAL_WHALESHARKS_ZINB_NO_CHLA, GLOBAL_WHALESHARKS_ZINB_NO_SST , GLOBAL_WHALESHARKS_ZINB_NO_TEMPDEPTH , GLOBAL_WHALESHARKS_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)

# COMPARE MODELS ACCOUNTING FOR AND NOT ACCOUNTING FOR ENVIRONMENTAL VARIABLITY 
tab_model(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR, collapse.ci = TRUE)
tab_model(GLOBAL_WHALESHARKS_YEAR_ONLY )
summary(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR)
exp( 6.204e-02 )
tab_model(GLOBAL_WHALESHARKS_ZINB)


# TOP MODEL TABLES  -------------------------------------------------------

tab_model(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR, collapse.ci = TRUE, CSS = css_theme("cells"))

tab_model(GLOBAL_MANTAS_ZINB_NO_LUNAR, collapse.ci = TRUE, CSS = css_theme("cells"))

tab_model(MOBULA_MODEL_AVERAGE_ZINB , collapse.ci = TRUE, CSS = css_theme("cells"))




# MOBULA RAY FIGURES -----------------------------------------------------------------
Mobula_SST <- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("SST"))

Mobula_SST_FIG <- Mobula_SST +  ylab("Mobula Count") + xlab("SST")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("")  +  xlim(25,30)

print(Mobula_SST_FIG )

Mobula_Temp <- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("Temperature"))

Mobula_Temp_FIG <- Mobula_Temp  +  ylab("Mobula Count") + xlab("Temperature at Depth")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") +  xlim(25,30)+ ylim(0,0.2)

print(Mobula_Temp_FIG )

Mobula_ONI <- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("ONI"))

Mobula_ONI_FIG <- Mobula_ONI  +  ylab("Mobula Count") + xlab("ONI")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Mobula_ONI_FIG)

Mobula_Salinity <- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("SALINITY"))

Mobula_Salinity_FIG <- Mobula_Salinity +  ylab("Mobula Count") + xlab("Salinity")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Mobula_Salinity_FIG)

Mobula_Current <- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("CurrentCode"))

Mobula_Current_FIG <-Mobula_Current +  ylab("Mobula Count") + xlab("Current Code")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Mobula_Current_FIG)

Mobula_Visibility<- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("Visibility"))

Mobula_Visibility_FIG <- Mobula_Visibility +  ylab("Mobula Count") + xlab("Visibility")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Mobula_Visibility_FIG )

Mobula_Year <- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("Year.y"))

Mobula_Year_FIG <- Mobula_Year  +  ylab("Mobula Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Mobula_Year_FIG )

Mobula_Lunar <- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("LunarPhase8"))

Mobula_Lunar_FIG <- Mobula_Lunar  +  ylab("Mobula Count") + xlab("Lunar Phase")+  theme(text = element_text(size = 15), axis.text.x = element_text(angle = 90), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 
print(Mobula_Lunar_FIG)

ggarrange(Mobula_SST_FIG, Mobula_Temp_FIG, Mobula_ONI_FIG, Mobula_Salinity_FIG, Mobula_Current_FIG, Mobula_Visibility_FIG)


# MANTA RAY FIGURES --------------------------------------------------------------
Manta_SST <- plot_model(GLOBAL_MANTAS_ZINB_NO_LUNAR  , type = "pred", terms = c("SST"))

Manta_SST_FIG <- Manta_SST +  ylab("Manta Count") + xlab("SST")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("")  +  xlim(25,30)

print(Manta_SST_FIG )

Manta_Temp <- plot_model(GLOBAL_MANTAS_ZINB_NO_LUNAR  , type = "pred", terms = c("Temperature"))

Manta_Temp_FIG <- Manta_Temp  +  ylab("Manta Count") + xlab("Temperature at Depth")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") +  xlim(25,30) + ylim(0, 0.075)

print(Manta_Temp_FIG )


Manta_Visibility<- plot_model(GLOBAL_MANTAS_ZINB_NO_LUNAR  , type = "pred", terms = c("Visibility"))

Manta_Visibility_FIG <- Manta_Visibility +  ylab("Manta Count") + xlab("Visibility")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Manta_Visibility_FIG )

Manta_Year <- plot_model(GLOBAL_MANTAS_ZINB_NO_LUNAR  , type = "pred", terms = c("Year.y"))

Manta_Year_FIG <- Manta_Year  +  ylab("Manta Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Manta_Year_FIG )

ggarrange(Manta_SST_FIG, Manta_Temp_FIG,Manta_Visibility_FIG )

# WHALE SHARK FIGURES -----------------------------------------------------
Whale_Shark_Temp <- plot_model(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR  , type = "pred", terms = c("Temperature"))

Whale_Shark_Temp_FIG <- Whale_Shark_Temp  +  ylab("Whale Shark Count") + xlab("Temperature at Depth")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") +  xlim(25,30)+ ylim(0,0.025)

print(Whale_Shark_Temp_FIG )

Whale_Shark_CHLA <- plot_model(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR  , type = "pred", terms = c("CHLA"))

Whale_Shark_CHLA_FIG <- Whale_Shark_CHLA +  ylab("Whale Shark Count") + xlab("Chlorophyll A")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Whale_Shark_CHLA_FIG)

Whale_Shark_ONI <- plot_model(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR  , type = "pred", terms = c("ONI"))

Whale_Shark_ONI_FIG <- Whale_Shark_ONI  +  ylab("Whale Shark Count") + xlab("ONI")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Whale_Shark_ONI_FIG)


Whale_Shark_Visibility<- plot_model(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR  , type = "pred", terms = c("Visibility"))

Whale_Shark_Visibility_FIG <- Whale_Shark_Visibility +  ylab("Whale Shark Count") + xlab("Visibility")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Whale_Shark_Visibility_FIG )

Whale_Shark_Year <- plot_model(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR  , type = "pred", terms = c("Year.y"))

Whale_Shark_Year_FIG <- Whale_Shark_Year  +  ylab("Whale Shark Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Whale_Shark_Year_FIG )

ggarrange(Whale_Shark_Temp_FIG, Whale_Shark_ONI_FIG, Whale_Shark_CHLA_FIG, Whale_Shark_Visibility_FIG)

range(cocos_environmental$CHLA, na.rm = TRUE)
# MODEL COMPARISON --> GRAPHS FOR FIGURE 2 ###### 

Mobula_Year <- plot_model(GLOBAL_MOBULAS_ZINB  , type = "pred", terms = c("Year.y"))

Mobula_Year_FIG <- Mobula_Year  +  ylab("Mobula Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") + xlim(2000,2020)

print(Mobula_Year_FIG )

Mobula_Year2 <- plot_model(GLOBAL_MOBULAS_YEAR_ONLY   , type = "pred", terms = c("Year.y"))

Mobula_Year2_FIG <- Mobula_Year2   +  ylab("Mobula Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") + xlim(2000,2020)

ggarrange(Mobula_Year_FIG, Mobula_Year2_FIG)

Whale_Shark_Year <- plot_model(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR  , type = "pred", terms = c("Year.y"))

Whale_Shark_Year_FIG <- Whale_Shark_Year  +  ylab("Whale Shark Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") + xlim(2000,2020)


print(Whale_Shark_Year_FIG )


Whale_Shark_Year2 <- plot_model(GLOBAL_WHALESHARKS_YEAR_ONLY  , type = "pred", terms = c("Year.y"))

Whale_Shark_Year_FIG2 <- Whale_Shark_Year2  +  ylab("Whale Shark Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") + xlim(2000,2020)

print(Whale_Shark_Year_FIG2 )

ggarrange(Whale_Shark_Year_FIG,Whale_Shark_Year_FIG2 )


Manta_Year <- plot_model(GLOBAL_MANTAS_ZINB_NO_LUNAR  , type = "pred", terms = c("Year.y"))

Manta_Year_FIG <- Manta_Year  +  ylab("Manta Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") + xlim(2000,2020)


Manta_Year2 <- plot_model(GLOBAL_MANTAS_YEAR_ONLY  , type = "pred", terms = c("Year.y"))

Manta_Year_FIG2 <- Manta_Year2  +  ylab("Manta Count") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") + xlim(2000,2020)

ggarrange(Manta_Year_FIG, Manta_Year_FIG2)
