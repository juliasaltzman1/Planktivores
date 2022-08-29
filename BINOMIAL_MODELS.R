# CODED BY: JULIA SALTZMAN 
# LAST UPDATED AUGUST 25 2022 
# PLANKTIVORES PRESENCE ABSENCE MODELS
# CONVERT TO PA  ----------------------------------------------------------


cocos_environmental$WhaleSharks_PA <- ifelse(cocos_environmental$WhaleSharks > 0, "1", "0")
cocos_environmental$WhaleSharks_PA  <- as.numeric(cocos_environmental$WhaleSharks_PA )

cocos_environmental$Mantas_PA <- ifelse(cocos_environmental$MantaRays > 0, "1", "0")
cocos_environmental$Mantas_PA  <- as.numeric(cocos_environmental$Mantas_PA )


cocos_environmental$Mobulas_PA <- ifelse(cocos_environmental$MobulaRays > 0, "1", "0")
cocos_environmental$Mobulas_PA  <- as.numeric(cocos_environmental$Mobulas_PA  )


# general planktivore parameter  ------------------------------------------

cocos_environmental$Plank_Sum <- (cocos_environmental$MantaRays + cocos_environmental$MobulaRays + cocos_environmental$WhaleSharks)


cocos_environmental$Plank_PA <- ifelse(cocos_environmental$Plank_Sum  > 0, "1", "0")
cocos_environmental$Plank_PA <- as.numeric(cocos_environmental$Plank_PA )


# general plank model  ----------------------------------------------------


GLOBAL_Planks_PA = glmer(Plank_PA ~  ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 


GLOBAL_Planks_PA_NO_LUNAR =  glmer(Plank_PA ~  ONI + Temperature + SST + SALINITY + CHLA + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Planks_PA_NO_SALINITY = glmer(Plank_PA ~  ONI + Temperature + SST +  CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Planks_PA_NO_CHLA = glmer(Plank_PA ~ ONI + Temperature + SST + SALINITY +  LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Planks_PA_NO_SST =glmer(Plank_PA ~ ONI + Temperature +  SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Planks_PA_NO_TEMPDEPTH = glmer(Plank_PA ~  ONI + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Planks_PA_NO_CURRENTVIS = glmer(Plank_PA~ ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8  + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

Plank_YEAR = glmer(Plank_PA ~   Year.y + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

# Whale Shark Models  -----------------------------------------------------

GLOBAL_WhaleSharks_PA = glmer( WhaleSharks_PA ~  ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 


GLOBAL_WhaleSharks_PA_NO_LUNAR =  glmer( WhaleSharks_PA ~  ONI + Temperature + SST + SALINITY + CHLA + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_WhaleSharks_PA_NO_SALINITY = glmer( WhaleSharks_PA ~  ONI + Temperature + SST +  CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_WhaleSharks_PA_NO_CHLA = glmer( WhaleSharks_PA ~ ONI + Temperature + SST + SALINITY +  LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_WhaleSharks_PA_NO_SST =glmer( WhaleSharks_PA ~ ONI + Temperature +  SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_WhaleSharks_PA_NO_TEMPDEPTH = glmer( WhaleSharks_PA ~  ONI + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_WhaleSharks_PA_NO_CURRENTVIS = glmer( WhaleSharks_PA ~ ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8  + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

WHALE_SHARK_YEAR = glmer(WhaleSharks_PA ~   Year.y + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 


# Manta Models  -----------------------------------------------------------

GLOBAL_Mantas_PA  = glmer( Mantas_PA  ~  ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 


GLOBAL_Mantas_PA_NO_LUNAR =  glmer( Mantas_PA  ~  ONI + Temperature + SST + SALINITY + CHLA + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Mantas_PA_NO_SALINITY = glmer( Mantas_PA  ~  ONI + Temperature + SST +  CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Mantas_PA_NO_CHLA = glmer( Mantas_PA  ~ ONI + Temperature + SST + SALINITY +  LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Mantas_PA_NO_SST =glmer( Mantas_PA  ~ ONI + Temperature +  SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Mantas_PA_NO_TEMPDEPTH = glmer( Mantas_PA  ~  ONI + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Mantas_PA_NO_CURRENTVIS = glmer( Mantas_PA  ~ ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8  + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

MANTA_YEAR = glmer(Mantas_PA ~   Year.y + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 


# Mobula Models -----------------------------------------------------------

GLOBAL_Mobulas_PA   = glmer( Mobulas_PA   ~  ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 


GLOBAL_Mobulas_PA_NO_LUNAR =  glmer( Mobulas_PA   ~  ONI + Temperature + SST + SALINITY + CHLA + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Mobulas_PA_NO_SALINITY = glmer( Mobulas_PA   ~  ONI + Temperature + SST +  CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Mobulas_PA_NO_CHLA = glmer( Mobulas_PA   ~ ONI + Temperature + SST + SALINITY +  LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Mobulas_PA_NO_SST =glmer( Mobulas_PA   ~ ONI + Temperature +  SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial)  

GLOBAL_Mobulas_PA_NO_TEMPDEPTH = glmer( Mobulas_PA   ~  ONI + SST + SALINITY + CHLA + LunarDistance + LunarPhase8 + CurrentCode + Visibility + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

GLOBAL_Mobulas_PA_NO_CURRENTVIS = glmer( Mobulas_PA   ~ ONI + Temperature + SST + SALINITY + CHLA + LunarDistance + LunarPhase8  + Year.y   + SIN_TIME + COS_TIME + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

MOBULAS_YEAR = glmer(Mobulas_PA ~   Year.y + (1|SiteCode) + (1|DiverCode), data = cocos_environmental,  family=binomial) 

# AIC ---------------------------------------------------------------------


## General Plank AIC -------------------------------------------------------
list <- c("GLOBAL_Planks_PA","GLOBAL_Planks_PA_NO_LUNAR", "GLOBAL_Planks_PA_NO_SALINITY", "GLOBAL_Planks_PA_NO_CHLA", "GLOBAL_Planks_PA_NO_SST", "GLOBAL_Planks_PA_NO_TEMPDEPTH", "GLOBAL_Planks_PA_NO_CURRENTVIS")

cand.set <- list(GLOBAL_Planks_PA,GLOBAL_Planks_PA_NO_LUNAR, GLOBAL_Planks_PA_NO_SALINITY, GLOBAL_Planks_PA_NO_CHLA, GLOBAL_Planks_PA_NO_SST, GLOBAL_Planks_PA_NO_TEMPDEPTH, GLOBAL_Planks_PA_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)


## MODEL AVERAGE GENERAL PLANK ---------------------------------------------
library(MuMIn)
plank_avg = model.avg(GLOBAL_Planks_PA, GLOBAL_Planks_PA_NO_SALINITY)
tab_model(plank_avg)
## AIC Mobulas -------------------------------------------------------------


list <- c("GLOBAL_Mobulas_PA","GLOBAL_Mobulas_PA_NO_LUNAR", "GLOBAL_Mobulas_PA_NO_SALINITY", "GLOBAL_MMobulas_PA_NO_CHLA", "GLOBAL_Mobulas_PA_NO_SST", "GLOBAL_Mobulas_PA_NO_TEMPDEPTH", "GLOBAL_Mobulas_PA_NO_CURRENTVIS")

cand.set <- list(GLOBAL_Mobulas_PA,GLOBAL_Mobulas_PA_NO_LUNAR, GLOBAL_Mobulas_PA_NO_SALINITY, GLOBAL_Mobulas_PA_NO_CHLA, GLOBAL_Mobulas_PA_NO_SST, GLOBAL_Mobulas_PA_NO_TEMPDEPTH, GLOBAL_Mobulas_PA_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)

library(AICcmodavg)
library(sjPlot)
tab_model(GLOBAL_Mobulas_PA)


# GLOBAL_Mobulas_PA   
## AIC Mantas --------------------------------------------------------------

list <- c("GLOBAL_Mantas_PA","GLOBAL_Mantas_PA_NO_LUNAR", "GLOBAL_Mantas_PA_NO_SALINITY", "GLOBAL_Mantas_PA_NO_CHLA", "GLOBAL_Mantas_PA_NO_SST", "GLOBAL_Mantas_PA_NO_TEMPDEPTH", "GLOBAL_Mantas_PA_NO_CURRENTVIS")

cand.set <- list(GLOBAL_Mantas_PA,GLOBAL_Mantas_PA_NO_LUNAR, GLOBAL_Mantas_PA_NO_SALINITY, GLOBAL_Mantas_PA_NO_CHLA, GLOBAL_Mantas_PA_NO_SST, GLOBAL_Mantas_PA_NO_TEMPDEPTH, GLOBAL_Mantas_PA_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)


# GLOBAL_Mantas_PA_NO_LUNAR


## AIC Whale Sharks --------------------------------------------------------
list <- c("GLOBAL_WhaleSharks_PA","GLOBAL_WhaleSharks_PA_NO_LUNAR", "GLOBAL_WhaleSharks_PA_NO_SALINITY", "GLOBAL_WhaleSharks_PA_NO_CHLA", "GLOBAL_WhaleSharks_PA_NO_SST", "GLOBAL_WhaleSharks_PA_NO_TEMPDEPTH", "GLOBAL_WhaleSharks_PA_NO_CURRENTVIS")

cand.set <- list(GLOBAL_MWhaleSharks_PA,GLOBAL_MWhaleSharks_PA_NO_LUNAR, GLOBAL_WhaleSharks_PA_NO_SALINITY, GLOBAL_WhaleSharks_PA_NO_CHLA, GLOBAL_WhaleSharks_PA_NO_SST, GLOBAL_WhaleSharks_PA_NO_TEMPDEPTH, GLOBAL_WhaleSharks_PA_NO_CURRENTVIS)

aictab(cand.set, modnames=list, second.ord=TRUE, nobs=NULL, sort=TRUE)


# MODEL COMPARISON  -------------------------------------------------------

tab_model(GLOBAL_Mantas_PA_NO_LUNAR)
tab_model(MANTA_YEAR)
tab_model(GLOBAL_WhaleSharks_PA_NO_LUNAR)
tab_model(GLOBAL_WhaleSharks_PA_NO_LUNAR)
tab_model(WHALE_SHARK_YEAR)
tab_model(GLOBAL_Mobulas_PA)
tab_model(MOBULAS_YEAR)
tab_model(Plank_YEAR)
tab_model(Pl)


# general summary  -------------------------------------------------------

sum(cocos_environmental$Mantas_PA, na.rm= TRUE)
sum(cocos_environmental$MantaRays, na.rm= TRUE)

sum(cocos_environmental$MobulaRays, na.rm= TRUE)
sum(cocos_environmental$Mobulas_PA, na.rm= TRUE)

sum(cocos_environmental$Plank_Sum, na.rm= TRUE)


sum(cocos_environmental$Plank_PA, na.rm= TRUE)

# FIGURES  ----------------------------------------------------------------


## MOBULAS ------------------------------------------------------------------


Mobula_SST <- plot_model(GLOBAL_Mobulas_PA, type = "pred", terms = c("SST"))

Mobula_SST_FIG <- Mobula_SST +  ylab("Probablity of Encountering a Mobula") + xlab("SST")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") 

print(Mobula_SST_FIG)

Mobula_Year <- plot_model(GLOBAL_Mobulas_PA, type = "pred", terms = c("Year.y"))

Mobula_Year_FIG <- Mobula_Year  +  ylab("Probablity of Encountering a Mobula") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("") 


print(Mobula_Year_FIG)

Mobula_Lunar <- plot_model(GLOBAL_Mobulas_PA, type = "pred", terms = c("LunarPhase8"))

Mobula_Lunar_FIG <- Mobula_Lunar  +  ylab("Probablity of Encountering a Mobula") + xlab("Lunar Phase")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("") 

Mobula_Vis <- plot_model(GLOBAL_Mobulas_PA, type = "pred", terms = c("Visibility"))

Mobula_Vis_FIG <- Mobula_Vis  +  ylab("Probablity of Encountering a Mobula") + xlab("Visibility")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("") 

Mobula_Current<- plot_model(GLOBAL_Mobulas_PA, type = "pred", terms = c("CurrentCode"))

Mobula_Current_FIG <- Mobula_Current  +  ylab("Probablity of Encountering a Mobula") + xlab("Current")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("") 

print(Mobula_Current_FIG)

range(cocos_environmental$CurrentCode)

print(Mobula_Lunar_FIG)


## MANTAS 

Manta_Year <- plot_model(GLOBAL_Mantas_PA_NO_LUNAR, type = "pred", terms = c("Year.y"))

Manta_Year_FIG <- Manta_Year  +  ylab("Probablity of Encountering a Manta") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("") 

print(Manta_Year_FIG)

Manta_Vis <- plot_model(GLOBAL_Mantas_PA_NO_LUNAR, type = "pred", terms = c("Visibility"))

Manta_Vis_FIG <- Manta_Vis  +  ylab("Probablity of Encountering a Manta") + xlab("Visibility")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("") 

print(Manta_Vis_FIG)

## whale sharks  -----------------------------------------------------------

WhaleShark_Vis <- plot_model(GLOBAL_WhaleSharks_PA_NO_LUNAR, type = "pred", terms = c("Visibility"))

WhaleShark_Vis_FIG <- WhaleShark_Vis  +  ylab("Probablity of Encountering a Whale Shark") + xlab("Visibility")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("")

print(WhaleShark_Vis_FIG)

WhaleShark_Year <- plot_model(GLOBAL_WhaleSharks_PA_NO_LUNAR, type = "pred", terms = c("Year.y"))

WhaleShark_Year_FIG <- WhaleShark_Year  +  ylab("Probablity of Encountering a Whale Shark") + xlab("Year")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("")
print(WhaleShark_Year_FIG)


WhaleShark_ONI <- plot_model(GLOBAL_WhaleSharks_PA_NO_LUNAR, type = "pred", terms = c("ONI"))

WhaleShark_ONI_FIG <- WhaleShark_ONI  +  ylab("Probablity of Encountering a Whale Shark") + xlab("ONI")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("")
print(WhaleShark_ONI_FIG)

WhaleShark_CHLA <- plot_model(GLOBAL_WhaleSharks_PA_NO_LUNAR, type = "pred", terms = c("CHLA"))

WhaleShark_CHLA_FIG <- WhaleShark_ONI  +  ylab("Probablity of Encountering a Whale Shark") + xlab("Chlorophyll A")+  theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + ggtitle("") + xlim(0,2)
print(WhaleShark_CHLA_FIG)


# Residuals ---------------------------------------------------------------

plot(GLOBAL_Mantas_PA_NO_LUNAR)
plot(GLOBAL_WhaleSharks_PA_NO_LUNAR)
plot(GLOBAL_Mobulas_PA)