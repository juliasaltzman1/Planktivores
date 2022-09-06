
# RESIDUALS  --------------------------------------------------------------

# WHALE SHARKS
whalesharkres <- resid(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR)
plot(fitted(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR), whalesharkres )
abline(0,0)
plot(GLOBAL_WHALESHARKS_ZINB_NO_LUNAR)
qqnorm(whalesharkres )
qqline(whalesharkres) 


# MOBULA RAYS 
mobulares <- resid(GLOBAL_MOBULAS_ZINB_NO_SALINITY)
plot(fitted(GLOBAL_MOBULAS_ZINB_NO_LUNAR), mobulares  )
abline(0,0)
qqnorm(mobulares )
qqline(mobulares ) 

# MANTA RAYS 
mantares <- resid(GLOBAL_MANTAS_ZINB_NO_LUNAR)
plot(fitted(GLOBAL_MANTAS_ZINB_NO_LUNAR), mantares  )
abline(0,0)
qqnorm(mantares )
qqline(mantares ) 


