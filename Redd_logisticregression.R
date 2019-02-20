# Purpose
#The main goal is to preform a logistic regression invesigating redd presence (redd=1) and absence (redd=0). 
# Potential parameters include depth, velocity, csi, wsg, dar, and site (Merced River Ranch (mrr) and Robinson Reach (rr)). 
# The steps include generating a candidate set of models, run AIC, and additional statistics as needed.

####Read in the data####
library(dplyr)
hab <- read.csv("Data/mHabVarSite.csv", sep = ",", header = T) # all data pooled together by site
rr <- hab %>% filter(site == "rr")
mrr <- hab %>% filter(site == "mrr")

####Logistic Regression Model Setup####
#Proposing some basic models to make sure everything is functioning.
# All data pooled together by site
null <- glm(redd ~ 1,data = hab, family = binomial(link = "logit"))
s <- glm(redd ~ site, data = hab, family = binomial(link = "logit")) #Placeholder if we want to include site as a parameter (typically in combination with other variables)
dep <- glm(redd ~ dep, data = hab, family = binomial(link = "logit"))
v <- glm(redd ~ vel, data = hab, family = binomial(link = "logit"))
c <- glm(redd ~ csi, data = hab, family = binomial(link = "logit"))
dar <- glm(redd ~ dar, data = hab, family = binomial(link = "logit"))
w <- glm(redd ~ wsg, data = hab, family = binomial(link = "logit"))
dar.c <- glm(redd ~ dar + csi, data = hab, family = binomial(link = "logit"))
dar.w <- glm(redd ~ dar + wsg, data = hab, family = binomial(link = "logit"))
c.w.inter <- glm(redd ~ csi + wsg + csi*wsg, data = hab, family = binomial(link = "logit")) #interaction

#ADDITIONAL MODELS WITH SITE
depS <- glm(redd ~ dep + site, data = hab, family = binomial(link = "logit"))
vS <- glm(redd ~ vel + site, data = hab, family = binomial(link = "logit"))
cS <- glm(redd ~ csi + site, data = hab, family = binomial(link = "logit"))
darS <- glm(redd ~ dar + site, data = hab, family = binomial(link = "logit"))
wS <- glm(redd ~ wsg + site, data = hab, family = binomial(link = "logit"))
dar.cS <- glm(redd ~ dar + csi + site, data = hab, family = binomial(link = "logit"))
dar.wS <- glm(redd ~ dar + wsg + site, data = hab, family = binomial(link = "logit"))
c.w.interS <- glm(redd ~ csi + wsg + csi*wsg + site, data = hab, family = binomial(link = "logit")) #interaction

raw1 <- AIC(dep, v, c, dar, w, dar.c, dar.w, c.w.inter, null)
hab.output <- aictable(raw1,252)

raw1b <- AIC(s, depS, vS, cS, darS, wS, dar.cS, dar.wS,c.w.interS) #WITH ADDITIONAL SITES
hab.output2 <- aictable(raw1b,252)

#Best model of pooled data
#summary(dep)
#par(mfrow = c(2,2)) 
#plot(dep)
write.csv(hab.output, file = "Data/LROutput_alldata.csv", na = "")
write.csv(hab.output2, file = "Data/LROutput_alldata_WITHSITE.csv", na = "")

# RR
null <- glm(redd ~ 1,data = rr, family = binomial(link = "logit"))
dep <- glm(redd ~ dep, data = rr, family = binomial(link = "logit"))
v <- glm(redd ~ vel, data = rr, family = binomial(link = "logit"))
c <- glm(redd ~ csi, data = rr, family = binomial(link = "logit"))
dar <- glm(redd ~ dar, data = rr, family = binomial(link = "logit"))
w <- glm(redd ~ wsg, data = rr, family = binomial(link = "logit"))
dar.c <- glm(redd ~ dar + csi, data = rr, family = binomial(link = "logit"))
dar.w <- glm(redd ~ dar + wsg, data = rr, family = binomial(link = "logit"))
c.w.inter <- glm(redd ~ csi + wsg + csi*wsg, data = rr, family = binomial(link = "logit")) #interaction
raw2 <- AIC(dep, v, c, dar, w, dar.c, dar.w, c.w.inter, null)
rr.output <- aictable(raw2,142)

#Best model of Robinson data
summary(dar.c)
par(mfrow = c(2,2)) 
plot(dar.c)
write.csv(rr.output, file = "Data/LROutput_RR.csv", na = "")

# MRR
null <- glm(redd ~ 1,data = mrr, family = binomial(link = "logit"))
dep <- glm(redd ~ dep, data = mrr, family = binomial(link = "logit"))
v <- glm(redd ~ vel, data = mrr, family = binomial(link = "logit"))
c <- glm(redd ~ csi, data = mrr, family = binomial(link = "logit"))
dar <- glm(redd ~ dar, data = mrr, family = binomial(link = "logit"))
w <- glm(redd ~ wsg, data = mrr, family = binomial(link = "logit"))
dar.c <- glm(redd ~ dar + csi, data = mrr, family = binomial(link = "logit"))
dar.w <- glm(redd ~ dar + wsg, data = mrr, family = binomial(link = "logit"))
c.w.inter <- glm(redd ~ csi + wsg + csi*wsg, data = mrr, family = binomial(link = "logit")) #interaction
raw3 <- AIC(dep, v, c, dar, w, dar.c, dar.w, c.w.inter, null)
mrr.output <- aictable(raw3,142)

#Best model of Merced River Ranch data
summary(c.w.inter)
par(mfrow = c(2,2)) 
plot(c.w.inter)
write.csv(mrr.output, file = "Data/LROutput_MRR.csv", na = "")

#Model generating using MuMIn library
#  library(MuMIn) #install.packages(MuMIn)
#  options(na.action = na.fail)
#  
#  all <- glm(redd ~ site + dar + vel + csi + wsg + dep, data = hab, family = binomial(link = "logit"))
# allcomb <- dredge(all)

#If model averaging is needed:
#mod <- get.models(allcomb, subset = delta < 2 )# seq(nrow(allcomb)) 
#mod.avg <- model.avg(mod)

#'Best'model
#summary(get.models(allcomb, 1)[[1]])
