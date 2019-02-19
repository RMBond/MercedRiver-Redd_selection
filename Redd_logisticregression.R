# Purpose
#The main goal is to preform a logistic regression invesigating redd presence (redd=1) and absence (redd=0). 
# Potential parameters include depth, velocity, csi, wsg, dar, and site (Merced River Ranch (mrr) and Robinson Reach (rr)). 
# The steps include generating a candidate set of models, run AIC, and additional statistics as needed.

####Read in the data####
library(dplyr)
hab <- read.csv("Data/mHabVarSite.csv", sep=",", header = T)

####Logistic Regression Model Setup####
all <- glm(redd ~ site + dar + vel + csi + wsg + dep, data = hab, family = binomial(link = "logit"))



ary(MuMIn) #install.packages(MuMIn)
options(na.action = na.fail)

all <- glm(redd ~ site + dar + vel + csi + wsg + dep, data = hab, family = binomial(link = "logit"))

#llcomb <- dredge(all)
#allcomb <- dredge(all, evaluate = TRUE, trace = TRUE, fixed = "offset(OS)")
#mod <- get.models(allcomb, subset = delta < 2 )# seq(nrow(allcomb))
#mod.avg <- model.avg(mod)

#'Best'model
#ummary(get.models(allcomb, 1)[[1]])
