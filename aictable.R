aictable<-function(X,m){
#
#  This function will create a full model selection table based on AICc.
#  
#  Inputs:
#
#  X is the AIC table produced by R by using AIC(model1,model2, ...)
#  m is the sample size
#
#
rnames<-row.names(X)
AICc<-X$AIC+2*X$df*(X$df+1)/(m-X$df-1)     #small-sample correction
logL<-X$df-X$AIC/2                         #Log-likelihood
tab<-data.frame(X[,1],logL,AICc)           #Remove AIC column; add logL and AICc
colnames(tab)[1]<-c("Params")              #Rename "df" column   
row.names(tab)<-rnames
tab<-tab[order(tab$AICc),]                 #Sort by ascending AICc value
deltaAICc<-tab$AICc-min(tab$AICc)          #Delta AICc
weight<-exp(-deltaAICc/2)/sum(exp(-deltaAICc/2))  #Weights
cumwt<-weight                              #Column for cumulative weight
for(i in 2:dim(X)[1]){                  
cumwt[i]<-cumwt[i-1]+cumwt[i]              #Accumulate weight from the top
}
tab<-data.frame(tab,deltaAICc,weight,cumwt)
tab<-round(tab,4)
tab
}
