##group projects
##djabarov, kernbichler, schwingshackl, weninger
##code by TS

#set the right working directory on your PC
setwd("C:/Users/Thea/Desktop/vu_datenanalyse/Group project")

#load the needed packages
library(readxl)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(vegan)
library(ggfortify)

#import the dataset 
metabolism<-read_excel("Data_Metabolism.xlsx", sheet="data")%>%
  select(2,3, 8:27)


##First focus on the main hypothesis (provided by the plot)
#including: Nutriente, landuse, light, DOC and ER
#check normal distribution - still looking for nicer way to do this  
h1<-ggplot(data=metabolism, aes(x=Agr))+geom_histogram(bins=10)#looks good
h2<-ggplot(data=metabolism, aes(x=light))+geom_histogram(bins=10)#transformation
h3<-ggplot(data=metabolism, aes(x=ER))+geom_histogram(bins=10)#transformation
h4<-ggplot(data=metabolism, aes(x=SRP))+geom_histogram(bins=10)#transformation
h5<-ggplot(data=metabolism, aes(x=DIN))+geom_histogram(bins=10)#looks good
h6<-ggplot(data=metabolism, aes(x=DOC))+geom_histogram(bins=12)#transformation

(h1+h2+h3)/(h4+h5+h6)

#log transform data, that is not normally distributed
metabolism$log_light<-log(metabolism$light)
metabolism$log_ER<-log(metabolism$ER *-1) #befor log(), all values need to be positive
metabolism$log_SRP<-log(metabolism$SRP)
metabolism$log_DOC<-log(metabolism$DOC)

#check again
hist(metabolism$log_DOC)
hist(metabolism$log_ER)
hist(metabolism$log_light)#still not so good 
hist(metabolism$log_SRP)

#shapiro wilk test for more clarity
shapiro.test(metabolism$Agr) #p<1
shapiro.test(metabolism$log_light)#p<0.5
shapiro.test(metabolism$log_ER)#p<0.5
shapiro.test(metabolism$log_SRP)#p<0.5
shapiro.test(metabolism$log_DOC)#p<1
shapiro.test(metabolism$DIN)#p<0.05


#subset &plot the dataset 
met_sub<-select(metabolism, c(Agr, log_light, log_ER, log_SRP, log_DOC, DIN))
plot(met_sub)

#trying to build a linear model
mod1<-lm(log_ER~Agr, data=met_sub)
summary(mod1)
anova(mod1)

mod2<-lm(log_ER~Agr+log_light+log_SRP+DIN, data=met_sub)
anova(mod2)

mod3<-lm(log_DOC~Agr+light+log_SRP, data=met_sub)#with + every single factor by itselt, with * also the interactions 
anova(mod3)

#pca
#maybe a nice resource to understand pca better
#https://rstatisticsblog.com/data-science-in-action/data-preprocessing/complete-guide-to-principal-component-analysis-in-r/
pca.met<-prcomp(met_sub)
biplot(pca.met)#nr 1-33 are the sites. 


#other method to do a pca (like we did in the course)
dmat = vegdist(met_sub, method = "bray") # compute a dissimilarity matrix
pcoa = cmdscale(dmat, k = 2, eig=TRUE) # always computes all axes, but will only report scores of k=2
plot(pcoa$points) # score plot


