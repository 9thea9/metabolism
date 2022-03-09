##group projects
##djabarov, schwingshackl, weninger
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
library(car)

#import the dataset 
metabolism<-read_excel("Data_Metabolism.xlsx", sheet="data")%>%
  select(1,2,3, 8:27)


#To get a first impression of the dataset
#if mean and median differ a lot from each other - no ND
summary(metabolism)
###??? Do we need to calculate standard deviation etc.?? 
#maybe not because we're not comparing groups

####just playing around a bit with different variables 
ggplot(data=metabolism)+
  geom_jitter(aes(x=light, y=Con, col=Agr))
#is there any correlation between variables? 
cor.test(metabolism$Agr, metabolism$DIN)

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
metabolism<-within(metabolism, {
  log_light=log(light)
  log_ER=log(ER*-1) #befor log(), all values need to be positive
  log_SRP=log(SRP)
  log_DOC=log(DOC)
})

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
plot(met_sub)#linear relatinship btw. DIN~Agr, light and Agr don't seem to be correlated 

#trying to build a linear model
modfull<-lm(DIN~., data=met_sub)
#find the best model
best_mod<-step(modfull, direction = "both")

vif(modfull)
#very low vif- good-->should be lower than 10!!
#drop factors with vif higher than 10 

#test this full model
summary(modfull)
##
qqnorm(scale(residuals(mod1)))
qqline(scale(residuals(mod1)), lty=2)

#AIC - how probable are the models 
AIC(modfull)

#when model with significant interactions --> post hoc test: TukeyHSD()

#to plot a linear model in a nice way
ggplot(hydrop, aes(x = width, y = weight)) + geom_point() + 
  geom_smooth(method="glm")

###
###
###
#find and plot clusters in the dataset 
dmat = vegdist(met_sub, method = "bray") # compute a dissimilarity matrix
clu1<-hclust(dmat, method="single")
clu2<-hclust(dmat, method="ward.D")
plot(clu1)#not very useful without names 
#creating groups - just for the names 
metabolism<-mutate(metabolism, agriculture = ifelse(Agr <0.5,"low","high"))
metabolism<-mutate(metabolism, lightness=ifelse(light<12000000, "dark", "bright"))
metabolism<-mutate(metabolism, production=ifelse(GPP<1.2, "lame", "productive"))
combifac<-paste(metabolism$site, metabolism$agriculture, metabolism$lightness, metabolism$production, sep="_")#names of the factors of the ORIGINAL DATASET           

plot(clu1, hang=-1, label=combifac, ylab="BC")

cophenetic(clu1)#linkage distance in dendrogram, opposite of a cluster analysis 
#cophenetic tells the distance between points, computed from the tree (cluster Dendrogram)
plot(dmat, cophenetic(clu1))#ideally this resembles the dissimilarity 
cor(dmat,cophenetic(clu1), method="spearman")#correlation
cor(dmat, cophenetic(clu2), method="spearman")#cluster 1 is better than cluster 2




#pca
#maybe a nice resource to understand pca better
#https://rstatisticsblog.com/data-science-in-action/data-preprocessing/complete-guide-to-principal-component-analysis-in-r/
pca.met<-prcomp(met_sub)
biplot(pca.met)#nr 1-33 are the sites. 


#other method to do a pca (like we did in the course)
pcoa = cmdscale(dmat, k = 2, eig=TRUE) # always computes all axes, but will only report scores of k=2
plot(pcoa$points) # score plot

#same plot but using ggplot
ggdata<-data.frame(pcoa$points)
colnames(ggdata)<-c("pcoa1", "pcoa2")
ggdata$agriculture<-metabolism$agriculture
ggdata$lightness<-metabolism$lightness

ggplot(ggdata)+
  geom_point(aes(x=pcoal, y=pcoa2, shape=agriculture, color=lightness))+
  scale_color_discrete(type=c("green", "darkgreen"))



