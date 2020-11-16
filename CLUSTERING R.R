pd = read.csv("D:/DSC/CC GENERAL.csv")
View (pd)

#pre processing
dim(pd)
summary(pd)
str(pd)

#Drop Cloumn with categorical variable
pd = pd[,-1]
dim(pd)

#mean and median imputation
clean1<-pd
mean <- mean(clean1$MINIMUM_PAYMENTS, na.rm = TRUE)
mean
clean1$MINIMUM_PAYMENTS[is.na(clean1$MINIMUM_PAYMENTS)]<-999999999
clean1$MINIMUM_PAYMENTS[clean1$MINIMUM_PAYMENTS==99999999]<-mean
sum(is.na(clean1$MINIMUM_PAYMENTS)) #check missing in MINIMUM_PAYMENTS

clean2<-clean1
median <- median(clean2$CREDIT_LIMIT, na.rm = TRUE)
median
clean2$CREDIT_LIMIT[is.na(clean2$CREDIT_LIMIT)]<-9999999
clean2$CREDIT_LIMIT[clean2$CREDIT_LIMIT==99999999]<-median
sum(is.na(clean2$CREDIT_LIMIT))

#Clustering
library(cluster)
library(corrplot)
library(factoextra)
library(NbClust)

#Normalize
normlz <- function(x){
  (x-min(x))/(max(x)-min(x))
}
df_norm<-apply(clean2,2,normlz)
View(df_norm)

#Standardize
df_scaled<-scale(clean2, center= T, scale = T)
View(df_scaled)

#Silhoutte method

#
kmeans4_scaled<-kmeans(df_scaled, 4, nstart = 25)
kmeans4_scaled
kmeans2_scaled<-kmeans(df_scaled, 2, nstart = 25)
kmeans2_scaled
kmeans7_scaled<-kmeans(df_scaled, 7, nstart = 25)
kmeans7_scaled

kmeans4_scaled<-kmeans(df_norm, 4, nstart = 25 )
kmeans4_scaled

kmeans2_scaled<-kmeans(df_norm, 2, nstart = 25 )
kmeans2_scaled

kmeans7_scaled<-kmeans(df_norm, 7, nstart = 25 )
kmeans7_scaled

kmeans7_scaled$cluster #to print cluster
kmeans7_scaled$cluster #to print to number of each cluster

kmeans4_scaled$cluster #to print cluster
kmeans4_scaled$cluster #to print to number of each cluster


