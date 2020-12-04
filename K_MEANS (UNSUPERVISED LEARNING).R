pd = read.csv("D:/DSC/CC GENERAL.csv")
View(pd)

###Pre Processing###
dim(pd)
summary(pd) #there are missing values
str(pd) #there are categorical variable

#Drop column with categorical variable
pd = pd[, -1]
dim(pd)

#mean and median imputation
clean1 = pd
mean = mean(clean1$MINIMUM_PAYMENTS, na.rm=TRUE) #MINIMUM_PAYMENTS
mean
clean1$MINIMUM_PAYMENTS[is.na(clean1$MINIMUM_PAYMENTS)] = 99999999
clean1$MINIMUM_PAYMENTS[clean1$MINIMUM_PAYMENTS == 99999999]= mean
sum(is.na(clean1$MINIMUM_PAYMENTS)) #check missing value in MINIMUM_PAYMENTS

clean2 = clean1
median= median(clean2$CREDIT_LIMIT, na.rm = TRUE) #CREDIT_LIMIT
median
clean2$CREDIT_LIMIT[is.na(clean2$CREDIT_LIMIT)] = 99999999
clean2$CREDIT_LIMIT[clean2$CREDIT_LIMIT == 99999999]= mean
sum(is.na(clean2$CREDIT_LIMIT)) #check missing value in CREDIT_LIMIT


#CLUSTERING
library(cluster)
library(corrplot)
library(NbClust)

#NORMALIZE
normlz = function(x){(x-min(x))/(max(x)-min(x))}
df_norm <- apply(clean2, 2, normlz)
View(df_norm)

#STANDARDIZE
df_scaled = scale(clean2, center = T, scale =T)
View(df_scaled)

#ELBOW METHOD

#SILHOUTTE METHOD

kmeans4_scaled = kmeans(df_scaled, 4, nstart =25)
kmeans4_scaled
kmeans2_scaled = kmeans(df_scaled, 2, nstart =25)
kmeans2_scaled
kmeans7_scaled = kmeans(df_scaled, 7, nstart =25)
kmeans7_scaled

kmeans4_scaled = kmeans(df_norm, 4, nstart =25)
kmeans4_scaled
kmeans2_scaled = kmeans(df_norm, 2, nstart =25)
kmeans2_scaled
kmeans7_scaled = kmeans(df_norm, 7, nstart =25) 
kmeans7_scaled

kmeans7_scaled$cluster #to print cluster
kmeans7_scaled$size #to print the number of each cluster

clean2["cluster"] = kmeans7_scaled$cluster


