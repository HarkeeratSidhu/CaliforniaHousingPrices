#Clustering and price modeling for California Housing Dataset

#Nima Ghadessi 
#Harkeerat Sidhu



################################################################################

#loading packages used for the Code

#function below installs the indcated packages anhd loads them into your 
#enviroment

ins_packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only = TRUE)){
    install.packages(pkgs = x,repos = "http://cran.r-project.org")
    require(x, character.only = TRUE)
  }
}

ins_packages(ggplot2);
ins_packages(clustMixType) ;
ins_packages(mice)
ins_packages(klaR)
ins_packages(factoextra)
ins_packages(ggfortify)
ins_packages(FactoMineR)
ins_packages(GGally)
ins_packages(ggiraphExtra)

library(klaR)
library(clustMixType)
library(nnet)
library(feather)
library(reshape2)
library(ggplot2)
library(reshape2)
library(mice)
library(randomForest)
library(factoextra)
library(ggfortify)

################################################################################

#loading in the dataset

getwd()
setwd("C:/Users/Nima/Desktop/Math151")
data = read.csv("housing.csv")
housing = as.data.frame(data)

#View(housing)

head(housing)


#random seeed
set.seed(10)

################################################################################

#Visualizing the datasets


summary(housing)
# there are many missing values in the total_bedrooms columns
#the max in the columns median_income and median_house_value represent values that
#can be higher than that amount


# creating graphs to visualize the uncleaned data

par(mfrow=c(2,5))
ggplot(data = melt(housing), mapping = aes(x = value)) +
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')
par(mfrow=c(1,1))


# we can see that there are many outliers in the dataset based off the bar graphs

################################################################################

pr.out = prcomp(housing2[,-10], scale = T)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)
###
pve <- pr.var / sum(pr.var)
pve
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")
###




################################################################################

#conducting the Data Cleaning on the data set

#checking to see if the 10th column is a factor

is.factor(housing[,10]) # it is not a factor

housing[,10] = as.factor(housing[,10])


# removing all median housing values of 500001 because they account for a value
# that can be more than that


outlier_housing_value = which(housing$median_house_value == 500001)
housing_clean = housing[-outlier_housing_value,]
sum(housing_clean$median_house_value == 500001) # we now have successfully removed the housing values

#View(housing_clean)

#Viewing the cleaned dataset without the outliers for median housing value

par(mfrow=c(2,5))
ggplot(data = melt(housing_clean), mapping = aes(x = value)) +
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')
par(mfrow=c(1,1))



#importiant to note that high house values are attributed to high income as well
#However, the entries that are associated with high housing values have a wide range
# of incomes levels.

outlier_dataframe = housing[outlier_housing_value,]

median_age = range(outlier_dataframe$housing_median_age)
total_room = range(outlier_dataframe$total_rooms)
total_bedrooms = range(na.exclude(outlier_dataframe$total_bedrooms))
population = range((outlier_dataframe$population))
households = range(outlier_dataframe$households)
median_income = range(outlier_dataframe$median_income)


par(mfrow=c(2,5))
ggplot(data = melt(outlier_dataframe), mapping = aes(x = value)) +
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')
par(mfrow=c(1,1))

table1 = cbind(median_age,total_room, total_bedrooms, population, households, median_income)


table1 # shows us the range of each variable in our outlier table
# the range is varied and large. It is almost identical to the
# range of the original dataset
# Possible that the high median_house values include apartment buildings
# as well as expensive homes

#However, since the median income is capped off, we will commit to keeping
# those entries removed from the final celaned dataset


################################################################################

#showcasing mean imputation however, we do not feel confident in this method because it
#can disturb the distribution. logistic regression was considered but the class
#slides tell us that we should avoid it if possible.

housing2.mean.imp <- mice(housing_clean, method = "mean", m = 2, maxit = 10)

data2=complete(housing2.mean.imp,2L)
sum(is.na(data2))


################################################################################

#Sochastic Regression

#we are committed to using Stochastic Regression Imputation because it adds the appropriate
#noise to the predictions and reflects the uncertainty. However, it is symmetric and constant
#error restrictive

housing2 = mice(housing_clean, method = "norm.nob",
                m = 3, maxit = 10, seed = 1)
housing2 = complete(housing2)
sum(is.na(housing2))
#View(housing2)



#Visualizing the imputed dataset with histograms and boxplots

par(mfrow=c(2,5))
ggplot(data = melt(housing2), mapping = aes(x = value)) +
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')
par(mfrow=c(1,1))

par(mfrow=c(2,5))
ggplot(data = melt(housing2), mapping = aes(x = value)) +
  geom_boxplot() + facet_wrap(~variable, scales = 'free_x')
par(mfrow=c(1,1))

#we can see that the boxplots show us many outliers in the dataset. However,
# these outliers account for most of the dataset. Therefore, we will keep them

#the final cleaned data set is one in which the median house values of over
#5000001 are removed.



################################################################################

#CLUSTER ANALYSIS

#CAUTION CODE TAKES FOREVER TO RUN


#Choosing the appropriate number of clusters

wss <- sapply(1:5,
              function(k){kproto(housing2, k)$tot.withinss})

plot(1:5, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

{
  
  # https://stats.stackexchange.com/questions/293877/optimal-number-of-clusters-using-k-prototypes-method-in-r
} # reference used for the above code

#using the elbow method, we decide to set K=2

#using s.vals to double check the optimal k
Essil <- numeric(5)
for(i in 2:6){
  kpres <- kproto(housing2, k = i,na.rm=FALSE )
  val_sil<-validation_kproto(method = "silhouette", object=kpres)
  Essil[i] <- val_sil
}
plot(1:6, Essil, type = "b", ylab = "Silhouette", xlab = "Number of clusters")
Essil

{
  #source for this code:
  #https://stackoverflow.com/questions/66931997/silhouette-value-of-each-cluster-using-clustmixtype
} #source for this code

#We will use K-prototype since we have mixted type dataset

kproto = kproto(housing2, 2)


#END CAUTION

table(kproto$cluster,housing2[,10])
summary(kproto)

clprofiles(kproto, housing2)

fit_df <- factor(kproto$cluster, order =  TRUE,
                 levels = c(1:2))
fit <- data.frame(housing2, fit_df)
result_df <- kproto$centers
kproto.size <- kproto$size
result <- data.frame(kproto.size, result_df)
result

table(kproto$cluster, housing2[,10]) # confusion matrix

ggplot(fit, aes(x = median_house_value, y = median_income, color = fit_df)) +
  geom_point() +
  labs(title = "median_house_value ~ median_income",
       x = "median_house_value", y = "median_income")
+ guides(color = guide_legend(title = "Cluster"))

{
  #https://rpubs.com/areyhan02/c3segm
} # source for above code

################################################################################

#fitting a linear model to predict housing prices

#logistic regression to fit


trainl=sample(nrow(housing2[,-c(1,2,10)]),round(nrow(housing2))*0.7)
train=housing2[trainl,]
test=housing2[-trainl,]
testL=housing2[-trainl,]$ocean_proximity


Logistic <- multinom( ocean_proximity~., data =train )

summary(Logistic)

#predictions
LogRes=predict(Logistic,test)
table1 = table(LogRes, testL)
table1

# % correct predictions for:
table1[1,1]/sum(table1[1,]) #  <1H OCEAN
table1[2,2]/sum(table1[2,]) # INLAND   
table1[3,3]/sum(table1[3,]) # ISLAND   
table1[4,4]/sum(table1[4,]) # NEAR BAY  
table1[5,5]/sum(table1[5,]) # NEAR OCEAN  




mean(LogRes == testL) #overall accuracy

#coeff of the model

exp(coef(Logistic))

################################################################################
#linear model to calculate house value based on all other variables


trainl=sample(nrow(housing2),round(nrow(housing2))*0.7)
train=housing2[trainl,]
test=housing2[-trainl,]

price.lm<-lm( median_house_value~., data =train, family=binomial)
summary(price.lm)

qqnorm(price.lm$res)
qqline(price.lm$res)
#not the best model for our predictions


#attempting to create a log transform to our predictor. This Transformation gives us
# a better model but the normality assumptions and the variance assumptions ar still being violated

price.lm<-lm( log(median_house_value) ~ . , data =train)
summary(price.lm)

plot(price.lm$res)
qqnorm(price.lm$res)
qqline(price.lm$res)

#linear model on the first cluster 

cluster1 = housing2[kproto$cluster == 1,]

trainl=sample(nrow(cluster1),round(nrow(cluster1))*0.7)
train=cluster1[trainl,]
test=cluster1[-trainl,]

price.lm<-lm( median_house_value~^.5., data =train, family=binomial)
summary(price.lm)

#linear model on second cluster
cluster2 = housing2[kproto$cluster == 2,]
trainl=sample(nrow(cluster1),round(nrow(cluster1))*0.7)
train=cluster1[trainl,]
test=cluster1[-trainl,]

price.lm<-lm( median_house_value~ ., data =train, family=binomial)
summary(price.lm)
################################################################################

#decision tree
#housing price as the dependent variable



sample = sample.int(n = nrow(housing2), size = floor(.8*nrow(housing2)), replace = F)
train = housing2[sample, ] #just the samples
test  = housing2[-sample, ] #everything but the samples

train_y = train[,'median_house_value']
train_x = train[, names(train) !='median_house_value']

test_y = test[,'median_house_value']
test_x = test[, names(test) !='median_house_value']


#Caution the random forrest model takes a very long time to run

rf = randomForest(train_x, train_y, ntree = 750, importance = T)

y_pred = predict(rf , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rmse = sqrt(test_mse)
test_rmse
