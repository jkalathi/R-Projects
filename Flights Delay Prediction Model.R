##########################################################################################
####Departure Delay Prediction of Flights
##########################################################################################

#setwd("C:/Users/Admin/Desktop/MSBAIM/Fall Mod 1/UR4A/Project/Files")

library(data.table)  
library(caret)
library(ggplot2)


file_names <- dir()
flights <- do.call(rbind,lapply(file_names,read.csv))

#dim(flights)

#Selecting random 150,000 rows at random
flights<- flights[sample(nrow(flights), 150000), ]
write.csv(flights,file="flights.csv")

##########################################################################################
#Loading the data file
flights <- read.csv("flights.csv")

###########################################################################################
#Exploratory Data Analysis and Data Cleaning
###########################################################################################

#Removing unnessary and duplicate columns
flights$FlightDate <- NULL
flights$FlightNum <- NULL
flights$OriginCityZip <- NULL
flights$DepTimeBlk <- NULL
flights$LATITUDE <- NULL
flights$LONGITUDE <- NULL

#corecing to the right column data type


flights$Quarter <- as.numeric(flights$Quarter)
flights$Month <- as.numeric(flights$Month)
flights$DayofMonth <- as.numeric(flights$DayofMonth)
flights$DayofWeek <- as.factor(flights$DayOfWeek)
flights$Carrier <- as.factor(flights$Carrier)
flights$Origin <- as.factor(flights$Origin)
flights$AirportTimeQuality <-as.factor(flights$AirportTimeQuality)
flights$AirportTrafficIndex <- as.numeric(flights$AirportTrafficIndex)
flights$AirportTraffic <- as.numeric(flights$AirportTraffic)
flights$OriginCityName <- as.factor(flights$OriginCityName)
flights$OriginCityPopulation <- as.numeric(flights$OriginCityPopulation)
flights$OriginStateName <- as.factor(flights$OriginStateName)
flights$OriginStatePopulation <- as.numeric(flights$OriginStatePopulation)
flights$DestCityName <- as.factor(flights$DestCityName)
flights$DestStateName <- as.factor(flights$DestStateName)
flights$DepTime <- as.numeric(flights$DepTime)
flights$DepHour <- as.numeric(flights$DepHour)
flights$DepMinute <- as.numeric(flights$DepMinute)
flights$DepDelayMinutes <- as.numeric(flights$DepDelayMinutes)
flights$DepDel15 <- as.factor(flights$DepDel15)
flights$ArrDelay <- as.numeric(flights$ArrDelay)
flights$AirTime <- as.numeric(flights$AirTime)
flights$Distance <- as.numeric(flights$Distance)
flights$DistanceGroup <- as.factor(flights$DistanceGroup)

##################################################################################
##Exploratory Data Analysis and Data Cleaning
#################################################################################

#dealing with NA values
# % of rowing having missing values
dim(flights[!complete.cases(flights),])[[1]]/nrow(flights)*100
colSums(is.na(flights))
#droping NA rows
flights <- flights[!is.na(flights$ArrDelay),]

# Make target variable first column in dataset
flights <- flights[,c(21,1:20,22:26)]
# Make target column name "y"
names(flights)[1] <- "y"

flightsy <- flights$y

###################################################################################
##Feature Engineering Incorporated and EDA
####################################################################################

#Creating a new numeric variable to calculate holidays

holidays <- c('2017-01-02','2017-01-16','2017-02-20','2017-04-17','2017-05-14',
              '2017-05-29','2017-06-18','2017-07-04','2017-09-04','2017-10-09',
              '2017-11-11','2017-11-13','2017-11-23','2017-11-24','2017-12-25')

holidayDates <- as.Date(holidays)

#Function to calculate minimun days to holiday

DaysToHoliday <- function(Date){
  numDays <- as.numeric(min(abs(Date-holidayDates)))
  return(numDays)
}

#Adding new column called date of flight
flights$Date <- as.Date(paste(flights$Year,flights$Month,flights$DayofMonth,sep='-'))

#applying function
flights$Holidays <- mapply(DaysToHoliday,flights$Date)
######################################################################################

#Multiplot function to plot various figures on same grid
########################################################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###################################################################################
##Scatter Plots

p1 <- ggplot(flights,aes(x=DepDelayMinutes,y=Distance)) + geom_point() 
p2 <- ggplot(flights,aes(x=DepDelayMinutes,y=AirTime)) + geom_point()
p3 <- ggplot(flights,aes(x=DepDelayMinutes,y=ArrDelay)) + geom_point()
multiplot(p1, p2, p3, cols=2)

#drop distance above 3500
#drop airtime above 500
#drop airdelay above 150 
flights <- subset(flights,Distance<3500)
#flights < - subset(flights,AirTime<500) 
flights <- subset(flights,ArrDelay<150)
flights <- subset(flights,DepDelayMinutes<90)


##Scatter Plots
ggplot(flights,aes(x=DepDelayMinutes,y=Distance)) + geom_point(aes(color=DayofWeek)) +
  facet_wrap(~DayofWeek) + geom_smooth() + ggtitle("Scatter Plots of Departure Delay by Distance by Carrier by WeekDay")

ggplot(flights,aes(x=DepDelayMinutes,y=AirTime)) + geom_point(aes(color=Carrier)) +
  facet_wrap(~Carrier) + geom_smooth() + ggtitle("Scatter Plots of Departure Delay by AirTime by Carrier")

ggplot(flights,aes(x=DepTime,y=DepDelayMinutes)) + geom_point(alpha=0.3)+
ggtitle("Scatter Plots of Departure Delay by Departure Time")

##########################################################################################
##Convert Feature Function
convertFeature <- function (colname, input) {
  colname <- as.character(colname)
  for (s in input) {
    s <- strsplit(s,":")[[1]]
    fac <- s[1]
    num <- s[2]
    colname <- replace(colname, colname == fac , num)
  }
  colname
}

########################################################################################
##EDA for coverting categorical to numerical

###########################################
#scatterplot by carrier
#######################
ggplot(flights,aes(x=DepDelayMinutes,y=ArrDelay)) + geom_point(aes(color=Carrier)) +
 facet_wrap(~Carrier) + geom_smooth()

barplot(table(flights$Carrier), main = "barplot")

#if a carrier is having more flights, it means its more popular than the others
#Popularity Rating5: WN,DL,AA;;;PR4: OO,UA; PR3: B6,EV, PR2: AS,NK ;; PR1: F9,HA,VX
flights$CarrierPopularity <- as.numeric(convertFeature(flights$Carrier,c('WN:5','DL:5','AA:5',
                                                                         'OO:4','UA:4','B6:3','EV:3','AS:2','NK:2','F9:1','HA:1','VX:1')))

table(flights$Carrier)

#####################################
#Day of Week
############

#Box Plots
ggplot(flights,aes(x='',y=DepDelayMinutes)) +geom_boxplot(aes(color=DayOfWeek)) + facet_wrap(~DayOfWeek)+ 
  coord_cartesian(ylim=c(0,10)) +ggtitle("Box Plot of DelayTime by DayOfWeek")

#friday =1, monday and thursday,sunday = 3, saturday,tuesday,wednesday = 5
flights$DayOfWeekBin <- as.numeric(convertFeature(flights$DayOfWeek,c('Friday:1','Monday:3','Thursday:3',
                                                                      'Sunday:3','Saturday:5','Tuesday:5','Wednesday:5')))

############################
#month of year
###############

ggplot(flights,aes(x='',y=DepDelayMinutes)) +geom_boxplot(aes(color=Month)) + facet_wrap(~Month)+ 
  coord_cartesian(ylim=c(0,10)) + ggtitle("BoxPlot of DepDelay by Month")
## 1,6,7,12: 1; 3,4,5,8:3 ;2,9,10,11: 5
barplot(table(flights$Month), main = "barplot")

flights$MonthBin <- as.numeric(convertFeature(flights$Month,c('1:1','6:1','8:1','12:1','3:2','4:2','5:2',
                                                              '7:1','2:3','9:3','10:3','11:3')))

#########################################################################
#creating new features based on population
##########################################

summary(flights$OriginCityPopulation)
#20000> -> Town
#20000 to 100000  -> large town
#100000 to 300000 ->city
#300000 to 1000000  -> large city
#1000000< -->metropolis
flights$CityBin <- ifelse(flights$OriginCityPopulation<20000,'Town',ifelse(flights$OriginCityPopulation < 100000 &
                         flights$OriginCityPopulation >=20000,'Large Town',ifelse(flights$OriginCityPopulation<300000 &
                 flights$OriginCityPopulation >=100000,'City',ifelse(flights$OriginCityPopulation<1000000 &
                flights$OriginCityPopulation >=300000,'Large City',ifelse(flights$OriginCityPopulation>1000000,'Metropolis',NA )))))

any(is.na(flights$CityBin))


##########################################
#making the right factors
flights$CityBin <- as.factor(flights$CityBin)
table(flights$CityBin)

###############################
#Removing unwanted columns
flights$DayofWeek <-NULL
flights$DepDelayMinutes <-NULL
flights$Date <- NULL
flights$Year <- NULL

#dataset for classification
f1 <- flights



rm(holidayDates,holidays,convertFeature,DaysToHoliday,multiplot)   #cleanup

####################################################################################################
################################################################################
## Creating Dummy Variables
################################################################################
# Here we want to take a 'factor variable' or 'categorical variable' and create
# columns for each factor level.
#install.packages("caret")

library(caret)
dummies <- dummyVars(y ~ ., data = f1)            # create dummyes for Xs
ex <- data.frame(predict(dummies, newdata = f1))  # actually creates the dummies
names(ex) <- gsub("\\.", "", names(ex))         # removes dots from col names
f1 <- cbind(f1$y, ex)                              # combine your target variable with Xs
names(f1)[1] <- "y"                               # make target variable called 'y'

rm(dummies, ex) #clean up

################################################################################
# Remove Zero- and Near Zero-Variance Predictors
################################################################################

dim(f1) # dimension of dataset
nzv <- nearZeroVar(f1[,2:ncol(f1)], uniqueCut=10) # identify columns that are "near zero"
f1_filtered <- f1[,2:ncol(f1)][, -nzv]            # remove those columns from your dataset
dim(f1_filtered)  


f1 <- cbind(f1$y, f1_filtered)   # combine y with the Xs
names(f1)[1] <- "y"            # fix the y variable name

rm(f1_filtered, nzv)           # clean up 


################################################################################
# Identify Correlated Predictors and remove them
################################################################################

nums <- which(sapply(f1,is.numeric))
f1_nums <-f1[,c(nums)] 



# calculate correlation matrix using Pearson's correlation formula
descrCor <-  cor(f1_nums[,1:ncol(f1_nums)])                           # correlation matrix
descrCor[is.na(descrCor)] <- 0
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .85) # number of Xs having a corr > some value
summary(descrCor[upper.tri(descrCor)])

# remove all rows with non-finite values
descrCor[!rowSums(!is.finite(descrCor)),]
# replace all non-finite values with 0
descrCor[!is.finite(descrCor)] <- 0

# which columns in your correlation matrix have a correlation greater than some
# specified absolute cutoff?
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.85)
filteredDescr <- f1_nums[,1:ncol(f1_nums)][,-highlyCorDescr] # remove those specific columns from your dataset
descrCor2 <- cor(filteredDescr) 

colSums(is.na(filteredDescr))

# summarize those correlations to see if all features are now within our range
summary(descrCor2[upper.tri(descrCor2)])

# update our d dataset by removing those filtered variables that were highly correlated
f1_nums <- cbind(f1$y, filteredDescr)
names(f1_nums)[1] <- "y"


##########################################################################################
################################################################################
# Identifying linear dependencies and remove them
################################################################################
library(caret)


# first save response
y <- f1_nums$y

# create a column of 1s. This will help identify all the right linear combos
f1_nums <- cbind(rep(1, nrow(f1_nums)), f1_nums[2:ncol(f1_nums)])
names(f1_nums)[1] <- "ones"




#######################################################################

# identify the columns that are linear combos
comboInfo <- findLinearCombos(f1_nums)
comboInfo

# remove columns identified that led to linear combos
f1_nums <- f1_nums[,-comboInfo$remove]

# remove the "ones" column in the first column
f1_nums <- f1_nums[, c(2:ncol(f1_nums))]

# Add the target variable back to our data.frame
f1_nums <- cbind(y, f1_nums)
#########################################################
f1_nums <- f1

rm(comboInfo,descrCor,descrCor2,f1_nums,filteredDescr,highCorr,highlyCorDescr,nums,y)
################################################################################
# Standardize (and/ normalize) your input features.
################################################################################

f1_final <- f1
any(is.na(f1_final))

#######################################################################


numcols <- apply(X=f1, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(f1)
catcols <- apply(X=f1, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(f1)
f1Nums <- f1[,numcols]
f1Cats <- f1[,catcols]

# Step 1) figures out the means, standard deviations, other parameters, etc. to 
# transform each variable
preProcValues <- preProcess(f1Nums[,1:ncol(f1Nums)], method = c("center","scale"))

# Step 2) the predict() function actually does the transformation using the 
# parameters identified in the previous step. Weird that it uses predict() to do 
# this, but it does!
f1Nums <- predict(preProcValues, f1Nums)

# combine the standardized numeric features with the dummy vars
f1_final <- cbind(f1Cats, f1Nums)
f1 <- f1_final

rm(f1Nums,f1Cats,preProcValues,catcols,numcols)  #cleanup


#################################################################
#Data Partition
##################################################################
# 60% train/ 40% test split. You might modify this.
inTrain <- createDataPartition(y = f1_final$y,   # outcome variable
                               p = .60,   # % of training data you want
                               list = F)

# create your partitions
train <- f1_final[inTrain,]  # training data set
test <- f1_final[-inTrain,]  # test data set

# save my response for later
trainy <- train$y
testy <- test$y

# remove response variable from train and test sets for now
train$y <- NULL
test$y <- NULL

rm(inTrain,f1_final,flightsy)   #clean up

#############################################################################
##Clustering
#############################################################################
#dividing numeric and categorical features in train
numcols <- apply(X=train, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(train)
catcols <- apply(X=train, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(train)
train_Nums <- train[,numcols]
train_Cats <- train[,catcols]



###############################################################################

#dividing numeric and categorical features in test
numcols <- apply(X=test, MARGIN=2, function(c) sum(c==0 | c==1)) != nrow(test)
catcols <- apply(X=test, MARGIN=2, function(c) sum(c==0 | c==1)) == nrow(test)
test_Nums <- test[,numcols]
test_Cats <- test[,catcols]

rm(numcols,catcols)

##################################
##kmeans for 1 to 20 clusters
##################################
#flights_df <- data.frame()
#for (k in 1:20){
#  kmeanstrain_Nums <- kmeans(train_nums,centers = k, nstart=20,iter.max = 50)
#  kmeanstest_Nums <- kmeans(test_nums,centers = k, nstart=20,iter.max = 50)
# 
#  flights_df <- rbind(flights_df,cbind(k,kmeanstrain_Nums$tot.withinss,kmeanstest_Nums$tot.withinss))
#}


#Elbow Plot to check optimum clusters
#par(mfrow=c(1,1))
#flights_df[,2:3] <-flights_df[,2:3]/1000
#plot(x=flights_df$k,y=flights_df$V2,main='k-means Elbow Plot',col="blue",pch=19,
#     type='b',cex.lab=1.2,xlab="Number of clusters",ylab="MSE(in 1000s)")
#points(x=flights_df$k,y=flights_df$V3,col='green')

#################################################
#hartigan rule
################################################
#library(useful)

#househart <- FitKMeans(train_nums, max.clusters=25, nstart=20, seed=1234)
#househart
###################################################
#Using Gap Stastics
#library(cluster)
#theGap <- clusGap(train_nums, FUN=kmeans, K.max=20)
#gapDF <- as.data.frame(theGap$Tab)
#gapDF

#########################################################################
#k for 2,3,4
#k=18
#km18 <- kmeans(train_nums, 18)
#dist18 <- dist(train_nums, method="euclidean")
#sil18 <- silhouette(km18$cluster, dist18)
#plot(sil18, col=c("black","red","green"), main="Silhouette plot (k=3) K-means",
#     border = NA)
##average length is 0.15

#k=19
#km19 <- kmeans(train_nums, 19)
#dist19 <- dist(train_nums, method="euclidean")
#sil19 <- silhouette(km19$cluster, dist19)
#plot(sil19, col=c("black","red","green"), main="Silhouette plot (k=3) K-means",
#     border = NA)
##average length is 0.12
#library(cluster)
#Clustering for k=4
#km4 <- kmeans(train, 20)
#dist2 <- dist(train, method="euclidean")
#sil2 <- silhouette(km4$cluster, dist2)
#plot(sil2, col=c("black","red","green"), main="Silhouette plot (k=4) K-means",border = NA)
#average length is 0.1
#rm()
########################################################################################
#k=4 is the optimum value
########################################################################################
#train with 4 clusters

#clustering with k=4
train_cluster <- kmeans(train_Nums,centers=4)

#adding the column to train
train_cluster_final <- cbind(train_Nums,trainy)
names(train_cluster_final)[18] <- "trainy"
train_cluster_final <- cbind(train_cluster_final,train_cluster$cluster)
names(train_cluster_final)[19] <- "Cluster"
train_cluster_final$Cluster <- as.factor(train_cluster_final$Cluster)
#dummies
dummies <- dummyVars("trainy~.", data = train_cluster_final)            # create dummyes for Xs
train_cluster_final <- data.frame(predict(dummies, newdata = train_cluster_final))

#remove one column from train set
train_cluster_final$Cluster.1 <-NULL
##################################################
#converting clusters to factors
##################################################
train_cluster_final$Cluster.2 <- as.factor(train_cluster_final$Cluster.2)
train_cluster_final$Cluster.3 <- as.factor(train_cluster_final$Cluster.3)
train_cluster_final$Cluster.4 <- as.factor(train_cluster_final$Cluster.4)

train <- cbind(trainy,train_cluster_final,train_Cats)

rm(train_Cats,train_cluster,train_cluster_final,train_Nums,nums,dummies)  #clean up

#test
#clustering with k=4
test_cluster <- kmeans(test_Nums,centers=4)
#adding the column to test
test_cluster_final <- cbind(test_Nums,testy)
names(test_cluster_final)[18] <- "testy"
test_cluster_final <- cbind(test_cluster_final,test_cluster$cluster)
names(test_cluster_final)[19] <- "Cluster"
test_cluster_final$Cluster <- as.factor(test_cluster_final$Cluster)
#dummies
dummies <- dummyVars("testy~.", data = test_cluster_final)            # create dummyes for Xs
test_cluster_final <- data.frame(predict(dummies, newdata = test_cluster_final))

#remove one column from train set
test_cluster_final$Cluster.1 <-NULL
#converting clusters to factors
##################################################
test_cluster_final$Cluster.2 <- as.factor(test_cluster_final$Cluster.2)
test_cluster_final$Cluster.3 <- as.factor(test_cluster_final$Cluster.3)
test_cluster_final$Cluster.4 <- as.factor(test_cluster_final$Cluster.4)

test <- cbind(testy,test_cluster_final,test_Cats)

rm(test_Cats,test_cluster,test_cluster_final,test_Nums,dummies)  #clean up


#train_final_sampled$DayofMonth <- NULL

#test_final_sampled <- sample_n(test,12000)

#test_final_sampled$DayofMonth <- NULL

#str(train_final_sampled)


################################################################################
##Specifying Cross Validation
################################################################################


str(train)

library (caret)

ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=3,        # k number of times to do k-fold
                     classProbs = F,  # if you want probabilities
                     #summaryFunction = twoClassSummary, # for classification
                     summaryFunction = defaultSummary,  # for regression
                     allowParallel=T)

## Training a Linear Model
train$ArrDelay <- NULL
test$ArrDelay <- NULL

logisticmodel <- train(trainy ~ .,
                       data = train,
                       method = "glm",
                       trControl = ctrl,
                       #preProcess=c("center","scale"), # not needed; already transformed
                       tuneLength = 15,                # this specifies various s values
                       metric = "Accuracy")



summary(logisticmodel)


# Training a neural net model without tuning


NNModel <- train(trainy~ .,               # model specification
                 data = train,        # train set used to build model
                 method = "nnet",     # type of model you want to build
                 trControl = ctrl,    # how you want to learn
                      
                 tuneLength = 1:5,
                 maxit = 100,         # max # of iterations
                 metric = "Accuracy"       # performance measure
)

# Training a neural net model with tuning

my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
NNModelwithTuning <- train(trainy~ .,               # model specification
                              data = train,        # train set used to build model
                              method = "nnet",     # type of model you want to build
                              trControl = ctrl,    # how you want to learn
                              tuneGrid = my.grid,
                              maxit = 100,         # max # of iterations
                              metric = "Accuracy"       # performance measure
)

NNModelwithTuning
## train a C5.0 classification tree

classtree <- train(trainy~ .,               # model specification
                   data = train,        # train set used to build model
                   method = "C5.0Tree",    # type of model you want to build
                   trControl = ctrl,    # how you want to learn
                   tuneLength = 1:15,   # vary tree size
                   metric = "Accuracy"       # performance measure
)
classtree

## train a Naive Bayes classification tree

Naive_bayes <- train(trainy~ .,               # model specification
                   data = train,        # train set used to build model
                   method = "naive_bayes",    # type of model you want to build
                   trControl = ctrl,    # how you want to learn
                   tuneLength = 1:15,   # vary tree size
                   metric = "Accuracy"       # performance measure
)
summary(Naive_bayes)




# generate class predictions on train set

Logit_Train_prediction <- predict(logisticmodel, train)
NeuralNet_Train_prediction <- predict(NNModel, train)
NeuralNetwithTuning_Train_prediction <- predict(NNModelwithTuning, train)
ClassTree_Train_prediction <- predict(classtree, train)
NaiveBayes_Train_prediction <- predict(Naive_bayes, train)

logisticmodel$

Logit_Train_prediction
# generate probability predictions on train set

Logit_Train_prediction_probability <- predict(logisticmodel, train, type='prob')[,1]
NeuralNet_Train_prediction_probability  <- predict(NNModel, train, type='prob')[,1]
ClassTree_Train_prediction_probability  <- predict(classtree, train, type='prob')[,1]


###### generate class predictions on test set

Logit_Test_prediction  <- predict(logisticmodel, test)
NeuralNet_Test_prediction  <- predict(NNModel, test)
NeuralNetwithTuning_Test_prediction <- predict(NNModelwithTuning, test)
ClassTree_Test_prediction  <- predict(classtree, test)
NaiveBayes_Test_prediction <- predict(Naive_bayes, test)

##### generate probability predictions on test set

Logit_Test_prediction_probability   <- predict(logisticmodel, test, type='prob')[,1]
NeuralNet_Test_prediction_probability   <- predict(NNModel, test, type='prob')[,1]
ClassTree_Test_prediction_probability   <- predict(classtree, test, type='prob')[,1]


# calculate performance
tr_results <- rbind(
  postResample(pred = Logit_Train_prediction, obs = trainy),
  postResample(pred = NeuralNet_Train_prediction, obs = trainy),
  postResample(pred = NeuralNetwithTuning_Train_prediction, obs = trainy),
  postResample(pred = ClassTree_Train_prediction, obs = trainy),
  postResample(pred = NaiveBayes_Train_prediction, obs = trainy
               )
)



te_results <- rbind(
  postResample(pred = Logit_Test_prediction, obs = testy),
  postResample(pred = NeuralNet_Test_prediction, obs = testy),
  postResample(pred = NeuralNetwithTuning_Test_prediction, obs = testy),
  postResample(pred = ClassTree_Test_prediction, obs = testy),
  postResample(pred = NaiveBayes_Test_prediction, obs = testy)
  
)


###########################################################################################

#logistic regression
conf_matrix_log<-table(Logit_Test_prediction,testy)
sensitivity(conf_matrix_log)
specificity(conf_matrix_log)

#neural networks
conf_matrix_nnet<-table(NeuralNetwithTuning_Test_prediction,testy)
sensitivity(conf_matrix_nnet)
specificity(conf_matrix_nnet)

###########################


