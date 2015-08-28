# --------------- File Summary Start -------------- 
#	Title:   		  Neighborhood Desirability Score Calculation
#	Description: 	Calculate NDS by scaling the predicted median home price of the Neighborhood
# Date:    		  June 03, 2015
# Version: 		  1.2
# File:    		  NDS_Calculation_Regression.R
# Author:  		  Jai Dhawan
# Owner:   		  HomeUnion Inc
# Rights:  		  Copyright HomeUnion Inc 2015
# --------------- File Summary End -------------- 

library(car)

# Load Function to calculate 1st quartile
fn1Q<- function(x){return(quantile(x)[[2]])}

# Load Function to calculate 3rd quartile
fn3Q<- function(x){return(quantile(x)[[4]])}

# Load Function to replace NA with Median
NA_to_median<-function(x) {replace(x, is.na(x), median(x, na.rm = T))} 

# Load Function to normalize values b/w 0 and 1
normalized <-  function(x){(x-min(x))/(max(x)-min(x))}

# define scoring scale parameters
minNDS <- 100
maxNDS <- 900
#meanNDS <- 500

#define type of file
type="NeighborhoodsCombined"

file.InputDir <- '/Users/Eden/Documents/Data/'
file.InputFilename <- paste('features_20150803.txt', sep = '')

# Has these column names
# nid|neighborhoodid|neighborhoodname|county|state|source|sorta|medianavmstate|medianavmcounty|medianavm|medianrent|medianincomenbhd|relativeincome|collegegrads|whitecollar|schools|unemployment|employmentdiversity|occupancy|safety|"RN"|"PropForeclosures"|"Nbhd_Size"

file.OutputDir <- '/Users/Eden/Documents/Data/'
file.OutputFilename <- paste(type, 'Score_v35_Intermediate.txt' , sep = '')

# Load Data
NDS.data <- read.table(paste(file.InputDir, file.InputFilename, sep = ''), header = TRUE, 
                       sep = '|', quote = '"')
names(NDS.data)
summary(NDS.data)

# select appropriate columns
NDS.CleanedData <- NDS.data[ , -7]

# remove duplicates
NDS.CleanedData <- unique(NDS.CleanedData)

# define sqrtMedianAVM
NDS.CleanedData$sqrtMedianAVM <- sqrt(NDS.CleanedData$medianavm)
NDS.CleanedData$Residuals <- 0
summary(NDS.CleanedData)
names(NDS.CleanedData)

# Replace NA with Median
NDS.CleanedData[, 8] <- data.frame(apply(as.matrix(NDS.CleanedData[ , 8]), 2, NA_to_median))
NDS.CleanedData[,10:ncol(NDS.CleanedData)] <- data.frame(apply(as.matrix(NDS.CleanedData[ , 10:ncol(NDS.CleanedData)]), 2, NA_to_median))

#only NA data
NDS.CleanedDataOnlyNA <- NDS.CleanedData[rowSums(is.na(NDS.CleanedData)) > 0,]

# remove NA
NDS.CleanedData <- na.omit(NDS.CleanedData)

# remove unrealistic data
NDS.CleanedData[which(NDS.CleanedData$collegegrads > 100 ), ]$collegegrads <- 99

# summary of Cleaned Data
summary(NDS.CleanedData)

#Outliers
NDS.CleanedDataUpper <- NULL
NDS.CleanedDataLower <- NULL
for( i in 7:ncol(NDS.CleanedData)){
  # Define upper and lower limits
  UpperLimit <- fn3Q(NDS.CleanedData[ , i]) + 2*IQR(NDS.CleanedData[ , i])
  LowerLimit <- fn1Q(NDS.CleanedData[ , i]) - 2*IQR(NDS.CleanedData[ , i])
  #   names(NDS.CleanedData[i]) 
  #   UpperLimit
  #   LowerLimit
  if(length(NDS.CleanedDataUpper[i]) == 0){
    NDS.CleanedDataUpper <- NDS.CleanedData[which(NDS.CleanedData[ , i] > UpperLimit), ]
  } else {
    # Upper Outliers Selection
    NDS.CleanedDataUpper <- rbind(NDS.CleanedDataUpper,NDS.CleanedData[which(NDS.CleanedData[ , i] > UpperLimit), ])}
  if(length(NDS.CleanedDataLower[i]) == 0){
    NDS.CleanedDataLower <- NDS.CleanedData[which(NDS.CleanedData[ , i] < LowerLimit), ]
  }  else{
    # Lower Outliers Selection
    NDS.CleanedDataLower <- rbind(NDS.CleanedDataLower,NDS.CleanedData[which(NDS.CleanedData[ , i] < LowerLimit), ])}
  
  # Training data excluding upper and lower outliers
  NDS.CleanedData <- NDS.CleanedData[which(NDS.CleanedData[ , i] <= UpperLimit
                                           & NDS.CleanedData[ , i] >= LowerLimit),]
}
NDS.Clean <- NDS.CleanedData

# build linear regression model
NDS.lmBeginModel <- lm( sqrtMedianAVM ~ medianrent, data = NDS.CleanedData)

NDS.lmEndModel <- lm(sqrtMedianAVM ~ medianrent ###
                     +medianincomenbhd
                     # +relativeincome
                     +schools###
                     +collegegrads###
                     +safety###
                     +employmentdiversity####
                     # +unemployment###
                     +I(medianincomenbhd^(1/2))###
                     # +I(relativeincome^(1/2))
                     +medianavmstate
                     +medianavmcounty
                     +occupancy###
                     # +I(occupancy^2)
                     +whitecollar###
                     +I(medianavmcounty^(1/2))###
                     +I(medianavmstate^(1/2))###
                     # + PropForeclosures
                     + I(PropForeclosures^(1/2))###
                     # + I(log(PropForeclosures))
                     , data = NDS.CleanedData)

NDS.lm <- NDS.lmEndModel
summary(NDS.lm) 
NDS.lmOld <-NDS.lmEndModel

NDS.lm <-  step(NDS.lmBeginModel, scope = list(lower = NULL, upper = NDS.lmEndModel), direction = 'both')
summary(NDS.lm)
vif(NDS.lm)


# Remove those with residuals beyond threshold
NDS.CleanedData$Residuals <- as.data.frame(NDS.lm$residuals)[ , ]
NDS.CleanedDataUpper <- rbind(NDS.CleanedDataUpper, NDS.CleanedData[which(!(NDS.CleanedData$Residuals > -55 & NDS.CleanedData$Residuals < 55)), ])

# Training and Testing Sets
NDS.CleanedData <- NDS.CleanedData[which(NDS.CleanedData$Residuals > -55 & NDS.CleanedData$Residuals < 55), ]
nRowsTrain <- nrow(NDS.CleanedData) * 0.5
NDS.CleanedDataTrain <- NDS.CleanedData[sample(1:nrow(NDS.CleanedData), nRowsTrain, replace = FALSE), ]
NDS.CleanedDataValid <- NDS.CleanedData[which(!(rownames(NDS.CleanedData) %in% rownames(NDS.CleanedDataTrain))), ]

NDS.lmEndModel <- lm(sqrtMedianAVM ~ medianrent
                     # +medianincomenbhd
                     # +relativeincome
                     +schools###
                     +collegegrads###
                     +safety###
                     +employmentdiversity####
                     # +unemployment###
                     +I(medianincomenbhd^(1/2))###
                     # +I(relativeincome^(1/2))
                     # +medianavmstate
                     # +medianavmcounty
                     # +occupancy###
                     # +I(occupancy^2)
                     # +whitecollar###
                     +I(medianavmcounty^(1/2))###
                     +I(medianavmstate^(1/2))###
                     # + PropForeclosures
                     + I(PropForeclosures^(1/2))###
                     # + I(log(PropForeclosures))
                     , data = NDS.CleanedDataTrain)

NDS.lm <- NDS.lmEndModel
summary(NDS.lm) 

# Training Set
NDS.CleanedDataTrain$PredictedsqrtMedianAVM <-  predict(NDS.lm, NDS.CleanedDataTrain)
Rsq <- sum((NDS.CleanedDataTrain$PredictedsqrtMedianAVM - mean(NDS.CleanedDataTrain$PredictedsqrtMedianAVM))^2)/(sum((NDS.CleanedDataTrain$PredictedsqrtMedianAVM - mean(NDS.CleanedDataTrain$PredictedsqrtMedianAVM))^2) + sum((NDS.CleanedDataTrain$sqrtMedianAVM - NDS.CleanedDataTrain$PredictedsqrtMedianAVM)^2))
plot(NDS.CleanedDataTrain$PredictedsqrtMedianAVM ~ NDS.CleanedDataTrain$sqrtMedianAVM, ylab = "PredictedSqrtMedianAVM", xlab = "SqrtMedianAVM", main = paste("Training Dataset"))
mtext(paste("correlation:", round(cor(NDS.CleanedDataTrain$PredictedsqrtMedianAVM, NDS.CleanedDataTrain$sqrtMedianAVM), digits = 2), ", R-Squared:", round(Rsq, digits = 3)), side = 3)

# Validation Set
NDS.CleanedDataValid$PredictedsqrtMedianAVM <-  predict(NDS.lm, NDS.CleanedDataValid)
Rsq <- sum((NDS.CleanedDataValid$PredictedsqrtMedianAVM - mean(NDS.CleanedDataValid$PredictedsqrtMedianAVM))^2)/(sum((NDS.CleanedDataValid$PredictedsqrtMedianAVM - mean(NDS.CleanedDataValid$PredictedsqrtMedianAVM))^2) + sum((NDS.CleanedDataValid$sqrtMedianAVM - NDS.CleanedDataValid$PredictedsqrtMedianAVM)^2))
plot(NDS.CleanedDataValid$PredictedsqrtMedianAVM ~ NDS.CleanedDataValid$sqrtMedianAVM, ylab = "PredictedSqrtMedianAVM", xlab = "SqrtMedianAVM", main = paste("Test Dataset"))
mtext(paste("correlation:", round(cor(NDS.CleanedDataValid$PredictedsqrtMedianAVM, NDS.CleanedDataValid$sqrtMedianAVM), digits = 2), ", R-Squared:", round(Rsq, digits = 3)), side = 3)

###########################
#These are the variables used in the final model
NDS.lmEndModel <- lm( sqrtMedianAVM~ medianrent
                      # +medianincomenbhd
                      # +relativeincome
                      +schools###
                      +collegegrads###
                      +safety###
                      +employmentdiversity####
                      # +unemployment###
                      +I(medianincomenbhd^(1/2))###
                      # +I(relativeincome^(1/2))
                      # +medianavmstate
                      # +medianavmcounty
                      # +occupancy###
                      # +I(occupancy^2)
                      # +whitecollar###
                      +I(medianavmcounty^(1/2))###
                      +I(medianavmstate^(1/2))###
                      # + PropForeclosures
                      + I(PropForeclosures^(1/2))###
                      # + I(log(PropForeclosures))
                      , data = NDS.CleanedData)

NDS.lm <- NDS.lmEndModel
summary(NDS.lm)
vif(NDS.lm)
########

#Outliersss
#explicitly removing some records from analysis **
NDS.OutlierData <- rbind(NDS.CleanedDataLower, NDS.CleanedDataUpper)
NDS.OutlierData$PredictedsqrtMedianAVM <-  predict(NDS.lm, NDS.OutlierData)

plot(NDS.OutlierData$PredictedsqrtMedianAVM~NDS.OutlierData$sqrtMedianAVM,ylab="PredictedSqrtMedianAVM",xlab="SqrtMedianAVM",main=paste("Outlier Dataset"))
#Use identify function to id outliers (must select)
# x <- identify(NDS.OutlierData$sqrtMedianAVM, NDS.OutlierData$PredictedsqrtMedianAVM, labels = NDS.OutlierData$nid)
# > x
# [1] 26498 29415 42954 43541 44529 44731 45848 45856 47677 47767 47859 47910 47921 47966 48114 48756 49010 50276 51287 51719
# > x
# [1] 42100 42954 44731 45848 45856 47767 47859 47910 47966 48114 48756 49010 50276
x <- c(26498, 42100, 42954, 43541, 44529, 44731, 45848, 45856, 47677, 47767, 47859, 47910, 47921, 47966, 48114, 48756, 49010, 50276)


NDS.RentOutlierData <- NDS.OutlierData[x, ]
NDS.RentOutlierData <- rbind(NDS.RentOutlierData,NDS.OutlierData[which((NDS.OutlierData$medianrent/NDS.OutlierData$medianavm < 0.003)),])
NDS.OutlierData <- NDS.OutlierData[-x, ]
NDS.OutlierData <- NDS.OutlierData[which(!(NDS.OutlierData$medianrent/NDS.OutlierData$medianavm < 0.003)),]
NDS.lmRentOutliers <- lm(medianrent~sqrtMedianAVM,data=NDS.CleanedData)
NDS.RentOutlierData$medianrent <- predict(NDS.lmRentOutliers,NDS.RentOutlierData)
NDS.OutlierData <- rbind(NDS.OutlierData,NDS.RentOutlierData)
NDS.OutlierData <- unique(NDS.OutlierData)


#PREDICT
NDS.OutlierData$PredictedsqrtMedianAVM <-  predict(NDS.lm, NDS.OutlierData)
NDS.CleanedDataTrain$PredictedsqrtMedianAVM <-  predict(NDS.lm, NDS.CleanedDataTrain)
NDS.CleanedDataValid$PredictedsqrtMedianAVM <-  predict(NDS.lm, NDS.CleanedDataValid)

#re-aligning some extreme values
NDS.OutlierData[which(NDS.OutlierData$PredictedsqrtMedianAVM > 1680), ]$PredictedsqrtMedianAVM <- 1680 + 20 * normalized(NDS.OutlierData[which(NDS.OutlierData$PredictedsqrtMedianAVM > 1680), ]$PredictedsqrtMedianAVM)

#Outlier Analysis
Rsq <- sum((NDS.OutlierData$PredictedsqrtMedianAVM - mean(NDS.OutlierData$PredictedsqrtMedianAVM))^2)/(sum((NDS.OutlierData$PredictedsqrtMedianAVM - mean(NDS.OutlierData$PredictedsqrtMedianAVM))^2) + sum((NDS.OutlierData$sqrtMedianAVM - NDS.OutlierData$PredictedsqrtMedianAVM)^2))
plot(NDS.OutlierData$PredictedsqrtMedianAVM~NDS.OutlierData$sqrtMedianAVM,ylab="PredictedSqrtMedianAVM",xlab="SqrtMedianAVM",main=paste("Outlier Dataset"), col = alpha('black', 0.2))
mtext(paste("correlation:", round(cor(NDS.OutlierData$PredictedsqrtMedianAVM,NDS.OutlierData$sqrtMedianAVM),digits=2),", R-Squared:",round(Rsq,digits=3)), side = 3)

#Anlysis on Complete
# Combine Upper and Lower datasets with main datasets
NDS.CleanedData <-  rbind(NDS.CleanedDataTrain,NDS.CleanedDataValid,NDS.OutlierData)

#calculate RMSE of predicted values
mse <- mean((NDS.CleanedData$PredictedsqrtMedianAVM - NDS.CleanedData$sqrtMedianAVM)^2)
rmse<- round(sqrt(mse), digits = 2)
Rsq <- sum((NDS.CleanedData$PredictedsqrtMedianAVM - mean(NDS.CleanedData$PredictedsqrtMedianAVM))^2)/(sum((NDS.CleanedData$PredictedsqrtMedianAVM - mean(NDS.CleanedData$PredictedsqrtMedianAVM))^2) + sum((NDS.CleanedData$sqrtMedianAVM - NDS.CleanedData$PredictedsqrtMedianAVM)^2))
plot(NDS.CleanedData$PredictedsqrtMedianAVM ~ NDS.CleanedData$sqrtMedianAVM, ylab = "PredictedSqrtMedianAVM", xlab = "SqrtMedianAVM", main = paste("Complete Dataset"))
mtext(paste("correlation:",round(cor(NDS.CleanedData$PredictedsqrtMedianAVM, NDS.CleanedData$sqrtMedianAVM), digits = 2),", R-Squared:", round(Rsq, digits = 3)), side = 3)

# Combine the main dataset with dataset containing missing prices
NDS.CleanedData <- rbind(NDS.CleanedData, NDS.CleanedDataOnlyNA)
summary(NDS.CleanedData)

# Calculate NDS
NDS.CleanedData$NDS <- minNDS + (maxNDS - minNDS) * normalized(NDS.CleanedData$PredictedsqrtMedianAVM)
summary(NDS.CleanedData$NDS)

plot(NDS.CleanedData$NDS ~ NDS.CleanedData$sqrtMedianAVM, ylab = "NDS", xlab = "Sqrt Median Price", main = paste("Neighborhood"))
mtext(paste("correlation:", round(cor(NDS.CleanedData$sqrtMedianAVM, NDS.CleanedData$NDS), digits = 2)), side = 3)

# sort dataset on NDS
NDS.CleanedData <- NDS.CleanedData[order(NDS.CleanedData$NDS),]

# histogram of NDS
hist(NDS.CleanedData$NDS, xlab = "NDS", main = type, col = "red", breaks = seq(0, 1000, 15), xlim = c(100, 900), axes = FALSE)
axis(1, at = seq(100, 900, by = 100), las = 2)
axis(2, at = seq(0, 200000, by = 5000), las = 2)
txt <- ""
for(i in 2:5){txt <- paste(txt, names(summary(NDS.CleanedData$NDS)[i]), ": ", summary(NDS.CleanedData$NDS)[[i]], ", ", sep = '')}
txt <- paste(txt, "SD", ": ", round(sd(NDS.CleanedData$NDS), digits = 2), "   ", sep = '') 
mtext(txt, side = 3)


#Generate Output File containing NDS
# write.table(NDS.CleanedData,file=paste(file.OutputDir,file.OutputFilename,sep=''),sep="|",row.names=FALSE)
 
##Percentile Rank & Class
perc.rank <- function(x) trunc(rank(x))/length(x)
NDS.CleanedData$perc_rank <- perc.rank(NDS.CleanedData$NDS) * 100

NDS.CleanedData$Class <- 0
NDS.CleanedData[NDS.CleanedData$perc_rank <= 5, 'Class'] <- 'D'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 10 & NDS.CleanedData$perc_rank > 5, 'Class'] <- 'C'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 20 & NDS.CleanedData$perc_rank > 10, 'Class'] <- 'CC'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 30 & NDS.CleanedData$perc_rank > 20, 'Class'] <- 'CCC'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 40 & NDS.CleanedData$perc_rank > 30, 'Class'] <- 'B'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 50 & NDS.CleanedData$perc_rank > 40, 'Class'] <- 'BB'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 60 & NDS.CleanedData$perc_rank > 50, 'Class'] <- 'BBB'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 70 & NDS.CleanedData$perc_rank > 60, 'Class'] <- 'A'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 80 & NDS.CleanedData$perc_rank > 70, 'Class'] <- 'AA'
NDS.CleanedData[NDS.CleanedData$perc_rank <= 95 & NDS.CleanedData$perc_rank > 80, 'Class'] <- 'AAA'
NDS.CleanedData[NDS.CleanedData$perc_rank > 95, 'Class'] <- 'LUX'
##

NDS.CleanedData2 <-  NDS.CleanedData[ , - c(20, 23, 24, 25)]
write.table(NDS.CleanedData2,file=paste(file.OutputDir,'Score_v35_Test8.txt',sep=''),sep="|",row.names=FALSE)

summary(NDS.lmOld)
summary(NDS.lm)
vif(NDS.lm)
anova(NDS.lm)
plot(NDS.lm)

plot(NDS.data$medianavm, NDS.data$PropForeclosures)
mtext(paste("correlation:", round(cor(NDS.data$medianavm, 
                                      NDS.data$PropForeclosures), digits = 2)), side = 3)



######
new <- NDS.CleanedData[, c('medianavmstate', 'medianavmcounty', 'medianavm', 'medianrent', 'collegegrads', "whitecollar", "schools", "unemployment", "employmentdiversity", "occupancy", "safety", "PropForeclosures", "Nbhd_Size")]
sum_tab <- NULL
for(i in 1:dim(new)[2]){
  temp <- new[, i]
  sum_tab <- cbind(sum_tab, summary(temp))
  print(i)
}
colnames(sum_tab ) <- colnames(new)

