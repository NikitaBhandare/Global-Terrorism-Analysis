install.packages(c("ISLR","rpart.plot","moments","PerformanceAnalytics","tidyr","dummies","gridExtra"))
library(ISLR)
library(caret)
library(mice)
library(psych)
library(pROC)
library(rpart)
library(rpart.plot)
library(cluster)
library(tree) 
library(e1071)
library(moments)
library(corrplot)
library(PerformanceAnalytics)
library(lattice)
library(doParallel)
library(reshape2)
library(tidyr)
library(dummies)
library(ggplot2)
library(grid)
library(gridExtra)
library(MLmetrics)

#setting the working directory

getwd()
setwd("/store/studenthome/mis620/2017Fall/adity2007@gmail.com/Data Crusaders/")

#Download the dataset and Read data file in RDS

GTD <-read.csv("GTD_attempt 7.csv")
saveRDS(GTD, "GTD.rds")

GTD <- readRDS("GTD.rds")

str(GTD)

#convert the variables into factors
convert <- c("INT_LOG","INT_IDEO","INT_MISC","INT_ANY","guncertain1","multiple","success","suicide",
             "extended","country","region","attacktype1","targtype1","targsubtype1","natlty1","claimed","compclaim","weaptype1",
             "weapsubtype1","propextent","property","ransom","ishostkid","hostkidoutcome")

GTD[,convert] <- data.frame(apply(GTD[convert],2,as.factor))

GTD <- GTD[,-c(9,10,11,13,27)]

str(GTD)

GTD$iyear <- as.factor(GTD$iyear)
GTD$imonth <- as.factor(GTD$imonth)
GTD$iday <- as.factor(GTD$iday)
GTD$nperps <- as.factor(GTD$nperps)
GTD$nperpcap <- as.factor(GTD$nperpcap)

#will walk through basic imputing
set.seed(192)

#caret has preprocess function - we are imputing missing data with bag (ensemble decision trees), scaling and centering, and filtering out
#highly correlated predictors
GTD.prepmodel <- preProcess(GTD, method=c("bagImpute"))


GTD.prepmodel$method

#apply pre processing model to training/test data
GTD.prepmodel <-  predict(GTD.prepmodel, GTD)

str(GTD.prepmodel)

GTD.prepmodel$nkill <- ceiling(GTD.prepmodel$nkill)
GTD.prepmodel$nkillter <- ceiling(GTD.prepmodel$nkillter)
GTD.prepmodel$nwound <- ceiling(GTD.prepmodel$nwound)
GTD.prepmodel$nwoundte <- ceiling(GTD.prepmodel$nwoundte)

class(GTD.prepmodel$iyear)
#GTD1<- subset(GTD, !(GTD$nperps %in% c(-99, -9)))
GTD.prepmodel$iyear <- as.numeric(GTD.prepmodel$iyear)
GTD.prepmodel$imonth <- as.numeric(GTD.prepmodel$imonth)
GTD.prepmodel$iday <- as.numeric(GTD.prepmodel$iday)
GTD.prepmodel$nperps <- as.numeric(GTD.prepmodel$nperps)
GTD.prepmodel$nperpcap <- as.numeric(GTD.prepmodel$nperpcap)

str(GTD.prepmodel)


write.csv(GTD_SA,file= "GTD_AllRegions.csv")

#GTD$nkillter, GTD$nwound, GTD$nperps
GTD_SA <- subset(GTD.prepmodel, (GTD$region %in% 6))

#removing country_txt, region_txt, propextent_txt, hostkidoutcome_txt, attack_type1_txt, targtype1_txt
# targsubtype1_txt, natlty1_txt,  weapsubtype1_txt

not_reqd <- c("country_txt", "region_txt","attacktype1_txt", "targtype1_txt", "targsubtype1_txt",
              "natlty1_txt", "weaptype1_txt", "weapsubtype1_txt", "propextent_txt", "hostkidoutcome_txt")

GTD_SA<- GTD_SA[, !names(GTD_SA) %in% not_reqd]

#converting gname to string for replacing the phaltu terrorist groups to others
GTD_SA$gname <- as.character(GTD_SA$gname)


#Subsetting terrorist group names by selecting top 5 from a region

hai <- c("Abdullah Azzam Brigades","Al_Badr","Al_Qaida","Al_Qaida in the Indian Subcontinent","Al_Qaida Network for Southwestern Khulna Division",
         "Al_Umar Mujahideen","All Tripura Tiger Force _ATTF_","Ansarul Islam _Pakistan_","Babbar Khalsa International _BKI_","Baloch Liberation Army _BLA_",
         "Baloch Liberation Front _BLF_","Baloch Liberation Tigers _BLT_","Baloch Republican Army _BRA_","Communist Party of India_ Marxist",
         "Communist Party of India_Maoist _CPI_Maoist_","Haqqani Network","Harakat ul_Mujahidin _HuM_","Harakat ul_Mujahidin Al_Almi","Harkatul Jihad_e_Islami",
         "Hizbul Mujahideen _HM_","Indian Mujahideen","Islamic Movement of Uzbekistan _IMU_","Jamaat_E_Islami _Bangladesh_","Jamaat_E_Islami _India/Pakistan_",
         "Jama'atul Mujahideen Bangladesh _JMB_","Jamiat ul_Mujahedin _JuM_","Jundallah _Pakistan_","Kanglei Yawol Kanna Lup _KYKL_",
         "Kangleipak Communist Party _KCP_","Khorasan Chapter of the Islamic State","Lashkar_e_Jhangvi","Lashkar_e_Taiba _LeT_","Liberation Tigers of Tamil Eelam _LTTE_",
         "Maoist Communist Center _MCC_","National Democratic Front of Bodoland _NDFB_","National Liberation Front of Tripura _NLFT_","New People's Army _NPA_",
         "People's Liberation Army _India_","People's Revolutionary Party of Kangleipak _PREPAK_","Sipah_e_Sahaba/Pakistan _SSP_","Students Islamic Movement of India _SIMI_",
         "Taliban","Tamil Nadu Liberation Army","Tehrik_e_Nafaz_e_Shariat_e_Mohammadi _TNSM_","Tehrik_i_Taliban Pakistan _TTP_","United Liberation Front of Assam _ULFA_",
         "United National Liberation Front _UNLF_")

GTD_SA <- GTD_SA[GTD_SA$gname %in% hai,]

table(GTD_SA$gname)


ExcludingTop5 <- GTD_SA[GTD_SA$gname %in% c("Abdullah Azzam Brigades","Al_Badr","Al_Qaida","Al_Qaida in the Indian Subcontinent","Al_Qaida Network for Southwestern Khulna Division",
                                            "Al_Umar Mujahideen","All Tripura Tiger Force _ATTF_","Ansarul Islam _Pakistan_","Babbar Khalsa International _BKI_","Baloch Liberation Army _BLA_",
                                            "Baloch Liberation Front _BLF_","Baloch Liberation Tigers _BLT_","Baloch Republican Army _BRA_","Communist Party of India_ Marxist",
                                            "Haqqani Network","Harakat ul_Mujahidin _HuM_","Harakat ul_Mujahidin Al_Almi","Harkatul Jihad_e_Islami",
                                            "Hizbul Mujahideen _HM_","Indian Mujahideen","Islamic Movement of Uzbekistan _IMU_","Jamaat_E_Islami _Bangladesh_","Jamaat_E_Islami _India/Pakistan_",
                                            "Jama'atul Mujahideen Bangladesh _JMB_","Jamiat ul_Mujahedin _JuM_","Jundallah _Pakistan_","Kanglei Yawol Kanna Lup _KYKL_",
                                            "Kangleipak Communist Party _KCP_","Khorasan Chapter of the Islamic State","Lashkar_e_Jhangvi","Lashkar_e_Taiba _LeT_",
                                            "Maoist Communist Center _MCC_","National Democratic Front of Bodoland _NDFB_","National Liberation Front of Tripura _NLFT_","New People's Army _NPA_",
                                            "People's Liberation Army _India_","People's Revolutionary Party of Kangleipak _PREPAK_","Sipah_e_Sahaba/Pakistan _SSP_","Students Islamic Movement of India _SIMI_",
                                            "Tamil Nadu Liberation Army","Tehrik_e_Nafaz_e_Shariat_e_Mohammadi _TNSM_","United National Liberation Front _UNLF_"),]

table(ExcludingTop5$gname)

GTD_SA$gname <- replace(GTD_SA$gname, GTD_SA$gname %in% ExcludingTop5$gname,"Others")


table(GTD_SA$gname)


#phi coefficient > .7 than highly corelated then combine this

GTD_SA$gname <- gsub(" ", "_",GTD_SA$gname)

#converting the gnames to factor
GTD_SA$gname <- as.factor(GTD_SA$gname)

write.csv(GTD_SA,file= "GTD_Final.csv")


############____________FEATURE SELECTION USING BORUTA________________###################


# selecting important variables using Boruta
library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(GTD_SA$gname ~ ., data=GTD_SA, doTrace=2)  # perform Boruta search
# Confirmed 10 attributes: Humidity, Inversion_base_height, Inversion_temperature, Month, Pressure_gradient and 5 more.
# Rejected 3 attributes: Day_of_month, Day_of_week, Wind_speed.
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables

# 100 Runs
#=>[1] "iyear"          "extended"       "country"        "success"        "suicide"       
#=>[6] "attacktype1"    "targtype1"      "targsubtype1"   "natlty1"        "nperps"        
#=>[11] "nperpcap"       "claimed"        "compclaim"      "weaptype1"      "weapsubtype1"  
#=>[16] "nkill"          "nkillter"       "nwound"         "nwoundte"       "property"      
#=>[21] "propextent"     "ishostkid"      "ransom"         "hostkidoutcome" "INT_LOG"       
#=>[26] "INT_IDEO"       "INT_ANY"          

### 50 runs
#=>[1] "iyear"          "imonth"         "extended"       "country"        "multiple"      
#=>[6] "success"        "suicide"        "attacktype1"    "targtype1"      "targsubtype1"  
#=>[11] "natlty1"        "guncertain1"    "nperps"         "nperpcap"       "claimed"       
#=>[16] "compclaim"      "weaptype1"      "weapsubtype1"   "nkill"          "nkillter"      
#=>[21] "nwound"         "nwoundte"       "property"       "propextent"     "ishostkid"     
#=>[26] "ransom"         "hostkidoutcome"
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  # plot variable importance

############____________Creating Training and Test splits________________###################

#=>[1] "iyear"          "extended"       "country"        "success"        "suicide"       
#=>[6] "attacktype1"    "targtype1"      "targsubtype1"   "natlty1"        "nperps"        
#=>[11] "nperpcap"       "claimed"        "compclaim"      "weaptype1"      "weapsubtype1"  
#=>[16] "nkill"          "nkillter"       "nwound"         "nwoundte"       "property"      
#=>[21] "propextent"     "ishostkid"      "ransom"         "hostkidoutcome" "INT_LOG"       
#=>[26] "INT_IDEO"       "INT_ANY"          

#including only the important variables
impvars <- c("iyear","extended","success","suicide","attacktype1","targtype1","targsubtype1", 
             "natlty1","nperps","nperpcap","claimed","compclaim","weaptype1","weapsubtype1",
             "nkill","nkillter","nwound","nwoundte","property","propextent","ishostkid","ransom","hostkidoutcome",
             "INT_LOG","INT_IDEO","INT_ANY")

table(GTD_SA$gname)

y <- GTD_SA$gname
x <- GTD_SA[,names(GTD_SA) %in% impvars]

x.dummy <- dummyVars(~.,data = x)

x<- as.data.frame(predict(x.dummy, x))

set.seed(199)
inTrain <- createDataPartition(GTD_SA$gname,p=.7, list=F)

GTD.x.train <- x[inTrain,]

str(GTD.x.train)

GTD.y.train <- y[inTrain]
str(GTD.y.train)

GTD.training <- cbind(GTD.x.train, GTD.y.train)

GTD.y.test<- y[-inTrain]
GTD.x.test <- x[-inTrain,]

ctrl <- trainControl(method = "cv", number=10, summaryFunction=multiClassSummary,
                     classProbs=T, allowParallel =  FALSE)


#to see what parameters are to be tuned:
set.seed(199)

class(GTD_SA$gname)


############____________Random Forrest________________###################
#Random Forest
set.seed(199)

m.rf <- train(y= GTD.y.train, x= GTD.x.train,
              trControl = ctrl,
              method="rf", 
              metric="logLoss") #tuneLength=15, #mtry= floor(mtry.val), tuneGrid = data.frame(mtry = c(floor(mtry.val))))
#  ntree = 100)

m.rf

saveRDS(m.rf, "Randomforest_model.rds")

getTrainPerf(m.rf)
impvars.rf<- varImp(m.rf)
saveRDS(impvars.rf, "Imp Variables for random forest model.rds")

#can plot the performance of different parameters affect on ROC
plot(m.rf)

#the best performing model trained on the full training set is saved 
##preprocessing using predict function with caret train object will be applied to new data
p.rf <- predict(m.rf,GTD.x.test)
cm.rf <- confusionMatrix(p.rf,GTD.y.test) #calc accuracies with confuction matrix on test set

#confusion Matrix
cm.rf


#Table of predicted against actual values
table(p.rf, GTD.y.test) #returns the confusion matrix


df <- as.data.frame(table(p.rf, GTD.y.test))

library(ggplot2)
ggplot(data =  df, mapping = aes(x = GTD.y.test, y = p.rf)) +
  geom_tile(aes(fill = df$Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", df$Freq)), vjust = 1) +
  scale_fill_gradient(low = "grey", high = "orange") +
  theme_bw() + theme(legend.position = "none")



############____________DECISION TREE________________###################

m.rpart <- train(y=GTD.y.train, x=GTD.x.train,
                 trControl = ctrl,
                 tuneLength=15,
                 metric = "logLoss", #using AUC to find best performing parameters
                 method = "rpart")
m.rpart
getTrainPerf(m.rpart)
varImp(m.rpart)

#can plot the performance of different parameters affect on ROC
plot(m.rpart)

#the best performing model trained on the full training set is saved 
##preprocessing using predict function with caret train object will be applied to new data
p.rpart <- predict(m.rpart,GTD.x.test)
cm.rpart <- confusionMatrix(p.rpart,GTD.y.test) #calc accuracies with confuction matrix on test set

#confusion Matrix
cm.rpart

#Table of predicted against actual values
table(p.rpart, GTD.y.test) #returns the confusion matrix

df.rpart <- as.data.frame(table(p.rpart, GTD.y.test))

library(ggplot2)
ggplot(data =  df.rpart, mapping = aes(x = GTD.y.test, y = p.rpart)) +
  geom_tile(aes(fill = df.rpart$Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", df.rpart$Freq)), vjust = 1) +
  scale_fill_gradient(low = "grey", high = "red") +
  theme_bw() + theme(legend.position = "none")

############____________NAIVE BAYES________________###################
#naive Bayes
set.seed(199)

class(GTD_SA$gname)

grid <- data.frame(fL = c(0,0.5,1), usekernel = TRUE, adjust = c(0,0.5,1))
m.nb <- train(y=GTD.y.train, x=GTD.x.train,
              trControl = ctrl,
              tuneGrid = grid,
              metric = "logLoss", #using AUC to find best performing parameters
              method = "nb",
              importance = TRUE)
m.nb
getTrainPerf(m.nb)
varImp(m.nb)


#can plot the performance of different parameters affect on ROC
plot(m.nb)

#the best performing model trained on the full training set is saved 
##preprocessing using predict function with caret train object will be applied to new data
p.nb <- predict(m.nb,GTD.x.test)
cm.nb <- confusionMatrix(p.nb,GTD.y.test) #calc accuracies with confuction matrix on test set

#confusion Matrix
cm.nb

#Table of predicted against actual values
table(p.nb, GTD.y.test) #returns the confusion matrix

df.nb <- as.data.frame(table(p.nb, GTD.y.test))

library(ggplot2)
ggplot(data =  df.nb, mapping = aes(x = GTD.y.test, y = p.nb)) +
  geom_tile(aes(fill = df.nb$Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", df.nb$Freq)), vjust = 1) +
  scale_fill_gradient(low = "grey", high = "green") +
  theme_bw() + theme(legend.position = "none")


############____________NEURAL NETWORKS________________###################
set.seed(199)

m.nn <- train(y=GTD.y.train, x=GTD.x.train,
              trControl = ctrl,
              preProc = c("scale"),
              metric = "logLoss", #using AUC to find best performing parameters
              method = "nnet")
m.nn
getTrainPerf(m.nn)

#only slightly better, but better none the less :)
plot(m.nn)
getTrainPerf(m.nn)
varImp(m.nn)


#the best performing model trained on the full training set is saved 
##preprocessing using predict function with caret train object will be applied to new data
p.nn <- predict(m.nn,GTD.x.test)
cm.nn <- confusionMatrix(p.nn,GTD.y.test) #calc accuracies with confuction matrix on test set

#confusion matrix
cm.nn

#Table of predicted against actual values
table(p.nn, GTD.y.test) #returns the confusion matrix

df.nn <- as.data.frame(table(p.nn, GTD.y.test))

library(ggplot2)
ggplot(data =  df.nn, mapping = aes(x = GTD.y.test, y = p.nn)) +
  geom_tile(aes(fill = df.nn$Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", df.nb$Freq)), vjust = 1) +
  scale_fill_gradient(low = "grey", high = "yellow") +
  theme_bw() + theme(legend.position = "none")

############____________BAGGING TREE________________###################

modelLookup("treebag") #we have some paramters to tune such as laplace correction
set.seed(192)
m.bag <- train(y=GTD.y.train, x=GTD.x.train,
               trControl = ctrl,
               metric = "logLoss", #using AUC to find best performing parameters
               method = "treebag")
m.bag

getTrainPerf(m.bag)

varImp(m.bag)
p.bag<- predict(m.bag,GTD.x.test)

cm.bag <- confusionMatrix(p.bag,GTD.y.test) #calc accuracies with confuction matrix on test set

#confusion matrix
cm.bag

#Table of predicted against actual values
table(p.bag, GTD.y.test) #returns the confusion matrix

df.bag <- as.data.frame(table(p.bag, GTD.y.test))

library(ggplot2)
ggplot(data =  df.bag, mapping = aes(x = GTD.y.test, y = p.bag)) +
  geom_tile(aes(fill = df.bag$Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", df.nb$Freq)), vjust = 1) +
  scale_fill_gradient(low = "grey", high = "purple") +
  theme_bw() + theme(legend.position = "none")


############____________BOOSTING________________###################

install.packages("maboost")

modelLookup("maboost") #we have some paramters to tune such as laplace correction
set.seed(192)
m.boost <- train(y=GTD.y.train, x=GTD.x.train,
                 trControl = ctrl,
                 metric = "logLoss", #using AUC to find best performing parameters
                 method = "gbm")
m.boost
getTrainPerf(m.boost)

varImp(m.boost)
p.boost<- predict(m.boost,GTD.x.test)
cm.boost <- confusionMatrix(p.boost,GTD.y.test) #calc accuracies with confuction matrix on test set

#confusion matrix
cm.boost

#Table of predicted against actual values
table(p.boost, GTD.y.test) #returns the confusion matrix

df.boost <- as.data.frame(table(p.boost, GTD.y.test))

library(ggplot2)
ggplot(data =  df.boost, mapping = aes(x = GTD.y.test, y = p.boost)) +
  geom_tile(aes(fill = df.boost$Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", df.boost$Freq)), vjust = 1) +
  scale_fill_gradient(low = "grey", high = "dark green") +
  theme_bw() + theme(legend.position = "none")

############____________SVM________________###################

modelLookup("svmRadial") #we have some paramters to tune such as laplace correction
set.seed(192)
m.svm <- train(y=GTD.y.train, x=GTD.x.train,
               trControl = ctrl,
               preProc = c("scale"), #scale variables
               metric = "logLoss", #using AUC to find best performing parameters
               method = "svmLinear")
m.svm
getTrainPerf(m.svm)

varImp(m.svm)
p.svm<- predict(m.svm,GTD.x.test)
cm.svm <- confusionMatrix(p.svm,GTD.y.test) #calc accuracies with confuction matrix on test set

#confusion matrix
cm.svm

#Table of predicted against actual values
table(p.svm, GTD.y.test) #returns the confusion matrix

df.svm <- as.data.frame(table(p.svm, GTD.y.test))

library(ggplot2)
ggplot(data =  df.svm, mapping = aes(x = GTD.y.test, y = p.svm)) +
  geom_tile(aes(fill = df.svm$Freq), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", df.nb$Freq)), vjust = 1) +
  scale_fill_gradient(low = "grey", high = "pink") +
  theme_bw() + theme(legend.position = "none")

#lets compare all resampling approaches
GTD.models <- list("Neural-Network"=m.nn,
                    "Naive-Bayes"=m.nb,"DecisionTree" = m.rpart, 
                    "BaggingTree" = m.bag,"BoostingTree" = m.boost,
                    "Support Vector Machine"= m.svm, "Random Forest" = m.rf)

GTD.models

GTD.resamples = resamples(GTD.models)

#plot performance comparisons
bwplot(GTD.resamples, metric="Accuracy")
 
bwplot(GTD.resamples, metric="Mean_Sensitivity") #predicting default dependant on threshold
bwplot(GTD.resamples, metric="Mean_Specificity")

bwplot(GTD.resamples, metric="logLoss")

