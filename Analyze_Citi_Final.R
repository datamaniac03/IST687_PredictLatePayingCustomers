library(caret)
library(foreach)
library(kernlab)
library(e1071)
library(ggplot2)
library(vcd)
library(plyr)
library(ggmap)
library(zipcode)
library(arules)

#Load data set 
citi.consumption <- read.csv(file.choose(), header = T, stringsAsFactors = F)

#------------------------------------------------------
# CLEANING & PROCESSING DATA 
#------------------------------------------------------

#Reformat zipcode, extract only first 5 digits 
citi.consumption$Zip <- substr(citi.consumption$Zip, 1,5)
# Find and remove dirty values in "Zip" and "Ward" columns 
unique(citi.consumption$Zip)  #dirty values: " ", "-6064"
citi.consumption <- citi.consumption[citi.consumption$Zip != "-6064" & citi.consumption$Zip != " ", ]
unique(citi.consumption$Ward) #dirty values: " ", "XX" 
citi.consumption <- citi.consumption[citi.consumption$Ward != "XX" & citi.consumption$Ward != " ", ]

#Extract Customer's Service Class and Service SubClass 
temp <- strsplit(citi.consumption$Service.Class, ":")
citi.consumption$Service.Class <- foreach(i=1:length(temp), .combine = 'c') %do% temp[[i]][1]
#Create a new column named SubClass for Subclass Service type: " General Manufacturing", "Hotel",..
citi.consumption$SubClass <- foreach(i=1:length(temp), .combine = 'c') %do% temp[[i]][2]

#Build new columns to label late-payment customers 
citi.consumption$late.30 <-  as.factor(ifelse(citi.consumption$PastDue.30.Days > 0 ,1,0))
citi.consumption$late.60 <-  as.factor(ifelse(citi.consumption$PastDue.60.Days > 0 ,1,0))
citi.consumption$late.90 <-  as.factor(ifelse(citi.consumption$PastDue.90.Days > 0 ,1,0))
citi.consumption$late.120 <-  as.factor(ifelse(citi.consumption$PastDue.120.Days > 0 ,1,0))
citi.consumption$late <- as.factor(ifelse(citi.consumption$late.30 == 1 | citi.consumption$late.60 == 1
                                     | citi.consumption$late.90 == 1 | citi.consumption$late.120 == 1, 1, 0))

citi.consumption$Ward <- as.factor(citi.consumption$Ward)
citi.consumption$Zip <- as.factor(citi.consumption$Zip)
citi.consumption$Service.Class <- as.factor(citi.consumption$Service.Class)

#--------------------------------------------------------
#INITIAL ANALYSIS
#--------------------------------------------------------

#Create a function to identify relationships between 2 variables in data set using Association Rules 
arules.citi <- function (df.arules, sup, con)
{ 
        rules <- apriori(df.arules, parameter = list(supp = sup, conf = con))
        rules.sorted <- sort(rules, by = "lift")
        return(inspect(rules.sorted))
}

#find assocation rules between Customer Service Class and late payment 
df.arules <- data.frame(citi.consumption$Class, citi.consumption$late)
arules.citi(df.arules, 0.03, 0.5)   #{late=1}    => {ServiceClass=Residential} 0.1082603  0.8209570 0.9752112

#rules between SubClass and late -> can not find any significant!!!
df.arules <- data.frame(citi.consumption$SubClass, citi.consumption$late)
arules.citi(df.arules, 0.003, 0.5)

#rules between Ward and late payment
df.arules <- data.frame(citi.consumption$Ward, citi.consumption$late)
arules.citi(df.arules, 0.003, 0.2)  #Ward 16, 24,20,7,21
        #lhs                           rhs                           support confidence      lift
        #1  {citi.consumption.Ward=16} => {citi.consumption.late=1} 0.003611296  0.2970535 2.2409332
        #2  {citi.consumption.Ward=24} => {citi.consumption.late=1} 0.005892114  0.2846045 2.1470196
        #3  {citi.consumption.Ward=20} => {citi.consumption.late=1} 0.004649361  0.2778506 2.0960688
        #4  {citi.consumption.Ward=7}  => {citi.consumption.late=1} 0.004627430  0.2702818 2.0389709
        #5  {citi.consumption.Ward=17} => {citi.consumption.late=1} 0.003413917  0.2650397 1.9994253

#rules between BadDebt and late payment 
df.arules <- data.frame(citi.consumption$BadDebt, citi.consumption$late)
arules.citi(df.arules, 0.003, 0.2)
        #lhs                             rhs                             support confidence      lift
        #1 {citi.consumption.BadDebt=Y} => {citi.consumption.late=1}    0.02860353  0.9341434 7.0837713

#Rules between Connection Size  and late payment
df.arules <- data.frame(citi.consumption$ConnectionSize, citi.consumption$late)
arules.citi(df.arules, 0.0003,0.3)  #No association between Connection Size and Late

#VISUALIZE CUSTOMER TYPES WITH LATE PAYMENT
#Calculate Late vs Ontime per customer types - VISUALIZE using stacked Barchart 
#make a data frame showing number of customers pay late and on time per service class 
citi.late.servicetype <- ddply(citi.consumption, c("Service.Class","late"), summarise, No = length(late))
ggplot(citi.late.servicetype, aes(x=Service.Class, y=No, fill = late)) + 
        geom_bar(position = "dodge", stat = "identity", color = "black") + 
        scale_fill_brewer("Pastel1")

#MAPING LATE CUSTOMER ON CHICAGO MAP
#Extract locations having late customers 
citi.late.location <- citi.consumption[citi.consumption$late == 1,]
#Map locations with latitude + longitude values using package zipcode
citi.late.location$latitude <- zipcode$latitude[match(citi.late.location$Zip, zipcode$zip)]
citi.late.location$longitude <- zipcode$longitude[match(citi.late.location$Zip, zipcode$zip)]

#Generate map of Chicago 
citiChicago3  <- get_map("South Side Chicago , city of Chicago", zoom = 11, maptype = "roadmap")
ggmap(citiChicago3)
#Plot density map of Chicago with late customer data
ggmap(citiChicago3) + stat_density2d (aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..)
                                      , size = 0.5, bins = 5
                                      , data = citi.late.location
                                      , geom = "polygon") + 
        scale_fill_gradient(low = "black", high = "red")


#--------------------------------------------------------
# PREDICT LATE CUSTOMERS 
#--------------------------------------------------------

# Predict late payment customer using supervised learning methods 
#Create function, applying Support Vector Machine method 
SVM.f <- function(df, numerator,denominator,predictors, outcome) {
        input <- data.frame(predictors)
        #Label the ground truth for prediction 
        input$outcome <- outcome
        #set cutpoint 
        randIndex <- sample(1:dim(df)[1])
        cutPoint <- floor(numerator*dim(df)[1]/denominator)
        #build training & testing data 
        training <<- input[randIndex[1:cutPoint],]
        testing <<- input[randIndex[(cutPoint+1):dim(input)[1]],]
        #Build model to predict 
        svmOutput <- ksvm(outcome ~ ., data=training, kernel="rbfdot",kpar="automatic",C=5,cross=3, prob.model=TRUE)
        svmPred <- predict(svmOutput, testing, type = "votes")
        conf <<- confusionMatrix(testing$outcome,svmPred[1,]) 
        accuracy <- conf$overall['Accuracy']
        return (accuracy)
}

#Predictors: "Ward", "Service.Class", build model from training data (2/3 total data)
SVM.f(citi.consumption,2,3,list(citi.consumption$Ward, citi.consumption$Service.Class), citi.consumption$late)
#Accuracy : 13.41 %   

#Predictors: Service.Class 
SVM.f(citi.consumption,2,3,list(citi.consumption$Service.Class), citi.consumption$late)
#Can not build model 

#Predictors: Ward 
SVM.f(citi.consumption,2,3,list(citi.consumption$Ward), citi.consumption$late)
#Can not build model 

#Predictors: Avg.mo 
SVM.f(citi.consumption,2,3,list(citi.consumption$Avg.mo), citi.consumption$late)
#Accuracy : 13.27 %

#Predictors: "Ward", "Service.Class", "Avg.mo"
SVM.f(citi.consumption,2,3,list(citi.consumption$Service.Class, citi.consumption$Ward, citi.consumption$Avg.mo), citi.consumption$late)
 #Accuracy : 13.49 %  

#Predictors: "Connection Size" 
SVM.f(citi.consumption,2,3,list(citi.consumption$Service.Class, citi.consumption$ConnectionSize), citi.consumption$late)
 #Accuracy : 13.38 %  

#---------------------------------------------------------------------------
#TRY NAIVE-BAYES METHOD 
#Create function to run Naive-Bayes for different variables based on input cut point 
NB.f <- function(df, numerator,denominator,predictors, outcome) {
        input <- data.frame(predictors)
        #Label the ground truth for prediction 
        input$outcome <- outcome
        #set cutpoint 
        randIndex <- sample(1:dim(df)[1])
        cutPoint <- floor(numerator*dim(df)[1]/denominator)
        #build training & testing data 
        training <<- input[randIndex[1:cutPoint],]
        testing <<- input[randIndex[(cutPoint+1):dim(input)[1]],]
        #Build model to predict 
        model <- naiveBayes(outcome ~ . , data = training)
        pre <- predict(model, testing)
        #Get confusion matrix to calculate predict accuracy 
        con <<- confusionMatrix(pre, testing$outcome)
        accuracy <- con$overall['Accuracy']
        return (accuracy)
}

#Predictors: Service Class 
NB.f(citi.consumption, 2,3,list(citi.consumption$Service.Class), citi.consumption$late)   #Accuracy: 86.578% 
#Predictor: Ward 
NB.f(citi.consumption, 2,3,list(citi.consumption$Ward), citi.consumption$late)         #Accuracy: 86.66% 
#Predictor: Service Class + Ward 
NB.f(citi.consumption, 2,3,list(citi.consumption$Service.Class, citi.consumption$Ward), citi.consumption$late)   #Accuracy: 86.686% 


#-----------------------------------------------------------
#Identify and Predict High-risk customers 
#Calculate total debt by service subclass 
citi.debt.subclass <- ddply(citi.consumption, c("SubClass"), summarise, 
      TotalDebt = sum(PastDue.30.Days+ PastDue.60.Days + PastDue.90.Days +PastDue.120.Days))
citi.debt.subclass.order <- citi.debt.subclass[order(citi.debt.subclass$TotalDebt, decreasing = T),]

#Analyze Debt 
summary(citi.debt.subclass.order$TotalDebt) 
quantile(citi.debt.subclass.order$TotalDebt, probs = c(0.25,0.5,0.75,0.9)) #there's outlier affect median and mean values 
#Filter only "Residental" customer 
citi.res <- citi.filter[citi.filter$Service.Class == "Residential",]
citi.res$totalDebt <- citi.res$PastDue.30.Days + citi.res$PastDue.60.Days + citi.res$PastDue.90.Days + citi.res$PastDue.120.Days
#remove customers have debt <0
citi.res <- citi.res[citi.res$totalDebt > 0,]
summary(citi.res$totalDebt) 
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.01     99.32    284.20   1058.00    764.50 130000.00 
#------------------LOW --MEDIUM-----HIGH RISK----------

citi.res$Risk[citi.res$totalDebt > 1058] <- "High"
citi.res$Risk[citi.res$totalDebt >= 284.20 & citi.res$totalDebt <= 1058] <- "Medium"
citi.res$Risk[citi.res$totalDebt  < 284.20] <- "Low"
citi.res$Risk <- factor(citi.res$Risk, levels = c("Low", "Medium", "High"),
                                labels = c(0, 1, 2))

#USe Naive-Bayes to predict high-risk customers 
#predictors: Ward
NB.f(citi.res, 2,3, list(citi.res$Ward), citi.res$Risk)  #Accuracy: 50.7 % 
#predictors: Avg.mo
NB.f(citi.res, 2,3, list(citi.res$Avg.mo), citi.res$Risk)  #Accuracy: 57.33 % 
#predictors: Zip
NB.f(citi.res, 2,3, list(citi.res$Zip), citi.res$Risk)  #Accuracy: 50.62 % 

#Predictors: "Ward", "Zip", "Avg.mo"
SVM.f(citi.res,2,3,list(citi.res$Ward, citi.res$Avg.mo), citi.res$Risk)
