
#boxplot
#linear model
#eda talks about 
# 1.preprocessing 
#to find of missing data
#2.outliers 
#build the model
#do normality & heteroscedacity
#if it does satisficy
#remove outliers
#boxcox transformation
#create train & test data 
#build the model - traindata
#check model
#prediction
#still if doesnot satisy go for other model

file.choose()
boston<-read.csv("C:\\Users\\User\\Downloads\\Boston_missing Data (1).csv")
boston
View(boston)
table(is.na(boston))
library(mice)
imp <- mice(boston , m=5 , method = "pmm" , maxit = 100)
full<-complete(imp,2)
full
View(full)
bind<-cbind(boston$black,full$black)
View(bind)

write.csv(full,"C:\\Users\\User\\Downloads\\Boston_missing Data (1).csv")
table(is.na(full))
View(full)

#fivenum() is used to find out of minimum,1st quartile,2nd quartile,3rd quartile,maximum
fivenum(boston)

#to check the outlier
out<-boxplot(full$medv,boxplot.stats(full$medv)$out)
out$out

library(caret)
sample1 <- createDataPartition(full$medv , p=0.7 , list = F)
train <- full[sample1, -1]
test <- full[-sample1, -1]
View(train)

#to check normality test-data should be distributed normally
shapiro.test(full$medv)
library(nortest)
ad.test(full$medv)
lillie.test(full$medv)

full1 <- full[ ,-1] # to remove x column that is dummy variable
View(full1)

#model bulding
lm1 <- lm(medv~. , data = train)
summary(lm1)

car::ncvTest(lm1)
qt(0.025, 354)

ycap <- lm1$fitted.values
ycap
y <- traind$medv
y
r <- (cor(y,ycap))^2
r
plot(lm1)

qf(0.05,13,340)
nrow(traind)
aov(lm1)
write.csv(train,"C:\\Users\\User\\Downloads\\Boston_missing Data (1).csv")
trellis.device()
par(mfrow =c(2,2))
plot(lm1)

#to remove heteroscadasity by boxcox transformation
library(caret)
boxmodel <- BoxCoxTrans(full$medv)
full$transvar <- predict(boxmodel , full$medv)
head(full)

sample2 <- createDataPartition(full$transvar , p=0.7 ,list = F)
train1 <- full[sample2 , -c(1,15)]
View(train1)
testd1 <- full[-sample2 , -c(1,15)]
#linear model
linmod <- lm(transvar~., data = train1)
summary(linmod)
car::ncvTest(linmod)
lmtest::gqtest(linmod)
lmtest::bptest(linmod)

lmtest::dwtest(linmod)

car::vif(linmod)

outl <- boxplot(full$medv ,boxplot.stats(full$medv)$out)
a <- boxplot.stats(full$medv)$out
a

outlier <- which(full$medv %in%a) # shows the position of outlier - %in%
outlier
newfull <- full[-outlier, ]
newfull
boxplot.stats(newfull$medv)

transform <- BoxCoxTrans(newfull$medv)
transform
newfull$newtrans <- predict(transform , newfull$medv)
View(newfull)

library(caret) 
sample3 <- createDataPartition(newfull$transvar , p=0.7 , list = F)
train2 <- newfull[sample3, -c(1,16,17)]
test2 <- newfull[sample3 , ]
View(train2)

linmod1 <- lm(medv~. , data = traind2[ ,-c(9,10)])
summary(linmod1)

linmod2 <- lm(medv~. , data = traind2)
summary(linmod2)
anova(linmod2,linmod1)
anova(linmod1,linmod2)

median(full$medv)

full$costly <- ifelse(full$medv>21.2 , 1 , 0)

View(full)

###################logi model

logisticdata <- full[ ,-c(1,15,16)]
View(logisticdata)

library(caret)
sample_logistic <- createDataPartition(full$costly , p=0.7 , list = F)
trainlogi <- full[sample_logistic, ]
testlogi <- full[-sample_logistic, ]

logimod <- glm(costly~. , data = trainlogi , family = binomial(link = logit))

logimod$deviance
logimod$deviance/logimod$df.residual

#30-10-2019
file.choose()
admit<-read.csv("C:\\Users\\User\\Documents\\Admission_Predict.csv")
dim(admit)
str(admit)
View(admit)

#build the model
logindex<-sample(1:nrow(admit),0.7*nrow(admit))
trainlogi<-admit[logindex, ]
testlogi<-admit[-logindex, ]
#logit is to find the probability function
#admit is the response variable
logimod<-glm(admit~.,data = trainlogi,family = binomial(link = "logit"))
logimod
#observed variance should be greater than predicted value is overdispersion
#it should be approx equal to one
#talks about randomness
overdispersion<-logimod$deviance/logimod$df.residual
overdispersion
summary(logimod)

#to make practical examination
exp(logimod$coefficients)

#creation of another moodel taking only significant one
logimod_reduced<-glm("admit~GRE+LOR+CGPA",data = trainlogi,family = binomial(link = "logit"))
anova(logimod,logimod_reduced)

install.packages("rcompanion")
library(rcompanion)
nagelkerke(logimod)
nagelkerke(logimod_reduced)

#by comparing the logimod and logimod_reduced the logimod is best one bcoz the pseudo r square is high & probability value 
#using full model that is logimod 

anova(logimod)
#here its analysis of deviance not variance because the response variable is 0 and 1 

pred<-predict(logimod,newdata = testlogi, type = "response")
pred
#the above gives the probability value


pred1<-ifelse(pred>0.7,1,0)
table(pred1, testlogi$admit)

#find the accuracy
library(caret)
t<-table(pred1, testlogi$admit)
confusionMatrix(t)

install.packages("proc")
library(pROC)

#we use pred in roc for probability
#specificity is negative as negative 
#sensitivity is positive as positive 
roc(testlogi$admit,pred)
plot(roc(testlogi$admit,pred))

#HOSMER LAMESHOW TEST - GOODNESS FIT 

install.packages("ResourceSelection")
library(ResourceSelection)
ResourceSelection::hoslem.test(testlogi$admit,pred)
#accepts the null hypothesis so the model fits
#g is the bin value 
ResourceSelection::hoslem.test(testlogi$admit,pred,g=2)

################decision tree
#we cant see the significance 

install.packages("rpart")
library(rpart)
dt<-rpart(admit~.,data = trainlogi,method = "class")
install.packages("rattle")
library(rpart.plot)
#auto is text size
rpart.plot(dt,type = 2,extra = 'auto')

dt$cptable
plot(dt$cptable, type = 'o')

#take the least xerror value 0.4137931+0.05436603=0.46
#0.4137931-0.05436603=0.35
#then find the value which lies between 0.46 and 0.35 that is ur cp value to prune the tree

prun_tree<-prune(dt,cp=0.01293103)
rpart.plot(prun_tree,type = 2, extra = 'auto')

dpred<-predict(dt,newdata = testlogi,type = "class")
table(testlogi$admit,dpred)
#find the accuracy
tt<-table(testlogi$admit,dpred)
confusionMatrix(tt)

#31-10-2019
###Regression tree
#u will not get unique value for independent varaible i.e,, X 
#we used cars data
library(rpart)
#dist is the function of speed
rt<-rpart(dist~.,data = cars, method = 'anova')
rt
rt$variable.importance
#regression importance - varaible importance
pre<-predict(rt,newdata = cars)
pre
#the above if u give u will get the bucket values in probability
plot(pre)
plot(pre,type = 'l')
plot(pre,type = 'o')
anova(rt)
rt$splits
rt$y
var(pre)
var(cars$dist-pre)
#280 is the regresion value
lmodel<-lm(dist~.,data = cars)
aov(lmodel)
15.37959*15.37959
#236.5318 is the linear model


###############Random forest
#when u are using same samples for more than one model to find the accuracy use set.seed 
#before creating any sample just type set.seed
#random forest will give more accuracy than decision tree
View(admit)
install.packages("randomForest")

set.seed(100)
a<-sample(1:nrow(admit),0.7*nrow(admit))
a
set.seed(100)
rftrain<-admit[a, ]
rftest<-admit[-a, ]
str(admit)
#it is in integer
rftrain$admit<-as.factor(rftrain$admit)
library(randomForest)
rf<-randomForest(admit~., data = rftrain, na.action = na.roughfix, importance =T)
rf$predicted
rf
library(caret)
trainconf<-table(rftrain$admit, rf$predicted)
caret::confusionMatrix(trainconf)

pre_test<-predict(rf,newdata = rftest)
pre_test
test_conf<-table(rftest$admit, pre_test)
caret::confusionMatrix(test_conf)

rf$confusion
rf$importance
 
## graphical user interface
install.packages("Rcmdr")
library(Rcmdr)
###
install.packages("rattle")
library(rattle)
rattle()
