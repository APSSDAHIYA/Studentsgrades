dataw = read.csv("student.csv", sep=",",header = TRUE)

head(dataw)
####

datac = dataw[c(2,4,5,11,12,16,17,18,19,20,21,22)]

head(datac)

sexd = model.matrix(~sex,-1,data = datac)
head(sexd)

sexdum =sexd[,2]

head(sexdum)
head(datac)
####
add= model.matrix(~address,-1,data=datac)
adddum=add[,2]
head(adddum)
head(datac)
#######
fam=model.matrix(~famsize,-1,data = datac)
famlsizedum =fam[,2]
######

res =model.matrix(~reason,-1,data = datac)
head(res)
resonhome =res[,2]
reasonother=res[,3]
reasonrepo =res[,4]
####
head(datac)
guard =model.matrix(~guardian,-1,data = datac)
head(guard)
guardianmother =guard[,2]
guardianother =guard[,3]
######
head(datac)
sklsup =model.matrix(~schoolsup,-1,data = datac)
head(sklsup)
schoolsupport= sklsup[,2]
head(schoolsupport)
#####
head(datac)
famsup = model.matrix(~famsup,-1,data = datac)
head(famsup)
familysupport = famsup[,2]
head(familysupport)
####
head(datac)
paiddum = model.matrix(~paid,-1,data = datac)
head(paiddum)
paidyes = paiddum[,2]
head(paidyes)
######
head(datac)
act =model.matrix(~activities,-1,data = datac)
head(act)
activitiesyes =act[,2]
head(activitiesyes)
####
head(datac)
nurs =model.matrix(~nursery,-1,data = datac)
head(nurs)
nurseryyes = nurs[,2]

head(nurseryyes)
#####
head(datac)
high = model.matrix(~higher,-1,data = datac)
head(high)
higheryes = high[,2]
head(higheryes)
###
head(datac)
internet = model.matrix(~internet,-1,data= datac)
head(internet)
internetyes =internet[,2]
######
numericdata =dataw[c(-1,-2,-4,-5,-11,-12,-16,-17,-18,-19,-20,-21,-22)]
head(numericdata)
finalnumericdata= numericdata[c(-5,-6,-10)]

head(finalnumericdata)
###Adding the categrical data to numerical data
finalnumericdata$internetyes =internetyes
finalnumericdata$higheryes =higheryes
finalnumericdata$nurseryyes=nurseryyes
finalnumericdata$activitiesyes =activitiesyes
finalnumericdata$paidyes = paidyes
finalnumericdata$familysupport = familysupport
finalnumericdata$schoolsupport = schoolsupport
finalnumericdata$guardianother= guardianother
finalnumericdata$reasonrepo =reasonrepo
finalnumericdata$reasonother = reasonother
finalnumericdata$resonhome = resonhome
finalnumericdata$famlsizedum = famlsizedum
finalnumericdata$address = adddum
finalnumericdata$sexdum = sexdum


head(finalnumericdata)

###linerregressoin

install.packages("leaps")
library(leaps)
install.packages("MASS")
library(MASS)
head(finalnumericdata)
linreg = lm(G3~., data = finalnumericdata)

summary(linreg, scipen =1000)
head(finalnumericdata)
#####G1 dependencty

dataforg1 = finalnumericdata[c(-16,-17)]
dataforg2 =finalnumericdata[c(-15,-17)]

head(dataforg1)

linearforg1 = lm(G1~.,data = dataforg1)
summary(linearforg1, scipen =1000)
linearforg2 = lm(G2~., data = dataforg2)
summary(linearforg2, scipen=1000)
########dividing the data
sampleg1 = nrow(dataforg1)
sampleg2 =nrow(dataforg2)
sampleg1
sampleg2
set.seed(15)
########shuffling
shuffledataforg1 = dataforg1[sample(sampleg1),]
shuffledataforg2 = dataforg2[sample(sampleg2),]


#########dividing
trainindicies = 1:round(0.65*sampleg1)
testindicies = round(0.65*sampleg1)+1:sampleg1
trainforg1 = shuffledataforg1[trainindicies,]
testforg1 = shuffledataforg1[testindicies,]
head(trainforg1)
##forg2
trainforg2 =shuffledataforg2[trainindicies,]
testforg2 = shuffledataforg2[testindicies,]
head(testforg2)
###########
######regressionfor g1

linregg1 = lm(G1~.,data = trainforg1)


summary(linregg1, scipen=1000)
install.packages("leaps")
library(leaps)
stepg1 = stepAIC(linregg1, direction= "backward", trace= FALSE)

summary(stepg1, scipen = 1000)
######
linregg2 =lm(G2~.,data = trainforg2)
stepg2 = stepAIC(linregg2, direction = "backward", trace = FALSE)
summary(stepg2, scipen =1000)

######
install.packages("forecast", dependencies = TRUE)
library(forecast)

pridictg1 =predict(stepg1,testforg2)

head(pridictg1)
data.frame("pridict"=pridictg1[1:20],
           "Actaul"=testforg1$G1[1:20],
           "residual"=pridictg1[1:20]-testforg1$G1[1:20])
#######
pridictg2 = predict(stepg2,testforg2)

data.frame("pridict"= pridictg2[1:20],
           "Actual" = testforg2$G2[1:20], 
           "residual"= pridictg2[1:20]-testforg2$G2[1:20])
######
accuracy(pridictg1,testforg1$G1)
accuracy(pridictg2,testforg2$G2)

#Predicting student performance 
#Predict grade 1 when a male student wants to do higher studies,studies for 8 hours, has school support, has never failed, has internet at home, lives in urban area and consumes very less alcohol daily, mother has secondary education..

predict_df <-data.frame(higheryes=1,studytime=3,schoolsupport=1,failures=0,Medu=3, Dalc=1, address=1, internetyes=1, sexdum=1)

#predict
predit_out <-predict(stepg1, predict_df, interval="prediction", level=0.95)

#print output
print(predit_out)

#Predict grade 2 when a female student wants to do higher studies,studies for 12 hours, has school support, has never failed, has internet at home, lives in urban area and consumes very less alcohol daily,mother has higher education.

predict_df <-data.frame(higheryes=1,studytime=4,schoolsupport=1,failures=0,Medu=4, Dalc=1, address=1, internetyes=1, sexdum=0)

#predict
predit_out <-predict(stepg1, predict_df, interval="prediction", level=0.95)

#print output
print(predit_out)
pridictg12 = data.frame(higheryes=0,studytime=3,schoolsupport=1,failures=0,Medu=3, Dalc=1, address=1, internetyes=1, sexdum=1)
outg12 =predict(stepg1, pridictg12, interval="prediction", level=0.95)
print(outg12)
head(finalnumericdata)
