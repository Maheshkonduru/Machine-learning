setwd("C:\\data science\\Linear Regression in R")
data<-read.csv("DirectMarketing.csv")
library(dplyr)
library(ggplot2)
#install.packages('car')
library(car) #has relevant functions to perform linear regression

head(data)
dim(data)
str(data)

##Do exploratory analysis##
plot(data$Age) #for categorical plot returns a bar plot 
plot(data$Age,data$AmountSpent,col="red") #returns box plot if it is a combination of categorical and continuous
plot(data$Salary) #for continuous plot returns a scatter plot for both single and multiple variables

#Combine the Middle and Old levels together
data$Age1<-ifelse(data$Age!="Young","Middle-Old",as.character(data$Age))
data$Age1<-as.factor(data$Age1)
summary(data$Age1)
plot(data$Age1,data$AmountSpent)

#Gender
plot(data$Gender,data$AmountSpent,col="red")

#Own house
summary(data$OwnHome)
plot(data$OwnHome,data$AmountSpent,col="red")

#Married
summary(data$Married)
plot(data$Married,data$AmountSpent,col="red")

#Location
summary(data$Location)
plot(data$Location,data$AmountSpent,col="red")

#Salary
summary(data$Salary)
plot(data$Salary,data$AmountSpent)#Might be heteroescadasticity

#Children
summary(data$Children)
data$Children<-as.factor(data$Children)
plot(data$Children,data$AmountSpent,col="red")

data$Children1<-ifelse(data$Children==3|data$Children==2,"3-2",as.character(data$Children))
data$Children1<-as.factor(data$Children1)
summary(data$Children1)
plot(data$Children1,data$AmountSpent,col="red")

#History
summary(data$History)
#Impute Missing values
tapply(data$AmountSpent,data$History,mean) #function similar to aggregate
ind<-which(is.na(data$History))
mean(data[ind,"AmountSpent"]) #not enough to impute all the missing values into one group

#Create a category called missing
data$History1<-ifelse(is.na(data$History),"Missing", as.character(data$History)) #as.character is given because when factor is applied inside ifelse it is converting factors to 1,2,3 in the output
data$History1<-as.factor(data$History1)
summary(data$History)
summary(data$History1)
class(data$History1)

#Catalogues
summary(data$Catalogs) #categorical, but too many classes, treating it as continuous
table(data$Catalogs)
names(data)
data1<-data[,-c(1,7,8)] #remove columns from which other derived columns were created

mod1<-lm(AmountSpent~.,data=data1)
summary(mod1)

mod2<-lm(formula = AmountSpent ~ Gender + Location + Salary + Catalogs + Children1 + History1, data = data1)
summary(mod2)

#Remove insignificant variabes
#HistoryMissing
#GenderMale

#Create dummy variables
data1$Male_d<-ifelse(data1$Gender=="Male",1,0)
data1$Female_d<-ifelse(data1$Gender=="Female",1,0)

data1$Missing_d<-ifelse(data$History1=="Missing",1,0)
data1$Low_d<-ifelse(data$History1=="Low",1,0)
data1$Med_d<-ifelse(data$History1=="Medium",1,0)
data1$High_d<-ifelse(data$History1=="High",1,0)

summary(data1$Low_d)
summary(data$History1)

mod3<-lm(formula = AmountSpent ~ Male_d + Location + Salary + Catalogs + Children1+Med_d+Low_d , data = data1)
#Now Med_d and Low_d act as separate variables and can be interpreted as an increase in 1 for Med_d wrt to !Med_d
# And as an increase in 1 for Low_d wrt to !Low_d
summary(mod3)

mod4<-lm(formula = AmountSpent ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)
#Note that the adjusted R square increases when Male_d was present in Mod3. 
#So Male_d can be included in the model too, but it's direction is wrong and therefore we exclude it
  
summary(mod4)


#check for Signs of the variable
tapply(data$AmountSpent,data$History1,mean)
data1%>%filter(History1!="Medium",History1!="Low")%>%summarize(Mean=mean(AmountSpent)) #inline
tapply(data1$AmountSpent,data1$Location,mean) #inline

#Assumption checks
#Normality
dim(data)
hist(mod4$residuals)
qqPlot(mod4$residuals)#In q-q plot the 1000 records are ordered by residuals and divided into quantiles
#Each residual is plotted in y-axis against the standard normal distributions values in the x-axis
#considering a standard normal distribution with 1000 records (mean = 0, standard deviation = 1)
#If there is a linear trend, then the two distributions have a linear relationship or the residuals are also normal
#If each residual is plotted against another normal distribution with mean = mean of residuals and stdev = stdev of residuals
#then the graph should lie along the x=y line, in case both the distributions are similar
#But we plot against standard normal distribution and therefore if the graph is a line we are good
#Also note that each point is plotted here and each one is a quantile since two distributions being compared are of equal size
#But when two distributions compared are not of equal size, certain number of quantiles can be created

#Non normal behaviour is observed in the residuals which means there is some pattern that is to be explained
#linear data with transformation required? Non-linear models can explain better?

#Multicollinearity Check
vif(mod4)
#1/(1-R^2)

#Constant  variance check
plot(mod4$fitted.values,mod4$residuals) #Funnel shape
#shows there is heteroskedasticity in the data

#Remedy: Apply log transform to y variable

mod5<-lm(formula = log(AmountSpent) ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)
summary(mod5)

qqPlot(mod5$residuals)#qqplot looks okay
plot(mod5$fitted.values,mod5$residuals)# looks okay with little funeling

summary(mod5)

#Apply log to salary relationship because it showed variation before
plot(data$Salary,data$AmountSpent)
plot(data$Salary,log(data$AmountSpent))
plot(log(data$Salary),data$AmountSpent) #the curve looks exponential, therefore there is scope for some log transformation
plot(log(log(data$Salary)),data$AmountSpent)
plot(log(log(data$Salary)),log(data$AmountSpent))

mod5a<-lm(formula = log(AmountSpent) ~ Location + log(log(Salary)) + Catalogs + Children1+Med_d+Low_d, data = data1)
summary(mod5a) #has highest R sqaured 

qqPlot(mod5a$residuals)#qqplot looks okay
plot(mod5a$fitted.values,mod5a$residuals)# no funelling

vif(mod5a)

#Apply square root transform
mod6<-lm(formula = sqrt(AmountSpent) ~ Location + Salary + Catalogs + Children1+Med_d+Low_d, data = data1)
summary(mod6)
qqPlot(mod6$residuals)
plot(mod6$fitted.values,mod6$residuals)#Seems okay, but funelling is worse 

vif(mod6)

predicted<-mod$fitted.values
actual<-sqrt(data1$AmountSpent)

dat<-data.frame(predicted,actual)

#create run chart
p<-ggplot(dat,aes(x=row(dat)[,2],y=predicted))
p+geom_line(colour="blue")+geom_line(data=dat,aes(y=actual),colour="black")


#For mod5a
predicted<-mod5a$fitted.values
actual<-log(data1$AmountSpent)

dat<-data.frame(predicted,actual)

#create run chart
p<-ggplot(dat,aes(x=row(dat)[,2],y=predicted))
p+geom_line(colour="blue")+geom_line(data=dat,aes(y=actual),colour="black")

#once can also take MAPE of two models on the validation data and compare to finalize the model
#Validation is not a neccessary requirement for Linear model since meeting the assumptions means validating the model




