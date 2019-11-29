#--------------------------------Loading the libraries--------------------------------
library(dplyr)             #used for data manipulation, filling null values
library(car)               #used for recoding the variables, checking multicollinearity
library(MASS)              #used for 2 sample-t test and variance test
library(lmtest)            #used for likelihood ratio test
library(aod)               #used for wald test
library(ggplot2)           #used for plotting the graphs
library(gganimate)         #used for adding animations to the graph
library(caTools)           #used for splitting the dataset
library(pROC)              #used for plotting roc curve and finding auc values
library(psych)             #for plotting correlation between variables
library(ResourceSelection) #for hosmer lemeshow test
library(cowplot)           #for providing theme to ggplot2
library(tidyverse)         #used for filtering data   

#---------------------------Loading The dataset-------------------------------------

titanic<- read.csv("C:\\Users\\owner\\Desktop\\titanic.csv")
head(titanic)
colnames(titanic)
str(titanic)
colSums(is.na(titanic))

#------------------------Data Manipulation And Cleaning-----------------------------

#Checking median of Age according to Each Pclass
titanic %>%
  group_by(Pclass) %>% 
  summarize(median_age = median(Age,na.rm = TRUE))

#Filling missing values of Age with median value of age according to the Pclass

titanic[c("Age","Pclass")]<- titanic[c("Age","Pclass")] %>% 
  mutate(Age = ifelse(is.na(Age) & Pclass==1,37 ,Age)) %>% 
  mutate(Age = ifelse(is.na(Age) & Pclass==2,29 ,Age)) %>% 
  mutate(Age = ifelse(is.na(Age) & Pclass==3,24 ,Age))               

#Creating a new calculated variable group to know how many people were actually present alongwith that person 

titanic$Alone<-  titanic$SibSp + titanic$Parch
titanic <- titanic %>% mutate(Alone= ifelse(Alone==0,"Yes", "No"))
titanic$Alone <- factor(titanic$Alone, levels = c("No","Yes"))

#median fare according to each Pclass

titanic %>%
  group_by(Pclass) %>% 
  summarize(median_fare = median(Fare, na.rm = TRUE))

#Treating Pclass into ordinal data and assigning the values 3 to pclass 1 as its weightage(meadian fare) is more and 1 to Pclass 3 as its median fare is less.

titanic$Pclass<- as.character(titanic$Pclass)
titanic$Pclass<- as.numeric(recode(titanic$Pclass, "'1'= '3';'2'='2'; '3'='1'"))
head(titanic)
colSums(is.na(titanic))

#Dropping unwanted columns

titanic<- titanic[,!names(titanic)%in%c("PassengerId","Ticket","Name","SibSp", "Parch","Cabin")]
str(titanic)
head(titanic)
colnames(titanic)
write.csv(titanic,"new.csv")    #Saving a new version of data for sas coding

#----------------Significance Tests------------------------------------------------------

#Variance and 2 sample t-test of Age
#Variances are same for both the groups 

a<-subset(titanic, Survived==0)$Age
b<-subset(titanic, Survived==1)$Age
var.test(a,b)

#Two sample t test says that the mean age of two groups are same 

t.test(a,b,var.equal = T)

#Variance and 2 sample t-test of Fare
#Variances are different for both the groups 

a<-subset(titanic, Survived==0)$Fare
b<-subset(titanic, Survived==1)$Fare
var.test(a,b)

#Two sample t test says that the mean fare of two groups are different 

t.test(a,b,var.equal = F)

#Chi-square test for Pclass
#Survived and Pclass are dependent

p.table<- table(titanic$Survived, titanic$Pclass)
chisq.test(p.table)

#Chi-square test for Sex
#Survived and Sex are dependent.

sex.table<- table(titanic$Survived,titanic$Sex)
sex.table
chisq.test(sex.table)

#Chi-square test for Alone
#Survived and Group are dependent .

aln.table<- table(titanic$Survived, titanic$Alone)
aln.table
chisq.test(aln.table)

#Chi-square test for Embarked
#Survival rate and Embarked are dependent

p.table<- table(titanic$Survived, titanic$Embarked)
chisq.test(p.table)

#------------------------------Checking correlation-----------------------------------------

pairs.panels(titanic[,!names(titanic)%in%c("Survived", "Pclass", "Sex","Alone","Embarked")])

#-----------------------------MULTICOLLINEARITY TEST----------------------------------------
#Since GVIF^(1/(2*Df)) is less than 3.16, we can say that there is no multicolinearity in our dataset

vif(glm(Survived~., family = binomial(link = "logit"), data= titanic))

#-------------------------------INTERACTION PLOTS--------------------------------------------
#As the lines are not parallel, we can say that the two variables shows interactions

interaction.plot(titanic$Pclass,titanic$Sex, titanic$Survived, col=2:3,main= "Interaction Plot: Pclass vs Sex", xlab = "Pclass", ylab="Sex")

#---------------------------------Splitting the dataset------------------------------------

set.seed(100)  #It is used so that each time the dataset we get after splitting is the same
sample_size<- sample.split(titanic$Survived, SplitRatio = 7/10) #Splitting the dataset into 70/30 ratio
train<-subset(titanic, sample_size==T)
test<-subset(titanic, sample_size==F)
nrow(train)
nrow(test)

#--------------------------------MODEL BUILDING----------------------------------------------

model1<- glm(Survived~Pclass+Sex+Age+Fare+Alone, family = binomial(link = "logit"), data=train)
summary(model1)
step(model1, direction = "backward" )     #choosing significant variables using backward elimination method

model2.int<- glm(Survived~Pclass+Sex+Age+Fare+Alone+Pclass*Sex, family = binomial(link = "logit"), data=train)
summary(model2.int)
step(model2.int, direction = "backward" ) #choosing significant variables using backward elimination method   

#-----------------------------Fitting Refined Model To Our Training Set---------------------

train.model1<- glm(Survived~ Pclass+Age+Sex, family = binomial("logit"), data=train)
summary(train.model1)

train.model2<- glm(Survived~Pclass+Age+Sex+Pclass*Sex, family=binomial(link="logit"), data=train)
summary(train.model2)

#---------------------------------Likelihood-Ratio Test------------------------------------
#Its result shows that the full model (model with interaction term) is appropriate
lrtest(train.model1,train.model2)

#-------------------------------------Wald Test--------------------------------------------

anova(train.model2)

#------------------Predictions For Test Dataset using our selected model 2---------------------------------------------

predictions1<- predict(train.model1, test, type="response")
predictions2<- predict(train.model2, test, type="response")
cutoff<- mean(train$Survived)
cutoff

#----------Function To Assign Survival Category Based On Probabilit Cutoff------------------  

for (i in (1:length(predictions1))) {
  if (predictions1[i]>cutoff){
    predictions1[i]<- 1}else{
      predictions1[i]<-0
    }}

for (i in (1:length(predictions2))) {
  if (predictions2[i]>cutoff){
    predictions2[i]<- 1}else{
      predictions2[i]<-0
    }}

#------------------------------------Classification report -----------------------------------
class_rpt.model1<-xtabs(~predictions1+test$Survived) #for my test dataset
class_rpt.model1


class_rpt.model2<-xtabs(~predictions2+test$Survived) #for my test dataset
class_rpt.model2

#-------------------------Calculation Of Specificity, Sensitivity For Titanic----------------

sensitivity1<- 83/(83+20)
specificity1<- 139/(139+26)
accuracy1<- (139+83)/(139+83+20+26)
sensitivity1 #80.5%
specificity1 #84.2%
accuracy1    #82.8%

senstivity2 <- 77/(77+26)
specificity2 <- 149/(149+16)
accuracy2<- (149+77)/(149+26+77+16)
senstivity2  #74.7%
specificity2 #90.3%
accuracy2    #84.3%

#---------------------------------------ROC Curve---------------------------------------------
#roc curve for test set
#model 1

roc.test1<- roc(Survived~fitted(train.model1), data= train)
r1<-plot.roc(roc.test1, legacy.axes = T,print.auc = TRUE)
auc(roc.test1)

#model 2

roc.test2<- roc(Survived~fitted(train.model2), data= train)
r2<-plot.roc(roc.test2, legacy.axes = T,print.auc = TRUE)
auc(roc.test2)

#As ROC curve for model 2 occupies greater area than model1, we say that model 2 is better than model 1

#--------------------Plotting ROC curves in one plot----------------------------------------

ggroc(list(ROC.model1=r1, ROC.model2=r2), legacy.axes = T)+geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="black", linetype="dashed") + ggtitle("ROC CURVE")

#------------------------------Hosmer Lemeshow test-----------------------------------------
#The results shows that the fitted model does not show lack of fit

hl<-hoslem.test(train$Survived, fitted(train.model2))
hl

#----------------------------- Plotting Sigmoidal curve of fitted model---------------------

data<-data.frame(prob=train.model2$fitted.values, Sur_Probability=train$Survived)
data<-data[order(data$prob,decreasing = F),]
data$rank<-1:nrow(data)
plot<- ggplot(data=data,aes(x=rank,y=prob)) 
plot<-plot+geom_point(aes(color=Sur_Probability),alpha=0.6,shape=4,stroke=2)
plot+xlab("Observation number")+ ylab("Predicted Probability of Survival")

#------------------ Adding predictions of model 2 to our test dataset---------------

test$Prediction<- predictions2
head(test)

