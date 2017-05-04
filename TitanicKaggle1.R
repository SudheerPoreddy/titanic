rm(list = ls())
library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
rm(list=ls())
getwd()

train<-read.csv("train.csv")
test<-read.csv("test.csv")
full<-bind_rows(train,test)
data1<-bind_cols(train,test)
View(full)
?bind_cols
?rbind

full$Title<-gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Title,full$Sex)
?gsub
rare_title<-c('Dona','Lady','the Countess','Capt','Col','Don','Dr','Major','Rev','Sir','Jonkheer')
full$Title[full$Title=='Mlle']<-'Miss'
full$Title[full$Title=='Mme']<-'Mrs'
full$Title[full$Title%in%rare_title]<-'Rare Title'
table(full$Title,full$Sex)

table(full$Title,full$Sex)
full$Surname<-sapply(full$Name,function(x)strsplit(x,split = '[,.]')[[1]][1])
cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))
full$Fsize<-full$SibSp+full$Parch+1
full$Family<-paste(full$Surname,full$Fsize,sep = '_')
plot(full$Family,full$Survived)
length(full)
length(full$PassengerId)
length(train$PassengerId)

###'''ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
 # geom_bar(stat='count', position='dodge') +
  #scale_x_continuous(breaks=c(1:11)) +
  #labs(x = 'Family Size') +
  #theme_few()'''###
ggplot(full[1:891,],aes(x=Fsize,fill=factor(Survived)))+geom_bar(stat='count',position = 'dodge')+
  scale_x_continuous(breaks=c(0:9))+labs(x='Family Size')+theme_few()



full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

#mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)
mosaicplot(table(full$FsizeD,full$Survived),main = 'family size by survival',shade = T)

full$Cabin

#strsplit(full$Cabin[2], NULL)[[1]]

strsplit(full$Cabin[1],NULL)[1]

full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1]))



sum(is.na(full))
sum(is.na(full$Embarked))
sum(is.na(full$PassengerId))
length(train)
length(train$PassengerId)

names(full)

sum(is.na(full$Fare))
full[1044,]
full$Fare[1044]
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = 'blue', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()
median(full$Fare,na.rm=T)
median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
median(full[full$Pclass==3& full$Embarked=='S',]$Fare,na.rm=T)
sum(is.na(full$Age))
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)
library(randomForest)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)
par(mfrow=c(1,1))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
full$Age<-mice_output$Age
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~sex) + 
  theme_few()
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'


table(full$Child,full$Survived)
full$Mother<-'not mother'
full$Mother[full$Sex=='female'&full$Parch>0&full$Age>18]<-'mother'
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

train <- full[1:891,]
test <- full[892:1309,]

set.seed(100)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         train)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)
plot(rf_model,ylim=c( 0,0.4))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
sum(is.na(test$PassengerId))
sum(!is.na(prediction))

write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

prediction(is.na(prediction))

prediction[is.na(prediction)]<-prediction[10]
class(prediction[is.na(prediction)])
prediction[is.na(prediction)]<-as.numeric(prediction[is.na(prediction)])
sum(is.na(prediction))
 











