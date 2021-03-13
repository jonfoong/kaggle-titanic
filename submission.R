library(caret)
library(tidyverse)
library(randomForest)
library(party)

#combine train and test
train_dat<-read_csv('train.csv')
test_dat <- read_csv('test.csv') %>% 
  mutate(Survived=NA)
combi<-rbind(train_dat,test_dat)

#extract title
title<-combi$Name %>% str_extract('\\s\\w+\\.') %>% str_remove(' ') 
title<-title %>% str_replace('Ms.','Miss.') %>% as.factor()

#extract last name
famname<-combi$Name %>% str_extract('\\w+,') %>% str_remove(',')

#insert title into dataset
combi <- combi %>% mutate(Title=title,
                          Familysize=(SibSp+Parch+1)) 

#extract cabin
combi$Cabin[which((combi$Cabin %>% str_extract('\\w')) %in% c('A','B','C'))]<-'ABC'
combi$Cabin[which((combi$Cabin %>% str_extract('\\w')) %in% c('D','E'))]<-'DE'
combi$Cabin[which((combi$Cabin %>% str_extract('\\w')) %in% c('F','G'))]<-'FG'
combi<-combi %>% mutate(Cabin=factor(ifelse(is.na(Cabin),'M',Cabin)))

#generate family IDs
FamID<-paste(famname,combi$Familysize)
FamID<-ifelse(str_detect(FamID,'\\w+\\s1|2'),'small',FamID) # family size 2 or less is small

combi <- combi %>% mutate(FamID=factor(FamID),
                          Sex=factor(Sex)) %>% 
  select(-PassengerId,-Name,-Ticket)

#clean family IDs
famIDs <- data.frame(table(combi$FamID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamID[combi$FamID %in% famIDs$Var1] <- 'small'
combi$FamID <- factor(combi$FamID)

#create mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#insert median age for masters & median fare & mode for Embarked
combi <- combi %>% mutate(Age = ifelse(is.na(Age)&Title=='Master.', median(Age, na.rm = TRUE), Age), # NA age to median age
                          Fare = ifelse(is.na(Fare), median(Fare, na.rm = TRUE), Fare),
                          Embarked = factor(ifelse(is.na(Embarked),getmode(Embarked),Embarked)))

#reduce levels of 'TItle'
combi <- combi %>%
  mutate(Title = factor(ifelse(str_detect(Title,"Master."),as.character(Title),ifelse(str_detect(Title,"Mr."),as.character(Title),ifelse(str_detect(Title,"Mrs."),as.character(Title),ifelse(str_detect(Title,"Miss."),as.character(Title),"OTHER"))))))

#generating dummy vars from categorical
#DummyRF_mod <- dummyVars(~Title + Embarked,
#                         data = combi,
#                        levelsOnly = FALSE)
#combi<-cbind(combi,as_tibble(predict(DummyRF_mod,newdata = combi))) %>%
# select(-Title,-Embarked) %>% as_tibble()

#impute data using rfimpute
impute<-preProcess(combi,method='bagImpute')
combi<-predict(impute,combi)

#reseparate dataset into train and test
train <- combi[1:891,] %>% mutate(Survived=factor(Survived))
test <- combi[892:1309,] %>% select(-Survived)

#train data using randomforest
train_rf <- train(Survived ~ .,
                  data = train,
                  method = "rf",
                  ntree = 500,
                  tuneGrid = data.frame(mtry = 2:5))
train_rf$bestTune #check which mtry value is best

# predict data on test set
Survived<-predict(train_rf,test) %>% as.numeric()
Survived<-ifelse(Survived==1,0,1)

predictions<-cbind(test,Survived) %>% 
  mutate(Survived=ifelse(Sex=='male' & Age>20 & Survived==1,0,Survived))%>%
  as_tibble()
predictions %>% select(Sex,Age,Survived) %>% filter(Sex=='male' & Survived==1)

predictions<-cbind(test_dat$PassengerId,Survived) %>% as_tibble()

colnames(predictions)<-c('PassengerId','Survived')
write_csv(predictions,'predictions.csv')


#train data using conditional forest
train_cf<-cforest(Survived~.,data=train,controls=cforest_unbiased(ntree=2000, mtry=3))
Survived<-predict(train_cf, newdata=test, OOB = T, type='response') %>% as.numeric()
Survived<-ifelse(Survived==1,0,1)

predictions<-cbind(test,Survived) %>% 
  
  as_tibble()
predictions %>% select(Sex,Age,Survived) %>% filter(Sex=='male' & Survived==1)

predictions<-cbind(test_dat$PassengerId,Survived) %>% as_tibble()

colnames(predictions)<-c('PassengerId','Survived')
write_csv(predictions,'predictions.csv')
