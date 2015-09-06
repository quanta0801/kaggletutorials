setwd("~/GitHub/kaggletutorials/titanic/")
library(mice)
library(randomForest)

train<-read.csv("../input/train.csv",na.strings=c('NA',''),stringsAsFactors=F)
test<-read.csv("../input/test.csv",na.strings=c('NA',''),stringsAsFactors=F)

check.missing<-function(x) return(paste0(round(sum(is.na(x))/length(x),4)*100,'%'))
data.frame(sapply(train,check.missing))
data.frame(sapply(test,check.missing))

#combine train/test data for pre-processing
train$Cat<-'train'
test$Cat<-'test'
test$Survived<-NA
full<-rbind(train,test)

#Embarked
full$Embarked[is.na(full$Embarked)]<-'S'

#Extract Title from Name
full$Title = sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][2])
full$Title<-gsub(' ','',full$Title)
full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#Adding FamilySize
full$FamilySize<-full$Parch+full$SibSp+1

#Perform Imputation to remove NAs
set.seed(144)
vars.for.imputation = setdiff(names(full), "Survived")
imputed = complete(mice(full[vars.for.imputation]))
full[vars.for.imputation] = imputed

#Adding Mother
full$Mother<-0
full$Mother[full$Sex=='female' & full$Parch>0 & full$Age>18 & full$Title!='Miss']<-1
#Adding Child
full$Child<-0
full$Child[full$Parch>0 & full$Age<=18]<- 1

#FamilyId2
Surname<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][1])
FamilyId<-paste0(full$FamilySize,Surname)
full$FamilyId<-factor(FamilyId)
Family<-data.frame(table(FamilyId))
SmallFamily<-Family$FamilyId[Family$Freq<=2]
FamilyId[FamilyId %in% SmallFamily]<-'Small'
full$FamilyId2<-factor(FamilyId)

#Exact Deck from Cabin number
full$Deck<-sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1])

#Excat Position from Cabin number
full$CabinNum<-sapply(full$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
full$num<-as.numeric(full$CabinNum)
num<-full$num[!is.na(full$num)]
Pos<-kmeans(num,3)
full$CabinPos[!is.na(full$num)]<-Pos$cluster
full$CabinPos<-factor(full$CabinPos)
levels(full$CabinPos)<-c('Front','End','Middle')
full$num<-NULL

full<-transform(full,
                Pclass=factor(Pclass),
                Sex=factor(Sex),
                Embarked=factor(Embarked),
                Title=factor(Title),
                Mother=factor(Mother),
                Child=factor(Child),
                FamilyId2=factor(FamilyId2),
                Deck=factor(Deck)
)

#split train/test data
train<-full[full$Cat=='train',]
test<-full[full$Cat=='test',]
train$Survived<-factor(train$Survived)

#rf.fit = randomForest(Survived ~ Pclass + Age + Sex + Title + Mother + Child + Fare, data=train, ntree = 100, nodesize = 25)
#test$Survived = predict(rf.fit, test)

library(party)
#cf.fit<-cforest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,data=train,controls=cforest_unbiased(ntree=2000, mtry=3))
cf.fit<-cforest(train$Survived~FamilyId2+CabinPos+Deck+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child+Deck,data=train,controls=cforest_unbiased(ntree=2000, mtry=3))
test$Survived = predict(cf.fit, test, OOB=TRUE,type='response')

submission<-test[,1:2]
write.csv(submission,'submission.csv',row.names=F)