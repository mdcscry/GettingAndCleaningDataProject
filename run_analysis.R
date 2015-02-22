library(dplyr)

#-----------------------------DATA READ BEGIN

activity_lable <- read.csv("./activity_labels.txt",sep="",strip.white=TRUE,head=FALSE)
features <- read.csv("./features.txt",sep="",strip.white=TRUE,head=FALSE)
X_train <- read.csv("./train/X_train.txt",sep="",strip.white=TRUE,head=F)
Y_train <- read.csv("./train/Y_train.txt",sep="",strip.white=TRUE,head=F)
X_test <- read.csv("./test/X_test.txt",sep="",strip.white=TRUE,head=F)
Y_test <- read.csv("./test/Y_test.txt",sep="",strip.white=TRUE,head=F)
subject_train <- read.csv("./train/subject_train.txt",sep="",strip.white=TRUE,head=F)
subject_test <- read.csv("./test/subject_test.txt",sep="",strip.white=TRUE,head=F)

#-----------------------------DATA READ END


#-----------------------------CLEANSE FEATURES BEGIN
feature_names_cleansed <- as.data.frame(gsub("bodybody","body",
                                             gsub("\\.+","",
                                                  make.names(
                                                    gsub("-","_",features[,2]),allow_=TRUE))))

names(feature_names_cleansed)[1] <- "test"; 
select_columns <- as.vector((grep("mean|std", feature_names_cleansed[,1],ignore.case=TRUE)))
#-----------------------------CLEANSE FEATURES END


#-----------------------------JOIN DATASETS BEGIN

X_obs <- rbind(X_train,X_test)   #Join the X observations
  for (i in 1:nrow(feature_names_cleansed)) {  # add data lables
    names(X_obs)[i]<-as.character(feature_names_cleansed[i,1])
  }

Y_act <- inner_join(rbind(Y_train,Y_test),activity_lable, by="V1") # union the y observations and join activity lable(sic)
Y_subj <-rbind(subject_train,subject_test) ; # union the subject sets
Y_combine <- cbind(Y_subj,Y_act[,2]); names(Y_combine)[1] <- "Subject" ; names(Y_combine)[2] <- "Activity";

final <- cbind(Y_combine[,1:2],X_obs[,select_columns])

#-----------------------------JOIN DATASETS END


#---------------------------- Summarize Final Dataset Begin

tidy_data <- aggregate(x=final, by = list(Subject=final$Subject,Activity=final$Activity), FUN="mean")
tidy_data <- tidy_data[,-3:-4]
write.table(tidy_data,"./tidy_data.csv",col.names = FALSE,sep=",")
#---------------------------- Summarize Final Dataset END


















        






