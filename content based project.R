rm(list = ls(all=T))
setwd("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine")

Combined_Jobs_Final <- read.csv("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine/Combined_Jobs_Final.csv", comment.char="#")
Combined_Jobs_Final = Combined_Jobs_Final[,-c(2,3,4,10,11,12,13,22,23)]

sum(is.na(Combined_Jobs_Final$Education.Required))/nrow(Combined_Jobs_Final)


sum(is.na(Combined_Jobs_Final$Company))/nrow(Combined_Jobs_Final)
sum(is.na(Combined_Jobs_Final$City))/nrow(Combined_Jobs_Final)
sum(is.na(Combined_Jobs_Final$State.Name))/nrow(Combined_Jobs_Final)
sum(is.na(Combined_Jobs_Final$Industry))/nrow(Combined_Jobs_Final)
sum(is.na(Combined_Jobs_Final$Job.Description))/nrow(Combined_Jobs_Final)
sum(is.na(Combined_Jobs_Final$Requirements))/nrow(Combined_Jobs_Final)
sum(is.na(Combined_Jobs_Final$Salary))/nrow(Combined_Jobs_Final)
sum(is.na(Combined_Jobs_Final$Listing.Start))/nrow(Combined_Jobs_Final)
sum(is.na(Combined_Jobs_Final$Education.Required))/nrow(Combined_Jobs_Final)

Combined_Jobs_Final = Combined_Jobs_Final[,-c(7,9,10,11,12)]

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)   

library(tm)   
docs <- Corpus(DataframeSource(Combined_Jobs_Final))

inspect(docs)

docs <- tm_map(docs, removePunctuation)   
# inspect(docs[3]) # Check to see if it worked.
inspect(docs[3])
for(j in seq(docs))   
{ docs[[j]]  <-gsub("â", " ", docs[[j]])
  docs[[j]]  <-gsub("-", " ", docs[[j]])
  docs[[j]] <- gsub("/", " ", docs[[j]])   
  docs[[j]] <- gsub("@", " ", docs[[j]])   
  docs[[j]] <- gsub("\\|", " ", docs[[j]])   
}   
# inspect(docs[1]) # You can check a document (in this case the first) to see if it worked.
docs <- tm_map(docs, tolower)   
# inspect(docs[3]) # Check to see if it worked. 
# For a list of the stopwords, see:   
# length(stopwords("english"))   
# stopwords("english")   
docs <- tm_map(docs, removeWords, stopwords("english"))   
# inspect(docs[3]) # Check to see if it worked. 
library(SnowballC)   
docs <- tm_map(docs, stemDocument)   
# inspect(docs[3]) # Check to see if it worked
docs <- tm_map(docs, stripWhitespace)   
# inspect(docs[3]) # Check to see if it worked.  
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)   
dtm[1,]

y=t(dtm)
















Credentials <- read.csv("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine/Credentials.csv")
View(Credentials)
Credentials = Credentials[,-c(3,4)]# removing un wanted attributes
sum(is.na(Credentials))

Education <- read.csv("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine/Education.csv")
View(Education)
Education = Education[,-c(4,6,7)]# removing un wanted attributes
sum(is.na(Education))

Experience <- read.csv("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine/Experience.csv")
View(Experience)
Experience = Experience[,-c(4,5,6,10,11,12,13)]# removing un wanted attributes

Interests <- read.csv("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine/Interests.csv")
Interests= Interests[,-c(3,4)]# removing un wanted attributes
View(Interests)


Leadership <- read.csv("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine/Leadership.csv")
Leadership= Leadership[,-c(3,4)]# removing un wanted attributes
View(Leadership)

Positions_Of_Interest <- read.csv("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine/Positions_Of_Interest.csv")
Positions_Of_Interest = Positions_Of_Interest[,-c(3,4)]# removing un wanted attributes
View(Positions_Of_Interest)


Main_Info <- read.csv("C:/Users/DELL/Desktop/job reccomendation/Job recommendation engine/Main_Info.csv")
Main_Info = Main_Info[,-c(5,16,17)]# removing un wanted attributes
View(Main_Info)

combined =Reduce(function(x,y) merge(x,y,by="Applicant.ID"), list(Experience,Interests,Leadership,Main_Info,Positions_Of_Interest,Credentials,Education))
sum(is.na(combined))

length(unique (Education$Applicant.ID))
length(unique (combined$Applicant.ID))





