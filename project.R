rm(list = ls(all=T))
setwd("C:\Users\DELL\Desktop\job reccomendation\Job recommendation engine")

# Reading dataset Train data into R Environment
train = read.csv("TrainData.csv",header = T)
length(unique(train$ApplicantID))

length(unique(train$JobID))

# Formation of Job/Applicant Matrix using dcast funtion(reshape2)

library(reshape2)
library(base)

# forming a job application matrix with jobID as rows
jbapp_matrix = acast(train,train$ApplicantID~train$JobID, value.var = "Rating")

applicants = rownames(jbapp_matrix)
Jobs = colnames(jbapp_matrix)
str(applicants)

# checking wheather all the values are feed into matrix
nrow(train)==(length(unique(train$JobID))*length(unique(train$ApplicantID)))- sum(is.na(jbapp_matrix))

sum(is.na(jbapp_matrix)) # no of NA values
sum(is.na(jbapp_matrix))/(length(unique(train$JobID))*length(unique(train$ApplicantID)))

# This is highly sparse with only 0.04% of non zero values

# imputing NA values with zero

jbapp_matrix[is.na(jbapp_matrix)]=0

jbapp_matrix =as.matrix(jbapp_matrix)


# Buildind svd on jbapp_matrix

SVDmodel = svd(jbapp_matrix)

eiganvalues = SVDmodel$d

cumsum = cumsum((eiganvalues*100)/sum(eiganvalues))
length(cumsum[cumsum<90])
# 90% of variation in data is explained top 2020 eiganvalues and eiganvectors 

SVDmodel = svd(jbapp_matrix, nu =2020, nv= 2020)
eiganvalues_2 = SVDmodel$d
u = (SVDmodel$u) # applicants vs eiganvectors matrix.
v = (SVDmodel$v) # jobs vs eiganvector matrix. 
v = as.matrix(v)
rownames(u) = applicants
rownames(v)= Jobs

dim(u)
dim(v)
s_2 = diag(SVDmodel$d[1:2020])
dim(s_2)
vt = t(v)
# as "u" and "v" are unitory matrix so 
# i am proving the strength to the eiganvectors 
# by multipling "u" with diagonal matrix and
# "v" with diagonal matrix
# of u and v and buliding new matrix
x= u%*%s_2 # "u" with diagonal matrix
y= v%*%s_2 # "v" with diagonal matrix
dim(y)
dim(x)

# making each row a unit vector 
# so as to make a.b(dot product) = cos(a,b)

for (i in 1:nrow(x)){
  x[i,]=(x[i,]/sqrt(sum(x[i,]*x[i,])))
}

for (i in 1:nrow(y)){
  y[i,]=(y[i,]/sqrt(sum(y[i,]*y[i,])))
}
# as now dot product = cosine of angle below matrix contains cosine values in it
# i could have cosine function in lsa but that taking lot of time
similarityapp = x%*%t(x) 
similarityjobs = y%*%t(y)
dim(similarityjobs)
dim(similarityapp)

# getting colunm no. of top 10 similar user for all users
simapplicants= data.frame()

for(i in 1:nrow(u)){
  simapplicants = rbind(simapplicants,order(similarityapp[i,],decreasing = TRUE)[2:11])
}
rownames(simapplicants)=applicants

# getting colunm no. of top 10 similar jobs for all jobs
simjobs = data.frame()
for(i in 1:nrow(v)){
  simjobs = rbind(simjobs,order(similarityjobs[i,],decreasing = TRUE)[2:11])
}

rownames(simjobs)=Jobs


#replacing column no. with application Id  for all 10 similar users
g=vector()
for(i in simapplicants){
  g=c(g,i)
}

x=c(1:length(g))
z= 3027*(1:10)
a= x%%3027
a[z]= 3027
b=((x-a)/3027)+1

for(i in x){
  simapplicants[a[i],b[i]]= applicants[g[i]]
}

#replacing column no. with job Id  for all 10 similar jobs
j=vector()
for(i in simjobs){
  j=c(j,i)  
}

y=c(1:(nrow(simjobs)*10))
z=6270*(1:10)
c= y%%6270
c[z]=6270
h=((y-c)/6270)+1
for(i in y){
  simjobs[c[i],h[i]]= Jobs[j[i]]
}
#write.csv(simapplicants,file="simapplicants.csv")
#write.csv(simjobs,file="simjobs.csv")

#simapplicants=read.csv(simapplicants)
#simjobs=read.csv(simjobs)

#Loading the Main Job Views files
main_job = read.csv("MainJobViews.csv", header = TRUE)

#Selecting only Applicant ID,Job ID and Job Applied
main_job = main_job[,c(2,3,5,9)]
jobs= data.frame()

top10App = simapplicants[2,] 
#getting job views by those 10 similar users

for(j in top10App ){
  top_jobs <- subset(main_job, main_job$Applicant.ID == j)
  jobs= rbind(jobs,top_jobs)
}

View(jobs)

##Unique jobs applied by those 10 ppl

Unique_JobID = unique(jobs$Job.ID)
str(Unique_JobID)
Unique_JobID = as.data.frame(Unique_JobID)
colnames(Unique_JobID)= 'JOB_ID'
# Adding the rownames as one columns so that 
#we don't missout the viewed jobs of similar users
simjobs =cbind(rownames(simjobs),simjobs)
recom_1= data.frame()
#pulling out the similar jobs of those unique jobs
for(i in Unique_JobID$JOB_ID){
  recom_1 = rbind(recom_1, simjobs[which(simjobs[,1]==i),])}
View(recom_1)
# forming a vector of jobs.
jobs_1 = vector()
for(i in recom_1){jobs_1=c(jobs_1,i)}

# Taking only unique jobs 
uniq_jobs_1 = unique(jobs_1)

#Getting the cityname of user from  MainInfo 

maininfo  = read.csv("Main_Info.csv")
maininfo = subset(maininfo, select = c(Applicant.ID, City))
maininfo$City = as.character(maininfo$City)
applicant = subset(maininfo, maininfo$Applicant.ID == applicants[2])

AppcityName = applicant$City
AppcityName = as.vector(AppcityName)
AppcityName

#Getting the the unique jobs from the Combined jobs

Combined_Jobs_Final = read.csv("Combined_Jobs_Final.csv")
str(Combined_Jobs_Final)
Combined_Jobs_Final$City = as.character(Combined_Jobs_Final$City)
# taking only job.ID, city, state & postions as attributes
Combined_Jobs_Final = subset(Combined_Jobs_Final,  select= c(Job.ID,City, State.Name, Position))
sum(is.na(Combined_Jobs_Final))
# omitting the na values
Combined_Jobs_Final=na.omit(Combined_Jobs_Final)
uniq_jobs_1 = as.integer(uniq_jobs_1)
# getting the information of those unique jobs
reco_1 = data.frame()
for(i in uniq_jobs_1){
  reco = subset(Combined_Jobs_Final, Combined_Jobs_Final[,1]== i)
  reco_1 = rbind(reco_1,reco)
}

# filtering the jobs based on the applicant location.
View(reco_1)
reco_1 = reco_1[which(reco_1$City=="San Francisco"),]

Positions_Of_Interest = read.csv("Positions_Of_Interest")
Positions_Of_Interest = Positions_Of_Interest[,-c(3,4)]
applicants = as.integer(applicants)
Positions_Of_Interest_1 = Positions_Of_Interest[which(Positions_Of_Interest$Applicant.ID == applicants[2]),]
