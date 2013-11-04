#GSengupta
#Mon Nov  4 20:34:52 GMT 2013
#tutorial: http://www.youtube.com/watch?v=mxf_RtJFQUQ
#data source http://wknapp.com/class/stats/fall12/statslab08.csv


#change working directory
system("pwd")
setwd("./STAT//STATISTICAL_ANALYSIS")
system("pwd")

#download the file locally
system("wget -c http://wknapp.com/class/stats/fall12/statslab08.csv")

#load the file 
data=read.csv(file="statslab08.csv",row.names=1)

#view data in RStudio
View(data)

#view description of data frame
str(data)

#read the first 5 lines of the data frame, this is otherwise available in RStudio
#as well
data[c(1:10),]

#following command gives errors
iv
#but the following shows values
data$iv

#we attach the data frame so that we can address its columns with names directly
attach(data)

#now iv works like data$iv
iv

#showing value of list as vector
cbind(iv)

#showing the first 10 values as vector
cbind(cbind(iv)[c(1:10),])

#calculating correlation
#NOTE: correlation does not imply casuality
n <- length(iv)
n

miv <- mean(iv)
miv

#standard deviation using function
sd(iv)
#standard deviation using formula
sdiv <- sqrt(sum((iv - miv)^2)/(n-1))
sdiv

mdv1 <- mean(dv1)
mdv1

#standard deviation using function
sd(dv1)
#standard deviation using formula
sddv1 <- sqrt(sum((dv1 - mdv1)^2)/(n-1))
sddv1

#plotting and checking what it all means




#committing to github
system("git add ./Correlations\\(20131104\\).R")
system("git add ./statslab08.csv")
system("git commit ./Correlations\\(20131104\\).R -m \"Committing\"")
system("git commit ./statslab08.csv")
system("git push")
system("git status")




