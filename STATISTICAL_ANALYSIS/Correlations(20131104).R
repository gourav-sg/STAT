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
(iv - miv)
(dv1 - mdv1)
(iv - miv)*(dv1 - mdv1)

#entering the values in a data frame so that we can see it clearly
#calculating covariance
cvdf <- data.frame(iv, dv1, A=cbind(iv - miv),B=cbind(dv1 - mdv1), 
           C=(iv - miv)*(dv1 - mdv1),
           D=sum((iv - miv)*(dv1 - mdv1)),
           E=sum((iv - miv)*(dv1 - mdv1))/(n-1)
           )
#seeing first five rows of the data frame 
cvdf[c(1:5),]


cvdf$E
(sddv1*sdiv)

correlation <- cvdf$E / (sddv1*sdiv)

correlation

#correcting this value as a vector
correlation <- cvdf$E[1]/ (sddv1*sdiv)

correlation

#understanding the meaning graphically

par(mfrow=c(1,1))
plot(iv[c(1:3)], col="red")

#the graph considers the value as Y axis the 
#number of observation as X axis

par(mfrow=c(1,2))
#by default the graph adjusts the y axis to start from 20
plot(iv,col="red")
#therefore we use range
plot(iv,col="red",xlim=range(0:30),ylim=range(0:max(iv)))

#plotting just the mean
plot(data.frame(dv1,mdv1)[c(1:30),c(2)],type="l", col="blue")


#learning to stitch the above together into a single graph

#just entering both the variables does not help as shown below
plot(iv,dv1)

#stitching them together using both the variables also does not help because it
# takes the range of x and y axis from above plot
par(mfrow=c(1,1))
plot(iv, dv1, type="n")
points(iv,pch=1)
points(dv1,pch=8)

#just entering one variable does not solve the purpose because most of 
# the values of iv are discarded when they are outside the max and min 
# value range of dv1
plot(dv1, type="n")
points(dv1,pch=8)
points(iv,pch=1)




par(mfrow=c(1,1))
plot(iv,dv1, type="n", xlim=range(0:length(iv)), ylim=range(-50:max(max(iv),max(dv1))))
points(dv1,pch=8)
points(iv,pch=1,col="red")
points(iv,dv1,pch=3, col="blue")
#getting the line for mean for dv1
points(data.frame(dv1,mdv1)[c(1:30),c(2)],type="l")
#getting the line for mean for dv1
points(data.frame(iv,miv)[c(1:30),c(2)],type="l",col="red")

points(data.frame(iv,(iv - miv))[c(1:30),c(2)],type="l",col="red", pch=10)
points(data.frame(dv1,(dv1 - mdv1))[c(1:30),c(2)],type="l", pch=10)

points(data.frame(dv1,((dv1 - mdv1)*(iv - miv))/(length(iv) - 1))[c(1:30),c(2)],type="l", col="blue", pch=10)


       

#committing to github
system("git add ./Correlations\\(20131104\\).R")
system("git add ./statslab08.csv")
system("git commit ./Correlations\\(20131104\\).R -m \"Committing\"")
system("git commit ./statslab08.csv")
system("git push")
system("git status")




