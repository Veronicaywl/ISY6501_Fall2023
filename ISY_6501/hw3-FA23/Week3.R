#clean the R environment
rm(list = ls())
set.seed(10)

#load data into dataframe
crimedf <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)
head(crimedf)

#load library
#install.packages("outliers")
library(outliers)
library(ggplot2)
library(tidyverse)

#H0:There is no outlier in crime data.

#use grubbs.test to check the data set
grubbs.test(crimedf$Crime)

#As the result shown, the p-value = 0.07887 > alpha = 0.05. In this case, we don't have strong evidence 
#using type = 10 to test for 1 outlier. test the maximum value
grubbs.test(crimedf$Crime, type = 10, opposite = FALSE, two.sided = FALSE)
#test the minimum value
grubbs.test(crimedf$Crime, type = 10, opposite = TRUE, two.sided = FALSE)

#using type =11 to test for 2 outliers on opposite tails.
grubbs.test(crimedf$Crime, type = 11, opposite = FALSE, two.sided = FALSE)
#test the minimum value
grubbs.test(crimedf$Crime, type = 11, opposite = TRUE, two.sided = FALSE)

#shapiro.test(crimedf$Crime)

#qqnorm(crimedf$Crime)
#qqline(crimedf$Crime)

#plot box graph to visualize finding
boxplot(crimedf$Crime,
        main = "Outlier for Crime in U.S. Crime",
        xlab = "Population Density",
        ylab = "Crime Committed",
        horizontal = FALSE)

#install.packages("lubridate")
library(repr)
library(reshape)

rm(list = ls())
temp <- read.table("temps.txt", stringsAsFactors = FALSE, header = TRUE)
head(temp)

#first approach we look at each day from 1996-2015. To observe the date when weather starts cooling down.

# average the temperature for each day across the years
avg_date_temp <- rowMeans(temp[c(2:length(temp))], na.rm=T)
# compute the mean of the average temperature by the same date from 1996-2015.
mu <- mean(avg_date_temp)
#find the standard deviation 
std_temp <- sd(avg_date_temp)
# compute the difference between the mean of the time series and each "day"
#date_mu <- avg_date_temp - mu
# set C = 4. I've keep changing C from 1-5. The best C for plot a precise graph is 4.
C <- std_temp

# set threshold T. Based on documentations, we usually set the threshold 4 or 5 times standard deviation.
T <- 4*std_temp

# create an empty column to store the data 
temp[,"St"]<-NA

# starts loop from 0.

temp[1,"St"]<-0
for(i in 1:nrow(temp)){
  
  temp[i,"St"]<-max(0,(temp[i-1,"St"]+mu-avg_date_temp[i]-C))
  
  
}

temp$St

cat("The day a change in trend is detected is:",temp[which(temp$St>T),"DAY"][1])

#change data type for "day" as month/date
temp[,"Date"]<-as.Date(temp[,"DAY"],"%d-%B")
temp[,"Date"]<-format(temp[,"Date"],format="%m/%d")

#plot the control graph for visualization.
options(repr.plot.width=10, repr.plot.height=5)
ggplot(data = temp, aes(x = Date, y =`St`,group=2)) + 
  geom_line()+
  geom_hline(yintercept=T,color="red")+
  
  scale_x_discrete(breaks = unique(temp$Date)[seq(1,123,10)])+
  xlab("Days") +
  ylab("CUSUM") +
  ggtitle("CUSUM Chart for July-October Daily-high Temperature for Atlanta 1996-2015")+
  theme(plot.title = element_text(hjust = 0.5))

#As we can observed from the graph. On 10/18 the temperature starts to cool down. 


#creating a data frame to store the cusum values for each year instead of same day on every year.
cusum_temp<-data.frame(matrix(nrow=nrow(temp),ncol=length(1996:2015)+1))

#assigning columns names to cusum data frame
colnames(cusum_temp)<-colnames(temp[,1:21])

#converting Day into Date that the loop can process easier.
cusum_temp$DAY<-temp$Date

# starts from zero
#calculating cusum values for each year
for(y in 2:ncol(cusum_temp)){
  cusum_temp[1,y]<-0 #initial St value for each column,set to zero
  mu<-mean(temp[,y]) #mean of each sample space(each year's observations)
  std<-sd(temp[,y]) #sd of each sample,also used as allowable slack
  threshold<-5*std #using 5 sd as threshold value,different T for each year 
  change<-NULL # to store dates with St over threshold,first value:first day change detected
  
  for(i in 2:nrow(cusum_temp)){
    cusum_temp[i,y]<-max(0,cusum_temp[i-1,y]+(mu-temp[i,y]-std))
    if (cusum_temp[i,y]>=threshold){
      change<-append(change,cusum_temp[i,y])}}
  cat("In ",colnames(cusum_temp[y])," first day of Fall started on",
      cusum_temp[which(cusum_temp[,y]==change[1]),"DAY"],"\n")
}
