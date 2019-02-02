#####2018 Pinewood Derby Stats####
#install and load packages
install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

#Load File
tempderby19 <- read.csv("Pack90Derby-Results.csv")
#convert to Data Frame dplyr package
derby19 <- tbl_df(tempderby19)
#remove old df
rm(tempderby19)


#Add full Name to data, Heat Order (dependnet on 6 runs by each racer), combined Heat order and Lane
derby19 <- mutate(derby19, FullName = paste(FirstName, LastName, sep = " "), 
                       HeatAdj = rep(1:6,times = nrow(derby19)/6), HeatAdjLane = paste0(HeatAdj, Lane))


#subset out class we don't need
derby19scout <- derby19[derby19$Class %in% c("Lion", "Tiger", "Wolf", "Bear", "Webelos", "Arrow of Light"),]

#Take out Ian
derby19scout <- derby19scout[derby19scout$LastName != "Mangen",]


#Fix Data Danny Hensley
danny <- derby19scout[derby19scout$FullName == "Daniel Hensley","FinishTime"]
mean(danny$FinishTime[2:6])
derby19scout$FinishTime[211]= mean(danny$FinishTime[2:6])
derby19scout$FinishTime[211]

#Fix Data Gage Husband
Cage <- derby19scout[derby19scout$FullName == "Gage Husband","FinishTime"]
mean(Cage$FinishTime[c(1:4,6)])
derby19scout$FinishTime[59]= mean(Cage$FinishTime[c(1:4,6)])
derby19scout$FinishTime[59]

#groupby name, Summarize Data, Arrange by Fastest Time
derby19byname <- group_by(derby19scout, FullName, Class)
derby19Summary <- summarize(derby19byname, AverageFinishTime = mean(FinishTime), FastestTime = min(FinishTime), StandardDeviation = sd(FinishTime))
derby19Summary <- arrange(derby19Summary, AverageFinishTime)

#groupby class, Summarize Data, Arrange by Fastest Time
derby19byClass <- group_by(derby19scout, Class)
derby19SummaryClass <- summarize(derby19byClass, AverageFinishTime = mean(FinishTime), FastestTime = min(FinishTime), StandardDeviation = sd(FinishTime))
derby19SummaryClass <- arrange(derby19SummaryClass, AverageFinishTime)

#groupby lane and heat adg, Summarize Data, SpreadLane to columns & HeatAdjust by rows
derby19byLane <- group_by(derby19scout, Lane, HeatAdj)
derby19SummaryLane <- summarize(derby19byLane, AverageFinishTime = mean(FinishTime))
spread(derby19SummaryLane, Lane, AverageFinishTime)


#loop created to produce rank for individual. Loop is dependent on each racer having 6 runs
#nrow(derby19scout)/6)
count <- 1
FinishTimeRank <- vector('numeric')
temp2 <- while(count<= (nrow(derby19scout)/6)){
  newcount <- count * 6
  temp <- derby19scout[(newcount-5):newcount, "FinishTime"] 
  temprank <- rank(temp$FinishTime)
  FinishTimeRank <- append(FinishTimeRank, temprank)
  count <- count+1}

#bind column to DF
derby19scout <- cbind(derby19scout, FinishTimeRank)
#summarize time rank by Lane
derby19scout %>% group_by(Lane) %>%  summarize(RankTime = mean(FinishTimeRank))
#summarize time rank by HeatAdj
derby19scout %>% group_by(HeatAdj) %>%  summarize(RankTime = mean(FinishTimeRank))

#summarize time rank by HeatAdjLane
derby19scout %>% group_by(HeatAdjLane) %>%  summarize(RankTime = mean(FinishTimeRank))


#summarize Stats by Class - Condensed Version
#derby19scout %>% group_by(Class) %>% summarize(AverageFinishTime = mean(FinishTime), FastestTime = min(FinishTime), StandardDeviation = sd(FinishTime)) %>% arrange(AverageFinishTime)


#Example test without loop
#temp <- derby19scout[1:6, "FinishTime"]
#rank(-temp$FinishTime)


#####PLOTS####
#Create plot to see if it works, then widen plotting area to get better picture, Impotant on the more complicated plots

#Summary of results broken out by class, color by person, with finish time
qplot(data = derby19scout, HeatAdj, FinishTime, geom = c("point"), aes(group = HeatAdj, color = LastName), 
      facets = .~ Class, ylim = c(3.1,3.6), xlab = "Heat (1:6)", ylab = "Finish Time", main = "2019 Pack 90 Pinewood Derby Results")

#Summary of scout NOT LIMITED
qplot(data = derby19scout, HeatAdj, FinishTime, geom = c("point"), aes(group = HeatAdj, color = FullName), 
      facets = .~ Class, xlab = "Heat (1:6)", ylab = "Finish Time", main = "2019 Pack 90 Pinewood Derby Results")

#Summary of ALL Participants
qplot(data = derby19, HeatAdj, FinishTime, geom = c("point"), aes(group = HeatAdj, color = FullName), 
      facets = .~ Class, xlab = "Heat (1:6)", ylab = "Finish Time", main = "2019 Pack 90 Pinewood Derby Results", ylim = c(3.1,3.8))

#Summary of ALL Particiants LIMITED
qplot(data = derby19, HeatAdj, FinishTime, geom = c("point"), aes(group = HeatAdj, color = FullName), 
      facets = .~ Class, xlab = "Heat (1:6)", ylab = "Finish Time", main = "2019 Pack 90 Pinewood Derby Results", ylim = c(3.1,3.8))


#Plot by Time, Heat Adj, colored by class
qplot(data = derby19scout, HeatAdj, FinishTime, geom = c("point"), aes(group = HeatAdj, color = Class), ylim = c(3.1,3.6))

#Box plot of same thing as above
qplot(data = derby19scout, HeatAdj, FinishTime, geom = c("boxplot"), aes(group = HeatAdj, color = Class), ylim = c(3.1,3.6))

#Box plot of Lane and Finish time LIMITED
qplot(data = derby19scout, Lane, FinishTime, geom = c("boxplot"), aes(group = Lane, color = Class), ylim = c(3.1, 3.6))

