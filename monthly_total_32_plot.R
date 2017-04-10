library(ggplot2)
library(scales)
setwd('C:/Users/Lily/Documents/GA/R/report/2017/')
tit <- "March"

mydata <- read.csv(paste("Monthly/", tit, "/total_dat/Australia.csv", sep=""), header = T)
head(mydata)
mydata$date <- as.Date(as.character(mydata$date), format="%Y%m%d")

multiplot<- function(cname){
  mydata <- read.csv(paste("Monthly/", tit, "/total_dat/", cname,".csv", sep=""))
  mydata$date <- as.Date(as.character(mydata$date), format="%Y%m%d")
  head(mydata)  
  #p <- ggplot(mydata, aes(x=mydata$date,y=mydata$sessions))
  #res <- c(sessions=mydata$sessions, country=cname, sum=S)
  return(mydata$sessions)
}
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")

data_set1 <- data.frame(lapply(clist, multiplot))
colnames(data_set1) <- clist

library(reshape2)
dataset_T <- data.frame(data_set1)
dataset_T$date <- mydata$date
mdat <- melt(dataset_T, id="date")
head(mdat)
colnames(mdat) <- c("date","country","sessions")  
ggplot(mdat, aes(date, sessions, colour = country))+#, linetype=country)) + 
  geom_line()
  #scale_colour_manual(values = c("#FF0000", "#0000FF", "#00FF00"))
###
SUM <- colSums(data.frame(data_set1))
top10 <- data.frame(SUM[order(-SUM)][1:20])
topname <- rownames(top10)
top10df <- dataset_T[,topname]
top10df$date <- mydata$date
top_mdat <- melt(top10df, id="date")
head(top_mdat)
colnames(top_mdat) <- c("date","country","sessions") 
top20_cname <- paste(unique(top_mdat$country), c(1:20))

V <- data.frame(rank = c(1:20), name = top20_cname, c_name = unique(top_mdat$country))
top_mdat[4] <- V[match(top_mdat$country, V$c_name),][2]
library(scales)
 
#top_mdat$date <- as.Date(as.character(top_mdat$date), format="%Y%m%d")
A <- ggplot(top_mdat, aes(date, sessions, colour = name))+#, linetype=country)) + 
  geom_line()+ 
  scale_x_date(labels = date_format("%m%d"))+
  labs(title = paste("Top 20 countries (", tit, " 2017)", sep=""), x = "date", y = "sessions") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), title = element_text(size = 10))
ggsave(paste("Monthly/", tit, "/total/top20.png", sep=""), A)

