library(reshape2)
library(ggplot2)
library(scales)
setwd("C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/")
# cname <- "Australia"
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
compare_biweek <- function(cname){
  thisdata <- read.csv(paste('0320-0331/total_dat/', cname, '.csv', sep=""), header = T)
  maxi_T <- max(thisdata$sessions)
  mini_T <- min(thisdata$sessions)
  ave_T <- round(mean(thisdata$sessions))
  sum_T <- sum(thisdata$sessions)
  lastdata <- read.csv(paste('0306-0317/total_dat/', cname, '.csv', sep=""), header = T)
  maxi_L <- max(lastdata$sessions)
  mini_L <- min(lastdata$sessions)
  ave_L <- round(mean(lastdata$sessions))
  sum_L <- sum(lastdata$sessions)
  maxi <- maxi_T - maxi_L
  mini <- mini_T - mini_L
  ave <- ave_T - ave_L
  summ <- sum_T - sum_L
  return(c(summ, maxi, mini, ave))
}
A <- as.data.frame(lapply(clist, compare_biweek))
colnames(A) <- clist
B <- t(A)
colnames(B) <- c("sum", "max", "min", "ave")
write.csv(B, '0320-0331/total_dat/compare_3.csv')
cname <- "Germany"
biweek_sesseions <- function(cname){
  thisdata <- read.csv(paste('0320-0331/total_dat/', cname, '.csv', sep=""), header = T)
  ST <- thisdata$sessions
  lastdata <- read.csv(paste('0306-0317/total_dat/', cname, '.csv', sep=""), header = T)
  SL <- lastdata$sessions
  data_set1 <- data.frame(date=as.character(thisdata$date),
                          this=ST,
                          last=SL)
  data_set1$date <- as.Date(as.character(data_set1$date), format="%Y%m%d")
  mdat <- melt(data_set1, id="date")
  head(mdat)
  colnames(mdat) <- c("date","biweek","sessions")  
  p <- ggplot(mdat, aes(date, sessions, colour = biweek))+#, linetype=country)) + 
    geom_line()+ 
    scale_x_date(labels = date_format("%m%d"))+
    labs(title = paste(cname), x = "date (this, comparing with 0306-0317)", y = "sessions")
    #xlim(1,12)
  ggsave(paste("total_compare/", cname,".png",sep=""))
  return()
}
lapply(clist, biweek_sesseions)
####
A



