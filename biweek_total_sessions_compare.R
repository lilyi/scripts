
setwd("C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/")
# cname <- "Australia"
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
compare_biweek <- function(cname){
  thisdata <- read.csv(paste('0306-0317/total_dat/', cname, '.csv', sep=""), header = T)
  maxi_T <- max(thisdata$sessions)
  mini_T <- min(thisdata$sessions)
  ave_T <- round(mean(thisdata$sessions))
  sum_T <- sum(thisdata$sessions)
  lastdata <- read.csv(paste('0220-0303/total_dat/', cname, '.csv', sep=""), header = T)
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
write.csv(B, '0306-0317/total_dat/compare.csv')
