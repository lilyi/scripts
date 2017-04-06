setwd("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/Q1/total_dat/")
cname <- "Australia"
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
stat <- function(cname){
  data <- read.csv(paste(cname, '.csv', sep=""), header=T)
  std <- round(sd(data$sessions),4)
  mean <- round(mean(data$sessions),2)
  upper <- round((mean + 2*std),2)
  lower <- round((mean - 2*std),2)
  return(c(mean, std, upper,lower))
}
stat(cname)
A <- as.data.frame(lapply(clist, stat))
colnames(A) <- clist
B <- t(A)
colnames(B) <- c("mean", "std", "upper", "lower")
write.csv(B, '0320-0331/total_dat/compare_3.csv')