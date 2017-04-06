setwd("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/Q1/total_dat/")
cname <- "Australia"
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
stat <- function(cname){
  Q1_data <- read.csv(paste(cname, '.csv', sep=""), header=T)
  std <- round(sd(Q1_data$sessions),4)
  mean <- round(mean(Q1_data$sessions),2)
  upper <- round((mean + 2*std),2)
  lower <- round((mean - 2*std),2)
  bi_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/0320-0331/total_dat/", cname, ".csv", sep=""), header=T)
  maxi <- max(bi_data$sessions)
  mini <- min(bi_data$sessions)
  if(maxi>upper){
    U_alert <- "+"
    if(mini < lower){
      L_alert <- "-"
    }
  }else if(mini < lower){
    L_alert <- "-"
    if(maxi > upper){
      U_alert <- "+"
    }
  }else{
    U_alert <- ""
    L_alert <- ""
  }
  return(c(mean, std, upper,lower, maxi, mini, U_alert, L_alert))
}
stat(cname)
A <- as.data.frame(lapply(clist, stat))
colnames(A) <- clist
B <- t(A)
colnames(B) <- c("mean", "std", "upper", "lower", "max", "min", "U_alert", "L_alert")
write.csv(B, 'stat.csv')
