require(devtools)
library(grid)
library(gridExtra)
library(googleAuthR)
require(RGoogleAnalytics)
library(scales)
library(ggplot2)
library(optparse)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/analytics"))
options("googleAuthR.client_id" = "908916142832-bf3o6rpn8phh344booolr1ovfla7ea9p.apps.googleusercontent.com")
options("googleAuthR.client_secret" = "MlS4oatMCIMqzI3bpvWMeH3W")
service_token <- gar_auth_service("C:/Users/Lily/Documents/GA/R/key_secrets.json", scope=getOption("googleAuthR.scopes.selected"))

client.id = "908916142832-bf3o6rpn8phh344booolr1ovfla7ea9p.apps.googleusercontent.com"
client.secret = "MlS4oatMCIMqzI3bpvWMeH3W"
token <- Auth(client.id,client.secret)
invisible(GetProfiles(token))

# option_list <- list(
#   make_option(c("-s", "--stime"), type="character", default="2017-03-26", 
#               help="start time as [default= %default]", metavar="character"),
#   make_option(c("-e", "--etime"), type="character", default="2017-03-31", 
#               help="end time as [default= %default]", metavar="character"),
#   make_option(c("-t", "--tit"), type="character", default="0326-0331", 
#               help="month as [default= %default]", metavar="character")
# )
# 
# opt_parser <- OptionParser(option_list=option_list)
# opt <- parse_args(opt_parser)
setwd('C:/Users/Lily/Documents/GA/R/report/2017/weekly/')
# stime <- opt$stime
# etime <- opt$etime
# tit <- opt$tit

stime <- "2017-04-09"
etime <- "2017-04-15"
s2 <- "2017-04-16"
e2 <- "2017-04-22"
#function to get SESSIONS of 32 COUNTRIES
myfunction <- function(cname, stime, etime, s2, e2){
  query.list_1 <- Init(table.id = "ga:3035421", start.date = stime,
                 end.date = etime,
                 dimensions="ga:date",
                 metrics="ga:sessions",
                 sort="ga:date",
                 segment = paste("sessions::condition::ga:country=@", cname, sep = ""))
  ga.query_1 <- QueryBuilder(query.list_1)
  gaData_1 <- GetReportData(ga.query_1, token)
  as.data.frame(gaData_1)
  SUM_1 <- sum(gaData_1$sessions)
  query.list_2 <- Init(table.id = "ga:3035421", start.date = s2,
                       end.date = e2,
                       dimensions="ga:date",
                       metrics="ga:sessions",
                       sort="ga:date",
                       segment = paste("sessions::condition::ga:country=@", cname, sep = ""))
  ga.query_2 <- QueryBuilder(query.list_2)
  gaData_2 <- GetReportData(ga.query_2, token)
  as.data.frame(gaData_2)
  SUM_2 <- sum(gaData_2$sessions)
  dif <- SUM_2-SUM_1
  if(dif > 0){
    sign <- "+"
  }else if(dif < 0){
    sign <- "-"
  }else{
    sign <- "--"
  }
  res <- cbind(SUM_1, SUM_2, dif, sign)
  return(res)
}

clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
res_sessions32 <- sapply(clist, myfunction, stime, etime, s2, e2)
res <- as.data.frame(t(res_sessions32))
row.names(res) <- clist
colnames(res) <- c("0409-0415", "0416-0422", "dif", "sign")
write.csv(res, 'weekly_sign_0422.csv')



