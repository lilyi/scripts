#install.packages("RGoogleAnalytics")
require(devtools)
#devtools::install_github("Tatvic/RGoogleAnalytics")
#install.packages("googleAuthR")
# if(!require(googleAuthR)){
#   if(!require(devtools)){
#     install.packages("devtools")
#   } else {
#     devtools::install_github("MarkEdmondson1234/googleAuthR")
#   }
# }
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
# googleAuthR::gar_auth()
service_token <- gar_auth_service("C:/Users/Lily/Documents/GA/R/key_secrets.json", scope=getOption("googleAuthR.scopes.selected"))

# Authorize the Google Analytics account
# This need not be executed in every session once the token object is created 
# and saved
client.id = "908916142832-bf3o6rpn8phh344booolr1ovfla7ea9p.apps.googleusercontent.com"
client.secret = "MlS4oatMCIMqzI3bpvWMeH3W"
token <- Auth(client.id,client.secret)
invisible(GetProfiles(token))
# Save the token object for future sessions
# save(token,file="./token_file")
# In future sessions it can be loaded by running load("./token_file")
# ValidateToken(token)

option_list <- list(
  make_option(c("-s", "--stime"), type="character", default="2017-03-06", 
              help="start time as [default= %default]", metavar="character"),
  make_option(c("-e", "--etime"), type="character", default="2017-03-17", 
              help="end time as [default= %default]", metavar="character"),
  make_option(c("-t", "--tit"), type="character", default="0306-0317", 
              help="month as [default= %default]", metavar="character")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)
setwd('C:/Users/Lily/Documents/GA/R/report/2017/')
stime <- opt$stime
etime <- opt$etime
tit <- opt$tit

# stime <- "2017-03-06"
# etime <- "2017-03-17"
# tit <- "0306-0317"
#function to get SESSIONS of 32 COUNTRIES
myfunction <- function(cname, stime, etime){
  query.list <- Init(table.id = "ga:3035421", start.date = stime,
                 end.date = etime,
                 dimensions="ga:date",
                 metrics="ga:sessions",
                 sort="ga:date",
                 segment = paste("sessions::condition::ga:country=@", cname, sep = ""))
  ga.query <- QueryBuilder(query.list)
  gaData <- GetReportData(ga.query, token)
  write.csv(gaData, paste("bi-weekly/", tit, "/total_dat/", cname, ".csv", sep="")) 
  gaData$date <- as.Date(gaData$date, format="%Y%m%d") 
  myplot <- myplotF(cname, gaData, gaData$date, gaData$sessions)
  return(myplot)
}
myplotF <- function(cname, data, x, y){
  p1 <- ggplot(data, aes(x=x, y=y))
  myplot1 <- p1 + 
    geom_line(lwd=1, colour = "#0099FF") + 
    scale_x_date(labels = date_format("%d")) +
    #geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE, colour = "#9933FF") +
    labs(title = paste(cname), x = "date", y = "sessions") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1), title = element_text(size = 10))
  ggsave(paste("bi-weekly/",tit,"/total/", cname,".png",sep="")) #!
  return(myplot1)
}
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
res_sessions32 <- lapply(clist, myfunction, stime, etime)
i <- seq(from=1, to=32, by=4)
k <- c(1:8)
arrangefunction <- function(i,k){
  title1 <- textGrob(paste("Total sessions (", tit, ")", sep=""), gp=gpar(fontsize=14, font=2))#fontface="bold")
  A <- do.call(arrangeGrob, c(res_sessions32[i:(i+3)], list(ncol=2), list(top=title1)))
  ggsave(paste("bi-weekly/", tit, "/total/0", k, ".png", sep=""), A)
  return(A) 
}
mapply(arrangefunction, i, k)
#######################try
# gridfunction <- function(i,j){
#   title1=textGrob(paste("Total sessions (", tit, ")", sep=""), gp=gpar(fontsize=20, font=2))#fontface="bold")
#   B1 <- grid.arrange(ncol = 2,grobs=c(res_sessions32[i:(i+3)]),top = title1)
#   ggsave(paste("/bi-weekly/",tit,"/total/00",j,".png",sep=""),B1,height=9,width=12,dpi=72)
#   return(B1) #!
# }
# RE <- mapply(arrangefunction, i, k)

