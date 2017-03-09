#install.packages("RGoogleAnalytics")
require(devtools)
#devtools::install_github("Tatvic/RGoogleAnalytics")
require(RGoogleAnalytics)
library(scales)
# Authorize the Google Analytics account
# This need not be executed in every session once the token object is created 
# and saved
client.id = "908916142832-bf3o6rpn8phh344booolr1ovfla7ea9p.apps.googleusercontent.com"
client.secret = "MlS4oatMCIMqzI3bpvWMeH3W"
token <- Auth(client.id,client.secret)
# Save the token object for future sessions
save(token,file="./token_file")
# In future sessions it can be loaded by running load("./token_file")
ValidateToken(token)

setwd('C:/Users/Lily/Documents/GA/R/report/2017/')
stime <- "2017-02-20"
etime <- "2017-03-03"
tit <-"0220-0303"

#function to get SESSIONS of 32 COUNTRIES
myfunction <- function(cname, stime, etime){
  print(paste(cname,'HI'))
  query.list <- Init(table.id = "ga:3035421", start.date = stime,
                 end.date = etime,
                 dimensions="ga:date",
                 metrics="ga:sessions",
                 sort="ga:date",
                 segment = paste("sessions::condition::ga:country=@", cname, sep = ""))
  ga.query <- QueryBuilder(query.list)
  gaData <- GetReportData(ga.query, token)
  print("datagot") #!
  write.csv(gaData, paste("bi-weekly/",tit,"/total_dat/", cname,".csv", sep="")) 
  gaData$date <- as.Date(gaData$date, format="%Y%m%d") 
  myplot <- myplotF(cname, gaData, gaData$date, gaData$sessions)
  myplot
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
  ggsave(paste("bi-weekly/",tit,"/total/",
               cname,".png",sep="")) #!
  return(myplot1)
}
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
#n_clist <- list("Austria","Belgium","Czechia","Denmark","France","Germany","Greece","Hungary","Italy","Netherlands","Norway","Poland","Portugal","Romania","Spain","Sweden","Switzerland","Turkey","United Kingdom","Hong Kong","India","Iran","Israel","Japan","South Korea","Taiwan","Thailand","Canada","Mexico","United States","South Africa","Australia")

res_sessions32 <- lapply(clist, myfunction, stime, etime)
#res_sessions32[1]
#A1 <- do.call(arrangeGrob, c(res_sessions32[1:4], list(ncol=2)))
#ggsave("C:/Users/Lily/Documents/GA/R/report/Monthly/20170201-20170228/32countrytotal/01.png", A1)
i <- seq(from=1, to=32, by=4)
k <- c(1:8)
arrangefunction <- function(i,k){
  title1=textGrob(paste("Total sessions (", tit, ")", sep=""), gp=gpar(fontsize=14, font=2))#fontface="bold")
  A <- do.call(arrangeGrob, c(res_sessions32[i:(i+3)], list(ncol=2),list(top=title1)))
  ggsave(paste("bi-weekly/",tit,"/total/0",k,".png",sep=""), A)
  return(A) #!
}

#######################try
gridfunction <- function(i,j){
  title1=textGrob(paste("Total sessions (", tit, ")", sep=""), gp=gpar(fontsize=20, font=2))#fontface="bold")
  B1 <- grid.arrange(ncol = 2,grobs=c(res_sessions32[i:(i+3)]),top = title1)
  ggsave(paste("/bi-weekly/",tit,"/total/00",j,".png",sep=""),B1,height=9,width=12,dpi=72)
  return(B1) #!
}
RE <- mapply(arrangefunction, i, k)

