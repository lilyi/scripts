#install.packages("RGoogleAnalytics")
require(devtools)
#devtools::install_github("Tatvic/RGoogleAnalytics")
#devtools::install_github("Tatvic/RGoogleAnalytics")
#install.packages("googleAuthR")
# if(!require(googleAuthR)){
#   if(!require(devtools)){
#     install.packages("devtools")
#   } else {
#     devtools::install_github("MarkEdmondson1234/googleAuthR")
#   }
# }
library(googleAuthR)
require(RGoogleAnalytics)
library(grid)
library(gridExtra)
library(optparse)
library(ggplot2)
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
  make_option(c("-s", "--stime"), type="character", default="2017-03-01",
              help="start time as [default= %default]", metavar="character"),
  make_option(c("-e", "--etime"), type="character", default="2017-03-31",
              help="end time as [default= %default]", metavar="character"),
  make_option(c("-t", "--tit"), type="character", default="Mar",
              help="month as [default= %default]", metavar="character")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)
setwd('C:/Users/Lily/Documents/GA/R/report/2017/')
stime <- opt$stime
etime <- opt$etime
tit <- opt$tit
# stime <- "2017-03-01"
# etime <- "2017-03-31"
# tit <- "Mar"

library(XML)
theurl <- "http://srcqnap.qnap.com.tw/en/product/_info.php"

html <- htmlParse(theurl)
sched <- readHTMLTable(html, stringsAsFactors = FALSE)
product <- readHTMLTable(html, stringsAsFactors = FALSE)[[2]]# 2nd table
#product <- as.data.frame(lapply(product, function(x) iconv(x, "UTF-8", "BIG-5")))
colnames(product) <- c("Model_ID", "Model_name", "time", "main_type", "sub_type", "type", "series", "history", "support")
write.csv(product, "table.csv") 
IDs <- product$Model_ID
#IDs <- product[,"Model_ID"]

keyclass <- unique(product$main_type)

#32國當月份流量前三機種
myfunction_countsessions <- function(ID,data){
  S <- sum(data$sessions[grep(paste("II=\\b", ID, "\\b", sep=""), data$urls)])
  return(S)
}
#sapply(IDs, myfunction_countsessions,result.data)
myfunction_top3 <- function(cname, stime, etime){
  #print(paste(cname,'HI'))
  query.list <- Init(table.id = "ga:3035421", 
                     start.date = stime,
                     end.date = etime, 
                     dimensions="ga:pagePath", 
                     metrics="ga:sessions",
                     sort="ga:pagePath",
                     max.results = 10000,
                     segment = paste("sessions::condition::ga:country=@", cname, sep = ""),
                     filters = "ga:pagePath=@/model\\.php\\?II=")
  ga.query <- QueryBuilder(query.list)
  gaData <- GetReportData(ga.query, token)
  write.csv(gaData, paste("Monthly/",tit,"/top3_dat/", cname,".csv", sep="")) 
  result.data <- data.frame(urls = gaData$pagePath, sessions = gaData$sessions)
  SUMs <- sapply(IDs, myfunction_countsessions,result.data)
  print(str(SUMs))
  table <- data.frame(Model_ID = rownames(data.frame(SUMs)), sum = SUMs)
  rownames(table) <- 1:nrow(table)
  #modelmain_type <- product[match(table$Model_ID, product$Model_ID), ][4]
  table[3] <- product[match(table$Model_ID, product$Model_ID), ][4]
  HomeSOHO_table <- table[table$main_type=="Home & SOHO",]
  SMB_table <- table[table$main_type=="SMB",]
  Enterprise_table <- table[table$main_type=="Enterprise",]
  #other_table <- table[table$main_type=="",]
  
  HomeSOHO_top3 <- HomeSOHO_table[with(HomeSOHO_table,order(-sum)),][1:3,]
  SMB_top3 <- SMB_table[with(SMB_table,order(-sum)),][1:3,]
  Enterprise_top3 <- Enterprise_table[with(Enterprise_table,order(-sum)),][1:3,]
  #other_top3 <- other_table[with(other_table,order(-sum)),][1:3,]
  
  HomeSOHO_top3_modelname <- product[match(HomeSOHO_top3$Model_ID, product$Model_ID),][,c(1:2,4)]
  SMB_top3_modelname <- product[match(SMB_top3$Model_ID, product$Model_ID),][,c(1:2,4)]
  Enterprise_top3_modelname <- product[match(Enterprise_top3$Model_ID, product$Model_ID),][,c(1:2,4)]
  #other_top3_modelname <- product[match(other_top3$Model_ID, product$Model_ID),][,c(1:2,4)]
  
  HomeSOHO_top3_result <- merge(HomeSOHO_top3,HomeSOHO_top3_modelname)
  SMB_top3_result <- merge(SMB_top3,SMB_top3_modelname)
  Enterprise_top3_result <- merge(Enterprise_top3,Enterprise_top3_modelname)
  #other_top3_result <- merge(other_top3,other_top3_modelname)
  result <- c(HomeSOHO_top3_result,SMB_top3_result,Enterprise_top3_result)#,other_top3_result)
  return(result)
}#return top3 ID & sum of sessions

clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")

TOP3_IDs <- sapply(clist, myfunction_top3, stime, etime)
result_TOP3 <- data.frame(sapply(TOP3_IDs, as.character), stringsAsFactors=FALSE)
coname <- c("ID","type","sessions","model")
CLIST <- c()
for(i in 1:32){
  if(i==1){
    CLIST[1:12] <- paste(clist[1],coname,sep=".") }else{
      CLIST[(12*i-11):(12*i)] <- paste(clist[i],coname,sep=".") 
    }
}

colnames(result_TOP3) <- CLIST
write.csv(result_TOP3, paste("Monthly/",tit,"/top3_dat/top3.csv", sep="")) 
t_top3 <- t(result_TOP3)


makeplotfunction <- function(i,cname){ 
  countryname <- data.frame(t(cbind(t_top3[i:(i+3),],t_top3[(i+4):(i+7),],t_top3[(i+8):(i+11),])), country = cname)
  colnames(countryname) <- c("ID","type","sessions","model","country")
  #print(head(countryname))
  countryname$sessions = as.double(levels(countryname$sessions))[countryname$sessions] 
  p1 <- ggplot(countryname[1:3,], aes(model, sessions)) + 
    geom_bar(stat="identity", position="dodge", width=0.5, fill="#FF6600") +   
    theme(axis.text.x = element_text(angle = 15, hjust = 1), title = element_text(size = 10)) +
    labs(title = "Home & SOHO")
  p2 <- ggplot(countryname[4:6,], aes(model, sessions)) + 
    geom_bar(stat="identity", position="dodge", width=0.5, fill="#0099FF") +   
    theme(axis.text.x = element_text(angle = 15, hjust = 1), title = element_text(size = 10)) +
    labs(title = "SMB")
  p3 <- ggplot(countryname[7:9,], aes(model, sessions)) + 
    geom_bar(stat="identity", position="dodge", width=0.5, fill="#9933FF") +   
    theme(axis.text.x = element_text(angle = 15, hjust = 1), title = element_text(size = 10)) +
    labs(title = "Enterprise")
  #p4 <- ggplot(countryname[10:12,], aes(model, sessions)) + 
   # geom_bar(stat="identity", position="dodge", width=0.5, fill="#00CC66") +   
    #theme(axis.text.x = element_text(angle = 15, hjust = 1), title = element_text(size = 10)) +
    #labs(title = "Other")
  title=textGrob(as.character(cname), gp=gpar(fontsize=15))#fontface="bold")
  A1 <- grid.arrange(p1, p2, p3, ncol = 2, top = title)
  print("plot!")
  ggsave(paste("Monthly/", tit, "/top3/Top3__", cname, ".png", sep = ""), A1)
  return(A1) 
}

i <- seq(from=1, to=384, by=12)
RE <- mapply(makeplotfunction, i, clist)

j <- seq(from=1, to=32, by=4)
k <- c(1:8)
gridfunction <- function(i,j){
  title1 <- textGrob(paste("Top 3 products (", tit,". 2017)", sep=""), gp=gpar(fontsize=20, font=2), just = "top")#fontface="bold")
  B1 <- grid.arrange(ncol = 2,grobs=c(RE[i:(i+3)]),top = title1)
  ggsave(paste("Monthly/", tit, "/top3/00", j, ".png", sep=""), B1, scale=2)
  return(B1)
}
RE2 <- mapply(gridfunction, j, k)

# title1=textGrob(paste("Top 3 products (", tit, " 2017)"), gp=gpar(fontsize=20, font=2), just = "top")
# B1 <- grid.arrange(ncol = 2, grobs=c(RE[9:12]), top = title1)
# j <- "03"
# ggsave(paste("./report/Monthly/20170201-20170228/top3/00",j,".png",sep=""),B1,scale=2)
