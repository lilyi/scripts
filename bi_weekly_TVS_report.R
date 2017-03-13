#install.packages("RGoogleAnalytics")
require(devtools)
#devtools::install_github("Tatvic/RGoogleAnalytics")
#devtools::install_github("Tatvic/RGoogleAnalytics")
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
library(reshape2)
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

#function to get THUNDERBOLT NAS's SESSIONS of EACH LANGUAGE
myfunction_fil <- function(lan, stime, etime, pID){
  query.list <- Init(table.id = "ga:3035421", start.date = stime,
                     end.date = etime,
                     metrics="ga:sessions",
                     max.results = 10000,
                     #sort="ga:date",
                     #segment = "sessions::condition::ga:pagePath=@/de-de/;ga:pagePath=@/model.php?II=233"
                     filters = paste("ga:pagePath=@", lan,";ga:pagePath=@/model\\.php\\?II=", pID, sep = ""))
  ga.query <- QueryBuilder(query.list)
  gaData2 <- GetReportData(ga.query, token)
  if (is.null(gaData2)){
    gaData2 <- 0
  }
  cat("pID = ", pID, "\n")
  cat("lan = ", lan, "\n")
  return(gaData2)}
#gaData2
lanlist <- c("/en/","/en-us/","/en-uk/","/en-au/","/en-in/","/de-de/","/es-es/","/es-mx/","/fr-fr/","/it-it/","/nl-nl/","/sv-se/","/zh-tw/","/zh-hk/","/pt-pt/","/pl-pl/","/ja-jp/","/ko-kr/","/cs-cz/")
TVS_1282T <- sapply(lanlist, myfunction_fil, stime, etime, pID = "233")
TVS_682T <- sapply(lanlist, myfunction_fil, stime, etime, pID = "231")
TVS_882T <- sapply(lanlist, myfunction_fil, stime, etime, pID = "232")
TVS_871T <- sapply(lanlist, myfunction_fil, stime, etime, pID = "198")
TVS_882ST2 <- sapply(lanlist, myfunction_fil, stime, etime, pID = "265")
result_2 <- cbind(TVS_1282T, TVS_682T, TVS_882T, TVS_871T, TVS_882ST2)
write.csv(result_2, paste("bi-weekly/",tit,"/TVS_dat/THNAS.csv",sep="")) 
res <- as.data.frame(t(result_2))
res <- lapply(res, as.integer)
res <- as.data.frame(res)
colnames(res) <- lanlist
res$NAS <- factor(c("TVS-1282T", "TVS-682T", "TVS-882T", "TVS-871T", "TVS-882ST2"), 
                  levels=c("TVS-1282T", "TVS-682T", "TVS-882T", "TVS-871T", "TVS-882ST2"))


mdat <- melt(res, id.vars="NAS", factorsAsStrings = TRUE)
head(mdat)
colnames(mdat) <- c("NAS","languages","sessions")
ggplot(mdat, aes(languages, sessions, fill=NAS)) + 
  geom_bar(stat="identity", position="dodge", width=0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.key.size = unit(0.3, "cm")) +
  labs(title = paste("Thunderbolt NAS (",tit, ")"))
ggsave(paste("bi-weekly/",tit,"/TVS/THNAS.png",sep=""))
