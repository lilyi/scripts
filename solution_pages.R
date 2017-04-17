require(devtools)
library(googleAuthR)
require(RGoogleAnalytics)
library(grid)
library(gridExtra)
library(optparse)
library(ggplot2)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/analytics"))
options("googleAuthR.client_id" = "908916142832-bf3o6rpn8phh344booolr1ovfla7ea9p.apps.googleusercontent.com")
options("googleAuthR.client_secret" = "MlS4oatMCIMqzI3bpvWMeH3W")
service_token <- gar_auth_service("C:/Users/Lily/Documents/GA/R/key_secrets.json", scope=getOption("googleAuthR.scopes.selected"))
client.id = "908916142832-bf3o6rpn8phh344booolr1ovfla7ea9p.apps.googleusercontent.com"
client.secret = "MlS4oatMCIMqzI3bpvWMeH3W"
token <- Auth(client.id,client.secret)
invisible(GetProfiles(token))

setwd('C:/Users/Lily/Documents/GA/R/report/2017/')

stime <- "2017-03-01"
etime <- "2017-03-31"
lan <- "/en/"
page <- "qiot-containers"
page_list <- c("cross-platform-file-sharing","smb-solution-qsync","vjbod","mac-users","surveillance-milestone","ESQSG","qnap-platform9","backup-and-disaster-recovery","remote-backup","snapshots","ransomware","vm-backup","hybrid-backup-sync","xopero","xopero-free","data_loss","thunderbolt3-nas","qrm","ifttt_agent","wirelessap-station","qfiling","exfat_file_system","cloudlink","qmailagent","qcontactz","qiot-containers","ssd-cache","qtier-auto-tiering","thunderbolt-nas","10gbe-ready","qcenter","qsirch","linux-station","surveillance-station-intro","note-station","signage-station","chromebook_ready","proxy-server","qvpc","virtualization","DataCore-Ready","virtualization-station","container_station","p2v")
lan_list <- c("/en/","/en-us/","/en-uk/","/en-au/","/en-in/","/de-de/","/es-es/","/es-mx/","/fr-fr/","/it-it/","/nl-nl/","/sv-se/","/zh-tw/","/zh-hk/","/pt-pt/","/pl-pl/","/ja-jp/","/ko-kr/","/cs-cz/","/th-th/")
lan_list2 <- c("en","en_us","en_uk","en_au","en_in","de_de","es_es","es_mx","fr_fr","it_it","nl_nl","sv_se","zh_tw","zh_hk","pt_pt","pl_pl","ja_jp","ko_kr","cs_cz", "th_th")

solution <- function(page, lan){
  query.list <- Init(table.id = "ga:3035421", start.date = stime,
                     end.date = etime,
                     metrics="ga:sessions",
                     dimensions = "ga:pagePath",
                     sort = "-ga:sessions",
                     # max.results = 10000,
                     filters = paste("ga:pagePath=~/solution", ";ga:pagePath=@", lan, ";ga:pagePath=~", page, sep=""))
  ga.query <- QueryBuilder(query.list)
  gaData <- GetReportData(ga.query, token)
  SUM <- sum(gaData$sessions)
  return(SUM)
}
FF <- function(lan){
  re <- sapply(page_list, solution, lan)
  return(re)
}
A <- lapply(lan_list, FF)

B <- as.data.frame(A)
colnames(B) <- lan_list

write.csv(B, "test.csv") 

table_fun <- function(i){
  lan_sessions <- B[with(B,order(-B[i])),][1:3,][i]
  page <- row.names(lan_sessions)
  return(cbind(lan_sessions, page))
}
i <- c(1:20)
res <- as.data.frame(lapply(i, table_fun))
row.names(res) <- c(1:3)
View(res)
write.csv(res, "solution_top3.csv") 

