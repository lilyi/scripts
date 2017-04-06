
query.list <- Init(table.id = "ga:3035421", start.date = stime,
                   end.date = etime,
                   metrics="ga:sessions",
                   max.results = 10000,
                   #sort="ga:date",
                   #segment = "sessions::condition::ga:pagePath=@/de-de/;ga:pagePath=@/model.php?II=233"
                   filters = paste("ga:pagePath=@", lan,";ga:pagePath=~/model\\.php\\?II=", pID, sep = ""))
ga.query <- QueryBuilder(query.list)
gaData <- GetReportData(ga.query, token)
write.csv(gaData, paste("Monthly/", tit, "/total_dat/", cname, ".csv", sep="")) 