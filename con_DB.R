install.packages("RMySQL")
install.packages("dbConnect")
library(RMySQL)
library(dbConnect)
con <- dbConnect(MySQL(), username="root", host="localhost", dbname="web_analytics", password="root", unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
dbListTables(con)                 
# result = dbSendQuery(con, "select * from qtip_product_specs where field_id='757'")
# data.frame = fetch(result, n=5) # Fetch all the records(with n = -1) and store it as a data frame.
# print(data.frame)
# dbSendQuery(mysqlconnection, "update mtcars set disp = 168.5 where hp = 110")
# dbSendQuery(mysqlconnection,
#             "insert into mtcars(row_names, mpg, cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb)
# values('New Mazda RX4 Wag', 21, 6, 168.5, 110, 3.9, 2.875, 17.02, 0, 1, 4, 4)"
# )
# # Create the connection object to the database where we want to create the table.
# mysqlconnection = dbConnect(MySQL(), user='root', password='', dbname='sakila', host='localhost')

# Use the R data frame "mtcars" to create the table in MySql.
# All the rows of mtcars are taken inot MySql.
# dbWriteTable(mysqlconnection, "mtcars", mtcars[, ], overwrite = TRUE)
# dbSendQuery(mysqlconnection, 'drop table if exists mtcars')
dbWriteTable(con, "mtcars", mtcars[, ], overwrite = TRUE)