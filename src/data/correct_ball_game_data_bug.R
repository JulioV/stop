library(RMySQL)
library(tibble)
rmysql.settingsfile<-"../.envR"
rmysql.group<-"stop_release_1"
stopDB<-dbConnect(RMySQL::MySQL(),default.file=rmysql.settingsfile,group=rmysql.group)


# WARNING!
# This script modifies the MySQL database

# Participants with the JSON format bug
# P07 34ccaa7f-3ad4-4a5c-bfc9-f07ee3191b90
# P08 e5981cc8-d738-465f-89a4-cbe9b1a54071
# P11 da4b523b-55a2-4040-ae05-b8a8baf1e9e1
query<-"SELECT * FROM ball_game WHERE device_id like 'da4b523b-55a2-4040-ae05-b8a8baf1e9e1'"

rs = dbSendQuery(stopDB,query)
table<- (dbFetch(rs))
table$data = paste0(substr(table$data,1,nchar(table$data)-1),"}")

for(row in seq(1, nrow(table))){
  row = table[row,]
  query = paste0("UPDATE ball_game  SET data = '",row$data,"' WHERE  device_id like '",row$device_id,"' AND timestamp=", row$timestamp)
  rs = dbSendQuery(stopDB, query)
}


dbDisconnect(stopDB)