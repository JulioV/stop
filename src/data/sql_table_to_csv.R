library(RMySQL)
library(tibble)
source("data/common_source.R")

main <- function(args){
  
  rmysql.settingsfile<-"../.envR"
  rmysql.group<-"stop_release_1"
   
  
  # If you want to update certain participants only uncomment line below
  # participants = c("p11" = "da4b523b-55a2-4040-ae05-b8a8baf1e9e1")
  
  for(p_id in names(participants)){
    print(p_id)
    for(table_name in tables_names){
      print(table_name)
      stopDB<-dbConnect(RMySQL::MySQL(),default.file=rmysql.settingsfile,group=rmysql.group)
      query<-paste("SELECT * FROM ", table_name," WHERE device_id LIKE '", participants[p_id], "'",sep="")
      table = dbGetQuery(stopDB,query)
      
      csv_path = paste0("../data/raw/",table_name,"/", p_id, ".csv")
      dir.create(dirname(csv_path), recursive=TRUE)
      write.table(table, csv_path, row.names = F, quote = F, sep = "\t")
      dbDisconnect(stopDB)
    }
  }
  
}

main()