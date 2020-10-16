college_data <- read_csv("college_data.csv")


csvpath <- "~/R/NYCDSA/Shiny App Project/college_tuition/college_data.csv"
dbname <- "./college_data.sqlite" 
tblname <- "college_data"

data <- fread(input = csvpath,
                      sep = ",",
                      header = TRUE) 

conn <- dbConnect(drv = SQLite(),
                  dbname = dbname)

dbWriteTable(conn = conn, 
             name = tblname, 
             value = data, 
             overwrite = TRUE)

#list tables
dbListTables(conn)

dbConnector <- function(session, dbname) {
  ## setup connection to database
  conn <- dbConnect(drv = SQLite(),
                    dbname = dbname)
  ## disconnect database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(conn)
  })
  ## return connection
  conn
}

dbGetData <- function(conn, tblname, month, day) {
  ## filter by month and day
  query <- paste("SELECT * FROM",
                 tblname,
                 "WHERE month =",
                 as.character(month),
                 "AND day =",
                 as.character(day))
  ## database query returns a data.frame
  as.data.table(dbGetQuery(conn = conn,
                           statement = query))
}
