
if(FALSE){
  require("RHJDBC")
  # initition
  cp = dir(system.file("java", package="RHJDBC"),full.names = TRUE)
  .jinit(classpath=cp)
  options( java.parameters = "-Xmx8g" ) # set jvm
  drv <- JDBC("org.apache.hive.jdbc.HiveDriver")
  con <- dbConnect(drv,...)
  class(con) = "JHDBCConnection" # 设置类
  dbRemoveTable(con, "tmp.mtcars")
  dbCreateTable(con, "tmp.mtcars", mtcars)
  dbWriteTable(con, "tmp.mtcars", mtcars,overwrite=FALSE)
  query = "select * from tmp.mtcars"
  data <- dbFetch(dbSendQuery(con,query))
  data
}

