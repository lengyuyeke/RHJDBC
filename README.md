# RHJDBC

R connect hive, create a new class `JHDBCConnection` which inherits from `JDBCConnection`,rewrite  function `dbWriteTable`,`dbCreateTable`

##  install 
```
library(devtools)
install_github("lengyuyeke/RHJDBC")
```
## usage
```
dbWriteTable(conn, name, value,partition_column = NULL, partition_value = NULL,overwrite=TRUE,batch=1000L) # if value has partition values, partition_value should be left as NULL
```
## EXAMPLE
- basic example

```
options( java.parameters = "-Xmx8g" ) # set jvm first
require("RHJDBC")
# initition
cp = dir(system.file("java", package="RHJDBC"),full.names = TRUE) # you can use your own jdbc driver
.jinit(classpath=cp)  # init
drv <- JDBC("org.apache.hive.jdbc.HiveDriver") # set driver
#con <- dbConnect(drv,"jdbc:hive2://ip:port/default","username","password") 
con <- dbConnect(drv,...)                      # set connection,url,username,password
class(con) = "JHDBCConnection" # change the class of con 
dbRemoveTable(con, "tmp.mtcars")
# if the data only have one column,it will raise a error,just add another column
dbCreateTable(con, "tmp.mtcars", mtcars)
dbWriteTable(con, "tmp.mtcars", mtcars,overwrite=FALSE)
query = "select * from tmp.mtcars"
data <- dbFetch(dbSendQuery(con,query))
data
```
- partition table example
```
sql="
CREATE TABLE `tmp.mtcars`(
  `mpg` double, 
  `cyl` double, 
  `disp` double, 
  `hp` double, 
  `drat` double, 
  `wt` double, 
  `qsec` double, 
  `vs` double, 
  `am` double, 
  `gear` double, 
  `carb` double)
  partitioned by (dt string )
"
dbRemoveTable(con,"tmp.mtcars")
dbSendUpdate(con,sql)
dbWriteTable(con, "tmp.mtcars", mtcars,partition_column = 'dt', partition_value = "2019")
mtcars$dt='2018'
dbWriteTable(con, "tmp.mtcars", mtcars,partition_column = 'dt')
```



