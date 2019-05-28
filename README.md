# RHJDBC

R connect hive, create a new class `JHDBCConnection` which inherits from `JDBCConnection`,rewrite  function `dbWriteTable`,`dbCreateTable`

- install 
```
library(devtools)
install_github("lengyuyeke/RHJDBC")
```

- example

```
require("RHJDBC")
# initition
cp = dir(system.file("java", package="RHJDBC"),full.names = TRUE) # you can use your own jdbc driver
.jinit(classpath=cp)  # init
options( java.parameters = "-Xmx8g" ) # set jvm
drv <- JDBC("org.apache.hive.jdbc.HiveDriver") # set driver
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



