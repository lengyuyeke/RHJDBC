

## 主要是拼成insert sql，然后插入数据即可
setClass("JHDBCConnection", representation("DBIConnection", jc="jobjRef", identifier.quote="character"),contains=c("DBIConnection","JDBCConnection"))
setMethod("dbDataType", signature(dbObj="JHDBCConnection", obj = "ANY"),
          def = function(dbObj, obj, ...) {
            if (is.integer(obj)) "INT"
            else if (is.numeric(obj)) "DOUBLE"
            else "STRING"
          },
          valueClass = "character")
.sql.qescape <- function(s, identifier=FALSE, quote="\"") {
  s <- as.character(s)
  if (identifier) {
    vid <- grep("^[A-Za-z]+([A-Za-z0-9_]*)$",s)
    if (length(s[-vid])) {
      if (is.na(quote)) stop("The JDBC connection doesn't support quoted identifiers, but table/column name contains characters that must be quoted (",paste(s[-vid],collapse=','),")")
      s[-vid] <- .sql.qescape(s[-vid], FALSE, quote)
    }
    return(s)
  }
  if (is.na(quote)) quote <- ''
  s <- gsub("\\\\","\\\\\\\\",s)
  if (nchar(quote)) s <- gsub(paste("\\",quote,sep=''),paste("\\\\\\",quote,sep=''),s,perl=TRUE)
  paste(quote,s,quote,sep='')
}

.sql.generate <- function(name,value){
  list <- lapply(value, function(o) if (!is.numeric(o)) paste('"',o,'"',sep="") else o)
  valued = do.call(cbind,list)
  sql_t = apply(valued,1,paste,collapse=",")
  sql =  paste("(",sql_t,")",collapse=",")
  paste("INSERT INTO ",name," VALUES" ,sql )

}


setMethod("dbCreateTable", "JHDBCConnection", def=function(conn,name,fields) {
  value = fields
  if (is.vector(value) && !is.list(value)) value <- data.frame(x=value)
  if (length(value)<1) stop("value must have at least one column")
  if (is.null(names(value))) names(value) <- paste("V",1:length(value),sep='')
  if (length(value[[1]])>0) {
    if (!is.data.frame(value)) value <- as.data.frame(value, row.names=1:length(value[[1]]))
  } else {
    if (!is.data.frame(value)) value <- as.data.frame(value)
  }
  fts <- sapply(value, dbDataType, dbObj=conn)
  fdef <- paste(.sql.qescape(names(value), TRUE, conn@identifier.quote),fts,collapse=',')
  qname <- .sql.qescape(name, TRUE, conn@identifier.quote)
  ct <- paste("CREATE TABLE ",qname," (",fdef,")",sep= '')
  dbSendUpdate(conn, ct)
})


setMethod("dbWriteTable", "JHDBCConnection", def=function(conn, name, value, overwrite=TRUE) {
  overwrite <- isTRUE(as.logical(overwrite))
  if (is.vector(value) && !is.list(value)) value <- data.frame(x=value)
  if (length(value)<1) stop("value must have at least one column")
  if (is.null(names(value))) names(value) <- paste("V",1:length(value),sep='')
  if (length(value[[1]])>0) {
    if (!is.data.frame(value)) value <- as.data.frame(value, row.names=1:length(value[[1]]))
  } else {
    if (!is.data.frame(value)) value <- as.data.frame(value)
  }
  fts <- sapply(value, dbDataType, dbObj=conn)
  fdef <- paste(.sql.qescape(names(value), TRUE, conn@identifier.quote),fts,collapse=',')
  qname <- .sql.qescape(name, TRUE, conn@identifier.quote)
  if (overwrite) {
    dbRemoveTable(conn,name=name)
    ct <- paste("CREATE TABLE ",qname," (",fdef,")",sep= '')
    dbSendUpdate(conn, ct)
  }
  if (length(value[[1]])) {
    sql = .sql.generate(qname,value)
    dbSendUpdate(conn,sql)
  }
})
