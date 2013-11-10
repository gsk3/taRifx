
# -------------------------- #
#    Extensions for RSQLite
# -------------------------- #

#' Function to return column names from a SQLite database
#' @param conn An RSQLite connection to a database
#' @param name Character string giving the name of the table you want column names for
#' @return Character vector of column names
dbGetColnames <- function(conn, name) {
  x <- dbGetQuery( conn, paste0("SELECT sql FROM sqlite_master WHERE tbl_name = '",name,"' AND type = 'table'") )[1,1]
  x <- sub( "^.*\\((.+)\\).*$", "\\1", x )
  x <- str_split(x,",")[[1]]
  x <- gsub('[\t\n"]','', x)
  x <- gsub('^ *','', x)
  vapply( str_split( x ," " ), first, "" )
}

#' Write a table via RSQLite with factors stored in another table
#' Handles data.tables efficiently for large datasets
#' @param conn The connection object (created with e.g. dbConnect)
#' @param name The name of the table to write
#' @param value The data.frame to write to the database
#' @param factorName The base name of the tables to store the factor labels in in the SQLite database (e.g. if factorName is "_factor_" and the data.frame in value contains a factor column called "color" and the name is "mytable" then dbWriteFactorTable will create a table called mytable_factor_color which will store the levels information)
#' @param append a logical specifying whether to append to an existing table in the DBMS.
#' @param \dots Options to pass along to dbWriteTable (e.g. append=TRUE)
#' @return A boolean indicating whether the table write was successful
#' @examples
#' library(RSQLite)
#' load_all( file.path(.db,"R-projects","taRifx") )
# Create
#' db <- dbConnect( SQLite(), dbname="~/temp/test.sqlite" )
# Write test
#' set.seed(1)
#' n <- 1000
#' testDat <- data.frame(key=seq(n), x=runif(n),y=runif(n),g1=sample(letters[1:10],n,replace=TRUE),g2=rep(letters[1:10],each=n/10),g3=factor( sample(letters[1:10],n,replace=TRUE) ))
#' if(dbExistsTable(db,"test")) dbRemoveTable(db,"test")
#' dbWriteTable( conn = db, name = "test", value = testDat, row.names=FALSE )
#' testDat2 <- data.frame( key=seq(n+1,n+100), x=runif(100) )
#' dbWriteTable( conn = db, name="test", value = testDat2, row.names=FALSE, append=TRUE  )
# Read test
#' testRecovery <- dbGetQuery(db, "SELECT * FROM test")
#' testSelection <- dbGetQuery(db, "SELECT * FROM test WHERE g3=='h' OR g3=='e' ")
#' testSelection
# Test removing rows matching criteria
#' for(i in 1:10) dbWriteTable( conn = db, name = "test", value = testDat, row.names=FALSE, append=TRUE )
#' dbSendQuery( db, "DELETE FROM test WHERE g3=='a'" )
#' # Test factor conversion
#' testDat <- data.frame(key=seq(n), x=runif(n),y=runif(n),g1=sample(letters[1:10],n,replace=TRUE),g2=rep(letters[1:10],each=n/10),g3=factor( sample(letters[1:10],n,replace=TRUE) ))
#' if(dbExistsTable(db,"test")) dbRemoveTable(db,"test")
#' dbWriteFactorTable( conn = db, name = "test", value = testDat, row.names=FALSE )
#' dbGetQuery(db, "SELECT * FROM test")
#' dbGetQuery(db, "SELECT * FROM test_factor_g3")
#' if(dbExistsTable(db,"test")) dbRemoveTable(db,"test")
#' dbWriteFactorTable( conn = db, name = "test", value = as.data.table(testDat), row.names=FALSE )
#' dbReadFactorTable( conn = db, name = "test" )
#' dbReadFactorTable( conn = db, name = "test", query="WHERE g3=='a'" )
#' # -- Test merging of tables where the columns don't line up -- #
#' set.seed(1)
#' n <- 1000
#' testDat <- data.frame(key=seq(n), x=runif(n),y=runif(n),g1=sample(letters[1:10],n,replace=TRUE),g2=rep(letters[1:10],each=n/10),g3=factor( sample(letters[1:10],n,replace=TRUE) ))
#' if(dbExistsTable(db,"test")) dbRemoveTable(db,"test")
#' dbWriteFactorTable( conn = db, name = "test", value = testDat, row.names=FALSE )
#' dbGetQuery( db, "SELECT * FROM test" )
#' # Add a table with columns that are a subset of the SQL table
#' testDat2 <- data.frame( key=seq(n+1,n+100), y=runif(100) )
#' dbWriteFactorTable( conn = db, name="test", value = testDat2, row.names=FALSE, append=TRUE  )
#' dbGetQuery( db, "SELECT * FROM test" )
#' # Add a table where the columns are a superset of the SQL table's
#' testDat3 <- data.frame( key=seq(n+101,n+200), x=runif(100), n=runif(100) )
#' dbWriteFactorTable( conn = db, name="test", value = testDat3, row.names=FALSE, append=TRUE  )
#' dbGetQuery( db, "SELECT * FROM test" )
dbWriteFactorTable <- function( conn, name, value, factorName="_factor_", append=FALSE, ... ) {
  require(RSQLite)
  # Test inputs
  stopifnot(class(conn)=="SQLiteConnection")
  stopifnot(class(name)=="character")
  stopifnot("data.frame" %in% class(value))
  stopifnot(class(factorName)=="character")
  if( grepl("[.]",factorName) ) stop("factorName must use valid characters for SQLite")
  if( "data.table" %in% class(value) )  {
    dt <- TRUE # Is value a data.table, if so use more efficient methods
  } else {
    dt <- FALSE
  }
  # Convert factors to character
  factorCols <- names( Filter( function(x) x=="factor", vapply( value, class, "" ) ) )
  if(length(factorCols>0)) {
    for( cl in which( colnames(value) %in% factorCols ) ) {
      cn <- colnames(value)[cl]
      factorTable <- data.frame( levels=levels(value[[ cn ]]) )
      factorTable$levelKey <- seq(nrow(factorTable))
      fctNm <- paste0(name,factorName,cn)
      dbWriteTable( conn = conn, name = fctNm, value = factorTable, row.names=FALSE, overwrite=TRUE )
      if( dt )  set( x=value, j=cl, value=as.character(value[[ cn ]]) )
    }
    if( !dt )  value <- japply( value, which( colnames(value) %in% factorCols ), as.character )
  } else {
    #warning("No factor columns detected.")
  }
  if( append ) {
    # If we're appending, check that the number of columns of the new table is equal to the number of columns of the old table
    # Only run this code if we're appending, because otherwise the table won't exist
    sqlColnames <- dbGetColnames( conn, name )
    colnamesSubset <- !all( sqlColnames %in% colnames(value) )
    colnamesSuperset <- !all( colnames(value) %in% sqlColnames )
    if( colnamesSuperset ) {
      addCols <- colnames(value)[ !colnames(value) %in% sqlColnames ]
      for( ac in addCols ) {
        warning(paste("Adding column",ac,"to SQL table"))
        dbSendQuery( conn,
                     paste(
                       "ALTER TABLE",
                       name,
                       "ADD COLUMN",
                       ac,
                       "DEFAULT NULL"
                     )
        )
      }
    } # If it's a superset but not a subset, then we're done (allow it to return back to the second if where it just writes value directly)
    if( colnamesSubset ) {
      # Write our database to a temporary table
      tempTableName <- "temp_dbWriteFactorTable"
      if(dbExistsTable(conn,tempTableName))  dbRemoveTable(conn,tempTableName)
      dbWriteTable( conn = conn, name=tempTableName, value = value, row.names=FALSE, append=FALSE  )
      # Add any columns to input data.frame that are in target table, then merge
      sqlColnames <- dbGetColnames( conn, name ) # Reset these now that we've possibly tinkered with them in the superset section
      dfColnames <- sqlColnames
      dfColnames[ !sqlColnames %in% colnames(value) ] <- "null"
      status <- dbSendQuery( conn, 
                             paste( 
                               "INSERT INTO", name, 
                               "(",paste(sqlColnames,collapse=","),")",
                               "SELECT",
                               paste( dfColnames, collapse="," ),
                               "FROM",
                               tempTableName
                             )
      )
      # Remove temporary table
      dbRemoveTable(conn,tempTableName)
    }
  } 
  if( !append || (append & !colnamesSubset) ) { # Either we're not appending, or the columns in the input and target tables exactly match (possibly after we added columns with the superset code)
    status <- dbWriteTable( conn = conn, name = name, value = value, append=append, ... )
  }
  return( status )
}

#' Read a table via RSQLite with factors stored in another table
#' @param conn The connection object (created with e.g. dbConnect)
#' @param name The name of the table to read
#' @param query A character string containing sequel statements to be appended onto the query (e.g. "WHERE x==3")
#' @param dt Whether to return a data.table vs. a plain-old data.frame
#' @param factorName The base name of the tables to store the factor labels in in the SQLite database (e.g. if factorName is "_factor_" and the data.frame in value contains a factor column called "color" and the name is "mytable" then dbWriteFactorTable will expect there to be a table called mytable_factor_color which holds the levels information)
#' @param \dots Options to pass along to dbGetQuery
#' @return A data.table or data.frame
dbReadFactorTable <- function( conn, name, query="", dt=TRUE, factorName="_factor_", ... ) {
  require(RSQLite)
  # Test inputs
  stopifnot(class(conn)=="SQLiteConnection")
  stopifnot(class(name)=="character")
  stopifnot(class(factorName)=="character")
  if( grepl("[.]",factorName) ) stop("factorName must use valid characters for SQLite")
  # Read main table
  if( dt ) {
    value <- as.data.table( dbGetQuery( conn, paste("SELECT * FROM",name,query), ... ) )
  } else {
    value <- dbGetQuery( conn, paste("SELECT * FROM",name,query), ... )
  }
  # Convert factors to character
  factorCols <- sub( paste0("^.*",name,factorName,"(.+)$"), "\\1", 
                     Filter( Negate(is.na), 
                             str_extract( dbListTables( conn ), paste0(".*",name,factorName,".*") ) 
                     )
  )
  if(length(factorCols>0)) {
    for( cn in factorCols ) {
      fctNm <- paste0(name,factorName,cn)
      factorTable <- dbGetQuery( conn, paste0("SELECT * FROM ",fctNm) )
      if( dt ) {
        cl <- which( colnames(value) %in% cn )
        set( x=value, j=cl, value=factor( value[[ cn ]], levels=factorTable$levels ) )
      } else {
        value[[ cn ]] <- factor( value[[ cn ]], levels=factorTable$levels )
      }
    }
  } else {
    warning("No factor columns detected.")
  }
  value
}