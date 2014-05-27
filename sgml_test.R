# http://iangow.wordpress.com/2011/08/29/getting-sec-filing-header-files/



get_sgml_file <- function(path) {
  
  # The name of the local file to be created and the remote file from which
  # it will be created.
  directory <- "/home/iangow/Documents/WRDS/filings/the_filings"
  local_filename <- gsub("^edgar\\/data(.*)\\.txt$", 
                         paste(directory,"\\1",".hdr.sgml",sep=""), 
                         path, perl=TRUE)
  remote_filename <- gsub("\\.txt$", ".hdr.sgml", path, perl=TRUE)                        
  
  # Only download the file if we don't already have a local copy
  if (!file.exists(local_filename)) {
    
    ftp <- paste("http://www.sec.gov/Archives",
                 dirname(path),
                 gsub("-|(.txt$)","",basename(path),perl=TRUE),
                 basename(remote_filename), sep="/") 
    dir.create(dirname(local_filename), showWarnings=FALSE)
    try(download.file(url=ftp, destfile=local_filename) )
  }                      
  
  # Return the local filename if the file exists
  if (file.exists(local_filename)) { 
    return(local_filename) 
  } else { return(NA) } 
}


# Connect to my database
library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
pg <- dbConnect(drv, db="crsp")

# Get FirstCall identifier, announcement date, and CIK for all observations on 
# the FirstCall CIG file. I can get CIK matches for almost all observations and
# there are very few (about 6) Security_IDs with more than one CIK match; 
# I guess that the correct CIK match will be arrived at by the limited time
# window used below.
dbGetQuery(pg, "DROP TABLE IF EXISTS cig_ciks")
dbGetQuery(pg,"CREATE TABLE cig_ciks AS SELECT * FROM
    (SELECT DISTINCT \"Security_ID\" AS security_id, anndate, cusip AS cusip8
        FROM fc.cig) AS a
    LEFT JOIN (SELECT DISTINCT substr(cusip, 1,8) AS cusip8, 
           (cik::integer)::text AS cik
        FROM (SELECT DISTINCT gvkey, cusip FROM comp.secm) AS b
        INNER JOIN (SELECT DISTINCT gvkey, cik FROM comp.company 
                   WHERE cik IS NOT NULL) AS c
        USING (gvkey)) AS d
    USING (cusip8)")

# Pull together a list of 8-K filings within the five-day window beginning with
# the announcement on CIG; I should be more careful here with weekends, etc.
file.list <- dbGetQuery(pg, "
    SELECT * 
    FROM cig_ciks AS a
    INNER JOIN filings.filings AS b
    USING (cik)
    WHERE b.date_filed BETWEEN a.anndate AND a.anndate + interval '4 days'
                 AND form_type='8-K'")



# Get the files
file.list$sgml_file <- unlist(lapply(file.list$file_name, get_sgml_file))




# Extract the list of items for each filing
item_scan <- function(sgml_file) {
  con <- file(sgml_file)
  items <- grep("^<ITEMS>", readLines(con=con), value=TRUE, perl=TRUE)
  close(con)
  items <- gsub("^<ITEMS>","", items, perl=TRUE)
  return(items)
}

file.list$items <- lapply(file.list$sgml_file, item_scan)

# A function to create an indicator for the presence of a given item
has_item <- function(item_list, item) {
  is.element(item, unlist(item_list))
}

# Create an indicator for the presence of Item 9.01 on each 8-K filing
file.list$has_2.02 <- unlist(lapply(file.list$items, has_item, "2.02"))
file.list$has_7.01 <- unlist(lapply(file.list$items, has_item, "7.01"))
file.list$has_9.01 <- unlist(lapply(file.list$items, has_item, "9.01"))

