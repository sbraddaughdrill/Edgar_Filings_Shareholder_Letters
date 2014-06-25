# http://iangow.wordpress.com/2011/08/26/getting-sec-filing-index-files/

getSECIndexFile <- function(year, quarter) {
  
  # Download the zipped index file from the SEC website
  tf <- tempfile()
  result <- try(download.file(
    url=paste("http://www.sec.gov/Archives/edgar/full-index/",
              year,"/QTR", quarter, "/company.zip",sep=""),
    destfile=tf))
  
  # If we didn't encounter and error downloading the file, parse it
  # and return as a R data frame
  if (!inherits(result, "try-error")) {
    
    # Small function to remove leading and trailing spaces
    trim <- function (string) {
      string <- enc2native(string)
      gsub("^\\s*(.*?)\\s*$","\\1", string, perl=TRUE)
    }
    
    # Read the downloaded file
    raw.data <- readLines(con=(zz<- unz(description=tf,
                                        filename="company.idx")))
    close(zz)
    raw.data <- raw.data[11:length(raw.data)] # Remove the first 10 rows.
    
    # Parse the downloaded file and return the extracted data as a data frame
    company_name <- trim(substr(raw.data,1,62))
    form_type <- trim(substr(raw.data,63,74))
    cik <- trim(substr(raw.data,75,86))
    date_filed <- as.Date(substr(raw.data,87,98))
    file_name <- trim(substr(raw.data,99,150))
    rm(raw.data)
    return(data.frame(company_name, form_type, cik, date_filed, file_name))
  } else { return(NULL)} 
}


addIndexFileToDatabase <- function(data) {
  if (is.null(data)) return(NULL)
  library(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  pg <- dbConnect(drv, db="crsp")
  
  rs <- dbWriteTable(pg, c("filings", "filings"), data, append=TRUE)
  dbDisconnect(pg)
  return(rs)
} 


library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
pg <- dbConnect(drv, db="crsp")
dbGetQuery(pg, "DROP TABLE IF EXISTS filings.filings")

for (year in 1993:2011) {
  for (quarter in 1:4) {
    addIndexFileToDatabase(getSECIndexFile(year, quarter))
  }
}

