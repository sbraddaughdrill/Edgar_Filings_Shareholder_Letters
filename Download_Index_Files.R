#install.packages("httr")
#install.packages("RCurl")
#install.packages("rjson")
#install.packages("ROAuth")
#install.packages("selectr")
#install.packages("XML")

library(httr)
library(rjson)
library(RCurl)
library(ROAuth)
library(selectr)
library(XML)

#=====================================================================;
#SOURCE DIRECTORY;
#=====================================================================;

codedirect <- "C:\\Users\\Brad\\Documents\\GitHub\\Edgar_Filings_Shareholder_Letters"     # Home

#=====================================================================;

#=====================================================================;
#PROJECT ROOT DIRECTORY;
#=====================================================================;
projectrootdirectory <- "C:\\Research_temp3"
#=====================================================================;

#=====================================================================;
#PARAMETERS;
#=====================================================================;

#If using windows, set to "\\" - if mac (or unix), set to "/";
slash <- '\\'

#First year you want index files for:
startyear <- 1993
#Last year you want index files for:
endyear <- 2012
#First qtr you want index files for (usually 1):
startqtr <- 1
#Last qtr you want index files for (usually 4):
endqtr <- 4
#Output folder:
indexfolder <- "full-index"


#=====================================================================;
#BEGIN SCRIPT;
cat("Begin Script \n")
#=====================================================================;

#Check to see if project root directory exists.  If not, create it.
if (file.exists(paste(projectrootdirectory, slash, sep = slash, collapse = slash))) {
  cat("projectrootdirectory exists and is a directory")
} else if (file.exists(projectrootdirectory)) {
  cat("projectroot exists but is a file")
  # you will probably want to handle this separately
} else {
  cat("projectrootdirectory does not exist  - creating")
  #dir.create(file.path(projectrootdirectory, indexfolder),showWarnings = TRUE)
  dir.create(projectrootdirectory,showWarnings = TRUE)
  
}

#Check to see if index folder exists.  If not, create it.
if (file.exists(paste(projectrootdirectory, indexfolder, slash, sep = slash, collapse = slash))) {
  cat("indexfolder exists in projectrootdirectory and is a directory")
} else if (file.exists(paste(projectrootdirectory, indexfolder, sep = slash, collapse = slash))) {
  cat("indexfolder exists in projectrootdirectory but is a file")
  # you will probably want to handle this separately
} else {
  cat("indexfolder does not exist in projectrootdirectory - creating")
  dir.create(paste(projectrootdirectory, indexfolder, sep = slash, collapse = slash),showWarnings = TRUE)
  
}


#get index files;
#FTP signin-
#$ftp = Net::FTP->new("ftp.sec.gov", Debug => 0) or die "Cannot connect to some.host.name: $@";

#This provides your user name and password.
#$ftp->login("anonymous",'-anonymous@') or die "Cannot login ", $ftp->message;

ftp <- "ftp.sec.gov"

#Get files loop- The program will loop through each year specified.
#Note that the counter (yr) starts with a value equal to start year and increments by 1 each time through.  The loop terminates after the counter exceeds $endyear.
yr <- startyear
qtr <- startqtr

for (yr in startyear:endyear)
{
  #yr <- startyear
  
  cat(yr,"\n")
  
  if (yr<endyear)
  {
    eqtr  <- 4
    
  } else
  {
    eqtr  <- endqtr
  }
  
  for (qtr in startqtr:endqtr)
  {
    #qtr <- startqtr
    
    cat(qtr,"\n")

    filetoget <- paste("/edgar/full-index/",yr,"/QTR",qtr,"/company.zip",sep="")
    
    fonly <- paste(projectrootdirectory,slash,indexfolder,slash,"company",yr,qtr,".zip",sep="")
    
    fidx <- paste(projectrootdirectory,slash,indexfolder,slash,"company",yr,qtr,".idx",sep="")
    
    download.file(paste("ftp://",ftp,filetoget,sep=""), fonly, quiet = FALSE, mode = "wb",cacheOK = TRUE)
    
    # unzip the files
    unzip( fonly , files="company.idx",exdir = paste(projectrootdirectory, indexfolder, sep = slash, collapse = slash) )
    
    # rename file
    file.rename(paste(projectrootdirectory,indexfolder,"company.idx",sep=slash), fidx)
    
  } 
  
} 


