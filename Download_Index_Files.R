# TODO: Add comment
# 
# Author:  Brad
# File:    Download_Index_Files.R
# Version: 1.0
# Date:    02.5.2014
# Purpose: This file downloads the index files from Edgar. Edgar has an index file for each quarter and 
#          year so you need to grab each one.  
#
###############################################################################

###############################################################################
# INITIAL SETUP;
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

# Clear workspace
rm(list = ls(all = TRUE))

# Limit History to not exceed 50 lines
Sys.setenv(R_HISTSIZE = 500)

repo <- c("http://cran.us.r-project.org")
options(repos = structure(repo))
options(install.packages.check.source = FALSE)
# String as factors is False -- used for read.csv
options(StringsAsFactors = FALSE)

# Default maxprint option
options(max.print = 500)
# options(max.print=99999)

# Memory limit
#memory.limit(size = 8183)

# Set location (1=HOME,2=WORK,3=CORALSEA FROM HOME,4=CORALSEA FROM WORK) Location <- 1
Location <- 2

if (Location == 1) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/Fund_Letters/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 2) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Letters/Data/",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R/",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp3/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp3/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/Brad/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data/", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp3/", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp3/",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R/", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
# FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

update.packages(ask=FALSE, checkBuilt=TRUE)

#Load External Packages
#external_packages <- c("compare","cwhmisc","data.table","fastmatch","foreign","formatR","gdata","gtools",
#                       "Hmisc","koRpus","mitools","pbapply","plyr","R.oo","reshape2","rJava","RWeka","RWekajars",
#                       "Snowball","sqldf","stringr","tcltk","tm")
#external_packages <- c("httr","rjson","RCurl","ROAuth","selectr","XML")
external_packages <- c("data.table","RCurl")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

#=====================================================================;
#PARAMETERS;
#=====================================================================;

#If using windows, set to "\\" - if mac (or unix), set to "/";
slash <- "\\"

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
if (file.exists(paste(output_directory, slash, sep = slash, collapse = slash))) {
  cat("output_directory exists and is a directory")
} else if (file.exists(output_directory)) {
  cat("projectroot exists but is a file")
  # you will probably want to handle this separately
} else {
  cat("output_directory does not exist  - creating")
  #dir.create(file.path(output_directory, indexfolder),showWarnings = TRUE)
  dir.create(output_directory,showWarnings = TRUE)
  
}

#Check to see if index folder exists.  If not, create it.
if (file.exists(paste(output_directory, indexfolder, slash, sep = slash, collapse = slash))) {
  cat("indexfolder exists in output_directory and is a directory")
} else if (file.exists(paste(output_directory, indexfolder, sep = slash, collapse = slash))) {
  cat("indexfolder exists in output_directory but is a file")
  # you will probably want to handle this separately
} else {
  cat("indexfolder does not exist in output_directory - creating")
  dir.create(paste(output_directory, indexfolder, sep = slash, collapse = slash),showWarnings = TRUE)
  
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
    
    fonly <- paste(output_directory,slash,indexfolder,slash,"company",yr,qtr,".zip",sep="")
    
    fidx <- paste(output_directory,slash,indexfolder,slash,"company",yr,qtr,".idx",sep="")
    
    download.file(paste("ftp://",ftp,filetoget,sep=""), fonly, quiet = FALSE, mode = "wb",cacheOK = TRUE)
    
    # unzip the files
    unzip( fonly , files="company.idx",exdir = paste(output_directory, indexfolder, sep = slash, collapse = slash) )
    
    # rename file
    file.rename(paste(output_directory,indexfolder,"company.idx",sep=slash), fidx)
    
  } 
  
} 


