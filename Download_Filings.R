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

downloadfolder <- "N-1"

#The sub directory you are going to download filings to
originalfolder <- "original"

#The file that will contain the filings you want to download.
outfile <- "getfiles.txt"

#Specifiy, in regular expression format, the filing you are looking for.
#Following is the for 10-k.
#In this case, I only want to keep 10-ks.
#I put a ^ at the beginning because I want the form type to start with 10, this gets rid of NT late filings.
#I also want to exclude amended filings so I specify that 10-k should not be followed by / (e.g., 10-K/A).

formget <- '(N-1  |N-1/A  |N-1/A  |N-1A  |N-1A/A  |N-1A EL  |N-1A EL/A  |497K  |497K1  |497K2  |497K3A  |497K3B  )'

company_name_start <- 0
company_name_length <- 61
form_type_start <- 62
form_type_length <- 12
cik_start <- 75
cik_length <- 10
file_date_start <- 86
file_date_length <- 10
fullfilename_start <- 98
fullfilename_length <- 43


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

#Check to see if download folder exists.  If not, create it.
if (file.exists(paste(projectrootdirectory, downloadfolder, slash, sep = slash, collapse = slash))) {
  cat("downloadfolder exists in projectrootdirectory and is a directory")
} else if (file.exists(paste(projectrootdirectory, downloadfolder, sep = slash, collapse = slash))) {
  cat("downloadfolder exists in projectrootdirectory but is a file")
  # you will probably want to handle this separately
} else {
  cat("downloadfolder does not exist in projectrootdirectory - creating")
  dir.create(paste(projectrootdirectory, downloadfolder, sep = slash, collapse = slash),showWarnings = TRUE)
  
}

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
  
  
  #Check to see if yr folder exists.  If not, create it.
  if (file.exists(paste(projectrootdirectory, downloadfolder, yr, slash, sep = slash, collapse = slash))) {
    cat("yr exists in projectrootdirectory and is a directory")
  } else if (file.exists(paste(projectrootdirectory, downloadfolder, yr, sep = slash, collapse = slash))) {
    cat("yr exists in projectrootdirectory but is a file")
    # you will probably want to handle this separately
  } else {
    cat("yr does not exist in projectrootdirectory - creating")
    dir.create(paste(projectrootdirectory, downloadfolder, yr, sep = slash, collapse = slash),showWarnings = TRUE)
    
  }
  
  for (qtr in startqtr:endqtr)
  {
    #qtr <- startqtr
    
    cat(qtr,"\n")
    
    fidx <- paste(projectrootdirectory,slash,indexfolder,slash,"company",yr,qtr,".idx",sep="")
    
    con <- file(fidx, 'r') 
    
    ofidx <- paste(projectrootdirectory,downloadfolder,yr,outfile,sep=slash)
    
    #Open the file you want to write to.  The first time through the file is opened to "replace" the existing file.
    #After that, it is opened to append .
    
    #     if (yr=startyear & qtr=startqtr ) {
    #       cat("replace \n")
    #       
    outfile <- file(ofidx, 'w') 
    #       
    #     } else {
    #       
    #       cat("append \n")
    #       
    #       outfile <- file.append(ofidx, con) 
    #       
    #     }
    
    #     
    #     while (length(input <- readLines(con, n=1000) > 0){ 
    #       for (i in 1:length(input)){ 
    #         
    #         ......your one line at a time processing 
    #         
    #         
    #       } 
    #       writeLines(output, con=outfile) 
    #     } 
    #     
    
    input <- readLines(con)
    input_df <- data.frame(input,
                           matrix(NA, nrow = length(input), ncol = 5,dimnames = list(NULL,c("company_name","form_type","cik","file_date","fullfilename"))),
                           stringsAsFactors=FALSE)
    
    count <- 1
    
    #     for (i in 11:nrow(input_df))
    #     {
    #       #i <- 10
    #       
    #       cat(i,"\n") 
    #       
    #       #Ignore the first 10 lines because they only contain header information
    #       if (i<11) {
    #         cat("Header Information \n")
    #         
    #       } else {
    #         cat("File Information \n")
    # 
    #         input_df[i,"form_type"] <- substr(input_df[i,"input"], 62, 12)
    #         input_df[i,"file_date"] <- substr(input_df[i,"input"], 86, 10)
    #         input_df[i,"fullfilename"] <- substr(input_df[i,"input"], 98, 43)
    #       
    #       }
    #       
    #     }
    
    input_df[,"company_name"] <- substring(input_df[,"input"], company_name_start, company_name_start+company_name_length)  
    input_df[,"form_type"] <- substring(input_df[,"input"], form_type_start, form_type_start+form_type_length)
    input_df[,"cik"] <- substring(input_df[,"input"], cik_start, cik_start+cik_length)
    input_df[,"file_date"] <- substr(input_df[,"input"], file_date_start, file_date_start+file_date_length)
    input_df[,"fullfilename"] <- substr(input_df[,"input"], fullfilename_start, fullfilename_start+fullfilename_length)
    
    for (i in 1:10)
    {
      #i <- 1
      
      input_df[i,"company_name"] <- NA
      input_df[i,"form_type"] <- NA
      input_df[i,"cik"] <- NA
      input_df[i,"file_date"] <- NA
      input_df[i,"fullfilename"] <- NA
      
    }
    
    
    
  } 
  
} 

