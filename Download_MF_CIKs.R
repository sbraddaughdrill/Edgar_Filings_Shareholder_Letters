# TODO: Add comment
# 
# Author:  Brad
# File:    Download_Index_Files.R
# Version: 1.0
# Date:    02.5.2014
# Purpose: This program reads the company.idx files and then files are 
#          downloaded to separate year directories
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
Location <- 1

if (Location == 1) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 2) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp3", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp3", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
# FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)

create_directory <- function(path,remove=1){
  if (file.exists(path)) {
    
    if (file.info(path)$isdir) {
      cat(path,"exists and is a directory. \n")
      
    } else {
      
      if (remove==1) {
        cat(path,"exists and is a file.  File will be removed and directory will be created.  \n")
        file.remove(path)
        dir.create(path,showWarnings = TRUE)
        
      } else {
        cat(path,"exists and is a file.  File will not be removed and directory will not be created. \n")
        
      }
      
    }
    
  } else {
    cat(path,"does not exist and will be created. \n")
    dir.create(path,showWarnings = TRUE)
    
  }
}


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
#external_packages <- c("compare","cwhmisc","data.table","fastmatch","foreign","formatR","gdata","gtools",
#                       "Hmisc","koRpus","mitools","pbapply","plyr","R.oo","reshape2","rJava","RWeka","RWekajars",
#                       "Snowball","sqldf","stringr","tcltk","tm")
#external_packages <- c("httr","rjson","RCurl","ROAuth","selectr","XML")
external_packages <- c("data.table","gdata","RCurl","reshape2")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

#=====================================================================;
#PARAMETERS;
#=====================================================================;

#If using windows, set to "\\" - if mac (or unix), set to "/";
slash <- "\\"

#First year you want index files for:

startyear <- 1993
#startyear <- 2013

#Last year you want index files for:
endyear <- 2013
#endyear <- 2009

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

#Output folder:
indexfolder <- "full-index"

downloadfolder <- "MF_CIKs"

#The file that will contain the filings you want to download.
outfile <- "MF_filings.csv"

formget <- c("N-1","N-1/A","N-1/A","N-1A","N-1A/A","N-1A EL","N-1A EL/A",
             "497","497J","497K","497K1","497K2","497K3A","497K3B",
             "N-CSR","N-CSRS","N-MFP","N-Q","N-SAR","NSAR-A","NSAR-B","NSAR-B/A","N-PX",
             "485APOS","485BPOS","N-30B-2")

#FTP address
ftp <- "ftp.sec.gov"

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

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)

#Check to see if download folder exists.  If not, create it.
download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(download_folder_path,remove=1)

#Create Summary Table
ts <- ts(NA, start=c(startyear, startqtr), end=c(endyear, endqtr), frequency=4) 

ts_cast <- tapply(ts, list(year = floor(time(ts)), qtr = cycle(ts)), c)
ts_cast_df <- data.frame(yr=row.names(ts_cast),
                         ts_cast,
                         stringsAsFactors=FALSE)
row.names(ts_cast_df) <- seq(nrow(ts_cast_df))
colnames(ts_cast_df) <- c("yr","1","2","3","4")

summary_full <- melt(ts_cast_df,id=c("yr"))
colnames(summary_full) <- c("yr","qtr","good_filing_count")

summary_full <- summary_full[order(summary_full[,"yr"],summary_full[,"qtr"]),]
summary_full[,"qtr"] <- as.integer(summary_full[,"qtr"])

summary <- summary_full
summary <- summary[!(summary[,"yr"]==startyear & summary[,"qtr"]<startqtr),]
summary <- summary[!(summary[,"yr"]==endyear & summary[,"qtr"]>endqtr),]

row.names(summary) <- seq(nrow(summary))


cik_list <- data.frame(CIK=NA,
                       CIK_no_pad=NA,                                
                       stringsAsFactors=FALSE)

#Get filings
yr <- startyear
qtr <- startqtr

for (yr in startyear:endyear)
{
  #yr <- 1993
  #yr <- 2013  
  
  cat(yr,"\n")
  
  if (yr<endyear)
  {
    eqtr  <- 4
    
  } else
  {
    eqtr  <- endqtr
  }
  
  #Check to see if yr folder exists.  If not, create it.
  yr_folder_path <- paste(output_directory, downloadfolder, yr, sep = slash, collapse = slash)   
  create_directory(yr_folder_path,remove=1)

  #Path to outfile
  ofidx <- paste(output_directory,downloadfolder,yr,outfile,sep=slash)
  
  for (qtr in startqtr:endqtr)
  {
    #qtr <- startqtr
    #qtr <- 1
    #qtr <- 2
    #qtr <- 3
    #qtr <- 4
    
    
    cat(qtr,"\n")
    
    #Path to infile
    fidx <- paste(output_directory,slash,indexfolder,slash,"company",yr,qtr,".idx",sep="")
    
    #Read infile
    con <- file(fidx, 'r') 
    input <- readLines(con)
    close(con)
    
    #Convert to data.frame
    expanded_cols <- c("company_name","form_type","cik","file_date","fullfilename","good_filing_flag")
    input_df <- data.frame(input,
                           matrix(NA, nrow = length(input), ncol = 6,dimnames = list(NULL,expanded_cols)),
                           stringsAsFactors=FALSE)
    
    #Populate new columns
    input_df[,"company_name"] <- substring(input_df[,"input"], company_name_start, company_name_start+company_name_length)  
    input_df[,"form_type"] <- substring(input_df[,"input"], form_type_start, form_type_start+form_type_length)
    input_df[,"cik"] <- substring(input_df[,"input"], cik_start, cik_start+cik_length)
    input_df[,"file_date"] <- substr(input_df[,"input"], file_date_start, file_date_start+file_date_length)
    input_df[,"fullfilename"] <- substr(input_df[,"input"], fullfilename_start, fullfilename_start+fullfilename_length)
    
    #Format new columns
    for (i in expanded_cols)
    {
      #i <- 1
      
      input_df[1:10,i] <- NA
      input_df[,i] <- trim(input_df[,i])
      
    }
    
    #Fix Date
    input_df[,"file_date"] <- as.Date(input_df[,"file_date"],format="%Y-%m-%d")
    
    #Determine if filing is one of the ones desired
    input_df[,"good_filing_flag"] <- ifelse(is.na(input_df[,"form_type"]), NA, 0)
    input_df[,"good_filing_flag"] <- ifelse(input_df[,"form_type"] %in% formget, 1, input_df[,"good_filing_flag"])
    
    good_filings <- input_df[(!is.na(input_df[,"good_filing_flag"]) & input_df[,"good_filing_flag"]==1),]
    good_filings <- subset(good_filings,select=-c(input))
    #row.names(good_filings) <- seq(nrow(good_filings))
    
    good_filings[,"file_date"] <- as.character(good_filings[,"file_date"])
    good_filings[,"good_filing_flag"] <- as.character(good_filings[,"good_filing_flag"])
    
    write.table(good_filings,file=ofidx,na="",sep=",",quote=TRUE,row.names=FALSE,col.names=FALSE,append=TRUE)
    unique_cik <- unique(good_filings[,"cik"])
                      
    if (length(unique_cik) != 0)
    {
      cik_list <- rbind(cik_list,data.frame(CIK=NA,CIK_no_pad=unique_cik,stringsAsFactors=FALSE))   
      
    } else
    {
      cat( "\n", "No Mutual Fund Filings in", yr,",qtr",qtr, "\n") 
    }
    
    #Put count in summary table
    summary[(summary[,"yr"]==yr & summary[,"qtr"]==qtr),"good_filing_count"] <- sum(input_df[,"good_filing_flag"], na.rm = TRUE)
    
  } 
  
  #Clean table
  mf_filings_org <- read.csv(ofidx,na.strings = "NA",header = FALSE)
  mf_filings_df <- data.frame(lapply(mf_filings_org, as.character), stringsAsFactors=FALSE)
  colnames(mf_filings_df) <- c("company_name","form_type","cik","file_date","fullfilename","good_filing_flag")
  
  mf_filings_u <- mf_filings_df
  mf_filings_u <- mf_filings_u[!(mf_filings_u[,"company_name"])=="company_name",]
  mf_filings_u <- mf_filings_u[!(mf_filings_u[,"form_type"])=="form_type",]
  mf_filings_u <- mf_filings_u[!(mf_filings_u[,"cik"])=="cik",]
  mf_filings_u <- mf_filings_u[!(mf_filings_u[,"fullfilename"])=="fullfilename",]
  mf_filings_u <- mf_filings_u[!(mf_filings_u[,"good_filing_flag"])=="good_filing_flag",]
  
  for(i in which(sapply(mf_filings_u,class)=="character"))
  {
    mf_filings_u[[i]] = trim(mf_filings_u[[i]])
  }
  for (i in 1:ncol(mf_filings_u))
  {

    mf_filings_u[,i] <- unknownToNA(mf_filings_u[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  } 
  mf_filings_u <- unique(mf_filings_u)
  row.names(mf_filings_u) <- seq(nrow(mf_filings_u))
  
  #Fix columns
  mf_filings_u[,"file_date"] <- as.Date(mf_filings_u[,"file_date"],format="%Y-%m-%d")
  mf_filings_u[,"good_filing_flag"] <- as.numeric(mf_filings_u[,"good_filing_flag"])
  
  #Output cleaned table
  write.csv(mf_filings_u,file=ofidx,na="",quote=TRUE,row.names=FALSE)

  
} 


#Output CIK list

cik_list_u <- unique(cik_list)

cik_list_u <- cik_list_u[(!is.na(cik_list_u[,"CIK_no_pad"])),]

row.names(cik_list_u) <- seq(nrow(cik_list_u))

cik_list_u[,"CIK_no_pad"] <- as.numeric(cik_list_u[,"CIK_no_pad"])
cik_list_u[,"CIK_no_pad"] <- round(cik_list_u[,"CIK_no_pad"], digits = 0)

for (i in 1:ncol(cik_list_u))
{
  cik_list_u[,i] <- unknownToNA(cik_list_u[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                      NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  cik_list_u[,i] <- ifelse(is.na(cik_list_u[,i]),NA, cik_list_u[,i])
} 

#Pad CIK
cik_list_u[,"CIK"] <- format(cik_list_u[,"CIK_no_pad"], trim=TRUE, digits = 0, scientific = 999)
cik_list_u[,"CIK"] <- sprintf("%010s",cik_list_u[,"CIK"])
cik_list_u[,"CIK"] <- gsub(" ", "0", cik_list_u[,"CIK"])

write.table(cik_list_u,file=paste(download_folder_path,"CIK_list.csv",sep="\\"),na="",sep=",",quote=TRUE,row.names=FALSE,append=FALSE)