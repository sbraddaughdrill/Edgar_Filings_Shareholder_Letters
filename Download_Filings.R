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
Location <- 2

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

#startyear <- 1993
startyear <- 2006

#Last year you want index files for:
#endyear <- 2012
endyear <- 2012

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

#Output folder:
indexfolder <- "full-index"

#downloadfolder <- "N-1"
downloadfolder <- "DEF 14A"

#The sub directory you are going to download filings to
originalfolder <- "original"

#The file that will contain the filings you want to download.
outfile <- "yearly_filings.csv"

#Specifiy, in regular expression format, the filing you are looking for.
#Following is the for 10-k.
#In this case, I only want to keep 10-ks.
#I put a ^ at the beginning because I want the form type to start with 10, this gets rid of NT late filings.
#I also want to exclude amended filings so I specify that 10-k should not be followed by / (e.g., 10-K/A).

#formget <- '(N-1  |N-1/A  |N-1/A  |N-1A  |N-1A/A  |N-1A EL  |N-1A EL/A  |497K  |497K1  |497K2  |497K3A  |497K3B  )'
#formget <- c("N-1","N-1/A","N-1/A","N-1A","N-1A/A","N-1A EL","N-1A EL/A","497K","497K1","497K2","497K3A","497K3B")
formget <- c("DEF 14A")


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
  #yr <- 2005
  
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
  
  #Check to see if originalfolder folder exists.  If not, create it.
  original_folder_path <- paste(output_directory, downloadfolder, yr, originalfolder, sep = slash, collapse = slash)    
  create_directory(original_folder_path,remove=1)
  
  #Path to outfile
  ofidx <- paste(output_directory,downloadfolder,yr,outfile,sep=slash)
  
  for (qtr in startqtr:endqtr)
  {
    #qtr <- startqtr
    
    cat(qtr,"\n")
    
    #Path to infile
    fidx <- paste(output_directory,slash,indexfolder,slash,"company",yr,qtr,".idx",sep="")
    
    #Read infile
    con <- file(fidx, 'r') 
    input <- readLines(con)
    close(con)
    
    #Convert to data.frame
    expanded_cols <- c("company_name","form_type","cik","file_date","fullfilename_txt","fullfilename_htm","good_filing_flag")
    input_df <- data.frame(input,
                           matrix(NA, nrow = length(input), ncol = 7,dimnames = list(NULL,expanded_cols)),
                           stringsAsFactors=FALSE)
    
    #Populate new columns
    input_df[,"company_name"] <- substring(input_df[,"input"], company_name_start, company_name_start+company_name_length)  
    input_df[,"form_type"] <- substring(input_df[,"input"], form_type_start, form_type_start+form_type_length)
    input_df[,"cik"] <- substring(input_df[,"input"], cik_start, cik_start+cik_length)
    input_df[,"file_date"] <- substr(input_df[,"input"], file_date_start, file_date_start+file_date_length)
    input_df[,"fullfilename_txt"] <- substr(input_df[,"input"], fullfilename_start, fullfilename_start+fullfilename_length)
    input_df[,"fullfilename_htm"] <- input_df[,"fullfilename_txt"] 
  
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
    
    input_df[,"fullfilename_htm"] <- gsub(".txt","",input_df[,"fullfilename_htm"])
    
    input_df[,"fullfilename_htm"] <- paste(gsub(".txt","",input_df[,"fullfilename_htm"]),"-index.htm",sep="")
    
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
      cat( "\n", "No Desired Filings in", yr,",qtr",qtr, "\n") 
    }
    
    #Open the file you want to write to.  The first time through the file is opened to "replace" the existing file.
    #After that, it is opened to append .
    
    #     if (yr==startyear & qtr==startqtr ) {
    #       cat("replace \n")
    #       
    #       write(good_filings,ofidx,append=FALSE)
    #       
    #     } else {
    #       
    #       cat("append \n")
    #       
    #       write(good_filings,ofidx,append=TRUE)
    #       
    #     }
    
    #write(good_filings,ofidx,append=TRUE)
    
    
    #Put count in summary table
    summary[(summary[,"yr"]==yr & summary[,"qtr"]==qtr),"good_filing_count"] <- sum(input_df[,"good_filing_flag"], na.rm = TRUE)
    
  } 
  
  
  #Clean table
  filings_org <- read.csv(ofidx,na.strings = "NA",header = FALSE)
  filings_df <- data.frame(lapply(filings_org, as.character), stringsAsFactors=FALSE)
  colnames(filings_df) <- c("company_name","form_type","cik","file_date","fullfilename_txt","fullfilename_htm","good_filing_flag")
  
  filings_u <- filings_df
  filings_u <- filings_u[!(filings_u[,"company_name"])=="company_name",]
  filings_u <- filings_u[!(filings_u[,"form_type"])=="form_type",]
  filings_u <- filings_u[!(filings_u[,"cik"])=="cik",]
  filings_u <- filings_u[!(filings_u[,"fullfilename_txt"])=="fullfilename_txt",]
  filings_u <- filings_u[!(filings_u[,"fullfilename_htm"])=="fullfilename_htm",]
  filings_u <- filings_u[!(filings_u[,"good_filing_flag"])=="good_filing_flag",]
  
  for(i in which(sapply(filings_u,class)=="character"))
  {
    filings_u[[i]] = trim(filings_u[[i]])
  }
  for (i in 1:ncol(filings_u))
  {
    
    filings_u[,i] <- unknownToNA(filings_u[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  } 
  filings_u <- unique(filings_u)
  row.names(filings_u) <- seq(nrow(filings_u))
  
  #Fix columns
  filings_u[,"file_date"] <- as.Date(filings_u[,"file_date"],format="%Y-%m-%d")
  filings_u[,"good_filing_flag"] <- as.numeric(filings_u[,"good_filing_flag"])
  
  #Output cleaned table
  write.csv(filings_u,file=ofidx,na="",quote=TRUE,row.names=FALSE)
  
  
  
  
  #Get name of all files already downloaded
  old <- list.files(original_folder_path)
  
  #Get the names of the files to download
  #ocon <- file(ofidx, 'r') 
  files_to_download <- data.frame(fullfilename=filings_u[,"fullfilename_txt"],
                                  filename_start=NA,
                                  filename=NA,
                                  already_downloaded=NA,
                                  stringsAsFactors=FALSE)
  #close(ocon)
  
  #Find starting position of file name
  #files_to_download[,"filename_start"] <- regexpr("\\.[^\\.]*$", files_to_download[,"fullfilename"])
  files_to_download[,"filename_start"] <- sapply(gregexpr("/", files_to_download[,"fullfilename"]), function(x) rev(x)[1])
  
  #Create short filename
  files_to_download[,"filename"] <- substr(files_to_download[,"fullfilename"], 
                                           (files_to_download[,"filename_start"]+1), 
                                           nchar(files_to_download[,"fullfilename"]))
  
  #checks to see what files on the current index listing are not in the directory
  files_to_download[,"already_downloaded"] <- ifelse(files_to_download[,"filename"] %in% old, 1, 0)
  
  #Download files
  for (i in which(files_to_download[,"already_downloaded"]==0))
  {
    #i<- head(which(files_to_download[,"already_downloaded"]==0),1)
    
    fileout <- paste(original_folder_path,files_to_download[i,"filename"],sep=slash)
    
    download.file(paste("ftp://",ftp,"/",files_to_download[i,"fullfilename"],sep=""), fileout, quiet = FALSE, mode = "wb",cacheOK = TRUE)
    
    cat("File",i,"of",length(which(files_to_download[,"already_downloaded"]==0)),"\n")

  } 
  
} 

