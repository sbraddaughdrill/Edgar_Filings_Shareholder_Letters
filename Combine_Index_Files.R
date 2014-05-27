# TODO: Add comment
# 
# Author:  Brad
# File:    Combine_Index_Files.R
# Version: 1.0
# Date:    02.5.2014
# Purpose: This program reads the company.idx files and merges them together
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
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
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
#startyear <- 2006
#startyear <- 2012

#Last year you want index files for:
endyear <- 2013
#endyear <- 2012

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

#Output folder:
indexfolder <- "full-index"

#downloadfolder <- "N-1"
#downloadfolder <- "DEF 14A"
#downloadfolder <- "EX"

#The sub directory you are going to download filings to
originalfolder <- "original"

#The file that will contain the filings you want to download.
outfile <- "index_combined.csv"

#Specifiy, in regular expression format, the filing you are looking for.
#Following is the for 10-k.
#In this case, I only want to keep 10-ks.
#I put a ^ at the beginning because I want the form type to start with 10, this gets rid of NT late filings.
#I also want to exclude amended filings so I specify that 10-k should not be followed by / (e.g., 10-K/A).

#formget <- '(N-1  |N-1/A  |N-1/A  |N-1A  |N-1A/A  |N-1A EL  |N-1A EL/A  |497K  |497K1  |497K2  |497K3A  |497K3B  )'
#formget <- c("N-1","N-1/A","N-1/A","N-1A","N-1A/A","N-1A EL","N-1A EL/A","497K","497K1","497K2","497K3A","497K3B")
#formget <- c("EX*")


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
#download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
#create_directory(download_folder_path,remove=1)

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

#Create Empty Output DF
expanded_cols <- c("company_name","form_type","cik","file_date","fullfilename_txt","fullfilename_htm","filing_seq")
output_df <- data.frame(matrix(NA, nrow = 0, ncol = 7,dimnames = list(NULL,expanded_cols)),
                       stringsAsFactors=FALSE)

#Path to outfile
ofidx <- paste(output_directory,indexfolder,outfile,sep=slash)

for (yr in startyear:endyear)
{
  #yr <- startyear
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
  #yr_folder_path <- paste(output_directory, downloadfolder, yr, sep = slash, collapse = slash)   
  #create_directory(yr_folder_path,remove=1)
  
  #Check to see if originalfolder folder exists.  If not, create it.
  #original_folder_path <- paste(output_directory, downloadfolder, yr, originalfolder, sep = slash, collapse = slash)    
  #create_directory(original_folder_path,remove=1)
  
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
    expanded_cols <- c("company_name","form_type","cik","file_date","fullfilename_txt","fullfilename_htm","filing_seq")
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
    input_df[,"filing_seq"] <- ifelse(is.na(input_df[,"form_type"]), NA, 0)
    
    input_df[,"fullfilename_htm"] <- gsub(".txt","",input_df[,"fullfilename_htm"])
    
    input_df[,"fullfilename_htm"] <- paste(gsub(".txt","",input_df[,"fullfilename_htm"]),"-index.htm",sep="")
    
    input_df_trim <- input_df[(!is.na(input_df[,"filing_seq"])),]
    input_df_trim <- subset(input_df_trim,select=-c(input))
    
    input_df_trim[,"file_date"] <- as.character(input_df_trim[,"file_date"])
    input_df_trim[,"filing_seq"] <- as.integer(seq(1,nrow(input_df_trim)))
    input_df_trim[,"filing_seq"] <- paste(yr,qtr,sprintf("%08d", input_df_trim[,"filing_seq"]),sep="_")
    row.names(input_df_trim) <- seq(nrow(input_df_trim))

    #write.table(input_df_trim,file=ofidx,na="",sep=",",quote=TRUE,row.names=FALSE,col.names=FALSE,append=TRUE)
    #unique_cik <- unique(input_df_trim[,"cik"])
    
    #if (length(unique_cik) != 0)
    #{
    #  cik_list <- rbind(cik_list,data.frame(CIK=NA,CIK_no_pad=unique_cik,stringsAsFactors=FALSE))   
    #  
    #} else
    #{
    #  cat( "\n", "No Desired Filings in", yr,",qtr",qtr, "\n") 
    #}
    
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
    #summary[(summary[,"yr"]==yr & summary[,"qtr"]==qtr),"good_filing_count"] <- sum(input_df[,"filing_seq"], na.rm = TRUE)
    
    output_df <- rbind(output_df,input_df_trim)
    
    rm(input_df_trim)
  } 
  
} 

#Clean table
output_df <- data.frame(lapply(output_df, as.character), stringsAsFactors=FALSE)
#colnames(output_df) <- c("company_name","form_type","cik","file_date","fullfilename_txt","fullfilename_htm","filing_seq")

#output_df <- output_df[!(output_df[,"company_name"])=="company_name",]
#output_df <- output_df[!(output_df[,"form_type"])=="form_type",]
#output_df <- output_df[!(output_df[,"cik"])=="cik",]
#output_df <- output_df[!(output_df[,"fullfilename_txt"])=="fullfilename_txt",]
#output_df <- output_df[!(output_df[,"fullfilename_htm"])=="fullfilename_htm",]
#output_df <- output_df[!(output_df[,"filing_seq"])=="filing_seq",]

for(i in which(sapply(output_df,class)=="character"))
{
  output_df[[i]] = trim(output_df[[i]])
}
for (i in 1:ncol(output_df))
{
  
  output_df[,i] <- unknownToNA(output_df[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                        NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                        NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
} 
output_df <- unique(output_df)
row.names(output_df) <- seq(nrow(output_df))

#Fix columns
output_df[,"file_date"] <- as.Date(output_df[,"file_date"],format="%Y-%m-%d")

output_df <- output_df[order(output_df[,"filing_seq"]),]
row.names(output_df) <- seq(nrow(output_df))


#Output cleaned table
write.csv(output_df,file=ofidx,na="",quote=TRUE,row.names=FALSE)


