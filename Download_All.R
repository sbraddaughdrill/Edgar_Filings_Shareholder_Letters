# TODO: Add comment
# 
# Author:  Brad
# File:    Download_All.R
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
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  #output_directory <- normalizePath("//tsclient/C/Research_temp3", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
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
#source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)

###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("gdata","plyr","RCurl")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

###############################################################################
#PARAMETERS;
###############################################################################

#If using windows, set to "\\" - if mac (or unix), set to "/";
slash <- "\\"

#First year you want index files for:

startyear <- 1993
#startyear <- 2006

#Last year you want index files for:
endyear <- 2013
#endyear <- 2012

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

#Output folder:
indexfolder <- "full-index"

downloadfolder <- "N-1A"
#downloadfolder <- "DEF 14A"
#downloadfolder <- "MF_All"

#The sub directory you are going to download filings to
originalfolder <- "original"

#The file that will contain the filings you want to download.
outfile <- "filings.csv"

#FTP address
ftp <- "ftp.sec.gov"

#Specifiy, in regular expression format, the filing you are looking for.
#Following is the for 10-k.
#In this case, I only want to keep 10-ks.
#I put a ^ at the beginning because I want the form type to start with 10, this gets rid of NT late filings.
#I also want to exclude amended filings so I specify that 10-k should not be followed by / (e.g., 10-K/A).

#formget <- c("N-1","N-1/A","N-1A","N-1A/A","N-1A EL","N-1A EL/A","497","497J","497K","497K1","497K2","497K3A","497K3B",
#             "N-CSR","N-CSR/A","N-CSRS","N-CSRS/A","N-MFP","N-Q","N-SAR","NSAR-A","NSAR-B","NSAR-B/A","N-PX","485APOS","485BPOS","N-30B-2",
#             "N-14","N-14/A")
#formget <- c("N-1","N-1/A","N-1A","N-1A/A","N-1A EL","N-1A EL/A","497","497J","497K","497K1","497K2","497K3A","497K3B")
#formget <- c("N-1A","N-1A/A","N-14","N-14/A","497K","497K1","497K2","497K3A","497K3B","NSAR-A","NSAR-B","N-Q","N-PX")
#formget <- NULL
formget <- c("N-1A")
formget_collapse <- paste("'",formget,"'",sep="")
formget_collapse <- paste(formget_collapse,collapse=",")
formget_collapse <- paste("(",formget_collapse,")",sep="")

yr_qtr_comb <- expand.grid(yr = seq(startyear, endyear, 1), qtr = seq(1, 4, 1))

yr_qtr_comb <- yr_qtr_comb[order(yr_qtr_comb[,"yr"],yr_qtr_comb[,"qtr"]),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==startyear & yr_qtr_comb[,"qtr"] < startqtr),NA,yr_qtr_comb[,"qtr"])
yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==endyear & yr_qtr_comb[,"qtr"] > endqtr),NA,yr_qtr_comb[,"qtr"])

yr_qtr_comb <- yr_qtr_comb[(!is.na(yr_qtr_comb[,"qtr"])),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

#Check to see if download folder exists.  If not, create it.
index_folder_path <- paste(output_directory, indexfolder, sep = slash, collapse = slash)  
create_directory(index_folder_path,remove=1)

index_combined_db <- paste(index_folder_path,"\\","index_combined.s3db",sep="")

index_combined_db_tables <- ListTables(index_combined_db)
index_combined_db_fields <- ListFields(index_combined_db)

###############################################################################
cat("Download files \n")
###############################################################################

#Check to see if download folder exists.  If not, create it.
download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(download_folder_path,remove=1)

filings_all <- ddply(.data=yr_qtr_comb, .variables=c("yr"), 
                     .fun = function(x, input_db, path_output, sub_path_output, outfile, forms, ftp_address){
                       
                       #x <- yr_qtr_comb[(yr_qtr_comb[,"yr"]==1993 & yr_qtr_comb[,"qtr"]==1),]
                       #x <- yr_qtr_comb[(yr_qtr_comb[,"yr"]==1994 & yr_qtr_comb[,"qtr"]==1),]   
                       #x <- yr_qtr_comb[(yr_qtr_comb[,"yr"]==2005 & yr_qtr_comb[,"qtr"]==1),]
                       #x <- yr_qtr_comb[(yr_qtr_comb[,"yr"]==2012 & yr_qtr_comb[,"qtr"]==4),]
                       #input_db <- index_combined_db
                       #path_output <- download_folder_path
                       #sub_path_output <- originalfolder
                       #outfile <- outfile
                       #forms <- formget_collapse
                       #ftp_address <- ftp
                       
                       yr <- unique(x[,"yr"])
                       
                       cat("\n",yr,"\n")
                       
                       #Check to see if yr folder exists.  If not, create it.
                       #cat("\n")
                       yr_folder_path <- paste(path_output, yr, sep = slash, collapse = slash)   
                       create_directory(yr_folder_path,remove=1)
                       
                       #Check to see if output folder exists.  If not, create it.
                       #cat("\n")
                       output_folder_path <- paste(path_output, yr, outfile, sep = slash, collapse = slash)   
                       create_directory(output_folder_path,remove=1)
                       
                       if(forms=="('')") {
                         
                         cat("All forms","\n")
                         
                         query_filings_yr <- ""   
                         query_filings_yr <- paste(query_filings_yr, "select       *                                          ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "from         index_combined                             ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "where        yr=", yr, "                                ", sep=" ")
                         query_filings_yr <- gsub(" {2,}", " ", query_filings_yr)
                         query_filings_yr <- gsub("^\\s+|\\s+$", "", query_filings_yr)
                         
                       } else {
                         
                         cat("Certain forms","\n")
                         
                         query_filings_yr <- ""   
                         query_filings_yr <- paste(query_filings_yr, "select       *                                          ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "from         index_combined                             ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "where        yr=", yr, "                                ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "and          form_type in ", forms, "                   ", sep=" ")
                         query_filings_yr <- gsub(" {2,}", " ", query_filings_yr)
                         query_filings_yr <- gsub("^\\s+|\\s+$", "", query_filings_yr)
                         
                       }
                       rm(yr)
                       
                       filings_yr <- data.frame(runsql(query_filings_yr,input_db),stringsAsFactors=FALSE)
                       rm(query_filings_yr)
                       
                       write.table(filings_yr,file=paste(yr_folder_path,outfile,sep="\\"),na="",sep=",",quote=TRUE,row.names=FALSE,append=FALSE)

                       #Get the names of the files to download
                       if(nrow(filings_yr)==0) {
                         
                         cat("No Matches","\n")
                         
                         
                       } else {
                         
                         cat("Matches","\n")
                         
                         #Get name of all files already downloaded
                         old <- data.frame(file=list.files(output_folder_path),
                                           stringsAsFactors=FALSE)
                         
                         old2 <- ddply(.data=old, .variables=c("file"), .fun = function(x,folder){
                           
                           #x <- old[1,]
                           #folder <- output_folder_path
                           
                           filepath <- paste(folder,x,sep="\\")
                           output <- data.frame(filepath=filepath,
                                                file.info(filepath),
                                                stringsAsFactors=FALSE)
                           
                         }, folder=output_folder_path, 
                         .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)
                         
                         rm(old)
                         
                         files_to_download <- data.frame(fullfilename=filings_yr[,"fullfilename_txt"],
                                                         filename_start=NA,
                                                         filename=NA,
                                                         already_downloaded=NA,
                                                         stringsAsFactors=FALSE)
                         
                         
                         #Find starting position of file name
                         #files_to_download[,"filename_start"] <- regexpr("\\.[^\\.]*$", files_to_download[,"fullfilename"])
                         files_to_download[,"filename_start"] <- sapply(gregexpr("/", files_to_download[,"fullfilename"]), function(x) rev(x)[1])
                         
                         #Create short filename
                         files_to_download[,"filename"] <- substr(files_to_download[,"fullfilename"], 
                                                                  (files_to_download[,"filename_start"]+1), 
                                                                  nchar(files_to_download[,"fullfilename"]))
                         
                         #checks to see what files on the current index listing are not in the directory
                         files_to_download[,"already_downloaded"] <- ifelse(files_to_download[,"filename"] %in% old2[,"file"], 1, 0)
                         
                         files_to_download_trim <- files_to_download[which(files_to_download[,"already_downloaded"]==0),]
                         
                         filings_downloaded <- ddply(.data=files_to_download_trim, .variables=c("fullfilename"), 
                                                     .fun = function(y,sub_path_output,ftp_address){
                                                       
                                                       #y <- files_to_download_trim[1,]
                                                       #sub_path_output <- output_folder_path
                                                       #ftp_address <- ftp_address
                                                       
                                                       filename <- unique(y[,"filename"])
                                                       fullfilename <- unique(y[,"fullfilename"])
                                                       
                                                       fileout <- paste(sub_path_output,filename,sep=slash)
                                                       
                                                       download.file(paste("ftp://",ftp_address,"/",fullfilename,sep=""), fileout, quiet = TRUE, mode = "wb",cacheOK = TRUE)
                                                       
                                                     },sub_path_output=output_folder_path,ftp_address=ftp_address,
                                                     .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                         
                         
                         rm(old2,files_to_download,files_to_download_trim,filings_downloaded)
                         
                       }
                       rm(yr_folder_path,output_folder_path)
                       
                       return(filings_yr)
                       
                     },
                     input_db=index_combined_db, path_output=download_folder_path, sub_path_output=originalfolder, outfile=outfile, forms=formget_collapse, ftp_address=ftp, 
                     .progress = "text",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
