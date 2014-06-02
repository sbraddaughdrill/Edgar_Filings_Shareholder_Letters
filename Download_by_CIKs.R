# TODO: Add comment
# 
# Author:  Brad
# File:    Download_by_CIKs.R
# Version: 1.0
# Date:    02.5.2014
# Purpose: This program reads the company.idx files and then files are 
#          downloaded to separate year directories by CIK
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
external_packages <- c("gdata","ibdreg","plyr","RCurl")
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

#downloadfolder <- "N-1"
#downloadfolder <- "DEF 14A"
#downloadfolder <- "MF_All"
#downloadfolder <- "MF_SemiAnnual_Reports"
#downloadfolder <- "MF_Annual_Reports"
#downloadfolder <- "MF_Shareholder_Reports_N-CSR-A"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS-A"
downloadfolder <- "MF_Shareholder_Reports_N-CSR"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS"

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
#formget <- c("N-1A","497K1")
#formget <- c("NSAR-B")
#formget <- c("NSAR-A")
#formget <- c("N-CSR/A")
#formget <- c("N-CSRS/A")
formget <- c("N-CSR")
#formget <- c("N-CSRS")

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
cat("Get CIKs \n")
###############################################################################
#Check to see if download folder exists.  If not, create it.
download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(download_folder_path,remove=1)

#CIKs <- read.table(file=paste(index_folder_path,"\\","summary","\\","CIK_list.csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
#                   sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CIKs <- read.table(file=paste(download_folder_path,"\\","CIK_list.csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                   sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")


#Output CIK list
CIKs[,"cik_no_pad"] <- as.numeric(CIKs[,"cik_no_pad"])
CIKs[,"cik_no_pad"] <- round(CIKs[,"cik_no_pad"], digits = 0)

CIKs <- CIKs[(!is.na(CIKs[,"cik_no_pad"])),]

CIKs_u <- unique(CIKs)
row.names(CIKs_u) <- seq(nrow(CIKs_u))

#Pad CIK
CIKs_u[,"cik"] <- format(CIKs_u[,"cik_no_pad"], trim=TRUE, digits = 0, scientific = 999)
CIKs_u[,"cik"] <- sprintf("%010s",CIKs_u[,"cik"])
CIKs_u[,"cik"] <- gsub(" ", "0", CIKs_u[,"cik"])

CIKs_u <- CIKs_u[order(CIKs_u[,"cik"]),]
row.names(CIKs_u) <- seq(nrow(CIKs_u))

rm(CIKs)
invisible(gc(verbose = FALSE, reset = TRUE))

ExportTable(index_combined_db,"CIKs_u",CIKs_u)

index_combined_db_tables <- ListTables(index_combined_db)
index_combined_db_fields <- ListFields(index_combined_db)


###############################################################################
cat("Download files \n")
###############################################################################

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
                       output_folder_path <- paste(path_output, yr, sub_path_output, sep = slash, collapse = slash)   
                       create_directory(output_folder_path,remove=1)
                       
                       if(forms=="('')") {
                         
                         cat("All forms","\n")
                         
                         query_filings_yr <- ""   
                         query_filings_yr <- paste(query_filings_yr, "select       *                                          ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "from         index_combined                             ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "where        yr=", yr, "                                ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "and          cik in (select       distinct cik_no_pad   ", sep=" ")   
                         query_filings_yr <- paste(query_filings_yr, "                     from         CIKs_u              ) ", sep=" ")   
                         query_filings_yr <- gsub(" {2,}", " ", query_filings_yr)
                         query_filings_yr <- gsub("^\\s+|\\s+$", "", query_filings_yr)
                         
                       } else {
                         
                         cat("Certain forms","\n")
                         
                         query_filings_yr <- ""   
                         query_filings_yr <- paste(query_filings_yr, "select       *                                          ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "from         index_combined                             ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "where        yr=", yr, "                                ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "and          cik in (select       distinct cik_no_pad   ", sep=" ")   
                         query_filings_yr <- paste(query_filings_yr, "                     from         CIKs_u              ) ", sep=" ")
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
                         
                         
                         #                         new <- ddply(.data=filings_yr, .variables=c("fullfilename_txt"), .fun = function(x,ftp_address,name_col){
                         #                           
                         #                           #x <- filings_yr[1,]
                         #                           #ftp_address <- ftp_address
                         #                           #name_col <- "fullfilename_txt"
                         #                           
                         #                           #filepath <- "ftp.sec.gov/edgar/data/1000069/0001000069-05-000020.txt"
                         #                           #filepath <- "ftp.sec.gov/edgar/data/1000249/0001000249-05-000003.txt"
                         #                           #filepath <- "http://www.sec.gov/Archives/edgar/data/1000069/0001000069-05-000020.txt"
                         #                           #filepath <- "http://www.sec.gov/Archives/edgar/data/709558/0000894189-05-000588.txt"
                         #                           
                         #                           filepath <- paste(ftp_address,x[,name_col],sep="/")
                         # 
                         #                           cat("\n",filepath,"\n")
                         #                           
                         #                           #Sys.sleep(1.5)
                         #                           
                         #                           res1 <- getURL(filepath, nobody=1L, header=1L,verbose=TRUE,ftp.use.epsv=TRUE,dirlistonly = TRUE)
                         #                           res2 <- data.frame(strsplit(res1,"\r\n"),stringsAsFactors=FALSE)
                         #                           res3 <- strsplit (res2[,1], " ")
                         #                           res4 <- do.call(rbind,res3)
                         #                           res5 <- data.frame(res4,stringsAsFactors=FALSE)
                         #                           colnames(res5) <- c("type","size")
                         #                           
                         #                           res6 <- res5[res5[,"type"]=="Content-Length:",]
                         #                           
                         #                           output <- data.frame(filepath=filepath,
                         #                                                size=res6[,"size"],
                         #                                                stringsAsFactors=FALSE)
                         #                         
                         #                         }, ftp_address=ftp_address, name_col="fullfilename_txt",
                         #                         .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)
                         
                         
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


###############################################################################
cat("Remove temp files \n")
###############################################################################

DeleteTable(index_combined_db,"CIKs_u")

index_combined_db_tables <- ListTables(index_combined_db)
index_combined_db_fields <- ListFields(index_combined_db)