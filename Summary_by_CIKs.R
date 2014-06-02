# TODO: Add comment
# 
# Author:  Brad
# File:    Summary_by_CIKs.R
# Version: 1.0
# Date:    02.5.2014
# Purpose: This program finds summary information for all the data by CIK
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
external_packages <- c("gdata","RCurl")
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
#downloadfolder <- "MF_Annual_Reports"
downloadfolder <- "MF_SemiAnnual_Reports"

#The sub directory you are going to download filings to
originalfolder <- "original"

#The file that will contain the filings you want to download.
outfile <- "yearly_filings.csv"

#FTP address
ftp <- "ftp.sec.gov"

#Specifiy, in regular expression format, the filing you are looking for.
#Following is the for 10-k.
#In this case, I only want to keep 10-ks.
#I put a ^ at the beginning because I want the form type to start with 10, this gets rid of NT late filings.
#I also want to exclude amended filings so I specify that 10-k should not be followed by / (e.g., 10-K/A).

formget <- NULL
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
summary_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(summary_folder_path,remove=1)

#CIKs <- read.table(file=paste(index_folder_path,"\\","summary","\\","CIK_list.csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
#                   sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
CIKs <- read.table(file=paste(summary_folder_path,"\\","CIK_list.csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
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
cat("Filings summary - yr,qtr \n")
###############################################################################

if(formget_collapse=="('')") {
  
  cat("\n","\n","All forms")
  
  query_summary_yr_qtr <- ""   
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "select       yr, qtr, form_type, count(form_type) as count       ", sep=" ")
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "from         index_combined                                      ", sep=" ")
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "where        cik in (select       distinct cik_no_pad            ", sep=" ")   
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "                     from         CIKs_u              )          ", sep=" ")   
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "group by     yr, qtr, form_type                                  ", sep=" ")
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "order by     yr, qtr, form_type                                  ", sep=" ")
  query_summary_yr_qtr <- gsub(" {2,}", " ", query_summary_yr_qtr)
  query_summary_yr_qtr <- gsub("^\\s+|\\s+$", "", query_summary_yr_qtr)
  
} else {
  
  cat("\n","\n","Certain forms")
  
  query_summary_yr_qtr <- ""   
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "select       yr, qtr, form_type, count(form_type) as count       ", sep=" ")
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "from         index_combined                                      ", sep=" ")
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "where        cik in (select       distinct cik_no_pad            ", sep=" ")   
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "                     from         CIKs_u              )          ", sep=" ")
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "and          form_type in ", formget_collapse, "                 ", sep=" ")
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "group by     yr, qtr, form_type                                  ", sep=" ")
  query_summary_yr_qtr <- paste(query_summary_yr_qtr, "order by     yr, qtr, form_type                                  ", sep=" ")
  query_summary_yr_qtr <- gsub(" {2,}", " ", query_summary_yr_qtr)
  query_summary_yr_qtr <- gsub("^\\s+|\\s+$", "", query_summary_yr_qtr)
  
}

summary_yr_qtr <- data.frame(runsql(query_summary_yr_qtr,index_combined_db),stringsAsFactors=FALSE)

write.table(summary_yr_qtr,file=paste(summary_folder_path,"summary_yr_qtr.csv",sep="\\"),na="",sep=",",quote=TRUE,row.names=FALSE,append=FALSE)



###############################################################################
cat("Filings summary - overall \n")
###############################################################################

if(formget_collapse=="('')") {
  
  cat("\n","\n","All forms")
  
  query_summary_overall <- ""   
  query_summary_overall <- paste(query_summary_overall, "select       form_type, count(form_type) as count                ", sep=" ")
  query_summary_overall <- paste(query_summary_overall, "from         index_combined                                      ", sep=" ")
  query_summary_overall <- paste(query_summary_overall, "where        cik in (select       distinct cik_no_pad            ", sep=" ")   
  query_summary_overall <- paste(query_summary_overall, "                     from         CIKs_u              )          ", sep=" ")   
  query_summary_overall <- paste(query_summary_overall, "group by     form_type                                           ", sep=" ")
  query_summary_overall <- paste(query_summary_overall, "order by     form_type                                           ", sep=" ")
  query_summary_overall <- gsub(" {2,}", " ", query_summary_overall)
  query_summary_overall <- gsub("^\\s+|\\s+$", "", query_summary_overall)
  
} else {
  
  cat("\n","\n","Certain forms")
  
  query_summary_overall <- ""   
  query_summary_overall <- paste(query_summary_overall, "select       form_type, count(form_type) as count                ", sep=" ")
  query_summary_overall <- paste(query_summary_overall, "from         index_combined                                      ", sep=" ")
  query_summary_overall <- paste(query_summary_overall, "where        cik in (select       distinct cik_no_pad            ", sep=" ")   
  query_summary_overall <- paste(query_summary_overall, "                     from         CIKs_u              )          ", sep=" ")
  query_summary_overall <- paste(query_summary_overall, "and          form_type in ", formget_collapse, "                 ", sep=" ")
  query_summary_overall <- paste(query_summary_overall, "group by     form_type                                           ", sep=" ")
  query_summary_overall <- paste(query_summary_overall, "order by     form_type                                           ", sep=" ")
  query_summary_overall <- gsub(" {2,}", " ", query_summary_overall)
  query_summary_overall <- gsub("^\\s+|\\s+$", "", query_summary_overall)
  
}

summary_overall <- data.frame(runsql(query_summary_overall,index_combined_db),stringsAsFactors=FALSE)

write.table(summary_overall,file=paste(summary_folder_path,"summary_overall.csv",sep="\\"),na="",sep=",",quote=TRUE,row.names=FALSE,append=FALSE)
