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
external_packages <- c("gdata","plyr")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

###############################################################################
# PARAMETERS;
###############################################################################

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

#downloadfolder <- "MF_All"
downloadfolder <- "MF_Annual_Reports"
#downloadfolder <- "MF_SemiAnnual_Reports"

#The file that will contain the filings you want to download.
outfile <- "filings.csv"

#These are godo filings to help differentiate MF filings from firm filings -- these shouldn't change!!!!
formget <- c("N-1A","N-1A/A","N-14","N-14/A","497K","497K1","497K2","497K3A","497K3B","NSAR-A","NSAR-B","N-Q","N-PX")
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



###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

index_combined_db <- paste(output_directory,"\\",indexfolder,"\\","index_combined.s3db",sep="")

index_combined_db_tables <- ListTables(index_combined_db)
index_combined_db_fields <- ListFields(index_combined_db)


###############################################################################
cat("Get unique CIKs \n")
###############################################################################

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)

#Check to see if download folder exists.  If not, create it.
download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(download_folder_path,remove=1)


query_CIKs <- ""   
query_CIKs <- paste(query_CIKs, "select       distinct cik                                         ", sep=" ")
query_CIKs <- paste(query_CIKs, "from         index_combined                                       ", sep=" ")
query_CIKs <- paste(query_CIKs, "where        form_type in ", formget_collapse, "                  ", sep=" ")
query_CIKs <- gsub(" {2,}", " ", query_CIKs)
query_CIKs <- gsub("^\\s+|\\s+$", "", query_CIKs)

CIKs <- data.frame(runsql(query_CIKs,index_combined_db),
                      temp=NA,
                      stringsAsFactors=FALSE)
colnames(CIKs) <- c("cik_no_pad","cik")

#Output CIK list
CIKs[,"cik_no_pad"] <- as.numeric(CIKs[,"cik_no_pad"])
CIKs[,"cik_no_pad"] <- round(CIKs[,"cik_no_pad"], digits = 0)

CIKs <- CIKs[(!is.na(CIKs[,"cik_no_pad"])),]

CIKs_u <- unique(CIKs)
row.names(CIKs_u) <- seq(nrow(CIKs_u))

for (i in 1:ncol(CIKs_u))
{
  CIKs_u[,i] <- unknownToNA(CIKs_u[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                        NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                        NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  CIKs_u[,i] <- ifelse(is.na(CIKs_u[,i]),NA, CIKs_u[,i])
} 

#Pad CIK
CIKs_u[,"cik"] <- format(CIKs_u[,"cik_no_pad"], trim=TRUE, digits = 0, scientific = 999)
CIKs_u[,"cik"] <- sprintf("%010s",CIKs_u[,"cik"])
CIKs_u[,"cik"] <- gsub(" ", "0", CIKs_u[,"cik"])

CIKs_u <- CIKs_u[order(CIKs_u[,"cik"]),]
row.names(CIKs_u) <- seq(nrow(CIKs_u))

write.table(CIKs_u,file=paste(download_folder_path,"CIK_list.csv",sep="\\"),na="",sep=",",quote=TRUE,row.names=FALSE,append=FALSE)


###############################################################################
cat("Filings per year \n")
###############################################################################

filings_yr <- ddply(.data=yr_qtr_comb, .variables=c("yr"), 
                       .fun = function(x,path,outfile,forms){
                         
                         #x <- yr_qtr_comb[yr_qtr_comb[,"yr"]==1995,]
                         #path <- download_folder_path
                         #outfile <- outfile
                         #forms <- formget_collapse
                         
                         yr <- unique(x[,"yr"])
                         
                         #Check to see if yr folder exists.  If not, create it.
                         cat("\n","\n")
                         yr_folder_path <- paste(path, yr, sep = slash, collapse = slash)   
                         create_directory(yr_folder_path,remove=1)
                         
                         #Path to outfile
                         ofidx <- paste(yr_folder_path,outfile,sep=slash)
                         
                         
                         query_filings_yr <- ""   
                         query_filings_yr <- paste(query_filings_yr, "select       *                                  ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "from         index_combined                     ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "where        form_type in ", forms, "           ", sep=" ")
                         query_filings_yr <- paste(query_filings_yr, "and          yr=", yr, "                        ", sep=" ")
                         query_filings_yr <- gsub(" {2,}", " ", query_filings_yr)
                         query_filings_yr <- gsub("^\\s+|\\s+$", "", query_filings_yr)
                         
                         filings_yr <- data.frame(runsql(query_filings_yr,index_combined_db),stringsAsFactors=FALSE)
                         
                         write.table(filings_yr,file=ofidx,na="",sep=",",quote=TRUE,row.names=FALSE,append=FALSE)
                         
                         return(filings_yr)
                         
                       },path=download_folder_path, outfile=outfile, forms=formget_collapse,
                       .progress = "text",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

