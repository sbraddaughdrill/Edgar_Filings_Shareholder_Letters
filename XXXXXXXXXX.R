# TODO: Add comment
# 
# Author:  Brad
# File:    XXXXXXXXXX.R
# Version: 1.0
# Date:    08.06.2014
# Purpose: Combine all the header files
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

# Set location (1=HOME,2=WORK,3=LAPTOP,4=CORALSEA FROM HOME,5=CORALSEA FROM WORK,6=CORALSEA FROM LAPTOP)
Location <- 1

if (Location == 1) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 2) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\", mustWork=TRUE) 
  #treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)  
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research/Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 5) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else if (Location == 6) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)         
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
# FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

#source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_text_parse.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","gdata","plyr","stringr","XML")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(external_packages,installed_packages,repo)


###############################################################################
#PARAMETERS;
###############################################################################

#If using windows, set to "\\" - if mac (or unix), set to "/";
slash <- "\\"

#First year you want index files for:

#startyear <- 1993
#startyear <- 2003
startyear <- 2006

#Last year you want index files for:
#endyear <- 2004
endyear <- 2013

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

main_folder <- "MF_Shareholder_Reports_N-SAR-B"
sub_header_folder <- "header"
sub_filing_folder <- "questions"

#The file that will contain the filings you want to download.
infile <- "filings_list_comb.csv"

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)

#Check NSAR Directories
main_folder_path <- paste(output_directory, main_folder, sep = slash, collapse = slash)  
create_directory(main_folder_path,remove=1)

sub_header_folder_path <- paste(output_directory, main_folder, sub_header_folder, sep = "\\", collapse = "\\")   
create_directory(sub_header_folder_path,remove=1)

sub_filing_folder_path <- paste(output_directory, main_folder, sub_filing_folder, sep = "\\", collapse = "\\")   
create_directory(sub_filing_folder_path,remove=1)


###############################################################################
cat("Get list files \n")
###############################################################################

yr_qtr_comb <- expand.grid(yr = seq(startyear, endyear, 1), qtr = seq(1, 4, 1))

yr_qtr_comb <- yr_qtr_comb[order(yr_qtr_comb[,"yr"],yr_qtr_comb[,"qtr"]),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==startyear & yr_qtr_comb[,"qtr"] < startqtr),NA,yr_qtr_comb[,"qtr"])
yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==endyear & yr_qtr_comb[,"qtr"] > endqtr),NA,yr_qtr_comb[,"qtr"])

rm(startyear,startqtr,endyear,endqtr)

yr_qtr_comb <- yr_qtr_comb[(!is.na(yr_qtr_comb[,"qtr"])),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb2 <- data.frame(yr_qtr_comb,yr_qtr=NA,stringsAsFactors=FALSE)

rm(yr_qtr_comb)

yr_qtr_comb2[,"yr_qtr"] <- paste(yr_qtr_comb2[,"yr"],yr_qtr_comb2[,"qtr"],sep="_")

# filings <- read.table(file=paste(main_folder_path,"\\",infile,sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
#                       sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
# 
# filings2 <- data.frame(yr_qtr=NA, filings, stringsAsFactors=FALSE)
# 
# rm(filings)
# 
# filings2[,"yr_qtr"] <- paste(filings2[,"yr"],filings2[,"qtr"],sep="_")
# 
# filings2 <- filings2[,c(c("yr","qtr","yr_qtr"), colnames(filings2[,!(colnames(filings2) %in% c("yr","qtr","yr_qtr"))]))]
# 
# filings_trim <- filings2[ filings2[,"yr_qtr"] %in% yr_qtr_comb2[,"yr_qtr"],]
# row.names(filings_trim) <- seq(nrow(filings_trim))
# 
# rm(filings2,yr_qtr_comb2)
# 
# filings_trim2 <- data.frame(overall_id=NA,filings_trim, stringsAsFactors=FALSE)
# filings_trim2[,"overall_id"] <- seq(1,nrow(filings_trim2),1)
# 
# rm(filings_trim)


###############################################################################
cat("Merge Header Files \n")
###############################################################################

nsar_header_all <- read.table(file=paste(sub_header_folder_path,"\\","header_all.csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                                     sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

nsar_q7 <- read.table(file=paste(sub_filing_folder_path,"\\","nsar_b_q007.csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                          sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")


