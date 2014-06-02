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
#source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)

source(file=paste(function_directory,"functions_load_unload.R",sep="\\"),echo=FALSE)

LoadFunction(file=paste(function_directory,"functions_utilities.R",sep="\\"),"create_directory")


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

library(gdata)
library(plyr)
library(RCurl)

#=====================================================================;
#PARAMETERS;
#=====================================================================;

#If using windows, set to "\\" - if mac (or unix), set to "/";
slash <- "\\"

#First year you want index files for:

startyear <- 1993
#startyear <- 1995
#startyear <- 2006
#startyear <- 2012


#Last year you want index files for:
#endyear <- 1995
#endyear <- 2012
endyear <- 2013

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

#Output folder:
indexfolder <- "full-index"

#FTP address
ftp <- "ftp.sec.gov"

string_lengths <- data.frame(company_name_start=0,
                             company_name_length=61,
                             form_type_start=62,
                             form_type_length=12,
                             cik_start=75,
                             cik_length=10,
                             file_date_start=86,
                             file_date_length=10,
                             fullfilename_start=98,
                             fullfilename_length=43,
                             stringsAsFactors=FALSE)


yr_qtr_comb <- expand.grid(yr = seq(startyear, endyear, 1), qtr = seq(1, 4, 1))

yr_qtr_comb <- yr_qtr_comb[order(yr_qtr_comb[,"yr"],yr_qtr_comb[,"qtr"]),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==startyear & yr_qtr_comb[,"qtr"] < startqtr),NA,yr_qtr_comb[,"qtr"])
yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==endyear & yr_qtr_comb[,"qtr"] > endqtr),NA,yr_qtr_comb[,"qtr"])

yr_qtr_comb <- yr_qtr_comb[(!is.na(yr_qtr_comb[,"qtr"])),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))


#=====================================================================;
#BEGIN SCRIPT;
cat("Begin Script \n")
#=====================================================================;

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)

invisible(gc(verbose = FALSE, reset = TRUE))

index_combined <- ddply(.data=yr_qtr_comb, .variables=c("yr","qtr"), 
                        .fun = function(x,lengths,path){
                          
                          #x <- yr_qtr_comb[(yr_qtr_comb[,"yr"]==1993 & yr_qtr_comb[,"qtr"]==1),]
                          #lengths <- string_lengths
                          #path <- paste(output_directory,slash,indexfolder,slash,sep="")
                          
                          yr <- unique(x[,"yr"])
                          qtr <- unique(x[,"qtr"])
                          
                          cat("\n","\n",yr,"-", qtr,"\n")
                          
                          #Path to infile
                          fidx <- paste(path,"company",yr,qtr,".idx",sep="")
                          
                          #Read infile
                          con <- file(fidx, 'r') 
                          input <- readLines(con)
                          close(con)
                          
                          rm(fidx,con)
                          
                          #Convert to data.frame
                          input_df <- data.frame(input,
                                                 matrix(NA, nrow = length(input), ncol = 6,
                                                        dimnames = list(NULL,c("company_name","form_type","cik","file_date","fullfilename_txt","fullfilename_htm"))),
                                                 stringsAsFactors=FALSE)
                          rm(input)
                          
                          input_df_trim <- input_df[1:10,]
                          row.names(input_df_trim) <- seq(nrow(input_df_trim))
                          
                          rm(input_df_trim)
                          
                          input_df <- input_df[11:nrow(input_df),]
                          row.names(input_df) <- seq(nrow(input_df))
                          
                          company_name_start <- lengths[1,"company_name_start"]
                          company_name_length <- lengths[1,"company_name_length"]
                          form_type_start <- lengths[1,"form_type_start"]
                          form_type_length <- lengths[1,"form_type_length"]
                          cik_start <- lengths[1,"cik_start"]
                          cik_length <- lengths[1,"cik_length"]
                          file_date_start <- lengths[1,"file_date_start"]
                          file_date_length <- lengths[1,"file_date_length"]
                          fullfilename_start <- lengths[1,"fullfilename_start"]
                          fullfilename_length <- lengths[1,"fullfilename_length"]
                          
                          #Populate new columns
                          input_df[,"company_name"] <- substring(input_df[,"input"], company_name_start, company_name_start+company_name_length)  
                          input_df[,"form_type"] <- substring(input_df[,"input"], form_type_start, form_type_start+form_type_length)
                          input_df[,"cik"] <- substring(input_df[,"input"], cik_start, cik_start+cik_length)
                          input_df[,"file_date"] <- substr(input_df[,"input"], file_date_start, file_date_start+file_date_length)
                          input_df[,"fullfilename_txt"] <- substr(input_df[,"input"], fullfilename_start, fullfilename_start+fullfilename_length)
                          input_df[,"fullfilename_htm"] <- input_df[,"fullfilename_txt"] 
                          
                          #Fix Date
                          input_df[,"file_date"] <- as.Date(input_df[,"file_date"],format="%Y-%m-%d")
                          
                          input_df[,"fullfilename_htm"] <- gsub(".txt","",input_df[,"fullfilename_htm"])
                          input_df[,"fullfilename_htm"] <- gsub("^\\s+|\\s+$", "", input_df[,"fullfilename_htm"])
                          
                          input_df[,"fullfilename_htm"] <- paste(input_df[,"fullfilename_htm"],"-index.htm",sep="")
                          
                          colnames(input_df)[1] <- "filing_seq"
                          input_df[,"filing_seq"] <- as.integer(seq(1,nrow(input_df)))
                          input_df[,"filing_seq"] <- paste(yr,qtr,sprintf("%08d", input_df[,"filing_seq"]),sep="_")
                          
                          input_df[,"file_date"] <- as.character(input_df[,"file_date"])
                          
                          rm(company_name_start,company_name_length)
                          rm(form_type_start,form_type_length)
                          rm(cik_start,cik_length)
                          rm(file_date_start,file_date_length)
                          rm(fullfilename_start,fullfilename_length)
                          rm(yr,qtr)
                          invisible(gc(verbose = FALSE, reset = TRUE))
                          
                          #Clean table
                          input_df <- data.frame(lapply(input_df, as.character), stringsAsFactors=FALSE)
                          
                          for(i in which(sapply(input_df,class)=="character"))
                          {
                            input_df[[i]] = trim(input_df[[i]])
                          }
                          rm(i)
                          for (i in 1:ncol(input_df))
                          {
                            
                            input_df[,i] <- unknownToNA(input_df[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
                          } 
                          rm(i)
                          
                          return(input_df)
                          
                        }, lengths=string_lengths,path=paste(output_directory,slash,indexfolder,slash,sep=""), 
                        .progress = "text",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


#Fix columns
index_combined[,"file_date"] <- as.Date(index_combined[,"file_date"],format="%Y-%m-%d")

index_combined <- index_combined[order(index_combined[,"filing_seq"]),]
row.names(index_combined) <- seq(nrow(index_combined))

#Output db
index_combined_db <- paste(output_directory,"\\",indexfolder,"\\","index_combined.s3db",sep="")

index_combined_db_tables <- ListTables(index_combined_db)
index_combined_db_fields <- ListFields(index_combined_db)

ExportTable(index_combined_db,"index_combined",index_combined)
