# TODO: Add comment
# 
# Author:  Brad
# File:    Combine_Headers.R
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
startyear <- 2003

#Last year you want index files for:
#endyear <- 2004
endyear <- 2013

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

infile <- "filings_list_comb.csv"

yr_qtr_comb <- expand.grid(yr = seq(startyear, endyear, 1), qtr = seq(1, 4, 1))

yr_qtr_comb <- yr_qtr_comb[order(yr_qtr_comb[,"yr"],yr_qtr_comb[,"qtr"]),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==startyear & yr_qtr_comb[,"qtr"] < startqtr),NA,yr_qtr_comb[,"qtr"])
yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==endyear & yr_qtr_comb[,"qtr"] > endqtr),NA,yr_qtr_comb[,"qtr"])

yr_qtr_comb <- yr_qtr_comb[(!is.na(yr_qtr_comb[,"qtr"])),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb2 <- data.frame(yr_qtr_comb,yr_qtr=NA,stringsAsFactors=FALSE)

rm(yr_qtr_comb)

yr_qtr_comb2[,"yr_qtr"] <- paste(yr_qtr_comb2[,"yr"],yr_qtr_comb2[,"qtr"],sep="_")


###############################################################################
cat("Collapse Headers \n")
###############################################################################

#Check Main Directory
create_directory(output_directory,remove=1)

#main_folder <- "MF_Shareholder_Reports_N-CSR"
#sub_header_folder <- "header"
#sub_filing_folder <- "letter"

#main_folder <- "MF_Shareholder_Reports_N-SAR-B"
#sub_header_folder <- "header"
#sub_filing_folder <- "questions"

folders <- data.frame(matrix(NA, ncol=2, nrow=2, dimnames=list(c(), c("main_folder","sub_header_folder"))), 
                      stringsAsFactors=FALSE)
folders[1,] <- c("MF_Shareholder_Reports_N-CSR","header")
folders[2,] <- c("MF_Shareholder_Reports_N-SAR-B","header")

for (i in 1:nrow(folders))
{
  #  i <- 1
  #  i <- 2
  
  main_folder <- folders[i,"main_folder"]
  sub_header_folder <- folders[i,"sub_header_folder"]
  
  #Check NSAR Directories
  main_folder_path <- paste(output_directory, main_folder, sep = slash, collapse = slash)  
  create_directory(main_folder_path,remove=1)
  
  sub_header_folder_path <- paste(output_directory, main_folder, sub_header_folder, sep = "\\", collapse = "\\")   
  create_directory(sub_header_folder_path,remove=1)
  
  #sub_filing_folder_path <- paste(output_directory, main_folder, sub_filing_folder, sep = "\\", collapse = "\\")   
  #create_directory(sub_filing_folder_path,remove=1)
  
  rm(main_folder,sub_header_folder)
  
  
  ###############################################################################
  cat("Get list files \n")
  ###############################################################################
  
  filings <- read.table(file=paste(main_folder_path,"\\",infile,sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                        sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  rm(main_folder_path)
  
  filings2 <- data.frame(yr_qtr=NA,filings,stringsAsFactors=FALSE)
  
  rm(filings)
  
  filings2[,"yr_qtr"] <- paste(filings2[,"yr"],filings2[,"qtr"], sep="_")
  
  filings2 <- filings2[,c(c("yr","qtr","yr_qtr"),colnames(filings2[,!(colnames(filings2) %in% c("yr","qtr","yr_qtr"))]))]
  
  filings_trim <- filings2[ filings2[,"yr_qtr"] %in% yr_qtr_comb2[,"yr_qtr"],]
  row.names(filings_trim) <- seq(nrow(filings_trim))
  
  rm(filings2)
  
  filings_trim2 <- data.frame(overall_id=NA, filings_trim, stringsAsFactors=FALSE)
  filings_trim2[,"overall_id"] <- seq(1,nrow(filings_trim2),1)
  
  rm(filings_trim)
  
  #filings_trim2_cols_all <- data.frame(type=NA,variables=colnames(filings_trim2),order=NA,stringsAsFactors=FALSE)
  #filings_trim2_cols_all[,"type"] <- "filings_trim2_cols_all"
  #filings_trim2_cols_all[,"order"] <- seq(1,nrow(filings_trim2_cols_all))
  
  
  ###############################################################################
  cat("Merge filings_trim2 and header_intro \n")
  ###############################################################################
  
  header_intro_file <- "header_intro_final"
  header_intro <- read.table(file=paste(sub_header_folder_path,"\\",header_intro_file,".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  rm(header_intro_file)
  
  filing_header_merge_common_id <- common_cols(filings_trim2, header_intro)
  
  filing_header_merge_filings_trim2_nonid_col <- colnames(filings_trim2)[!(colnames(filings_trim2) %in% filing_header_merge_common_id)]
  filing_header_merge_header_intro_nonid_col <- colnames(header_intro)[!(colnames(header_intro) %in% filing_header_merge_common_id)]
  
  filings_header <- merge(filings_trim2, header_intro, 
                          by.x=c(filing_header_merge_common_id), by.y=c(filing_header_merge_common_id), 
                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(filings_trim2,header_intro)
  rm(filing_header_merge_filings_trim2_nonid_col,filing_header_merge_header_intro_nonid_col)
  rm(filing_header_merge_common_id)
  
  
  
  ###############################################################################
  cat("Merge filings_header and filer_company_data \n")
  ###############################################################################
  
  filer_company_data_file <- "filer_company_data_final"
  filer_company_data <- read.table(file=paste(sub_header_folder_path,"\\",filer_company_data_file,".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  rm(filer_company_data_file)
  
  fh_company_data_common_id <- common_cols(filings_header, filer_company_data)
  
  fh_company_data_filings_header_nonid_col <- colnames(filings_header)[!(colnames(filings_header) %in% fh_company_data_common_id)]
  fh_company_data_filer_company_data_nonid_col <- colnames(filer_company_data)[!(colnames(filer_company_data) %in% fh_company_data_common_id)]
  
  fh_company_data <- merge(filings_header, filer_company_data, 
                           by.x=c(fh_company_data_common_id), by.y=c(fh_company_data_common_id), 
                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(filings_header,filer_company_data)
  rm(fh_company_data_filings_header_nonid_col,fh_company_data_filer_company_data_nonid_col)
  rm(fh_company_data_common_id)
  
  
  ###############################################################################
  cat("Merge fh_company_data and filer_filing_values \n")
  ###############################################################################
  
  filer_filing_values_file <- "filer_filing_values_final"
  filer_filing_values <- read.table(file=paste(sub_header_folder_path,"\\",filer_filing_values_file,".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  rm(filer_filing_values_file)
  
  fh_filing_values_common_id <- common_cols(fh_company_data, filer_filing_values)
  
  fh_filing_values_fh_company_data_nonid_col <- colnames(fh_company_data)[!(colnames(fh_company_data) %in% fh_filing_values_common_id)]
  fh_filing_values_filer_filing_values_nonid_col <- colnames(filer_filing_values)[!(colnames(filer_filing_values) %in% fh_filing_values_common_id)]
  
  fh_filing_values <- merge(fh_company_data, filer_filing_values, 
                            by.x=c(fh_filing_values_common_id), by.y=c(fh_filing_values_common_id), 
                            all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(fh_company_data,filer_filing_values)
  rm(fh_filing_values_fh_company_data_nonid_col,fh_filing_values_filer_filing_values_nonid_col)
  rm(fh_filing_values_common_id)
  
  
  ###############################################################################
  cat("Merge fh_filing_values and filer_business_address \n")
  ###############################################################################
  
  filer_business_address_file <- "filer_business_address_final"
  filer_business_address <- read.table(file=paste(sub_header_folder_path,"\\",filer_business_address_file,".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  rm(filer_business_address_file)
  
  fh_business_address_common_id <- common_cols(fh_filing_values, filer_business_address)
  
  fh_business_address_fh_filing_values_nonid_col <- colnames(fh_filing_values)[!(colnames(fh_filing_values) %in% fh_business_address_common_id)]
  fh_business_address_filer_business_address_nonid_col <- colnames(filer_business_address)[!(colnames(filer_business_address) %in% fh_business_address_common_id)]
  
  fh_business_address <- merge(fh_filing_values, filer_business_address, 
                               by.x=c(fh_business_address_common_id), by.y=c(fh_business_address_common_id), 
                               all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(fh_filing_values,filer_business_address)
  rm(fh_business_address_fh_filing_values_nonid_col,fh_business_address_filer_business_address_nonid_col)
  rm(fh_business_address_common_id)
  
  
  ###############################################################################
  cat("Merge fh_business_address and filer_mail_address \n")
  ###############################################################################
  
  filer_mail_address_file <- "filer_mail_address_final"
  filer_mail_address <- read.table(file=paste(sub_header_folder_path,"\\",filer_mail_address_file,".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  rm(filer_mail_address_file)
  
  fh_mail_address_common_id <- common_cols(fh_business_address, filer_mail_address)
  
  fh_mail_address_fh_business_address_nonid_col <- colnames(fh_business_address)[!(colnames(fh_business_address) %in% fh_mail_address_common_id)]
  fh_mail_address_filer_mail_address_nonid_col <- colnames(filer_mail_address)[!(colnames(filer_mail_address) %in% fh_mail_address_common_id)]
  
  fh_mail_address <- merge(fh_business_address, filer_mail_address, 
                           by.x=c(fh_mail_address_common_id), by.y=c(fh_mail_address_common_id), 
                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(fh_business_address,filer_mail_address)
  rm(fh_mail_address_fh_business_address_nonid_col,fh_mail_address_filer_mail_address_nonid_col)
  rm(fh_mail_address_common_id)
  
  
  ###############################################################################
  cat("Merge fh_mail_address and filer_former_company \n")
  ###############################################################################
  
  filer_former_company_file <- "filer_former_company_final"
  filer_former_company <- read.table(file=paste(sub_header_folder_path,"\\",filer_former_company_file,".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  rm(filer_former_company_file)
  
  fh_former_company_common_id <- common_cols(fh_mail_address, filer_former_company)
  
  fh_former_company_fh_mail_address_nonid_col <- colnames(fh_mail_address)[!(colnames(fh_mail_address) %in% fh_former_company_common_id)]
  fh_former_company_filer_former_company_nonid_col <- colnames(filer_former_company)[!(colnames(filer_former_company) %in% fh_former_company_common_id)]
  
  fh_former_company <- merge(fh_mail_address, filer_former_company, 
                             by.x=c(fh_former_company_common_id), by.y=c(fh_former_company_common_id), 
                             all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(fh_mail_address,filer_former_company)
  rm(fh_former_company_fh_mail_address_nonid_col,fh_former_company_filer_former_company_nonid_col)
  rm(fh_former_company_common_id)
  
  
  ###############################################################################
  cat("Merge series_class_contract and series_other \n")
  ###############################################################################
  
  series_class_contract_file <- "series_class_contract_final"
  series_class_contract <- read.table(file=paste(sub_header_folder_path,"\\",series_class_contract_file,".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  series_other_file <- "series_other_final"
  series_other <- read.table(file=paste(sub_header_folder_path,"\\",series_other_file,".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  rm(series_class_contract_file,series_other_file)
  
  series_merge_common_dup_col <- "CLASS_CONTRACT_INDEX"
  
  series_merge_common_id <- common_cols(series_class_contract, series_other)
  series_merge_common_id_trim <- series_merge_common_id[!(series_merge_common_id %in% series_merge_common_dup_col)]
  
  series_merge_series_class_contract_nonid_col <- colnames(series_class_contract)[!(colnames(series_class_contract) %in% series_merge_common_id)]
  series_merge_series_other_nonid_col <- colnames(series_other)[!(colnames(series_other) %in% series_merge_common_id)]
  
  series_all <- merge(series_other[,!(colnames(series_other) %in% series_merge_common_dup_col)], series_class_contract,
                      by.x=c(series_merge_common_id_trim), by.y=c(series_merge_common_id_trim), 
                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(series_class_contract,series_other)
  rm(series_merge_series_class_contract_nonid_col,series_merge_series_other_nonid_col)
  rm(series_merge_common_id)
  rm(series_merge_common_dup_col,series_merge_common_id_trim)
  
  
  ###############################################################################
  cat("Merge fh_former_company and series_all \n")
  ###############################################################################
  
  fh_series_merge_common_id <- common_cols(fh_former_company, series_all)
  #series_merge_common_id_trim <- series_merge_common_id[!(series_merge_common_id %in% "CLASS_CONTRACT_INDEX")]
  
  fh_series_merge_filings_header_nonid_col <- colnames(fh_former_company)[!(colnames(fh_former_company) %in% fh_series_merge_common_id)]
  fh_series_merge_series_all_nonid_col <- colnames(series_all)[!(colnames(series_all) %in% fh_series_merge_common_id)]
  
  fh_series_merge <- merge(fh_former_company, series_all, 
                           by.x=c(fh_series_merge_common_id), by.y=c(fh_series_merge_common_id), 
                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
  
  rm(fh_former_company,series_all)
  rm(fh_series_merge_filings_header_nonid_col,fh_series_merge_series_all_nonid_col)
  rm(fh_series_merge_common_id)
  
  
  ###############################################################################
  cat("Output Data \n")
  ###############################################################################
  
  #fh_series_merge <- fh_series_merge[order(fh_series_merge[,"order"], fh_series_merge[,"var"], fh_series_merge[,"type"]),]
  
  fh_series_merge_col_order <- c("overall_id","FORMER_COMPANY_INDEX","SERIES_INDEX","CLASS_CONTRACT_INDEX")
  fh_series_merge <- setorderv(data.table(fh_series_merge), fh_series_merge_col_order, rep(1,length(fh_series_merge_col_order)))
  fh_series_merge <- as.data.frame(fh_series_merge,stringsAsFactors=FALSE)
  
  rm(fh_series_merge_col_order)
  
  write.table(fh_series_merge,file=paste(sub_header_folder_path,"\\","header_all",".csv",sep=""), append=FALSE, na="NA", 
              sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
  
  rm(sub_header_folder_path,fh_series_merge)
  
}
rm(i,infile,folders)
rm(startyear,startqtr,endyear,endqtr,yr_qtr_comb2)

