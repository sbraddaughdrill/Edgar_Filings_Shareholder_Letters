# TODO: Add comment
# 
# Author:  Brad
# File:    Extract_Filings.R
# Version: 1.0
# Date:    06.10.2014
# Purpose: Extract the letters from the individual filings
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
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 2) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\", mustWork=TRUE) 
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  #setwd("C:/Research_temp3/")
  input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)  
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 5) {
  #setwd("//tsclient/C/Research_temp3/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else if (Location == 6) {
  #setwd("//tsclient/C/Research_temp2/")
  input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)         
  
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

import_strings <- function(file,str_col,backslash_flag) {
  
  #  file <- paste(output_directory,"Letter_Beginning.csv",sep="\\")
  #  str_col <- "BEGINNINGS"
  #  backslash_flag <- 1
  
  require(gdata)
  
  #letter_temp0 <- read.csv(file=file,header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
  letter_temp0 <- read.table(file=file, header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  #Clean
  letter_temp_clean <- letter_temp0
  
  for(i in which(sapply(letter_temp_clean,class)=="character"))
  {
    letter_temp_clean[[i]] <- gsub(" {2,}", " ", letter_temp_clean[[i]])
    letter_temp_clean[[i]] <- trim(letter_temp_clean[[i]])
  }
  rm(i)
  
  for (i in 1:ncol(letter_temp_clean))
  {
    letter_temp_clean[,i] <- unknownToNA(letter_temp_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                          NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                          NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
    letter_temp_clean[,i] <- ifelse(is.na(letter_temp_clean[,i]),NA, letter_temp_clean[,i])
  } 
  rm(i)
  
  letter_temp1 <- data.frame(letter_temp_clean,regex=NA,stringsAsFactors=FALSE)
  letter_temp1 <- letter_temp1[!is.na(letter_temp1[,str_col]),]
  letter_temp1 <- letter_temp1[!(letter_temp1[,str_col]==""),]
  letter_temp1 <- letter_temp1[letter_temp1[,"INCLUDE"]=="YES",]
  
  letter_temp1[,"regex"] <- letter_temp1[,str_col]
  letter_temp1[,"regex"] <- gsub("'", "", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub(",", " ", letter_temp1[,"regex"])
  
  if(backslash_flag) {  letter_temp1[,"regex"] <- gsub("/", " ", letter_temp1[,"regex"])}
  
  letter_temp1[,"regex"] <- gsub("\\(", " ", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub("\\)", " ", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub("&", " AND ", letter_temp1[,"regex"])
  
  letter_temp1[,"regex"] <- gsub(" {2,}", " ", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub("^\\s+|\\s+$", "", letter_temp1[,"regex"])
  letter_temp1 <- letter_temp1[!(letter_temp1[,str_col]==""),]
  
  #letter_temp1[,"regex"] <- gsub(" ", ".*", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub(" ", "\\\\s*", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- ifelse(letter_temp1[,"PAD"]=="YES",paste(" ", letter_temp1[,"regex"], " ", sep=""), letter_temp1[,"regex"])
  
  letter_temp1 <- unique(letter_temp1)
  row.names(letter_temp1) <- seq(nrow(letter_temp1))
  
  letter_temp <- letter_temp1[,!(colnames(letter_temp1) %in% c("PAD","INCLUDE"))]
  
  rm(letter_temp0,letter_temp1,letter_temp_clean)
  
  return(letter_temp)
  
}

regex_expand <- function(regex_stubs,strs,strs_col,priority_col,stub_beg_col,stub_end_col) {
  
  #  regex_stubs <- letter_beginning_regex0
  #  strs <- letter_beginning
  #  strs_col <- "regex"
  #  priority_col <- "priority"
  #  stub_beg_col <- "beg_txt"
  #  stub_end_col <- "end_txt"
  
  regex_expand0 <- ddply(.data=regex_stubs, .variables=priority_col, .fun = function(x,strs){
    
    #  x <- regex_stubs[1,]
    strs[,strs_col] <- paste(x[,stub_beg_col], strs[,strs_col] , x[,stub_end_col], sep="")
    return(strs)
  },strs=strs)
  
  return(regex_expand0)
}

regex_section_matches_collapse <- function(matches_expand,dv_col,txtid_col) {
  
  #  matches_expand <- filing_text_letter_id1
  #  dv_col <- "letter_beginning"
  #  txtid_col <- "text_id"
  
  matches_expand[,dv_col] <- ifelse(is.na(matches_expand[,c(dv_col)]),FALSE,TRUE)
  filing_text_letter1 <- ddply(.data=matches_expand, .variables=txtid_col, .fun = function(x,dv_col){
    
    x[,dv_col] <- any(x[,dv_col])
    return(x)
    
  },dv_col=dv_col)
  filing_text_letter1[,dv_col] <- ifelse(filing_text_letter1[,c(dv_col)]==TRUE,1,0)
  
  filing_text_letter1_dt <- data.table(filing_text_letter1)
  filing_text_letter1<- unique(filing_text_letter1_dt,use.key=FALSE)
  filing_text_letter1 <- as.data.frame(filing_text_letter1,stringsAsFactors=FALSE)
  
  rm(filing_text_letter1_dt)
  invisible(gc(verbose = FALSE, reset = TRUE))
  
  return(filing_text_letter1)
}

regex_section_matches_collapse_old <- function(matches_expand,dv_col,txtid_col) {
  
  #  matches_expand <- filing_text_letter_id1
  #  dv_col <- "letter_beginning"
  #  txtid_col <- "text_id"
  
  filing_text_letter1 <- ddply(.data=matches_expand, .variables=txtid_col, .fun = function(x,dv_col){
    
    x[,dv_col] <- ifelse(is.na(x[,c(dv_col)]),FALSE,TRUE)
    x[,dv_col] <- any(x[,dv_col])
    x[,dv_col] <- ifelse(x[,c(dv_col)]==TRUE,1,0)
    return(unique(x))
    
  },dv_col=dv_col)
  return(filing_text_letter1)
}

regex_section_matches_expand <- function(regex_strs,data,dv_col,txt_col) {
  
  #  regex_strs <- letter_beginning_regex[letter_beginning_regex[,"REGEX_PRIORITY"]==i,"regex"]
  #  data <- filing_text_letter0
  #  dv_col <- "letter_beginning"
  #  txt_col <- xmltrim_col
  
  #ptm1 <- proc.time()
  filing_text_letter_id1 <- ldply(.data=regex_strs, .fun = function(x,data,dv_col,txt_col){
    
    #  x <- regex_strs[[1]]
    
    data[,dv_col] <- ifelse(grepl(x, data[,c(txt_col)],ignore.case = TRUE, perl = TRUE), x, NA)
    
    #data_dt <- data.table(data)
    #data <- unique(data_dt,use.key=FALSE)
    #data <- as.data.frame(data,stringsAsFactors=FALSE)
    
    return(data)
    
  }, data=data, dv_col=dv_col, txt_col=txt_col)
  
  filing_text_letter_id1_dt <- data.table(filing_text_letter_id1)
  filing_text_letter_id1 <- unique(filing_text_letter_id1_dt,use.key=FALSE)
  filing_text_letter_id1 <- as.data.frame(filing_text_letter_id1,stringsAsFactors=FALSE)
  
  rm(filing_text_letter_id1_dt)
  invisible(gc(verbose = FALSE, reset = TRUE))
  #proc.time() - ptm1
  
  return(filing_text_letter_id1)
}

regex_section_matches_expand_old <- function(regex_strs,data,dv_col,txt_col) {
  
  #  regex_strs <- letter_beginning_regex[letter_beginning_regex[,"REGEX_PRIORITY"]==i,"regex"]
  #  data <- filing_text_letter0
  #  dv_col <- "letter_beginning"
  #  txt_col <- xmltrim_col
  
  filing_text_letter_id1 <- ldply(.data=regex_strs, .fun = function(x,data,dv_col,txt_col){
    
    #  x <- regex_strs[[1]]
    
    data[,dv_col] <- ifelse(grepl(x, data[,c(txt_col)],ignore.case = TRUE, perl = TRUE), x, NA)
    return(data)
    
  }, data=data, dv_col=dv_col, txt_col=txt_col)
  return(filing_text_letter_id1)
}


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","gdata","plyr","stringr","XML")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages)


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

#downloadfolder <- "N-1"
#downloadfolder <- "DEF 14A"
#downloadfolder <- "MF_All"
#downloadfolder <- "MF_SemiAnnual_Reports"
#downloadfolder <- "MF_Annual_Reports"
#downloadfolder <- "MF_Shareholder_Reports_N-CSR"
#downloadfolder <- "MF_Shareholder_Reports_N-CSR-A"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS"
downloadfolder <- "MF_Shareholder_Reports_N-CSRS-A"

#The sub directory where the downloaded filings are
txtfolder_clean <- "txt_clean"

#The sub directory where the combined filings will be located
txtfolder_section <- "letter"

#The file that will contain the filings you want to download.
infile <- "filings_list_comb.csv"

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

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)

#Check to see if download folder exists.  If not, create it.
download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(download_folder_path,remove=1)


###############################################################################
cat("Get list files \n")
###############################################################################

filings <- read.table(file=paste(download_folder_path,"\\",infile,sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                      sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

filings2 <- data.frame(yr_qtr=NA,
                       filings,
                       stringsAsFactors=FALSE)

rm(filings)

filings2[,"yr_qtr"] <- paste(filings2[,"yr"],
                             filings2[,"qtr"],
                             sep="_")

filings2 <- filings2[,c(c("yr","qtr","yr_qtr"),
                        colnames(filings2[,!(colnames(filings2) %in% c("yr","qtr","yr_qtr"))]))]

filings_trim <- filings2[ filings2[,"yr_qtr"] %in% yr_qtr_comb2[,"yr_qtr"],]
row.names(filings_trim) <- seq(nrow(filings_trim))

filings_trim2 <- data.frame(overall_id=NA,
                            filings_trim,
                            stringsAsFactors=FALSE)
filings_trim2[,"overall_id"] <- seq(1,nrow(filings_trim2),1)

rm(filings_trim)


###############################################################################
cat("Import HTML entities \n")
###############################################################################

#Encode HTML entities
#entity_encoding0 <- read.csv(file=paste(output_directory,"Entity_encoding.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
entity_encoding0 <- read.table(file=paste(output_directory,"Entity_encoding.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                               sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#Clean
entity_encoding_clean <- entity_encoding0
#for(i in 1:ncol(entity_encoding_clean))
#{
#  entity_encoding_clean[,i] <- iconv(entity_encoding_clean[,i], "latin1", "ASCII", sub="")
#}

for(i in which(sapply(entity_encoding_clean,class)=="character"))
{
  entity_encoding_clean[[i]] = trim(entity_encoding_clean[[i]])
}
rm(i)

for (i in 1:ncol(entity_encoding_clean))
{
  entity_encoding_clean[,i] <- unknownToNA(entity_encoding_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  entity_encoding_clean[,i] <- ifelse(is.na(entity_encoding_clean[,i]),NA, entity_encoding_clean[,i])
} 
rm(i)

entity_encoding <- entity_encoding_clean[(!(is.na(entity_encoding_clean[,"ASCII.Looks.Like"])) & 
                                            !(is.na(entity_encoding_clean[,"Entity.Encoding"]))),]
row.names(entity_encoding) <- seq(nrow(entity_encoding))

rm(entity_encoding0,entity_encoding_clean)


###############################################################################
cat("Import Letter Strings \n")
###############################################################################

letter_beginning <- import_strings(file=paste(output_directory,"Letter_Beginning.csv",sep="\\"),str_col="BEGINNINGS",backslash_flag=1)
letter_ending <- import_strings(file=paste(output_directory,"Letter_Ending.csv",sep="\\"),str_col="ENDINGS",backslash_flag=1)
letter_position <- import_strings(file=paste(output_directory,"letter_position.csv",sep="\\"),str_col="POSITIONS",backslash_flag=1)
letter_signature <- import_strings(file=paste(output_directory,"letter_signature.csv",sep="\\"),str_col="SIGNATURES",backslash_flag=0)


###############################################################################
cat("Clean Files \n")
###############################################################################

letters_all <- dlply(.data=filings_trim2, .variables=c("yr"), 
                     .fun = function(x, path_output,subfolder,entity_encoding,letter_beginning,letter_ending,letter_position,letter_signature){
                       
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2003),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2004),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2005),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2006),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2007),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2008),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2009),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2010),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2011),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2012),]
                       #  x <- filings_trim2[(filings_trim2[,"yr"]==2013),]
                       #  path_output <- paste(output_directory,downloadfolder,sep=slash)
                       #  subfolder <- txtfolder_clean
                       #  subfolder_output <- txtfolder_section
                       
                       #  entity_encoding <- entity_encoding
                       #  letter_beginning <- letter_beginning
                       #  letter_ending <- letter_ending
                       #  letter_position <- letter_position
                       #  letter_signature <- letter_signature
                       
                       
                       filings_trim2_short <- x[,!(colnames(x) %in% c("file_header","file_index_htm"))]
                       
                       yr <-  unique(x[,"yr"])
                       
                       cat("\n",yr,"\n")
                       
                       #Check to see if yr folder exists.  If not, create it.
                       yr_folder_path <- paste(path_output, yr, sep = "\\", collapse = "\\")   
                       create_directory(yr_folder_path,remove=1)
                       
                       sub_folder_path <- paste(yr_folder_path, subfolder, sep = "\\", collapse = "\\")   
                       create_directory(sub_folder_path,remove=1)
                       
                       #sub_folder_output_path <- paste(yr_folder_path, subfolder_output, sep = "\\", collapse = "\\")   
                       #create_directory(sub_folder_output_path,remove=1)
                       
                       #Get name of downloaded files
                       downloaded_files <- data.frame(file=list.files(sub_folder_path),stringsAsFactors=FALSE)
                       downloaded_files2 <- ddply(.data=downloaded_files, .variables=c("file"), .fun = function(x,folder){
                         
                         filepath <- paste(folder,x,sep="\\")
                         output <- data.frame(filepath=filepath,file.info(filepath),stringsAsFactors=FALSE)
                         
                       }, folder=sub_folder_path, 
                       .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)
                       
                       rm(downloaded_files)
                       
                       downloaded_files2 <- downloaded_files2[order(downloaded_files2[,"filepath"]),]
                       row.names(downloaded_files2) <- seq(nrow(downloaded_files2))
                       
                       downloaded_files3 <- data.frame(yr_id=NA,downloaded_files2,stringsAsFactors=FALSE)
                       downloaded_files3[,"yr_id"] <- seq(1,nrow(downloaded_files3),1)
                       
                       rm(downloaded_files2)
                       
                       letters <- dlply(.data=downloaded_files3, .variables=c("yr_id"), 
                                        .fun = function(y,entity_encoding,letter_beginning,letter_ending,letter_position,letter_signature){
                                          
                                          
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000072760-03-000038.csv"),]
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000088053-03-000790.csv"),]
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000721291-03-000011.csv"),]
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000820027-03-000786.csv"),]
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000949377-03-000778.csv"),]
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000950136-03-003115.csv"),]
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001017062-03-000375.csv"),]
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000811860-04-000012.csv"),]
                                          #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001193125-04-025975.csv"),]
                                          
                                          #  entity_encoding <- entity_encoding
                                          #  letter_beginning <- letter_beginning
                                          #  letter_ending <- letter_ending
                                          #  letter_position <- letter_position
                                          #  letter_signature <- letter_signature
                                          
                                          xmlcol <- "TEXT"
                                          xmltrim_col <- "TEXT_TRIM"
                                          
                                          file <- unique(y[,"file"])
                                          filepath <- unique(y[,"filepath"])
                                          
                                          #file_out <- gsub(".txt",".csv",file)
                                          #filepath_out <- paste(sub_folder_output_path,file_out,sep="\\")
                                          
                                          cat(file,"\n")
                                          
                                          filing <- read.table(file=filepath, header = TRUE, na.strings="",stringsAsFactors=FALSE, 
                                                               sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
                                          
                                          filing_no_text0 <- filing[,!(colnames(filing) %in% c(xmlcol))]
                                          filing_no_text1 <- unique(filing_no_text0)
                                          #filing_no_text2 <- data.frame(file=NA,filing_no_text1,stringsAsFactors=FALSE)
                                          #filing_no_text2[,"file"] <- file
                                          filing_no_text2 <- filing_no_text1
                                          rm(filing_no_text0,filing_no_text1)
                                          
                                          filing_text0 <- filing[,(colnames(filing) %in% c("DOCUMENT_INDEX",xmlcol))]
                                          filing_text1 <- data.frame(file=NA,filing_text0,stringsAsFactors=FALSE)
                                          filing_text1[,"file"] <- file
                                          rm(filing_text0)
                                          
                                          rm(filing)
                                          
                                          
                                          
                                          #Clean Tags
                                          filing_text_clean <- filing_text1
                                          rm(filing_text1)
                                          
                                          #for(i in which(sapply(filing_text_clean,class)=="character"))
                                          #  {
                                          #    #filing_text_clean[[i]] <- gsub(" {2,}", " ", filing_text_clean[[i]])
                                          #   filing_text_clean[[i]] <- trim(filing_text_clean[[i]])
                                          #  }
                                          # rm(i)
                                          
                                          for (i in 1:ncol(filing_text_clean))
                                          {
                                            filing_text_clean[,i] <- unknownToNA(filing_text_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                                                  NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                                                  NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
                                            filing_text_clean[,i] <- ifelse(is.na(filing_text_clean[,i]),"", filing_text_clean[,i])
                                          } 
                                          rm(i)
                                          
                                          
                                          #ENTITY DECODING
                                          #&AMP; -> &
                                          entity_encoding_trim <- entity_encoding
                                          entity_encoding_trim_amp <- entity_encoding_trim[(entity_encoding_trim[,"ASCII.Looks.Like"] %in% c("&")),]
                                          entity_encoding_trim_no_amp <-  entity_encoding_trim[!(entity_encoding_trim[,"ASCII.Looks.Like"] %in% c("&")),]
                                          rm(entity_encoding_trim)
                                          
                                          filing_text_decode <- filing_text_clean
                                          rm(filing_text_clean)
                                          
                                          col_val_decoded <- c(xmlcol)
                                          for (i in 1:length(col_val_decoded))
                                          {
                                            col_temp <- col_val_decoded[i]
                                            
                                            for (j in 1:nrow(entity_encoding_trim_amp))
                                            {
                                              pattern_temp1 <- entity_encoding_trim_amp[j,"Entity.Encoding"]
                                              replacement_temp1 <- entity_encoding_trim_amp[j,"ASCII.Looks.Like"]
                                              filing_text_decode[,col_temp] <- gsub(pattern_temp1, replacement_temp1, filing_text_decode[,col_temp],ignore.case = TRUE)
                                              rm(pattern_temp1,replacement_temp1)
                                            } 
                                            rm(j)
                                            
                                            for (k in 1:nrow(entity_encoding_trim_no_amp))
                                            {
                                              pattern_temp1 <- entity_encoding_trim_no_amp[k,"Entity.Encoding"]
                                              replacement_temp1 <- entity_encoding_trim_no_amp[k,"ASCII.Looks.Like"]
                                              filing_text_decode[,col_temp] <- gsub(pattern_temp1, replacement_temp1, filing_text_decode[,col_temp],ignore.case = TRUE)
                                              rm(pattern_temp1,replacement_temp1)
                                            } 
                                            rm(col_temp,k)
                                            
                                          } 
                                          rm(entity_encoding_trim_amp,entity_encoding_trim_no_amp,col_val_decoded,i)
                                          
                                          
                                          #SUBSTITUTE
                                          filing_text_sub <- filing_text_decode
                                          filing_text_sub[,c(xmlcol)] <- gsub("'", "", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("&", " AND ", filing_text_sub[,c(xmlcol)])
                                          
                                          filing_text_sub[,c(xmlcol)] <- gsub("CO OWNER", "COOWNER", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("CO-OWNER", "COOWNER", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("DEPUTY PRESIDENT", "DEPUTY-PRESIDENT", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("NON EXECUTIVE", "NON-EXECUTIVE", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("SECRETARY TREASURER", "SECRETARY-TREASURER", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("SOLE PROPRIETOR", "SOLE-PROPRIETOR", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("VICE CHAIR", "VICE-CHAIR", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("VICE CHAIRMAN", "VICE-CHAIRMAN", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("VICE PRESIDENT", "VICE-PRESIDENT", filing_text_sub[,c(xmlcol)])
                                          filing_text_sub[,c(xmlcol)] <- gsub("\\(S\\)", "S", filing_text_sub[,c(xmlcol)])
                                          
                                          rm(filing_text_decode)
                                          
                                          #COLLAPSE AT DOUBLE /N's
                                          filing_text_collapse <- filing_text_sub
                                          rm(filing_text_sub)
                                          
                                          
                                          #CREATE CLEAN DATA
                                          filing_text_letter0 <- data.frame(filing_text_collapse,text_id=NA,text_trim=NA,letter_beginning=NA,letter_ending=NA,letter_signature=NA,letter_position=NA,
                                                                            beg_cum_sum=NA,end_cum_sum=NA,stringsAsFactors=FALSE)
                                          colnames(filing_text_letter0)[match("text_trim",names(filing_text_letter0))] <- xmltrim_col
                                          #colnames(filing_text_letter0)[match("index_temp_overall",names(filing_text_letter0))] <- "LETTER_INDEX"
                                          
                                          filing_text_id_cols <- c("file","DOCUMENT_INDEX","text_id",xmlcol,xmltrim_col)
                                          filing_text_letter0 <- filing_text_letter0[,c(filing_text_id_cols,
                                                                                        colnames(filing_text_letter0[,!(colnames(filing_text_letter0) %in% filing_text_id_cols)]))]
                                          
                                          filing_text_letter0[,c("text_id")] <-  seq(1,nrow(filing_text_letter0),1)
                                          filing_text_letter0[,c(xmltrim_col)] <-  filing_text_letter0[,c(xmlcol)] 
                                          filing_text_letter0[,c(xmltrim_col)] <- gsub(" {2,}", " ",filing_text_letter0[,c(xmltrim_col)])
                                          filing_text_letter0[,c(xmltrim_col)] <- gsub("^\\s+|\\s+$", "", filing_text_letter0[,c(xmltrim_col)])
                                          
                                          filing_text_letter0[,c(xmltrim_col)] <- paste(" ", filing_text_letter0[,c(xmltrim_col)]," ",sep="")
                                          
                                          rm(filing_text_collapse)
                                          
                                          
                                          
                                          
                                          # THERE NEEDS TO BE A CHECK IF BEGINNIGN IS FOUND, IF NOT BREAK AND DON"T RUN REST OF CODE
                                          # IF BEGINNING IS FOUND, THEN CHECK ONLY END (OR I THINK IT MIGHT BE GOOD TO CHECK ALL 3 BUT WILL BE A PERFORMANCE HIT)
                                          
                                          
                                          
                        
                                          
                                          
                                          #FIND BEG MATCHES  
                                          letter_beginning_regex0a <- data.frame(REGEX_PRIORITY=1,beg_txt="^\\s+",end_txt="(S*)\\s*(,|:|-)\\s+$",stringsAsFactors=FALSE)
                                          letter_beginning_regex0b <- data.frame(REGEX_PRIORITY=2,beg_txt="^\\s+",end_txt="(S*)\\s+$",stringsAsFactors=FALSE)
                                          #letter_beginning_regex0c <- data.frame(REGEX_PRIORITY=3,beg_txt="^\\s+(?=(",end_txt="(S*)))",stringsAsFactors=FALSE)
                                          letter_beginning_regex0 <- rbindlist(list(letter_beginning_regex0a,letter_beginning_regex0b))
                                          letter_beginning_regex <- regex_expand(regex_stubs=letter_beginning_regex0,strs=letter_beginning,
                                                                                 strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")
                                          rm(letter_beginning_regex0a,letter_beginning_regex0b)
                                          
                                          cat("STEP 0:","\n")
                                          
                                          for (i in 1:nrow(letter_beginning_regex0))
                                          {
                                            #  i <- 1
                                            
                                            #cat("REGEX PRIORITY:",i,"\n")
   
                                            regex_temp <- letter_beginning_regex[letter_beginning_regex[,"REGEX_PRIORITY"]==i,]

                                            filing_text_letter_id1 <- regex_section_matches_expand(regex_strs=regex_temp[,"regex"],data=filing_text_letter0, dv_col="letter_beginning", txt_col=xmltrim_col)
                                            filing_text_letter_matches_beg <- filing_text_letter_id1[!is.na(filing_text_letter_id1[,"letter_beginning"]),]
                                            filing_text_letter_matches_beg <- filing_text_letter_matches_beg[,(colnames(filing_text_letter_matches_beg) %in% c(filing_text_id_cols,"letter_beginning"))]
                                            filing_text_letter1 <- regex_section_matches_collapse(matches_expand=filing_text_letter_id1, dv_col="letter_beginning", txtid_col="text_id")
                                            
                                            rm(regex_temp,filing_text_letter_id1)

                                            if (nrow(filing_text_letter_matches_beg)!=0) { filing_text_letter_matches_beg <- data.frame(filing_text_letter_matches_beg,regex_priority=i,stringsAsFactors=FALSE); break }
                                            if (i!=nrow(letter_beginning_regex0)) { rm(filing_text_letter_matches_beg,filing_text_letter1) }
  
                                          } 
                                          rm(letter_beginning_regex0,letter_beginning_regex,filing_text_letter0)
                                          invisible(gc(verbose = FALSE, reset = TRUE))
                                          
                                          
                                          #FIND END MATCHES
                                          letter_ending_regex0a <- data.frame(REGEX_PRIORITY=1,beg_txt="^\\s+",end_txt="(\\s*)(,|:|-)\\s+$",stringsAsFactors=FALSE)
                                          letter_ending_regex0b <- data.frame(REGEX_PRIORITY=2,beg_txt="^\\s+",end_txt="\\s+$",stringsAsFactors=FALSE)
                                          letter_ending_regex0c <- data.frame(REGEX_PRIORITY=3,beg_txt="^\\s+(?=(",end_txt="))",stringsAsFactors=FALSE)
                                          letter_ending_regex0d <- data.frame(REGEX_PRIORITY=4,beg_txt="^.*\\s+(?=(",end_txt="))",stringsAsFactors=FALSE)
                                          #letter_ending_regex0e <- data.frame(REGEX_PRIORITY=5,beg_txt=".*",end_txt=".*",stringsAsFactors=FALSE)
                                          letter_ending_regex0 <- rbindlist(list(letter_ending_regex0a,letter_ending_regex0b,letter_ending_regex0c,letter_ending_regex0d))
                                          letter_ending_regex <- regex_expand(regex_stubs=letter_ending_regex0,strs=letter_ending,
                                                                              strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")
                                          rm(letter_ending_regex0a,letter_ending_regex0b,letter_ending_regex0c,letter_ending_regex0d)
                                          
                                          for (j in 1:nrow(letter_ending_regex0))
                                          {
                                            #  j <- 1
                                            #  j <- 2
                                            #  j <- 3
                                            
                                            #cat("REGEX PRIORITY:",j,"\n")
                                            
                                            regex_temp <- letter_ending_regex[letter_ending_regex[,"REGEX_PRIORITY"]==j,]
                                            
                                            filing_text_letter_id2 <- regex_section_matches_expand(regex_strs=regex_temp[,"regex"],data=filing_text_letter1, dv_col="letter_ending", txt_col=xmltrim_col)
                                            filing_text_letter_matches_end <- filing_text_letter_id2[!is.na(filing_text_letter_id2[,"letter_ending"]),]
                                            filing_text_letter_matches_end <- filing_text_letter_matches_end[,(colnames(filing_text_letter_matches_end) %in% c(filing_text_id_cols,"letter_ending"))]
                                            filing_text_letter2 <- regex_section_matches_collapse(matches_expand=filing_text_letter_id2, dv_col="letter_ending", txtid_col="text_id")
                                            
                                            rm(regex_temp,filing_text_letter_id2)
                                            
                                            if (nrow(filing_text_letter_matches_end)!=0) { filing_text_letter_matches_end <- data.frame(filing_text_letter_matches_end,regex_priority=j,stringsAsFactors=FALSE); break }
                                            if (j!=nrow(letter_ending_regex0)) { rm(filing_text_letter_matches_end,filing_text_letter2) }
                                          }
                                          rm(letter_ending_regex0,letter_ending_regex,filing_text_letter1)
                                          
                                          
                                          #FIND SIGNATURE MATCHES
                                          letter_signature_regex0a <- data.frame(REGEX_PRIORITY=1,beg_txt="^\\s+(BY(:|-))\\s*",end_txt=".*$",stringsAsFactors=FALSE)
                                          letter_signature_regex0b <- data.frame(REGEX_PRIORITY=2,beg_txt="^\\s+",end_txt="\\s+$",stringsAsFactors=FALSE)
                                          letter_signature_regex0c <- data.frame(REGEX_PRIORITY=3,beg_txt="^\\s+(?=(",end_txt="))",stringsAsFactors=FALSE)
                                          letter_signature_regex0d <- data.frame(REGEX_PRIORITY=4,beg_txt="^.*\\s+(?=(",end_txt="))",stringsAsFactors=FALSE)
                                          letter_signature_regex0e <- data.frame(REGEX_PRIORITY=5,beg_txt=".*\\s+",end_txt="\\s+.*",stringsAsFactors=FALSE)
                                          letter_signature_regex0 <- rbindlist(list(letter_signature_regex0a,letter_signature_regex0b,letter_signature_regex0c,letter_signature_regex0d,letter_signature_regex0e))
                                          letter_signature_regex <- regex_expand(regex_stubs=letter_signature_regex0,strs=letter_signature,
                                                                                 strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")
                                          rm(letter_signature_regex0a,letter_signature_regex0b,letter_signature_regex0c,letter_signature_regex0d,letter_signature_regex0e)
                                          
                                          for (k in 1:nrow(letter_signature_regex0))
                                          {
                                            #  k <- 1
                                            
                                            #cat("REGEX PRIORITY:",k,"\n")
                                            
                                            regex_temp <- letter_signature_regex[letter_signature_regex[,"REGEX_PRIORITY"]==k,]
                                            
                                            filing_text_letter_id3 <- regex_section_matches_expand(regex_strs=regex_temp[,"regex"],data=filing_text_letter2, dv_col="letter_signature", txt_col=xmltrim_col)
                                            filing_text_letter_matches_sign <- filing_text_letter_id3[!is.na(filing_text_letter_id3[,"letter_signature"]),]
                                            filing_text_letter_matches_sign <- filing_text_letter_matches_sign[,(colnames(filing_text_letter_matches_sign) %in% c(filing_text_id_cols,"letter_signature"))]
                                            filing_text_letter3 <- regex_section_matches_collapse(matches_expand=filing_text_letter_id3, dv_col="letter_signature", txtid_col="text_id")
                                            
                                            rm(regex_temp,filing_text_letter_id3)
                                            
                                            if (nrow(filing_text_letter_matches_sign)!=0) { filing_text_letter_matches_sign <- data.frame(filing_text_letter_matches_sign,regex_priority=k,stringsAsFactors=FALSE); break }
                                            if (k!=nrow(letter_signature_regex0)) { rm(filing_text_letter_matches_sign,filing_text_letter3) }
                                          }
                                          rm(letter_signature_regex0,letter_signature_regex,filing_text_letter2)
                                          
                                          
                                          #FIND POSITION MATCHES
                                          letter_position_regex0a <- data.frame(REGEX_PRIORITY=1,beg_txt="^\\s+(TITLE(:|-))\\s*",end_txt="(S*)\\+$",stringsAsFactors=FALSE)
                                          letter_position_regex0b <- data.frame(REGEX_PRIORITY=2,beg_txt="^\\s+",end_txt="(S*)\\s+$",stringsAsFactors=FALSE)
                                          letter_position_regex0c <- data.frame(REGEX_PRIORITY=3,beg_txt="^\\s+(?=(",end_txt="(S*)))",stringsAsFactors=FALSE)
                                          letter_position_regex0d <- data.frame(REGEX_PRIORITY=4,beg_txt="^.*\\s+(?=(",end_txt="(S*)))",stringsAsFactors=FALSE)
                                          letter_position_regex0 <- rbindlist(list(letter_position_regex0a,letter_position_regex0b,letter_position_regex0c,letter_position_regex0d))
                                          letter_position_regex <- regex_expand(regex_stubs=letter_position_regex0,strs=letter_position,
                                                                                strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")
                                          rm(letter_position_regex0a,letter_position_regex0b,letter_position_regex0c,letter_position_regex0d)
                                          
                                          for (l in 1:nrow(letter_position_regex0))
                                          {
                                            #  l <- 1
                                            
                                            #cat("REGEX PRIORITY:",l,"\n")
                                            
                                            regex_temp <- letter_position_regex[letter_position_regex[,"REGEX_PRIORITY"]==l,]
                                            
                                            filing_text_letter_id4 <- regex_section_matches_expand(regex_strs=regex_temp[,"regex"],data=filing_text_letter3, dv_col="letter_position", txt_col=xmltrim_col)
                                            filing_text_letter_matches_pos <- filing_text_letter_id4[!is.na(filing_text_letter_id4[,"letter_position"]),]
                                            filing_text_letter_matches_pos <- filing_text_letter_matches_pos[,(colnames(filing_text_letter_matches_pos) %in% c(filing_text_id_cols,"letter_position"))]
                                            filing_text_letter4 <- regex_section_matches_collapse(matches_expand=filing_text_letter_id4, dv_col="letter_position", txtid_col="text_id")
                                            
                                            rm(regex_temp,filing_text_letter_id4)
                                            
                                            if (nrow(filing_text_letter_matches_pos)!=0) { filing_text_letter_matches_pos <- data.frame(filing_text_letter_matches_pos,regex_priority=l,stringsAsFactors=FALSE); break }
                                            if (l!=nrow(letter_position_regex0)) { rm(filing_text_letter_matches_pos,filing_text_letter4) }
                                          }
                                          rm(letter_position_regex0,letter_position_regex,filing_text_letter3)
                                          
                                          
                                          #CREATE MATCH SUMMARIES
                                          #matches_summary0a <- data.frame(file=file,type="letter_beginning",final_regex_priority=i,overall_matches=nrow(filing_text_letter_matches_beg),unique_matches=length(unique(filing_text_letter_matches_beg[,c("text_id")])),stringsAsFactors=FALSE)
                                          #matches_summary0b <- data.frame(file=file,type="letter_ending",final_regex_priority=j,overall_matches=nrow(filing_text_letter_matches_end),unique_matches=length(unique(filing_text_letter_matches_end[,c("text_id")])),stringsAsFactors=FALSE)
                                          #matches_summary0c <- data.frame(file=file,type="letter_signature",final_regex_priority=k,overall_matches=nrow(filing_text_letter_matches_sign),unique_matches=length(unique(filing_text_letter_matches_sign[,c("text_id")])),stringsAsFactors=FALSE)
                                          #matches_summary0d <- data.frame(file=file,type="letter_position",final_regex_priority=l,overall_matches=nrow(filing_text_letter_matches_pos),unique_matches=length(unique(filing_text_letter_matches_pos[,c("text_id")])),stringsAsFactors=FALSE)
                                          #filing_text_letter_matches_summary_old <- rbindlist(list(matches_summary0a,matches_summary0b,matches_summary0c,matches_summary0d))
                                          #rm(matches_summary0a,matches_summary0b,matches_summary0c,matches_summary0d,i,j,k,l)
                                          
                                          matches_summary0a <- data.frame(file=file,type="final_regex_priority",letter_beginning=i,letter_ending=j,letter_signature=k,letter_position=l,stringsAsFactors=FALSE)
                                          matches_summary0b <- data.frame(file=file,type="overall_matches",letter_beginning=nrow(filing_text_letter_matches_beg),letter_ending=nrow(filing_text_letter_matches_end),
                                                                          letter_signature=nrow(filing_text_letter_matches_sign),letter_position=nrow(filing_text_letter_matches_pos),stringsAsFactors=FALSE)
                                          matches_summary0c <- data.frame(file=file,type="unique_matches",letter_beginning=length(unique(filing_text_letter_matches_beg[,c("text_id")])),letter_ending=length(unique(filing_text_letter_matches_end[,c("text_id")])),
                                                                          letter_signature=length(unique(filing_text_letter_matches_sign[,c("text_id")])),letter_position=length(unique(filing_text_letter_matches_pos[,c("text_id")])),stringsAsFactors=FALSE)
                                          filing_text_letter_matches_summary <- rbindlist(list(matches_summary0a,matches_summary0b,matches_summary0c))
                                          rm(matches_summary0a,matches_summary0b,matches_summary0c,i,j,k,l)
                                          
                                          
                                          #REMOVE TEXT BEFORE LETTER
                                          filing_text_letter5 <- ddply(.data=filing_text_letter4, .variables=c("file","DOCUMENT_INDEX"), .fun = function(x){
                                            
                                            #  x <- filing_text_letter4[(filing_text_letter4[,"DOCUMENT_INDEX"]==1),]
                                            
                                            file_temp <- unique(x[,"file"])
                                            index_temp <- unique(x[,"DOCUMENT_INDEX"])
                                            
                                            x[,"beg_cum_sum"] <- cumsum(x[,"letter_beginning"])
                                            x_trim <- x[!(x[,"beg_cum_sum"]==0),]
                                            
                                            if (nrow(x_trim) == 0) {
                                              
                                              ##cat("NO BEGINNING MATCHES FOUND", "\n")
                                              x_trim[1,] <- NA
                                              x_trim[,"file"] <- file_temp
                                              x_trim[,"DOCUMENT_INDEX"] <- index_temp 
                                              
                                            } else {
                                              
                                              #cat("BEGINNING MATCHES FOUND", "\n")
                                              
                                            }
                                            return(x_trim)
                                          })
                                          rm(filing_text_letter4)
                                          
                                          
                                          #REMOVE TEXT AFTER LETTER
                                          filing_text_letter5_empty_cols <- as.data.frame(t(!colSums(is.na(filing_text_letter5))<nrow(filing_text_letter5)),stringsAsFactors=FALSE)
                                          
                                          if (filing_text_letter5_empty_cols[,"beg_cum_sum"]) {
                                            
                                            cat("NO BEGINNING MATCHES FOUND", "\n")
                                            
                                            filing_text_letter6 <- filing_text_letter5
                                            filing_text_letter6[,"beg_cum_sum"] <- 0
                                            
                                          } else {
                                            
                                            #cat("BEGINNING MATCHES FOUND", "\n")
                                            
                                            
                                            #THE CRITERIA NEEDS TO BE NOT IF END !=0.  WHAT ABOUT THE MINIMUM OF THE THREE????
                                            
                                            
                                            
                                            
                                            filing_text_letter6 <- ddply(.data=filing_text_letter5, .variables=c("file","DOCUMENT_INDEX","beg_cum_sum"), 
                                                                         .fun = function(x,bycol,xmlcol){
                                                                           
                                                                           # x <- filing_text_letter5[(filing_text_letter5[,"DOCUMENT_INDEX"]==1 & filing_text_letter5[,"beg_cum_sum"]==1),]
                                                                           # bycol <- c("file","DOCUMENT_INDEX","beg_cum_sum")
                                                                           # xmlcol <- xmlcol
                                                                           
                                                                           file_temp <- unique(x[,"file"])
                                                                           index_temp <- unique(x[,"DOCUMENT_INDEX"])
                                                                           beg_cum_sum_temp <- unique(x[,"beg_cum_sum"])
                                                                           
                                                                           filing_text_letter_extract0 <- x
                                                                           
                                                                           #letter_flag_beginning1 <- sum(filing_text_letter_extract0[,"letter_beginning"], na.rm = TRUE)
                                                                           letter_flag_ending1 <- sum(filing_text_letter_extract0[,"letter_ending"], na.rm = TRUE)
                                                                           letter_flag_signature1 <- sum(filing_text_letter_extract0[,"letter_signature"], na.rm = TRUE)
                                                                           letter_flag_position1 <- sum(filing_text_letter_extract0[,"letter_position"], na.rm = TRUE)
                                                                           
                                                                           if (letter_flag_ending1 != 0) {
                                                                             
                                                                             filing_text_letter_extract0[,"end_cum_sum"] <- cumsum(filing_text_letter_extract0[,"letter_ending"])
                                                                             letter_flag_good <- letter_flag_ending1
                                                                             
                                                                           } else if (letter_flag_signature1 != 0) {
                                                                             
                                                                             filing_text_letter_extract0[,"end_cum_sum"] <- cumsum(filing_text_letter_extract0[,"letter_signature"])
                                                                             letter_flag_good <- letter_flag_signature1
                                                                             
                                                                           } else if (letter_flag_position1 != 0) {
                                                                             
                                                                             filing_text_letter_extract0[,"end_cum_sum"] <- cumsum(filing_text_letter_extract0[,"letter_position"])
                                                                             letter_flag_good <- letter_flag_position1
                                                                             
                                                                           } else {
                                                                             
                                                                             filing_text_letter_extract0[,"end_cum_sum"] <- NA
                                                                             letter_flag_good <- NA
                                                                             
                                                                           }
                                                                           rm(letter_flag_ending1,letter_flag_signature1,letter_flag_position1)
                                                                           
                                                                           #Check for no ending matches
                                                                           if (is.na(letter_flag_good)) {
                                                                             
                                                                             cat("NO END MATCHES FOUND", "\n")
                                                                             
                                                                             
                                                                             filing_text_letter_extract0_trim <- filing_text_letter_extract0[1,]
                                                                             filing_text_letter_extract0_trim[,xmlcol] <- NA
                                                                             filing_text_letter_extract0_trim[,xmltrim_col] <- NA
                                                                             
                                                                             
                                                                           } else {
                                                                             
                                                                             #cat("END MATCHES FOUND", "\n")
                                                                             
                                                                             filing_text_letter_extract0_trim <- filing_text_letter_extract0[(filing_text_letter_extract0[,"end_cum_sum"]==0),]
                                                                             #filing_text_letter_extract0_trim[,"LETTER_INDEX"] <- filing_text_letter_extract0_trim[,"beg_cum_sum"]
                                                                             
                                                                           }
                                                                           rm(filing_text_letter_extract0,letter_flag_good)
                                                                           rm(file_temp,index_temp,beg_cum_sum_temp)
                                                                           
                                                                           return(filing_text_letter_extract0_trim)
                                                                           
                                                                         }, bycol=c("file","DOCUMENT_INDEX","beg_cum_sum"),xmlcol=xmlcol,
                                                                         .progress = "none", .inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)
                                          }
                                          
                                          colnames(filing_text_letter6)[match("beg_cum_sum",names(filing_text_letter6))] <- "LETTER_INDEX"
                                          filing_text_letter6 <- filing_text_letter6[,(colnames(filing_text_letter6) %in% c("file","DOCUMENT_INDEX","LETTER_INDEX",xmlcol))]
                                          rm(filing_text_letter5,filing_text_letter5_empty_cols)
                                          
                                          
                                          #CLEAN TEXT
                                          filing_text_parse_clean <- ddply(.data=filing_text_letter6, .variables=c("file","DOCUMENT_INDEX","LETTER_INDEX"), 
                                                                           .fun = function(x,bycol,xmlcol){
                                                                             
                                                                             # x <- filing_text_letter6[filing_text_letter6[,"LETTER_INDEX"]==0,]
                                                                             # x <- filing_text_letter6[filing_text_letter6[,"LETTER_INDEX"]==1,]
                                                                             # bycol <- c("file","DOCUMENT_INDEX","LETTER_INDEX")
                                                                             # xmlcol <- xmlcol
                                                                             
                                                                             file_temp <- unique(x[,"file"])
                                                                             index_temp <- unique(x[,"DOCUMENT_INDEX"])
                                                                             letter_temp <- unique(x[,"LETTER_INDEX"])
                                                                             
                                                                             x[,xmlcol] <- gsub("\n", " ", x[,xmlcol])
                                                                             
                                                                             for(i in which(sapply(x,class)=="character"))
                                                                             {
                                                                               x[[i]] <- gsub(" {2,}", " ", x[[i]])
                                                                               x[[i]] <- trim(x[[i]])
                                                                             }
                                                                             rm(i)
                                                                             
                                                                             for (i in 1:ncol(x))
                                                                             {
                                                                               x[,i] <- unknownToNA(x[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                                                     NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                                                     NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
                                                                               x[,i] <- ifelse(is.na(x[,i]),"", x[,i])
                                                                             } 
                                                                             rm(i)
                                                                             
                                                                             #Create empty row
                                                                             x_empty <- x[1,]
                                                                             x_empty[,xmlcol] <- ""
                                                                             
                                                                             #Remove rows until the beginning of the letter
                                                                             if (length(which(!x[,xmlcol]==""))==0) {
                                                                               
                                                                               #cat("ALL ROWS ARE EMPTY", "\n")
                                                                               x_trim <- x
                                                                               
                                                                             } else {
                                                                               
                                                                               #cat("ALL ROWS ARE NOT EMPTY", "\n")
                                                                               x_trim <- x[min(which(!x[,xmlcol]=="")):nrow(x),]
                                                                               
                                                                             }
                                                                             rm(x)
                                                                             
                                                                             #Remove rows until the beginning of the letter
                                                                             if (nrow(x_trim)==1) {
                                                                               
                                                                               x_expand_list <- list(x_empty,x_trim[1,],x_empty)
                                                                               
                                                                             } else {
                                                                               
                                                                               x_expand_list <- list(x_empty,x_trim[1,],x_empty,x_trim[2:nrow(x_trim),],x_empty)
                                                                               
                                                                             }
                                                                             x_expand <- rbindlist(l=x_expand_list, use.names=TRUE, fill=FALSE)
                                                                             rm(x_expand_list,x_empty,x_trim)
                                                                             
                                                                             #Find Empty Rows                                                                                   
                                                                             x_replace <- data.frame(x_expand,para_start=NA,stringsAsFactors=FALSE)
                                                                             #x_replace[,xmlcol] <- ifelse(x_replace[,xmlcol]=="","\n", x_replace[,xmlcol])
                                                                             x_replace[,"para_start"] <- ifelse(x_replace[,xmlcol]=="",1, 0)
                                                                             x_replace[,"para_start"] <- cumsum(x_replace[,"para_start"])
                                                                             rm(x_expand)
                                                                             
                                                                             #Pad Cells Before Collapse
                                                                             x_replace[,xmlcol] <- paste(" ", x_replace[,xmlcol], " ", sep="")
                                                                             
                                                                             text_collapse1 <-  ddply(.data=x_replace, .variables=c(bycol,"para_start"), .fun = function(z,xmlcol,collapse_str){ 
                                                                               
                                                                               z_out <- z
                                                                               z_out[,xmlcol] <- NA
                                                                               z_out <- unique(z_out)
                                                                               
                                                                               z_out[,xmlcol] <- paste(z[,xmlcol], collapse = collapse_str)
                                                                               z_out[,xmlcol] <- gsub(" {2,}", " ",z_out[,xmlcol])
                                                                               z_out[,xmlcol] <- gsub("^\\s+|\\s+$", "", z_out[,xmlcol])
                                                                               
                                                                               return(z_out)
                                                                               
                                                                             },xmlcol=xmlcol, collapse_str="", .progress = "none", .inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)
                                                                             
                                                                             rm(x_replace)
                                                                             
                                                                             if (length(which(!text_collapse1[,xmlcol]==""))==0) {
                                                                               
                                                                               #cat("ALL ROWS ARE EMPTY", "\n")
                                                                               text_collapse1_trim <- text_collapse1[1,]
                                                                               text_collapse1_trim <- text_collapse1_trim[,!(colnames(text_collapse1_trim) %in% c("para_start"))]
                                                                               
                                                                             } else {
                                                                               
                                                                               #cat("ALL ROWS ARE NOT EMPTY", "\n")
                                                                               text_collapse1_trim <- text_collapse1[!(text_collapse1[,xmlcol]==""),]
                                                                               text_collapse1_trim <- text_collapse1_trim[,!(colnames(text_collapse1_trim) %in% c("para_start"))]
                                                                               
                                                                             }
                                                                             rm(text_collapse1)
                                                                             
                                                                             text_collapse2 <-  ddply(.data=text_collapse1_trim, .variables=c(bycol), .fun = function(z,xmlcol,collapse_str){ 
                                                                               
                                                                               z_out <- z
                                                                               z_out[,xmlcol] <- NA
                                                                               z_out <- unique(z_out)
                                                                               
                                                                               z_out[,xmlcol] <- paste(z[,xmlcol], collapse = collapse_str)
                                                                               z_out[,xmlcol] <- gsub(" {2,}", " ",z_out[,xmlcol])
                                                                               z_out[,xmlcol] <- gsub("^\\s+|\\s+$", "", z_out[,xmlcol])
                                                                               
                                                                               return(z_out)
                                                                               
                                                                             },xmlcol=xmlcol, collapse_str="\n", .progress = "none", .inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)
                                                                             
                                                                             rm(text_collapse1_trim)
                                                                             
                                                                             
                                                                             if (length(which(!text_collapse2[,xmlcol]==""))==0) {
                                                                               
                                                                               #cat("ALL ROWS ARE EMPTY", "\n")
                                                                               text_collapse2_trim <- text_collapse2[1,]
                                                                               
                                                                             } else {
                                                                               
                                                                               #cat("ALL ROWS ARE NOT EMPTY", "\n")
                                                                               text_collapse2_trim <- text_collapse2[!(text_collapse2[,xmlcol]==""),]
                                                                             }
                                                                             rm(text_collapse2)
                                                                             
                                                                             
                                                                             #colnames(text_collapse2_trim) <- c(bycol,xmlcol)
                                                                             text_collapse3 <- text_collapse2_trim[,c(colnames(text_collapse2_trim[,!(colnames(text_collapse2_trim) %in% c(xmlcol))]),xmlcol)]
                                                                             
                                                                             rm(file_temp,index_temp,letter_temp,text_collapse2_trim)
                                                                             
                                                                             return(text_collapse3)
                                                                             
                                                                           }, bycol=c("file","DOCUMENT_INDEX","LETTER_INDEX"),xmlcol=xmlcol,
                                                                           .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                                          
                                          rm(filing_text_letter6)
                                          
                                          #CLEAN EMPTY ROWS
                                          #filing_text_parse_clean <- data.frame(filing_text_parse,bad_row=NA,stringsAsFactors=FALSE)
                                          #rm(filing_text_parse)
                                          
                                          #filing_text_parse_clean[,"bad_row"] <- ifelse(grepl("^\\s*$", filing_text_parse_clean[,xmlcol]), 1, 0)
                                          
                                          #filing_text_parse_clean_trim <- filing_text_parse_clean[!(filing_text_parse_clean[,"bad_row"]==1),]
                                          #filing_text_parse_clean_trim <- filing_text_parse_clean_trim[,!(colnames(filing_text_parse_clean_trim) %in% c("bad_row"))]
                                          #row.names(filing_text_parse_clean_trim) <- seq(nrow(filing_text_parse_clean_trim))
                                          #rm(filing_text_parse_clean)
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          #THERE NEEDS TO BE A TEST TO CHECK IF CHARACTER COUNT IS LARGER THAN 32,767
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          
                                          #CREATE FINAL DATA AND OUTPUT
                                          filing_text_comb <- merge(filing_no_text2, filing_text_parse_clean, 
                                                                    by.x=c("file","DOCUMENT_INDEX"), by.y=c("file","DOCUMENT_INDEX"), 
                                                                    all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
                                          
                                          rm(filing_no_text2,filing_text_parse_clean)
                                          
                                          #write.table(filing_text_comb,file=filepath_out, append=FALSE, na="", 
                                          #             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
                                          
                                          df_comb_list <- list(filing_text_comb,filing_text_letter_matches_summary,
                                                               filing_text_letter_matches_beg,filing_text_letter_matches_end,
                                                               filing_text_letter_matches_pos,filing_text_letter_matches_sign)
                                          
                                          rm(filing_text_comb,filing_text_letter_matches_summary)
                                          rm(filing_text_letter_matches_beg,filing_text_letter_matches_end)
                                          rm(filing_text_letter_matches_pos,filing_text_letter_matches_sign)
                                          rm(file,filepath)
                                          #rm(file_out,filepath_out)
                                          rm(xmlcol,xmltrim_col)
                                          
                                          return(df_comb_list)
                                          
                                        },
                                        entity_encoding=entity_encoding,letter_beginning=letter_beginning,
                                        letter_ending=letter_ending,letter_position=letter_position,letter_signature=letter_signature,
                                        .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                       
                       #letters <- bad_tags
                       
                       letters_comb0 <- sapply(letters, "[", 1)
                       letters_comb1 <- rbindlist(letters_comb0,fill=TRUE,use.names=TRUE)
                       #letters_comb  <- as.data.frame(letters_comb1,stringsAsFactors=FALSE) 
                       letters_comb <- data.frame(yr=NA,letters_comb1,stringsAsFactors=FALSE) 
                       letters_comb[,"yr"] <- yr
                       rm(letters_comb0,letters_comb1)
                       
                       letter_matches_summary0 <- sapply(letters, "[", 2)
                       letter_matches_summary1 <- rbindlist(letter_matches_summary0,fill=TRUE,use.names=TRUE)
                       letter_matches_summary <- data.frame(yr=NA,letter_matches_summary1,stringsAsFactors=FALSE) 
                       letter_matches_summary[,"yr"] <- yr
                       rm(letter_matches_summary0,letter_matches_summary1)
                       
                       letter_matches_beg0 <- sapply(letters, "[", 3)
                       letter_matches_beg1 <- rbindlist(letter_matches_beg0,fill=TRUE,use.names=TRUE)
                       letter_matches_beg <- data.frame(yr=NA,letter_matches_beg1,stringsAsFactors=FALSE) 
                       letter_matches_beg[,"yr"] <- yr
                       rm(letter_matches_beg0,letter_matches_beg1)
                       
                       letter_matches_end0 <- sapply(letters, "[", 4)
                       letter_matches_end1 <- rbindlist(letter_matches_end0,fill=TRUE,use.names=TRUE)
                       letter_matches_end <- data.frame(yr=NA,letter_matches_end1,stringsAsFactors=FALSE) 
                       letter_matches_end[,"yr"] <- yr
                       rm(letter_matches_end0,letter_matches_end1)
                       
                       letter_matches_pos0 <- sapply(letters, "[", 5)
                       letter_matches_pos1 <- rbindlist(letter_matches_pos0,fill=TRUE,use.names=TRUE)
                       letter_matches_pos <- data.frame(yr=NA,letter_matches_pos1,stringsAsFactors=FALSE) 
                       letter_matches_pos[,"yr"] <- yr
                       rm(letter_matches_pos0,letter_matches_pos1)
                       
                       letter_matches_sign0 <- sapply(letters, "[", 6)
                       letter_matches_sign1 <- rbindlist(letter_matches_sign0,fill=TRUE,use.names=TRUE)
                       letter_matches_sign <- data.frame(yr=NA,letter_matches_sign1,stringsAsFactors=FALSE) 
                       letter_matches_sign[,"yr"] <- yr
                       rm(letter_matches_sign0,letter_matches_sign1)
                       
                       df_comb_list_all <- list(letters_comb,letter_matches_summary,letter_matches_beg,letter_matches_end,letter_matches_pos,letter_matches_sign)
                       
                       rm(letters,letters_comb,letter_matches_summary,letter_matches_beg,letter_matches_end,letter_matches_pos,letter_matches_sign)
                       rm(yr,yr_folder_path,sub_folder_path,sub_folder_output_path,downloaded_files3)
                       
                       return(df_comb_list_all)
                       
                     },
                     path_output=paste(output_directory,downloadfolder,sep=slash),
                     subfolder=txtfolder_clean,entity_encoding=entity_encoding,
                     letter_beginning=letter_beginning,letter_ending=letter_ending,letter_position=letter_position,letter_signature=letter_signature,
                     .progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


###############################################################################
cat("Seperate Data \n")
###############################################################################

letter_all_comb0 <- sapply(letters_all, "[", 1)
letter_all_comb <- rbindlist(letter_all_comb0,fill=TRUE,use.names=TRUE)
rm(letter_all_comb0)

letter_all_matches_summary0 <- sapply(letters_all, "[", 2)
letter_all_matches_summary <- rbindlist(letter_all_matches_summary0,fill=TRUE,use.names=TRUE)
rm(letter_all_matches_summary0)

letter_all_matches_beg0 <- sapply(letters_all, "[", 3)
letter_all_matches_beg <- rbindlist(letter_all_matches_beg0,fill=TRUE,use.names=TRUE)
rm(letter_all_matches_beg0)

letter_all_matches_end0 <- sapply(letters_all, "[", 4)
letter_all_matches_end <- rbindlist(letter_all_matches_end0,fill=TRUE,use.names=TRUE)
rm(letter_all_matches_end0)

letter_all_matches_pos0 <- sapply(letters_all, "[", 5)
letter_all_matches_pos <- rbindlist(letter_all_matches_pos0,fill=TRUE,use.names=TRUE)
rm(letter_all_matches_pos0)

letter_all_matches_sign0 <- sapply(letters_all, "[", 6)
letter_all_matches_sign <- rbindlist(letter_all_matches_sign0,fill=TRUE,use.names=TRUE)
rm(letter_all_matches_sign0)


###############################################################################
cat("Output Combined Files \n")
###############################################################################

#Check to see if yr folder exists.  If not, create it.
out_folder_path <- paste(download_folder_path, txtfolder_section, sep = "\\", collapse = "\\")   
create_directory(out_folder_path,remove=1)

write.table(letter_all_comb,file=paste(out_folder_path,"\\","letter_all_comb",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_summary,file=paste(out_folder_path,"\\","letter_all_matches_summary",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_beg,file=paste(out_folder_path,"\\","letter_all_matches_beg",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_end,file=paste(out_folder_path,"\\","letter_all_matches_end",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_pos,file=paste(out_folder_path,"\\","letter_all_matches_pos",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_sign,file=paste(out_folder_path,"\\","letter_all_matches_sign",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

