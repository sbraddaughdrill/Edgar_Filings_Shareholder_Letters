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
  input_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research/Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
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

regex_execute <- function(data,levels,regex_data,regex_data_o_priority_col,regex_data_r_priority_col,regex_data_str_col,flags_all,flag_local,fileid_col,documentid_col,txtid_col,cum_cols) {
  
  #  data <- filing_text_letter0
  #  levels <- nrow(letter_beginning_regex0)
  #  regex_data <- letter_beginning_regex
  #  flag_local <- "letter_beginning"
  
  #  data <- filing_text_letter1
  #  levels <- nrow(letter_ending_regex0)
  #  regex_data <- letter_ending_regex
  #  flag_local <- "letter_ending"
  
  #  regex_data_o_priority_col="OVERALL_PRIORITY"
  #  regex_data_r_priority_col="REGEX_PRIORITY"
  #  regex_data_str_col <- "regex"
  #  flags_all <- flags
  #  fileid_col <- "file"
  #  documentid_col <- "DOCUMENT_INDEX"
  #  txtid_col <- "text_id"
  #  cum_cols <- c("beg_cum_sum","end_cum_sum")
  
  require(data.table)
  
  flags_nonlocal <- flags_all[!(flags_all %in% flag_local)]
  
  doc_txt_id_cols <- c(documentid_col,txtid_col)
  full_id_cols <- c(fileid_col,documentid_col,txtid_col)
  
  for (i in 1:levels)
  {
    #  i <- 1
    #  i <- 2
    
    #cat("REGEX PRIORITY:",i,"\n")
    
    #regex_temp <- regex_data[regex_data[,regex_data_r_priority_col]==i,regex_data_str_col]
    
    filing_text_letter_id_temp <- regex_section_matches_expand(regex_strs=regex_data[regex_data[,regex_data_r_priority_col]==i,regex_data_str_col],
                                                               data=data[,c(doc_txt_id_cols,xmltrim_col,flag_local)],
                                                               dv_col=flag_local, txt_col=xmltrim_col)
    
    filing_text_letter_id <- merge(data[,c(fileid_col,doc_txt_id_cols,xmlcol,flags_nonlocal,cum_cols)],
                                   filing_text_letter_id_temp,
                                   by.x=c(doc_txt_id_cols), by.y=c(doc_txt_id_cols),
                                   all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
    
    rm(filing_text_letter_id_temp)
    invisible(gc(verbose = FALSE, reset = TRUE))
    
    filing_text_letter_id <- filing_text_letter_id[,c(full_id_cols,xmlcol,xmltrim_col,flags_all,cum_cols)]
    
    filing_text_letter_id <- setorderv(data.table(filing_text_letter_id), c(full_id_cols), rep(1,length(full_id_cols)))
    filing_text_letter_id <- as.data.frame(filing_text_letter_id,stringsAsFactors=FALSE)
    
    #filing_text_letter_id <- filing_text_letter_id[order(filing_text_letter_id[,"file"], filing_text_letter_id[,"DOCUMENT_INDEX"], filing_text_letter_id[,txtid_col]),]
    #row.names(filing_text_letter_id) <- seq(nrow(filing_text_letter_id))
    
    filing_text_letter_matches_local <- filing_text_letter_id[!is.na(filing_text_letter_id[,flag_local]),
                                                              (colnames(filing_text_letter_id) %in% c(full_id_cols,xmlcol,xmltrim_col,flag_local))]
    filing_text_letter_temp <- regex_section_matches_collapse(matches_expand=filing_text_letter_id, dv_col=flag_local, txtid_col=txtid_col)
    
    #rm(regex_temp,filing_text_letter_id)
    rm(filing_text_letter_id)
    invisible(gc(verbose = FALSE, reset = TRUE))
    
    if (nrow(filing_text_letter_matches_local)!=0) { filing_text_letter_matches_local <- data.frame(filing_text_letter_matches_local,
                                                                                                    overall_priority=unique(regex_data[regex_data[,regex_data_r_priority_col]==i,regex_data_o_priority_col]),
                                                                                                    regex_priority=i,stringsAsFactors=FALSE); break }
    if (i!=levels) { rm(filing_text_letter_matches_local,filing_text_letter_temp) }
    
  }
  #rm(regex_data,data)
  rm(flags_nonlocal,doc_txt_id_cols,full_id_cols,i)
  invisible(gc(verbose = FALSE, reset = TRUE))

  return(list(filing_text_letter_temp,filing_text_letter_matches_local))
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
  filing_text_letter1 <- unique(filing_text_letter1_dt,use.key=FALSE)
  
  rm(filing_text_letter1_dt)
  invisible(gc(verbose = FALSE, reset = TRUE))
  
  return(as.data.frame(filing_text_letter1,stringsAsFactors=FALSE))
}


regex_section_matches_expand <- function(regex_strs,data,dv_col,txt_col) {
  
  #  regex_strs <- letter_beginning_regex[letter_beginning_regex[,"REGEX_PRIORITY"]==i,"regex"]
  #  regex_strs <- regex_temp[,"regex"]
  #  data <- filing_text_letter0
  #  dv_col <- "letter_beginning"
  #  txt_col <- xmltrim_col
  
  #ptm1 <- proc.time()
  
  invisible(gc(verbose = FALSE, reset = TRUE))
  filing_text_letter_id1_dt <- ldply(.data=regex_strs, .fun = function(x,data,dv_col,txt_col){
    
    #  x <- regex_strs[[1]]
    
    data[,dv_col] <- ifelse(grepl(x, data[,c(txt_col)],ignore.case = TRUE, perl = TRUE), x, NA)
    
    #data_dt <- data.table(data)
    #data <- unique(data_dt,use.key=FALSE)
    #data <- as.data.frame(data,stringsAsFactors=FALSE)
    
    return(data)
    
  }, data=data, dv_col=dv_col, txt_col=txt_col)
  
  filing_text_letter_id1_dt <- data.table(filing_text_letter_id1_dt)
  filing_text_letter_id1 <- unique(filing_text_letter_id1_dt,use.key=FALSE)
  
  rm(filing_text_letter_id1_dt)
  invisible(gc(verbose = FALSE, reset = TRUE))
  #proc.time() - ptm1
  
  return(as.data.frame(filing_text_letter_id1,stringsAsFactors=FALSE))
}


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","gdata","plyr","stringr","XML")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,repo)


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

#The sub directory where input filings are
txtfolder_in <- "txt_substitute"

#The sub directory where the output filings will go
txtfolder_out <- "letter"

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

rm(filings2,filings_trim,yr_qtr_comb2)


###############################################################################
cat("Import Regex Strings \n")
###############################################################################

letter_beginning <- read.table(file=paste(input_directory,"\\","letter_beginning_final",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_ending <- read.table(file=paste(input_directory,"\\","letter_ending_final",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_position <- read.table(file=paste(input_directory,"\\","letter_position_final",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_signature <- read.table(file=paste(input_directory,"\\","letter_signature_final",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_closing <- read.table(file=paste(input_directory,"\\","letter_closing_final",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")


###############################################################################
cat("Create Individual Regex Hash Tables \n")
###############################################################################

#BEGINNING MATCHES
letter_beginning_regex0a <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=1,beg_txt="^\\s+",end_txt="(S*)\\s*(,|:|-)\\s+$",stringsAsFactors=FALSE)
letter_beginning_regex0b <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=2,beg_txt="^\\s+",end_txt="(S*)\\s+$",stringsAsFactors=FALSE)
#letter_beginning_regex0c <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=3,beg_txt="^\\s+(?=(",end_txt="(S*)))",stringsAsFactors=FALSE)
letter_beginning_regex0 <- rbindlist(list(letter_beginning_regex0a,letter_beginning_regex0b))
letter_beginning_regex0 <- as.data.frame(letter_beginning_regex0,stringsAsFactors=FALSE)

letter_beginning_regex1 <- regex_expand(regex_stubs=letter_beginning_regex0,strs=letter_beginning,
                                        strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")

letter_beginning_regex <- merge(letter_beginning_regex0, letter_beginning_regex1, 
                                by.x=c("REGEX_PRIORITY"), by.y=c("REGEX_PRIORITY"), 
                                all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
colnames(letter_beginning_regex)[match("BEGINNINGS",names(letter_beginning_regex))] <- "STRING"
letter_beginning_regex[,"TYPE"] <- "BEG"
letter_beginning_regex <- letter_beginning_regex[,c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY",
                                                    colnames(letter_beginning_regex[,!(colnames(letter_beginning_regex) %in% c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY"))]))]

rm(letter_beginning_regex0a,letter_beginning_regex0b)
rm(letter_beginning_regex0,letter_beginning_regex1)


#ENDING MATCHES
letter_ending_regex0a <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=1,beg_txt="^\\s+",end_txt="(\\s*)(,|:|-)\\s+$",stringsAsFactors=FALSE)
letter_ending_regex0b <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=2,beg_txt="^\\s+",end_txt="\\s+$",stringsAsFactors=FALSE)
letter_ending_regex0c <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=3,beg_txt="^\\s+(?=(",end_txt="(\\s+)))",stringsAsFactors=FALSE)
letter_ending_regex0d <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=4,beg_txt="^.*\\s+(?=(",end_txt="(\\s+)))",stringsAsFactors=FALSE)
#letter_ending_regex0e <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=5,beg_txt=".*",end_txt=".*",stringsAsFactors=FALSE)
letter_ending_regex0 <- rbindlist(list(letter_ending_regex0a,letter_ending_regex0b,letter_ending_regex0c,letter_ending_regex0d))
letter_ending_regex0 <- as.data.frame(letter_ending_regex0,stringsAsFactors=FALSE)

letter_ending_regex1 <- regex_expand(regex_stubs=letter_ending_regex0,strs=letter_ending,
                                     strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")

letter_ending_regex <- merge(letter_ending_regex0, letter_ending_regex1, 
                             by.x=c("REGEX_PRIORITY"), by.y=c("REGEX_PRIORITY"), 
                             all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
colnames(letter_ending_regex)[match("ENDINGS",names(letter_ending_regex))] <- "STRING"
letter_ending_regex[,"TYPE"] <- "END"
letter_ending_regex <- letter_ending_regex[,c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY",
                                              colnames(letter_ending_regex[,!(colnames(letter_ending_regex) %in% c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY"))]))]

rm(letter_ending_regex0a,letter_ending_regex0b,letter_ending_regex0c,letter_ending_regex0d)
rm(letter_ending_regex0,letter_ending_regex1)


#SIGNATURE MATCHES
letter_signature_regex0a <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=1,beg_txt="^\\s+(\\/|:|-)*\\s*",end_txt="\\s*(\\/|:|-)*\\s+$",stringsAsFactors=FALSE)
letter_signature_regex0b <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=2,beg_txt="^\\s+(\\/|:|-)*\\s*(?=(",end_txt="(\\s*(\\/|:|-)*\\s+)))",stringsAsFactors=FALSE)
letter_signature_regex0c <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=3,beg_txt="^\\s+(\\/|:|-)*\\s*",end_txt="\\s*(\\/|:|-)*\\s+.*$",stringsAsFactors=FALSE)
letter_signature_regex0d <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=4,beg_txt="^.*\\s+(\\/|:|-)*\\s*(?=(",end_txt="(\\s*(\\/|:|-)*\\s+)))",stringsAsFactors=FALSE)
letter_signature_regex0e <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=5,beg_txt=".*\\s+(\\/|:|-)*\\s*",end_txt="\\s*(\\/|:|-)*\\s+.*",stringsAsFactors=FALSE)

#letter_signature_regex0a <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=1,beg_txt="^\\s+",end_txt="\\s+$",stringsAsFactors=FALSE)
#letter_signature_regex0b <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=2,beg_txt="^\\s+(?=(",end_txt="(\\s+)))",stringsAsFactors=FALSE)
#letter_signature_regex0c <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=3,beg_txt="^\\s+",end_txt="\\s+.*$",stringsAsFactors=FALSE)
#letter_signature_regex0d <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=4,beg_txt="^.*\\s+(?=(",end_txt="(\\s+)))",stringsAsFactors=FALSE)
#letter_signature_regex0e <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=5,beg_txt=".*\\s+",end_txt="\\s+.*",stringsAsFactors=FALSE)
letter_signature_regex0 <- rbindlist(list(letter_signature_regex0a,letter_signature_regex0b,letter_signature_regex0c,letter_signature_regex0d,letter_signature_regex0e))
letter_signature_regex0 <- as.data.frame(letter_signature_regex0,stringsAsFactors=FALSE)

letter_signature_regex1 <- regex_expand(regex_stubs=letter_signature_regex0,strs=letter_signature,
                                        strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")

letter_signature_regex <- merge(letter_signature_regex0, letter_signature_regex1, 
                                by.x=c("REGEX_PRIORITY"), by.y=c("REGEX_PRIORITY"), 
                                all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
colnames(letter_signature_regex)[match("SIGNATURES",names(letter_signature_regex))] <- "STRING"
letter_signature_regex[,"TYPE"] <- "SIG"
letter_signature_regex <- letter_signature_regex[,c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY",
                                                    colnames(letter_signature_regex[,!(colnames(letter_signature_regex) %in% c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY"))]))]

rm(letter_signature_regex0a,letter_signature_regex0b,letter_signature_regex0c,letter_signature_regex0d,letter_signature_regex0e)
rm(letter_signature_regex0,letter_signature_regex1)


#POSITION MATCHES
letter_position_regex0a <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=1,beg_txt="^\\s+(TITLE(:|-))\\s*",end_txt="(S*)\\+$",stringsAsFactors=FALSE)
letter_position_regex0b <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=2,beg_txt="^\\s+",end_txt="(S*)\\s+$",stringsAsFactors=FALSE)
letter_position_regex0c <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=3,beg_txt="^\\s+(?=(",end_txt="(S*)(\\s+)))",stringsAsFactors=FALSE)
#letter_position_regex0d <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=4,beg_txt="^.*\\s+(?=(",end_txt="(S*)(\\s+)))",stringsAsFactors=FALSE)
letter_position_regex0 <- rbindlist(list(letter_position_regex0a,letter_position_regex0b,letter_position_regex0c))
letter_position_regex0 <- as.data.frame(letter_position_regex0,stringsAsFactors=FALSE)

letter_position_regex1 <- regex_expand(regex_stubs=letter_position_regex0,strs=letter_position,
                                       strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")

letter_position_regex <- merge(letter_position_regex0, letter_position_regex1, 
                               by.x=c("REGEX_PRIORITY"), by.y=c("REGEX_PRIORITY"), 
                               all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
colnames(letter_position_regex)[match("POSITIONS",names(letter_position_regex))] <- "STRING"
letter_position_regex[,"TYPE"] <- "POS"
letter_position_regex <- letter_position_regex[,c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY",
                                                  colnames(letter_position_regex[,!(colnames(letter_position_regex) %in% c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY"))]))]

rm(letter_position_regex0a,letter_position_regex0b,letter_position_regex0c)
rm(letter_position_regex0,letter_position_regex1)


#CLOSING MATCHES
letter_closing_regex0a <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=1,beg_txt="^\\s+",end_txt="(\\s*)(,|:|-)\\s+$",stringsAsFactors=FALSE)
letter_closing_regex0b <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=2,beg_txt="^\\s+",end_txt="\\s+$",stringsAsFactors=FALSE)
letter_closing_regex0c <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=3,beg_txt="^\\s+(?=(",end_txt="(\\s+)))",stringsAsFactors=FALSE)
letter_closing_regex0d <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=4,beg_txt="^.*\\s+(?=(",end_txt="(\\s+)))",stringsAsFactors=FALSE)
#letter_closing_regex0e <- data.frame(TYPE=NA,OVERALL_PRIORITY=99,REGEX_PRIORITY=4,beg_txt="^.*\\s+(?=(",end_txt="(S*)(\\s+)))",stringsAsFactors=FALSE)
letter_closing_regex0 <- rbindlist(list(letter_closing_regex0a,letter_closing_regex0b,letter_closing_regex0c,letter_closing_regex0d))
letter_closing_regex0 <- as.data.frame(letter_closing_regex0,stringsAsFactors=FALSE)

letter_closing_regex1 <- regex_expand(regex_stubs=letter_closing_regex0,strs=letter_closing,
                                      strs_col="regex", priority_col="REGEX_PRIORITY",stub_beg_col="beg_txt",stub_end_col="end_txt")

letter_closing_regex <- merge(letter_closing_regex0, letter_closing_regex1, 
                              by.x=c("REGEX_PRIORITY"), by.y=c("REGEX_PRIORITY"), 
                              all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
colnames(letter_closing_regex)[match("CLOSINGS",names(letter_closing_regex))] <- "STRING"
letter_closing_regex[,"TYPE"] <- "CLS"
letter_closing_regex <- letter_closing_regex[,c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY",
                                                colnames(letter_closing_regex[,!(colnames(letter_closing_regex) %in% c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY"))]))]

rm(letter_closing_regex0a,letter_closing_regex0b,letter_closing_regex0c,letter_closing_regex0d)
rm(letter_closing_regex0,letter_closing_regex1)


###############################################################################
cat("Create Combined Regex Hash Table \n")
###############################################################################

#Add overall Priority for Letter Starting Phrases
letter_regex_start <- letter_beginning_regex

letter_regex_start[(letter_regex_start[,"TYPE"]=="BEG" & letter_regex_start[,"REGEX_PRIORITY"]==1),"OVERALL_PRIORITY"] <- 1
letter_regex_start[(letter_regex_start[,"TYPE"]=="BEG" & letter_regex_start[,"REGEX_PRIORITY"]==2),"OVERALL_PRIORITY"] <- 2

letter_regex_start <- letter_regex_start[order(letter_regex_start[,"OVERALL_PRIORITY"]),]

letter_regex_start_check <- unique(letter_regex_start[,c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","beg_txt","end_txt")]) 
row.names(letter_regex_start_check) <- seq(nrow(letter_regex_start_check))


#Add overall Priority for Letter Stopping Phrases
letter_regex_stop <- rbindlist(list(letter_ending_regex,letter_signature_regex,letter_position_regex,letter_closing_regex))
letter_regex_stop <- as.data.frame(letter_regex_stop,stringsAsFactors=FALSE)

letter_regex_stop[(letter_regex_stop[,"TYPE"]=="END" & letter_regex_stop[,"REGEX_PRIORITY"]==1),"OVERALL_PRIORITY"] <- 1
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="SIG" & letter_regex_stop[,"REGEX_PRIORITY"]==1),"OVERALL_PRIORITY"] <- 2
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="CLS" & letter_regex_stop[,"REGEX_PRIORITY"]==1),"OVERALL_PRIORITY"] <- 3
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="POS" & letter_regex_stop[,"REGEX_PRIORITY"]==1),"OVERALL_PRIORITY"] <- 4
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="END" & letter_regex_stop[,"REGEX_PRIORITY"]==2),"OVERALL_PRIORITY"] <- 5
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="SIG" & letter_regex_stop[,"REGEX_PRIORITY"]==2),"OVERALL_PRIORITY"] <- 6
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="CLS" & letter_regex_stop[,"REGEX_PRIORITY"]==2),"OVERALL_PRIORITY"] <- 7
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="POS" & letter_regex_stop[,"REGEX_PRIORITY"]==2),"OVERALL_PRIORITY"] <- 8
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="END" & letter_regex_stop[,"REGEX_PRIORITY"]==3),"OVERALL_PRIORITY"] <- 9
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="SIG" & letter_regex_stop[,"REGEX_PRIORITY"]==3),"OVERALL_PRIORITY"] <- 10
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="CLS" & letter_regex_stop[,"REGEX_PRIORITY"]==3),"OVERALL_PRIORITY"] <- 11
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="POS" & letter_regex_stop[,"REGEX_PRIORITY"]==3),"OVERALL_PRIORITY"] <- 12
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="END" & letter_regex_stop[,"REGEX_PRIORITY"]==4),"OVERALL_PRIORITY"] <- 13
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="SIG" & letter_regex_stop[,"REGEX_PRIORITY"]==4),"OVERALL_PRIORITY"] <- 14
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="CLS" & letter_regex_stop[,"REGEX_PRIORITY"]==4),"OVERALL_PRIORITY"] <- 15
letter_regex_stop[(letter_regex_stop[,"TYPE"]=="SIG" & letter_regex_stop[,"REGEX_PRIORITY"]==5),"OVERALL_PRIORITY"] <- 16

letter_regex_stop <- letter_regex_stop[order(letter_regex_stop[,"OVERALL_PRIORITY"]),]

letter_regex_stop_check <- unique(letter_regex_stop[,c("TYPE","OVERALL_PRIORITY","REGEX_PRIORITY","beg_txt","end_txt")]) 
row.names(letter_regex_stop_check) <- seq(nrow(letter_regex_stop_check))

rm(letter_regex_start_check,letter_regex_stop_check)
rm(letter_beginning_regex,letter_ending_regex,letter_signature_regex,letter_position_regex,letter_closing_regex)
rm(letter_beginning,letter_ending,letter_signature,letter_position,letter_closing)


###############################################################################
cat("Find Letters \n")
###############################################################################

letters_all <- dlply(.data=filings_trim2, .variables=c("yr"),  .fun = function(x, path_output,subfolder,letter_regex_start,letter_regex_stop){
  
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
  #  subfolder <- txtfolder_in
  #  subfolder_output <- txtfolder_out
  
  #  letter_regex_start <- letter_regex_start
  #  letter_regex_stop <- letter_regex_stop
  
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
  
  #downloaded_files2 <- downloaded_files2[order(downloaded_files2[,"filepath"]),]
  downloaded_files2 <- downloaded_files2[order(-downloaded_files2[,"size"]),]
  row.names(downloaded_files2) <- seq(nrow(downloaded_files2))
  
  downloaded_files3 <- data.frame(yr_id=NA,downloaded_files2,stringsAsFactors=FALSE)
  downloaded_files3[,"yr_id"] <- seq(1,nrow(downloaded_files3),1)
  
  rm(downloaded_files2)
  
  letters <- dlply(.data=downloaded_files3, .variables=c("yr_id"), .fun = function(y,letter_regex_start,letter_regex_stop){
    
    
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000072760-03-000038.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000088053-03-000790.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000721291-03-000011.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000820027-03-000786.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000949377-03-000778.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000950136-03-003115.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001017062-03-000375.csv"),]
    
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000930413-04-002211.csv"),]
    
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000950129-04-005427.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001193125-04-025954.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001193125-04-025975.csv"),]
    
    #  letter_regex_start <- letter_regex_start
    #  letter_regex_stop <- letter_regex_stop
    
    xmlcol <- "TEXT"
    xmltrim_col <- "TEXT_TRIM"
    
    file <- unique(y[,"file"])
    filepath <- unique(y[,"filepath"])
    
    #file_out <- gsub(".txt",".csv",file)
    #filepath_out <- paste(sub_folder_output_path,file_out,sep="\\")
    
    cat(file,"\n")
    
    filing <- read.table(file=filepath, header = TRUE, na.strings="",stringsAsFactors=FALSE, 
                         sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    
    filing_no_text0 <- filing[,!(colnames(filing) %in% c("text_id",xmlcol,xmltrim_col))]
    filing_no_text1 <- unique(filing_no_text0)
    #filing_no_text2 <- data.frame(file=NA,filing_no_text1,stringsAsFactors=FALSE)
    #filing_no_text2[,"file"] <- file
    filing_no_text2 <- filing_no_text1
    rm(filing_no_text0,filing_no_text1)
    
    filing_text0 <- filing[,(colnames(filing) %in% c("DOCUMENT_INDEX","text_id",xmlcol,xmltrim_col))]
    filing_text1 <- data.frame(file=NA,filing_text0,stringsAsFactors=FALSE)
    filing_text1[,"file"] <- file
    rm(filing_text0)
    
    rm(filing)
    
    
    #Clean Tags
    filing_text_clean <- filing_text1
    rm(filing_text1)
    
    for (i in 1:ncol(filing_text_clean))
    {
      filing_text_clean[,i] <- unknownToNA(filing_text_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                            NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                            NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
      filing_text_clean[,i] <- ifelse(is.na(filing_text_clean[,i]),"", filing_text_clean[,i])
    } 
    rm(i)
    
    
    filing_text_clean[,c(xmltrim_col)] <- paste(" ", filing_text_clean[,c(xmltrim_col)]," ",sep="")
    
    
    filing_text_clean[,c(xmltrim_col)] <- gsub(" {2,}", " ",filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("^\\s+|\\s+$", "", filing_text_clean[,c(xmltrim_col)])
    
    
    filing_text_clean[,c(xmltrim_col)] <- gsub("'", "", filing_text_clean[,c(xmltrim_col)]) 
    
    filing_text_clean[,c(xmltrim_col)] <- gsub("&", " AND ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("\\(S\\)", "S", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("-INITIAL-", " -INITIAL- ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("-INITIALS-", " -INITIALS- ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("-S-", " -S- ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("-SIGNATURE-", " -SIGNATURE- ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("-SIGNATURES-", " -SIGNATURES- ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("-SIGNED-", " -SIGNED- ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("/INITIAL/", " /INITIAL/ ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("/INITIALS/", " /INITIALS/ ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("/S/", " /S/ ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("/SIGNATURE/", " /SIGNATURE/ ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("/SIGNATURES/", " /SIGNATURES/ ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("/SIGNED/", " /SIGNED/ ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("INITIALS:", " INITIALS: ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("SIGNATURE:", " SIGNATURE: ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("SIGNATURES:", " SIGNATURES: ", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("SIGNED:", " SIGNED: ", filing_text_clean[,c(xmltrim_col)])
    
    filing_text_clean[,c(xmltrim_col)] <- gsub("BEST BUY", " BESTBUY ", filing_text_clean[,c(xmltrim_col)])
    
    
    #Pad punctuation
    
    #hash_pattern <- "([*=/.,:;!?()])"
    hash_pattern <- "([*=,:;!?()])"
    filing_text_clean[,c(xmltrim_col)] <- gsub(hash_pattern," \\1 ",filing_text_clean[,c(xmltrim_col)])
    #filing_text_clean[,c(xmltrim_col)] <- gsub("([[:punct:]])"," \\1 ",filing_text_clean[,c(xmltrim_col)])
    
    rm(hash_pattern)
    
    filing_text_clean[,c(xmltrim_col)] <- gsub("--", "-", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub(" -", "-", filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("- ", "-", filing_text_clean[,c(xmltrim_col)])
    #filing_text_clean[,c(xmltrim_col)] <- gsub("-", "", filing_text_clean[,c(xmltrim_col)])
    
    #filing_text_clean[,c(xmltrim_col)] <- gsub(",", "", filing_text_clean[,c(xmltrim_col)])
    #filing_text_clean[,c(xmltrim_col)] <- gsub(",", " ", filing_text_clean[,c(xmltrim_col)])
    
    #filing_text_clean[,c(xmltrim_col)] <- gsub(":", "", filing_text_clean[,c(xmltrim_col)])
    #filing_text_clean[,c(xmltrim_col)] <- gsub(":", " ", filing_text_clean[,c(xmltrim_col)])
    
    
    filing_text_clean[,c(xmltrim_col)] <- gsub(" {2,}", " ",filing_text_clean[,c(xmltrim_col)])
    filing_text_clean[,c(xmltrim_col)] <- gsub("^\\s+|\\s+$", "", filing_text_clean[,c(xmltrim_col)])
    
    filing_text_clean[,c(xmltrim_col)] <- paste(" ", filing_text_clean[,c(xmltrim_col)]," ",sep="")
    
    
    #ADD FLAGS TO DATA
    
    flags <- c("letter_beginning","letter_ending","letter_signature","letter_position","letter_closing")
    
    filing_text_id_cols <- c("file","DOCUMENT_INDEX","text_id",xmlcol,xmltrim_col)
    
    filing_text_letter0 <- data.frame(filing_text_clean, 
                                      matrix(NA, ncol=(length(flags)+2), nrow=nrow(filing_text_clean), 
                                             dimnames=list(c(), c(flags,"beg_cum_sum","end_cum_sum"))), 
                                      stringsAsFactors=FALSE)
    
    filing_text_letter0 <- filing_text_letter0[,c(filing_text_id_cols,
                                                  colnames(filing_text_letter0[,!(colnames(filing_text_letter0) %in% filing_text_id_cols)]))]
    
    rm(filing_text_clean)
    
    
    #FIND BEG MATCHES  
    
    letter_beginning_regex <- letter_regex_start[letter_regex_start[,"TYPE"]=="BEG",c("OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY","STRING","regex")]
    colnames(letter_beginning_regex)[match("STRING",names(letter_beginning_regex))] <- "BEGINNINGS"
    row.names(letter_beginning_regex) <- seq(nrow(letter_beginning_regex))
    
    letter_beginning_regex0 <- letter_regex_start[letter_regex_start[,"TYPE"]=="BEG",c("OVERALL_PRIORITY","REGEX_PRIORITY","beg_txt","end_txt")]
    letter_beginning_regex0 <- unique(letter_beginning_regex0)
    row.names(letter_beginning_regex0) <- seq(nrow(letter_beginning_regex0))
    
    filing_text_letter_beg_out <- regex_execute(data=filing_text_letter0,levels=nrow(letter_beginning_regex0),
                                                regex_data=letter_beginning_regex,regex_data_o_priority_col="OVERALL_PRIORITY",regex_data_r_priority_col="REGEX_PRIORITY",regex_data_str_col="regex",
                                                flags_all=flags,flag_local="letter_beginning",
                                                fileid_col="file",documentid_col="DOCUMENT_INDEX",txtid_col="text_id",cum_cols=c("beg_cum_sum","end_cum_sum"))
    
    filing_text_letter1 <- filing_text_letter_beg_out[[1]]
    filing_text_letter_matches_beg  <- filing_text_letter_beg_out[[2]]
    #i <- unique(filing_text_letter_matches_beg[,"regex_priority"])
    
    rm(letter_beginning_regex0,letter_beginning_regex,filing_text_letter0,filing_text_letter_beg_out)
    
    
    # CHECK TO SEE IF BEGINNING MATCHES FOUND
    
    if (nrow(filing_text_letter_matches_beg)==0) {
      
      cat("NO BEGINNING MATCHES FOUND", "\n")
      
      #CREATE MATCH SUMMARIES
      matches_summary0a_h <- data.frame(file=file,type="letter_beginning",
                                        final_overall_priority=unique(filing_text_letter_matches_beg[,"overall_priority"]),
                                        final_regex_priority=unique(filing_text_letter_matches_beg[,"regex_priority"]),
                                        overall_matches=nrow(filing_text_letter_matches_beg),
                                        unique_matches=length(unique(filing_text_letter_matches_beg[,c("text_id")])), stringsAsFactors=FALSE)
      matches_summary0b_h <- data.frame(file=file,type="letter_ending",final_overall_priority=NA,final_regex_priority=NA,overall_matches=NA, unique_matches=NA,stringsAsFactors=FALSE)
      matches_summary0c_h <- data.frame(file=file,type="letter_signature",final_overall_priority=NA,final_regex_priority=NA,overall_matches=NA, unique_matches=NA,stringsAsFactors=FALSE)
      matches_summary0d_h <- data.frame(file=file,type="letter_position",final_overall_priority=NA,final_regex_priority=NA,overall_matches=NA, unique_matches=NA,stringsAsFactors=FALSE)
      matches_summary0e_h <- data.frame(file=file,type="letter_closing",final_overall_priority=NA,final_regex_priority=NA,overall_matches=NA, unique_matches=NA,stringsAsFactors=FALSE)
      filing_text_letter_matches_summary_h <- rbindlist(list(matches_summary0a_h,matches_summary0b_h,matches_summary0c_h,matches_summary0d_h,matches_summary0e_h))
      filing_text_letter_matches_summary_h <- data.frame(filing_text_letter_matches_summary_h,stringsAsFactors=FALSE)
      rm(matches_summary0a_h,matches_summary0b_h,matches_summary0c_h,matches_summary0d_h,matches_summary0e_h)
      
      matches_summary0a_v <- data.frame(file=file,type="final_overall_priority",letter_beginning=unique(filing_text_letter_matches_beg[,"overall_priority"]),
                                        letter_ending=NA, letter_signature=NA, letter_position=NA,letter_closing=NA, stringsAsFactors=FALSE)
      matches_summary0b_v <- data.frame(file=file,type="final_regex_priority",letter_beginning=unique(filing_text_letter_matches_beg[,"regex_priority"]),
                                        letter_ending=NA, letter_signature=NA, letter_position=NA,letter_closing=NA, stringsAsFactors=FALSE)
      matches_summary0c_v <- data.frame(file=file,type="overall_matches", letter_beginning=nrow(filing_text_letter_matches_beg),
                                        letter_ending=NA,letter_signature=NA,letter_position=NA,letter_closing=NA, stringsAsFactors=FALSE)
      matches_summary0d_v <- data.frame(file=file,type="unique_matches",letter_beginning=length(unique(filing_text_letter_matches_beg[,c("text_id")])),
                                        letter_ending=NA, letter_signature=NA, letter_position=NA, letter_closing=NA,stringsAsFactors=FALSE)
      filing_text_letter_matches_summary_v <- rbindlist(list(matches_summary0a_v,matches_summary0b_v,matches_summary0c_v,matches_summary0d_v))
      filing_text_letter_matches_summary_v <- data.frame(filing_text_letter_matches_summary_v,stringsAsFactors=FALSE)
      rm(matches_summary0a_v,matches_summary0b_v,matches_summary0c_v,matches_summary0d_v)
      #rm(i)
      
      filing_text_letter_matches_end <- filing_text_letter_matches_beg
      colnames(filing_text_letter_matches_end)[match("letter_beginning",names(filing_text_letter_matches_end))] <- "letter_ending"
      
      filing_text_letter_matches_sign  <- filing_text_letter_matches_beg
      colnames(filing_text_letter_matches_sign)[match("letter_beginning",names(filing_text_letter_matches_sign))] <- "letter_signature"
      
      filing_text_letter_matches_pos <- filing_text_letter_matches_beg
      colnames(filing_text_letter_matches_pos)[match("letter_beginning",names(filing_text_letter_matches_pos))] <- "letter_position"
      
      filing_text_letter_matches_close  <- filing_text_letter_matches_beg
      colnames(filing_text_letter_matches_close)[match("letter_beginning",names(filing_text_letter_matches_close))] <- "letter_closing"
      
      filing_text_letter7 <- filing_text_letter1[1,]
      filing_text_letter7[,"beg_cum_sum"] <- 0
      filing_text_letter7[,xmlcol] <- NA
      filing_text_letter7[,xmltrim_col] <- NA
      
      rm(filing_text_letter1)
      
      
    } else {
      
      cat("BEGINNING MATCHES FOUND", "\n")
      
      
      # IF BEGINNING MATCHES FOUND, THEN CHECK FOR ENDINGS
      # TWO POSSIBILITTIES:
      #  1.  CHECK ALL 3 SO THAT YOU CAN APPLY MORE CRITERIA TO WHICH ENDING TO CHOOSE.  THIS WILL BE AS PERFORANCE HIT BUT SHOULD BE THE WAY TO DO IT BECAUSE IT ALLOWS FOR ME FLEXIBILITY
      #  2.  CHECK ONLY LETTER ENDINGS - IF NOT ENDINGS, CHECK SIGN, ETC.
      
      
      #FIND END MATCHES
      
      letter_ending_regex <- letter_regex_stop[letter_regex_stop[,"TYPE"]=="END",c("OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY","STRING","regex")]
      colnames(letter_ending_regex)[match("STRING",names(letter_ending_regex))] <- "ENDINGS"
      row.names(letter_ending_regex) <- seq(nrow(letter_ending_regex))
      
      letter_ending_regex0 <- letter_regex_stop[letter_regex_stop[,"TYPE"]=="END",c("OVERALL_PRIORITY","REGEX_PRIORITY","beg_txt","end_txt")]
      letter_ending_regex0 <- unique(letter_ending_regex0)
      row.names(letter_ending_regex0) <- seq(nrow(letter_ending_regex0))
      
      filing_text_letter_end_out <- regex_execute(data=filing_text_letter1,levels=nrow(letter_ending_regex0),
                                                  regex_data=letter_ending_regex,regex_data_o_priority_col="OVERALL_PRIORITY",regex_data_r_priority_col="REGEX_PRIORITY",regex_data_str_col="regex",
                                                  flags_all=flags,flag_local="letter_ending",
                                                  fileid_col="file",documentid_col="DOCUMENT_INDEX",txtid_col="text_id",cum_cols=c("beg_cum_sum","end_cum_sum"))
      
      filing_text_letter2 <- filing_text_letter_end_out[[1]]
      filing_text_letter_matches_end  <- filing_text_letter_end_out[[2]]
      #j <- unique(filing_text_letter_matches_end[,"regex_priority"])
      
      rm(letter_ending_regex0,letter_ending_regex,filing_text_letter1,filing_text_letter_end_out)
      
      
      #FIND SIGNATURE MATCHES
      
      letter_signature_regex <- letter_regex_stop[letter_regex_stop[,"TYPE"]=="SIG",c("OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY","STRING","regex")]
      colnames(letter_signature_regex)[match("STRING",names(letter_signature_regex))] <- "SIGNATURES"
      row.names(letter_signature_regex) <- seq(nrow(letter_signature_regex))
      
      letter_signature_regex0 <- letter_regex_stop[letter_regex_stop[,"TYPE"]=="SIG",c("OVERALL_PRIORITY","REGEX_PRIORITY","beg_txt","end_txt")]
      letter_signature_regex0 <- unique(letter_signature_regex0)
      row.names(letter_signature_regex0) <- seq(nrow(letter_signature_regex0))
      
      filing_text_letter_sign_out <- regex_execute(data=filing_text_letter2,levels=nrow(letter_signature_regex0),
                                                   regex_data=letter_signature_regex,regex_data_o_priority_col="OVERALL_PRIORITY",regex_data_r_priority_col="REGEX_PRIORITY",regex_data_str_col="regex",
                                                   flags_all=flags,flag_local="letter_signature",
                                                   fileid_col="file",documentid_col="DOCUMENT_INDEX",txtid_col="text_id",cum_cols=c("beg_cum_sum","end_cum_sum"))
      
      filing_text_letter3 <- filing_text_letter_sign_out[[1]]
      filing_text_letter_matches_sign  <- filing_text_letter_sign_out[[2]]
      #k <- unique(filing_text_letter_matches_sign[,"regex_priority"])
      
      rm(letter_signature_regex0,letter_signature_regex,filing_text_letter2,filing_text_letter_sign_out)
      
      
      #FIND POSITION MATCHES
      
      letter_position_regex <- letter_regex_stop[letter_regex_stop[,"TYPE"]=="POS",c("OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY","STRING","regex")]
      colnames(letter_position_regex)[match("STRING",names(letter_position_regex))] <- "POSITIONS"
      row.names(letter_position_regex) <- seq(nrow(letter_position_regex))
      
      letter_position_regex0 <- letter_regex_stop[letter_regex_stop[,"TYPE"]=="POS",c("OVERALL_PRIORITY","REGEX_PRIORITY","beg_txt","end_txt")]
      letter_position_regex0 <- unique(letter_position_regex0)
      row.names(letter_position_regex0) <- seq(nrow(letter_position_regex0))
      
      
      filing_text_letter_pos_out <- regex_execute(data=filing_text_letter3,levels=nrow(letter_position_regex0),
                                                  regex_data=letter_position_regex,regex_data_o_priority_col="OVERALL_PRIORITY",regex_data_r_priority_col="REGEX_PRIORITY",regex_data_str_col="regex",
                                                  flags_all=flags,flag_local="letter_position",
                                                  fileid_col="file",documentid_col="DOCUMENT_INDEX",txtid_col="text_id",cum_cols=c("beg_cum_sum","end_cum_sum"))
      
      filing_text_letter4 <- filing_text_letter_pos_out[[1]]
      filing_text_letter_matches_pos  <- filing_text_letter_pos_out[[2]]
      #l <- unique(filing_text_letter_matches_pos[,"regex_priority"])
      
      rm(letter_position_regex0,letter_position_regex,filing_text_letter3,filing_text_letter_pos_out)
      
      
      
      #FIND CLOSING MATCHES
      
      letter_closing_regex <- letter_regex_stop[letter_regex_stop[,"TYPE"]=="CLS",c("OVERALL_PRIORITY","REGEX_PRIORITY","PRIORITY","STRING","regex")]
      colnames(letter_closing_regex)[match("STRING",names(letter_closing_regex))] <- "CLOSINGS"
      row.names(letter_closing_regex) <- seq(nrow(letter_closing_regex))
      
      letter_closing_regex0 <- letter_regex_stop[letter_regex_stop[,"TYPE"]=="CLS",c("OVERALL_PRIORITY","REGEX_PRIORITY","beg_txt","end_txt")]
      letter_closing_regex0 <- unique(letter_closing_regex0)
      row.names(letter_closing_regex0) <- seq(nrow(letter_closing_regex0))
      
      filing_text_letter_close_out <- regex_execute(data=filing_text_letter4,levels=nrow(letter_closing_regex0),
                                                    regex_data=letter_closing_regex,regex_data_o_priority_col="OVERALL_PRIORITY",regex_data_r_priority_col="REGEX_PRIORITY",regex_data_str_col="regex",
                                                    flags_all=flags,flag_local="letter_closing",
                                                    fileid_col="file",documentid_col="DOCUMENT_INDEX",txtid_col="text_id",cum_cols=c("beg_cum_sum","end_cum_sum"))
      
      filing_text_letter5 <- filing_text_letter_close_out[[1]]
      filing_text_letter_matches_close  <- filing_text_letter_close_out[[2]]
      #m <- unique(filing_text_letter_matches_close[,"regex_priority"])
      
      rm(letter_closing_regex0,letter_closing_regex,filing_text_letter4,filing_text_letter_close_out)
      
      
      #CREATE MATCH SUMMARIES
      matches_summary0a_h <- data.frame(file=file,type="letter_beginning",
                                        final_overall_priority=unique(filing_text_letter_matches_beg[,"overall_priority"]),
                                        final_regex_priority=unique(filing_text_letter_matches_beg[,"regex_priority"]),
                                        overall_matches=nrow(filing_text_letter_matches_beg),
                                        unique_matches=length(unique(filing_text_letter_matches_beg[,c("text_id")])),
                                        stringsAsFactors=FALSE)
      matches_summary0b_h <- data.frame(file=file,type="letter_ending",
                                        final_overall_priority=unique(filing_text_letter_matches_end[,"overall_priority"]),
                                        final_regex_priority=unique(filing_text_letter_matches_end[,"regex_priority"]),
                                        overall_matches=nrow(filing_text_letter_matches_end),
                                        unique_matches=length(unique(filing_text_letter_matches_end[,c("text_id")])),
                                        stringsAsFactors=FALSE)
      matches_summary0c_h <- data.frame(file=file,type="letter_signature",
                                        final_overall_priority=unique(filing_text_letter_matches_sign[,"overall_priority"]),
                                        final_regex_priority=unique(filing_text_letter_matches_sign[,"regex_priority"]),
                                        overall_matches=nrow(filing_text_letter_matches_sign),
                                        unique_matches=length(unique(filing_text_letter_matches_sign[,c("text_id")])),
                                        stringsAsFactors=FALSE)
      matches_summary0d_h <- data.frame(file=file,type="letter_position",
                                        final_overall_priority=unique(filing_text_letter_matches_pos[,"overall_priority"]),
                                        final_regex_priority=unique(filing_text_letter_matches_pos[,"regex_priority"]),
                                        overall_matches=nrow(filing_text_letter_matches_pos),
                                        unique_matches=length(unique(filing_text_letter_matches_pos[,c("text_id")])),
                                        stringsAsFactors=FALSE)
      matches_summary0e_h <- data.frame(file=file,type="letter_closing",
                                        final_overall_priority=unique(filing_text_letter_matches_close[,"overall_priority"]),
                                        final_regex_priority=unique(filing_text_letter_matches_close[,"regex_priority"]),
                                        overall_matches=nrow(filing_text_letter_matches_close),
                                        unique_matches=length(unique(filing_text_letter_matches_close[,c("text_id")])),
                                        stringsAsFactors=FALSE)
      filing_text_letter_matches_summary_h <- rbindlist(list(matches_summary0a_h,matches_summary0b_h,matches_summary0c_h,matches_summary0d_h,matches_summary0e_h))
      filing_text_letter_matches_summary_h <- data.frame(filing_text_letter_matches_summary_h,stringsAsFactors=FALSE)
      rm(matches_summary0a_h,matches_summary0b_h,matches_summary0c_h,matches_summary0d_h,matches_summary0e_h)
      
      matches_summary0a_v <- data.frame(file=file,type="final_overall_priority",
                                        letter_beginning=unique(filing_text_letter_matches_beg[,"overall_priority"]),
                                        letter_ending=unique(filing_text_letter_matches_end[,"overall_priority"]),
                                        letter_signature=unique(filing_text_letter_matches_sign[,"overall_priority"]),
                                        letter_position=unique(filing_text_letter_matches_pos[,"overall_priority"]),
                                        letter_closing=unique(filing_text_letter_matches_close[,"overall_priority"]),
                                        stringsAsFactors=FALSE)
      matches_summary0b_v <- data.frame(file=file,type="final_regex_priority",
                                        letter_beginning=unique(filing_text_letter_matches_beg[,"regex_priority"]),
                                        letter_ending=unique(filing_text_letter_matches_end[,"regex_priority"]),
                                        letter_signature=unique(filing_text_letter_matches_sign[,"regex_priority"]),
                                        letter_position=unique(filing_text_letter_matches_pos[,"regex_priority"]),
                                        letter_closing=unique(filing_text_letter_matches_close[,"regex_priority"]),
                                        stringsAsFactors=FALSE)
      matches_summary0c_v <- data.frame(file=file,type="overall_matches",
                                        letter_beginning=nrow(filing_text_letter_matches_beg),
                                        letter_ending=nrow(filing_text_letter_matches_end),
                                        letter_signature=nrow(filing_text_letter_matches_sign),
                                        letter_position=nrow(filing_text_letter_matches_pos),
                                        letter_closing=nrow(filing_text_letter_matches_close),
                                        stringsAsFactors=FALSE)
      matches_summary0d_v <- data.frame(file=file,type="unique_matches",
                                        letter_beginning=length(unique(filing_text_letter_matches_beg[,c("text_id")])),
                                        letter_ending=length(unique(filing_text_letter_matches_end[,c("text_id")])),
                                        letter_signature=length(unique(filing_text_letter_matches_sign[,c("text_id")])),
                                        letter_position=length(unique(filing_text_letter_matches_pos[,c("text_id")])),
                                        letter_closing=length(unique(filing_text_letter_matches_close[,c("text_id")])),
                                        stringsAsFactors=FALSE)
      filing_text_letter_matches_summary_v <- rbindlist(list(matches_summary0a_v,matches_summary0b_v,matches_summary0c_v,matches_summary0d_v))
      filing_text_letter_matches_summary_v <- data.frame(filing_text_letter_matches_summary_v,stringsAsFactors=FALSE)
      rm(matches_summary0a_v,matches_summary0b_v,matches_summary0c_v,matches_summary0d_v)
      #rm(i,j,k,l,m)
      
      
      #REMOVE TEXT BEFORE LETTER
      filing_text_letter6 <- ddply(.data=filing_text_letter5, .variables=c("file","DOCUMENT_INDEX"), .fun = function(x){
        
        #  x <- filing_text_letter5[(filing_text_letter5[,"DOCUMENT_INDEX"]==1),]
        
        x[,"beg_cum_sum"] <- cumsum(x[,"letter_beginning"])
        x_trim <- x[!(x[,"beg_cum_sum"]==0),]
        
        #if (nrow(x_trim) == 0) {
        #  x_trim[1,] <- NA
        #  x_trim[,"file"] <- file_temp
        #  x_trim[,"DOCUMENT_INDEX"] <- index_temp 
        #} 
        
        return(x_trim)
      })
      rm(filing_text_letter5)
      
      
      #REMOVE TEXT AFTER LETTER
      
      #  THE CRITERIA NEEDS TO BE NOT IF END !=0.
      #  WHAT ABOUT THE MINIMUM OF THE THREE????
      #  WHAT ABOUT GETTING THE DIFFERENCE IN THE THREE ROWS: ABS(END-SIGN) & ABS(SIGN-POS).  IF END IS MUCH HIGHER ROW THAN SIGNATURE, USE SIGNATURE
      
      filing_text_letter7 <- ddply(.data=filing_text_letter6, .variables=c("file","DOCUMENT_INDEX","beg_cum_sum"),      .fun = function(x,bycol,xmlcol,summary){
        
        # x <- filing_text_letter6[(filing_text_letter6[,"DOCUMENT_INDEX"]==1 & filing_text_letter6[,"beg_cum_sum"]==1),]
        # x <- filing_text_letter6[(filing_text_letter6[,"DOCUMENT_INDEX"]==1 & filing_text_letter6[,"beg_cum_sum"]==2),]
        # bycol <- c("file","DOCUMENT_INDEX","beg_cum_sum")
        # xmlcol <- xmlcol
        # summary <- filing_text_letter_matches_summary_h
        
        file_temp <- unique(x[,"file"])
        index_temp <- unique(x[,"DOCUMENT_INDEX"])
        beg_cum_sum_temp <- unique(x[,"beg_cum_sum"])
        
        filing_text_letter_extract0 <- x
        
        ending_summary_temp <- data.frame(matrix(NA, ncol=7, nrow=4, dimnames=list(c(), c("file","DOCUMENT_INDEX","beg_cum_sum","TYPE","final_overall_priority","final_regex_priority","local_matches"))), stringsAsFactors=FALSE)
        ending_summary_temp[1,] <- c(file_temp,index_temp,beg_cum_sum_temp,"letter_ending",
                                     summary[summary[,"type"]=="letter_ending","final_overall_priority"],
                                     summary[summary[,"type"]=="letter_ending","final_regex_priority"],
                                     sum(filing_text_letter_extract0[,"letter_ending"], na.rm = TRUE))
        ending_summary_temp[2,] <- c(file_temp,index_temp,beg_cum_sum_temp,"letter_signature",
                                     summary[summary[,"type"]=="letter_signature","final_overall_priority"],
                                     summary[summary[,"type"]=="letter_signature","final_regex_priority"],
                                     sum(filing_text_letter_extract0[,"letter_signature"], na.rm = TRUE))
        ending_summary_temp[3,] <- c(file_temp,index_temp,beg_cum_sum_temp,"letter_closing",
                                     summary[summary[,"type"]=="letter_closing","final_overall_priority"],
                                     summary[summary[,"type"]=="letter_closing","final_regex_priority"],
                                     sum(filing_text_letter_extract0[,"letter_closing"], na.rm = TRUE))
        ending_summary_temp[4,] <- c(file_temp,index_temp,beg_cum_sum_temp,"letter_position",
                                     summary[summary[,"type"]=="letter_position","final_overall_priority"],
                                     summary[summary[,"type"]=="letter_position","final_regex_priority"],
                                     sum(filing_text_letter_extract0[,"letter_position"], na.rm = TRUE))
                
        ending_summary_temp[,"DOCUMENT_INDEX"] <- as.numeric(ending_summary_temp[,"DOCUMENT_INDEX"])
        ending_summary_temp[,"beg_cum_sum"] <- as.numeric(ending_summary_temp[,"beg_cum_sum"])
        ending_summary_temp[,"final_overall_priority"] <- as.numeric(ending_summary_temp[,"final_overall_priority"])
        ending_summary_temp[,"final_regex_priority"] <- as.numeric(ending_summary_temp[,"final_regex_priority"])
        ending_summary_temp[,"local_matches"] <- as.numeric(ending_summary_temp[,"local_matches"])
        
        #ending_summary_temp <- ending_summary_temp[order(ending_summary_temp[,"final_regex_priority"]),]
        ending_summary_temp <- ending_summary_temp[order(ending_summary_temp[,"final_overall_priority"]),]
        row.names(ending_summary_temp) <- seq(nrow(ending_summary_temp))
        
        for (z in 1:nrow(ending_summary_temp))
        {
          #  z <- 1
          
          #cat("CLOSING PRIORITY:",z,"\n")
          
          matches <- as.integer(ending_summary_temp[z,"local_matches"])
          
          filing_text_letter_extract0[,"end_cum_sum"] <- cumsum(filing_text_letter_extract0[,ending_summary_temp[z,"TYPE"]])
          letter_flag_good <- matches
          
          if (matches!=0) { rm(matches) ; break }
          if (z==nrow(ending_summary_temp)) { 
            
            filing_text_letter_extract0[,"end_cum_sum"] <- NA
            letter_flag_good <- NA
            rm(matches)
          } 
          
        }
        rm(ending_summary_temp,z)
        
        #Check for no ending matches
        if (is.na(letter_flag_good)) {
          
          #cat("NO END MATCHES FOUND", "\n")
          
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
        
      }, bycol=c("file","DOCUMENT_INDEX","beg_cum_sum"),xmlcol=xmlcol,summary=filing_text_letter_matches_summary_h,
      .progress = "none", .inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)
      rm(filing_text_letter6)
      
      
    }

    #filing_text_letter8 <- filing_text_letter7[,(colnames(filing_text_letter7) %in% c("file","DOCUMENT_INDEX","beg_cum_sum",xmlcol,xmltrim_col))]
    filing_text_letter8 <- filing_text_letter7[,c("file","DOCUMENT_INDEX","beg_cum_sum",xmlcol,xmltrim_col)]
    colnames(filing_text_letter8)[match("beg_cum_sum",names(filing_text_letter8))] <- "LETTER_INDEX"
    
    rm(filing_text_letter7)
    
    
    #CREATE FINAL DATA AND OUTPUT
    filing_text_comb <- merge(filing_no_text2, filing_text_letter8, 
                              by.x=c("file","DOCUMENT_INDEX"), by.y=c("file","DOCUMENT_INDEX"), 
                              all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
    
    rm(filing_no_text2,filing_text_letter8)
    
    #filing_text_letter_matches_summary <- filing_text_letter_matches_summary_h
    filing_text_letter_matches_summary <- filing_text_letter_matches_summary_v
    
    rm(filing_text_letter_matches_summary_h,filing_text_letter_matches_summary_v)
    
    #df_comb_list <- list(filing_text_comb,filing_text_letter_matches_summary,
    #                     filing_text_letter_matches_beg,filing_text_letter_matches_end,filing_text_letter_matches_sign,
    #                     filing_text_letter_matches_pos,filing_text_letter_matches_close)
    
    #rm(filing_text_comb,filing_text_letter_matches_summary)
    #rm(filing_text_letter_matches_beg,filing_text_letter_matches_end,filing_text_letter_matches_sign)
    #rm(filing_text_letter_matches_pos,filing_text_letter_matches_close)
    
    rm(xmlcol,xmltrim_col)
    rm(flags,file,filepath)
    #rm(file_out,filepath_out)
    
    return(list(filing_text_comb,filing_text_letter_matches_summary,
                filing_text_letter_matches_beg,filing_text_letter_matches_end,filing_text_letter_matches_sign,
                filing_text_letter_matches_pos,filing_text_letter_matches_close))
    
  },
  letter_regex_start=letter_regex_start,letter_regex_stop=letter_regex_stop,
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
  
  letter_matches_sign0 <- sapply(letters, "[", 5)
  letter_matches_sign1 <- rbindlist(letter_matches_sign0,fill=TRUE,use.names=TRUE)
  letter_matches_sign <- data.frame(yr=NA,letter_matches_sign1,stringsAsFactors=FALSE) 
  letter_matches_sign[,"yr"] <- yr
  rm(letter_matches_sign0,letter_matches_sign1)
  
  letter_matches_pos0 <- sapply(letters, "[", 6)
  letter_matches_pos1 <- rbindlist(letter_matches_pos0,fill=TRUE,use.names=TRUE)
  letter_matches_pos <- data.frame(yr=NA,letter_matches_pos1,stringsAsFactors=FALSE) 
  letter_matches_pos[,"yr"] <- yr
  rm(letter_matches_pos0,letter_matches_pos1)
  
  letter_matches_close0 <- sapply(letters, "[", 7)
  letter_matches_close1 <- rbindlist(letter_matches_close0,fill=TRUE,use.names=TRUE)
  letter_matches_close <- data.frame(yr=NA,letter_matches_close1,stringsAsFactors=FALSE) 
  letter_matches_close[,"yr"] <- yr
  rm(letter_matches_close0,letter_matches_close1)
  
  
  df_comb_list_all <- list(letters_comb,letter_matches_summary,
                           letter_matches_beg,letter_matches_end,letter_matches_sign,
                           letter_matches_pos,letter_matches_close)
  
  rm(letters,letters_comb,letter_matches_summary)
  rm(letter_matches_beg,letter_matches_end,letter_matches_sign,letter_matches_pos,letter_matches_close)
  rm(yr,yr_folder_path,sub_folder_path,downloaded_files3)
  
  return(df_comb_list_all)
  
},
path_output=paste(output_directory,downloadfolder,sep=slash),subfolder=txtfolder_in,
letter_regex_start=letter_regex_start,letter_regex_stop=letter_regex_stop,
.progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


###############################################################################
cat("Seperate Data \n")
###############################################################################

letter_all_comb0 <- sapply(letters_all, "[", 1)
letter_all_comb <- rbindlist(letter_all_comb0,fill=TRUE,use.names=TRUE)

letter_all_matches_summary0 <- sapply(letters_all, "[", 2)
letter_all_matches_summary <- rbindlist(letter_all_matches_summary0,fill=TRUE,use.names=TRUE)

letter_all_matches_beg0 <- sapply(letters_all, "[", 3)
letter_all_matches_beg <- rbindlist(letter_all_matches_beg0,fill=TRUE,use.names=TRUE)

letter_all_matches_end0 <- sapply(letters_all, "[", 4)
letter_all_matches_end <- rbindlist(letter_all_matches_end0,fill=TRUE,use.names=TRUE)

letter_all_matches_sign0 <- sapply(letters_all, "[", 5)
letter_all_matches_sign <- rbindlist(letter_all_matches_sign0,fill=TRUE,use.names=TRUE)

letter_all_matches_pos0 <- sapply(letters_all, "[", 6)
letter_all_matches_pos <- rbindlist(letter_all_matches_pos0,fill=TRUE,use.names=TRUE)

letter_all_matches_close0 <- sapply(letters_all, "[", 7)
letter_all_matches_close <- rbindlist(letter_all_matches_close0,fill=TRUE,use.names=TRUE)

#rm(letter_all_comb0,letter_all_matches_summary0)
#rm(letter_all_matches_beg0,letter_all_matches_end0,letter_all_matches_sign0,letter_all_matches_pos0,letter_all_matches_close0)


###############################################################################
cat("Output Combined Files \n")
###############################################################################

#Check to see if yr folder exists.  If not, create it.
out_folder_path <- paste(download_folder_path, txtfolder_out, sep = "\\", collapse = "\\")   
create_directory(out_folder_path,remove=1)

write.table(letter_all_comb,file=paste(out_folder_path,"\\","letter_all_comb",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_summary,file=paste(out_folder_path,"\\","letter_all_matches_summary",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_beg,file=paste(out_folder_path,"\\","letter_all_matches_beg",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_end,file=paste(out_folder_path,"\\","letter_all_matches_end",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_sign,file=paste(out_folder_path,"\\","letter_all_matches_sign",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_pos,file=paste(out_folder_path,"\\","letter_all_matches_pos",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_all_matches_close,file=paste(out_folder_path,"\\","letter_all_matches_close",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

