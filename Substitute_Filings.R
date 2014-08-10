# TODO: Add comment
# 
# Author:  Brad
# File:    Substitute_Filings.R
# Version: 1.0
# Date:    07.16.2014
# Purpose: Substitute common words, remove contractions, etc.
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

# regex_expand <- function(regex_stubs,strs,strs_col,priority_col,stub_beg_col,stub_end_col) {
#   
#   #  regex_stubs <- letter_beginning_regex0
#   #  strs <- letter_beginning
#   #  strs_col <- "regex"
#   #  priority_col <- "priority"
#   #  stub_beg_col <- "beg_txt"
#   #  stub_end_col <- "end_txt"
#   
#   regex_expand0 <- ddply(.data=regex_stubs, .variables=priority_col, .fun = function(x,strs){
#     
#     #  x <- regex_stubs[1,]
#     strs[,strs_col] <- paste(x[,stub_beg_col], strs[,strs_col] , x[,stub_end_col], sep="")
#     return(strs)
#   },strs=strs)
#   
#   return(regex_expand0)
# }
# 
# regex_section_matches_collapse <- function(matches_expand,dv_col,txtid_col) {
#   
#   #  matches_expand <- filing_text_letter_id1
#   #  dv_col <- "letter_beginning"
#   #  txtid_col <- "text_id"
#   
#   matches_expand[,dv_col] <- ifelse(is.na(matches_expand[,c(dv_col)]),FALSE,TRUE)
#   filing_text_letter1 <- ddply(.data=matches_expand, .variables=txtid_col, .fun = function(x,dv_col){
#     
#     x[,dv_col] <- any(x[,dv_col])
#     return(x)
#     
#   },dv_col=dv_col)
#   filing_text_letter1[,dv_col] <- ifelse(filing_text_letter1[,c(dv_col)]==TRUE,1,0)
#   
#   filing_text_letter1_dt <- data.table(filing_text_letter1)
#   filing_text_letter1<- unique(filing_text_letter1_dt,use.key=FALSE)
#   filing_text_letter1 <- as.data.frame(filing_text_letter1,stringsAsFactors=FALSE)
#   
#   rm(filing_text_letter1_dt)
#   invisible(gc(verbose = FALSE, reset = TRUE))
#   
#   return(filing_text_letter1)
# }
# 
# regex_section_matches_collapse_old <- function(matches_expand,dv_col,txtid_col) {
#   
#   #  matches_expand <- filing_text_letter_id1
#   #  dv_col <- "letter_beginning"
#   #  txtid_col <- "text_id"
#   
#   filing_text_letter1 <- ddply(.data=matches_expand, .variables=txtid_col, .fun = function(x,dv_col){
#     
#     x[,dv_col] <- ifelse(is.na(x[,c(dv_col)]),FALSE,TRUE)
#     x[,dv_col] <- any(x[,dv_col])
#     x[,dv_col] <- ifelse(x[,c(dv_col)]==TRUE,1,0)
#     return(unique(x))
#     
#   },dv_col=dv_col)
#   return(filing_text_letter1)
# }
# 
# regex_section_matches_expand <- function(regex_strs,data,dv_col,txt_col) {
#   
#   #  regex_strs <- letter_beginning_regex[letter_beginning_regex[,"REGEX_PRIORITY"]==i,"regex"]
#   #  data <- filing_text_letter0
#   #  dv_col <- "letter_beginning"
#   #  txt_col <- xmltrim_col
#   
#   #ptm1 <- proc.time()
#   filing_text_letter_id1 <- ldply(.data=regex_strs, .fun = function(x,data,dv_col,txt_col){
#     
#     #  x <- regex_strs[[1]]
#     
#     data[,dv_col] <- ifelse(grepl(x, data[,c(txt_col)],ignore.case = TRUE, perl = TRUE), x, NA)
#     
#     #data_dt <- data.table(data)
#     #data <- unique(data_dt,use.key=FALSE)
#     #data <- as.data.frame(data,stringsAsFactors=FALSE)
#     
#     return(data)
#     
#   }, data=data, dv_col=dv_col, txt_col=txt_col)
#   
#   filing_text_letter_id1_dt <- data.table(filing_text_letter_id1)
#   filing_text_letter_id1 <- unique(filing_text_letter_id1_dt,use.key=FALSE)
#   filing_text_letter_id1 <- as.data.frame(filing_text_letter_id1,stringsAsFactors=FALSE)
#   
#   rm(filing_text_letter_id1_dt)
#   invisible(gc(verbose = FALSE, reset = TRUE))
#   #proc.time() - ptm1
#   
#   return(filing_text_letter_id1)
# }
# 
# regex_section_matches_expand_old <- function(regex_strs,data,dv_col,txt_col) {
#   
#   #  regex_strs <- letter_beginning_regex[letter_beginning_regex[,"REGEX_PRIORITY"]==i,"regex"]
#   #  data <- filing_text_letter0
#   #  dv_col <- "letter_beginning"
#   #  txt_col <- xmltrim_col
#   
#   filing_text_letter_id1 <- ldply(.data=regex_strs, .fun = function(x,data,dv_col,txt_col){
#     
#     #  x <- regex_strs[[1]]
#     
#     data[,dv_col] <- ifelse(grepl(x, data[,c(txt_col)],ignore.case = TRUE, perl = TRUE), x, NA)
#     return(data)
#     
#   }, data=data, dv_col=dv_col, txt_col=txt_col)
#   return(filing_text_letter_id1)
# }


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","gdata","qdap","plyr","stringr","XML")
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
txtfolder_in <- "txt_clean"

#The sub directory where the output filings will go
txtfolder_out <- "txt_substitute"

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

#entity_encoding0 <- read.csv(file=paste(input_directory,"Entity_encoding.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
entity_encoding0 <- read.table(file=paste(input_directory,"Entity_encoding.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
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
cat("Import Hash Table \n")
###############################################################################

hash_table <- read.table(file=paste(input_directory,"\\","hash_table_final_small",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
#hash_table <- read.table(file=paste(input_directory,"\\","hash_table_final_large",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")


###############################################################################
cat("Substitute Text \n")
###############################################################################

substitute_all <- dlply(.data=filings_trim2, .variables=c("yr"), 
                        .fun = function(x, path_output,subfolder,subfolder_output,entity_encoding,hash_table){
                          
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
                          
                          #  entity_encoding <- entity_encoding
                          #  hash_table <- hash_table
                          
                          filings_trim2_short <- x[,!(colnames(x) %in% c("file_header","file_index_htm"))]
                          
                          yr <-  unique(x[,"yr"])
                          
                          cat("\n",yr,"\n")
                          
                          #Check to see if yr folder exists.  If not, create it.
                          yr_folder_path <- paste(path_output, yr, sep = "\\", collapse = "\\")   
                          create_directory(yr_folder_path,remove=1)
                          
                          sub_folder_path <- paste(yr_folder_path, subfolder, sep = "\\", collapse = "\\")   
                          create_directory(sub_folder_path,remove=1)
                          
                          sub_folder_output_path <- paste(yr_folder_path, subfolder_output, sep = "\\", collapse = "\\")   
                          create_directory(sub_folder_output_path,remove=1)
                          
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
                          
                          #Get name of all files already done
                          completed_files <- data.frame(file=list.files(sub_folder_output_path),stringsAsFactors=FALSE)
                          
                          downloaded_files4 <- data.frame(downloaded_files3,already_done=NA,stringsAsFactors=FALSE)
                          
                          
                          downloaded_files4[,"already_done"] <- ifelse(downloaded_files4[,"file"] %in% completed_files[,"file"], 1, 0)
                          
                          rm(downloaded_files3,completed_files)
                          
                          downloaded_files5 <- downloaded_files4[(downloaded_files4[,"already_done"]==0) ,]
                          
                          rm(downloaded_files4)
                          
                          substitute <- dlply(.data=downloaded_files5, .variables=c("yr_id"), 
                                              .fun = function(y,sub_folder_output_path,entity_encoding,hash_table){
                                                
                                                
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000072760-03-000038.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000088053-03-000790.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000721291-03-000011.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000820027-03-000786.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000949377-03-000778.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000950136-03-003115.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001017062-03-000375.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000811860-04-000012.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001193125-04-025975.csv"),]
                                                #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000766285-08-000036.csv"),]
                                                
                                                #  sub_folder_output_path <- sub_folder_output_path
                                                #  entity_encoding <- entity_encoding
                                                #  hash_table <- hash_table
                                                
                                                xmlcol <- "TEXT"
                                                xmltrim_col <- "TEXT_TRIM"
                                                
                                                file <- unique(y[,"file"])
                                                filepath <- unique(y[,"filepath"])
                                                
                                                file_out <- gsub(".txt",".csv",file)
                                                filepath_out <- paste(sub_folder_output_path,file_out,sep="\\")
                                                
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
                                                
                                                filing_text_sub <- filing_text_decode
                                                
                                                rm(filing_text_decode)
                                                
                                                
                                                
                                                #COLLAPSE AT DOUBLE /N's
                                                filing_text_collapse <- filing_text_sub
                                                rm(filing_text_sub)
                                                
                                                
                                                #CREATE CLEAN DATA
                                                filing_text_letter0_temp <- data.frame(filing_text_collapse,text_id=NA,text_trim=NA,NAstringsAsFactors=FALSE)
                                                colnames(filing_text_letter0_temp)[match("text_trim",names(filing_text_letter0_temp))] <- xmltrim_col
                                                #colnames(filing_text_letter0_temp)[match("index_temp_overall",names(filing_text_letter0_temp))] <- "LETTER_INDEX"
                                                
                                                filing_text_id_cols <- c("file","DOCUMENT_INDEX","text_id",xmlcol,xmltrim_col)
                                                filing_text_letter0_temp <- filing_text_letter0_temp[,c(filing_text_id_cols,
                                                                                                        colnames(filing_text_letter0_temp[,!(colnames(filing_text_letter0_temp) %in% filing_text_id_cols)]))]
                                                
                                                filing_text_letter0_temp[,c("text_id")] <-  seq(1,nrow(filing_text_letter0_temp),1)
                                                filing_text_letter0_temp[,c(xmltrim_col)] <-  filing_text_letter0_temp[,c(xmlcol)] 
                                                
                                                
                                                #CONVERT TO ASCII ENCODING
                                                filing_text_letter0_temp[,c(xmltrim_col)] <- iconv(filing_text_letter0_temp[,c(xmltrim_col)], "latin1", "ASCII", sub=" ")
                                                
                                                
                                                #REPLACE BRACKETS
                                                filing_text_letter0_temp[,c(xmltrim_col)] <- bracketX(clean(text.var=filing_text_letter0_temp[,c(xmltrim_col)]), bracket = "all", missing = "", names = FALSE)
                                                
                                                
                                                #REPLACE CONTRACTIONS
                                                #filing_text_letter0_temp[,c(xmltrim_col)] <-  replace_contraction(text.var=filing_text_letter0_temp[,c(xmltrim_col)], contraction = qdapDictionaries::contractions,
                                                #                                                                  replace = NULL, ignore.case = TRUE, sent.cap = TRUE)        
                                                
                                                #FIX NUMBERS THAT ARE TOO LONG FOR replace_number FUNCTION
                                                cutoff_len <- 19
                                                pattern <- paste("\\d{",cutoff_len+1,",}",sep="")
                                                matches <- gregexpr(pattern, filing_text_letter0_temp[,c(xmltrim_col)])
                                                
                                                replace_with_spaces <-lapply(regmatches(filing_text_letter0_temp[,c(xmltrim_col)], matches), function(x,cutoff) {
                                                  
                                                  mapply(function(x, n, cutoff){formatC(substr(x,1,cutoff), width=-n) }, x=x, n=nchar(x), MoreArgs=list(cutoff=cutoff))
                                                  
                                                },cutoff=cutoff_len)
                                                
                                                #combine with original values
                                                filing_text_letter0_temp[,c(xmltrim_col)]<- unlist(Map(function(a,b) paste0(a,c(b,""), collapse=""), 
                                                                                                       regmatches(filing_text_letter0_temp[,c(xmltrim_col)], matches, invert=T), replace_with_spaces))
                                                
                                                rm(cutoff_len,pattern,matches,replace_with_spaces)
                                                
                                                #REPLACE NUMBER
                                                filing_text_letter0_temp[,c(xmltrim_col)] <- replace_number(text.var=filing_text_letter0_temp[,c(xmltrim_col)], num.paste = TRUE)
                                                
                                                
                                                #REMOVE SYMBOLS
                                                filing_text_letter0_temp[,c(xmltrim_col)] <- replace_symbol(text.var=filing_text_letter0_temp[,c(xmltrim_col)], dollar = TRUE, percent = TRUE, pound = TRUE,at = TRUE, and = TRUE, with = TRUE)
                                                
                                                
                                                #REPLACE ABBREVIATIONS
                                                #filing_text_letter0_temp[,c(xmltrim_col)] <- replace_abbreviation(text.var=filing_text_letter0_temp[,c(xmltrim_col)], abbreviation = hash_table, 
                                                #                                                                  replace = NULL, ignore.case = TRUE)

                                              
                                                #hash_pattern <- "[/().%]"
                                                #hash_pattern <- "/"
                                                
                                                filing_text_letter0_temp_dt <-filing_text_letter0_temp
                                                filing_text_letter0_temp_dt[,c(xmltrim_col)] <- paste(" ",filing_text_letter0_temp_dt[,c(xmltrim_col)]," ",sep="")
                                                #filing_text_letter0_temp_dt[,c(xmltrim_col)] <- gsub(hash_pattern," \\1 ",filing_text_letter0_temp_dt[,c(xmltrim_col)])
                                                filing_text_letter0_temp_dt <- data.table(filing_text_letter0_temp_dt)
                                                
                                                #rm(letter_beginning0,letter_ending0,letter_position0,letter_signature0,letter_closing0)
                                                
                                                for(k in 1:nrow(hash_table))
                                                {
                                                  # k <- 1
                                                  # k <- 254
                                                  # k <- 10038
                                                  
                                                set(filing_text_letter0_temp_dt, i=NULL, j=xmltrim_col, value=gsub(hash_table[k,c("PATTERN")], hash_table[k,c("REPLACEMENT")], filing_text_letter0_temp_dt[[xmltrim_col]]))
                                                  
                                                  #progress_function(k,1,nrow(hash_table),1,1,1)
                                                }
                                                rm(k)
                                                
                                                filing_text_letter1_temp <- as.data.frame(filing_text_letter0_temp_dt,stringsAsFactors=FALSE)
                                                
                                                
                                 
                                                #REMOVE DASH
                                                #filing_text_letter0_temp[,c(xmltrim_col)] <- gsub("-", " ", filing_text_letter0_temp[,c(xmltrim_col)])
          
                                                #PREPARE TEXT
                                                
                                                #filing_text_letter0_temp[,c(xmltrim_col)] <-  qprep(text.var=filing_text_letter0_temp[,c(xmltrim_col)], rm.dash = TRUE, bracket = "all", missing = "",
                                                #                                                    names = FALSE, abbreviation = qdapDictionaries::abbreviations,
                                                #                                                    replace = NULL, ignore.case = TRUE, num.paste = TRUE)
                                                
                                                ####NEED TO TO REMOVE ALL PUNCTUATION! WHAT ABOUT HYPHEN????
                                           
                                                
                                                #CONVERT TO UPPER CASE
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- toupper(filing_text_letter1_temp[,c(xmltrim_col)])

                                                
                                                #CLEAN FILINGS
                                                
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub(" {2,}", " ",filing_text_letter1_temp[,c(xmltrim_col)])
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub("^\\s+|\\s+$", "", filing_text_letter1_temp[,c(xmltrim_col)])
                                                
                                                
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub("'", "", filing_text_letter1_temp[,c(xmltrim_col)])
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub("-", "", filing_text_letter1_temp[,c(xmltrim_col)])
                                                
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub(",", " ", filing_text_letter1_temp[,c(xmltrim_col)])
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub(":", " ", filing_text_letter1_temp[,c(xmltrim_col)])
                                                
                                                #filing_text_letter1_temp[,c(xmltrim_col)] <- gsub("'", "", filing_text_letter1_temp[,c(xmltrim_col)])
                                                #filing_text_letter1_temp[,c(xmltrim_col)] <- gsub(",", "", filing_text_letter1_temp[,c(xmltrim_col)])
                                                #filing_text_letter1_temp[,c(xmltrim_col)] <- gsub(":", "", filing_text_letter1_temp[,c(xmltrim_col)])
                                                
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub("&", " AND ", filing_text_letter1_temp[,c(xmltrim_col)])
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub("\\(S\\)", "S", filing_text_letter1_temp[,c(xmltrim_col)])
                                                
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub(" {2,}", " ",filing_text_letter1_temp[,c(xmltrim_col)])
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- gsub("^\\s+|\\s+$", "", filing_text_letter1_temp[,c(xmltrim_col)])
                                                
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- paste(" ", filing_text_letter1_temp[,c(xmltrim_col)]," ",sep="")
                                                
                                                rm(filing_text_collapse)
                                                
                                                
                                                
                                                #SCRUB TEXT
                                                filing_text_letter1_temp[,c(xmltrim_col)] <- scrubber(filing_text_letter1_temp[,c(xmltrim_col)])
                                                
                                                
                                                
                                                
                                                #CREATE FINAL DATA AND OUTPUT
                                                filing_text_comb <- merge(filing_no_text2, filing_text_letter1_temp, 
                                                                          by.x=c("file","DOCUMENT_INDEX"), by.y=c("file","DOCUMENT_INDEX"), 
                                                                          all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
                                                
                                                rm(filing_no_text2,filing_text_letter1_temp)
                                                
                                                write.table(filing_text_comb,file=filepath_out, append=FALSE, na="", 
                                                             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
                                                
                                                df_comb_list <- list(file)
                                                
                                                rm(filing_text_comb)
                                                rm(file,filepath)
                                                rm(file_out,filepath_out)
                                                rm(xmlcol,xmltrim_col)
                                                
                                                return(df_comb_list)
                                                
                                              },
                                              sub_folder_output_path=sub_folder_output_path,
                                              entity_encoding=entity_encoding, hash_table=hash_table,
                                            .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

                          df_comb_list_all <- list(substitute)
                          
                          rm(substitute)
                          rm(yr,yr_folder_path,sub_folder_path,sub_folder_output_path,downloaded_files5)
                          
                          return(df_comb_list_all)
                          
                        },
                        path_output=paste(output_directory,downloadfolder,sep=slash),
                        subfolder=txtfolder_in,subfolder_output=txtfolder_out,entity_encoding=entity_encoding, hash_table=hash_table,
                       .progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


###############################################################################
cat("Seperate Data \n")
###############################################################################
# 
# letter_all_comb0 <- sapply(letters_all, "[", 1)
# letter_all_comb <- rbindlist(letter_all_comb0,fill=TRUE,use.names=TRUE)
# rm(letter_all_comb0)
# 
# letter_all_matches_summary0 <- sapply(letters_all, "[", 2)
# letter_all_matches_summary <- rbindlist(letter_all_matches_summary0,fill=TRUE,use.names=TRUE)
# rm(letter_all_matches_summary0)
# 
# letter_all_matches_beg0 <- sapply(letters_all, "[", 3)
# letter_all_matches_beg <- rbindlist(letter_all_matches_beg0,fill=TRUE,use.names=TRUE)
# rm(letter_all_matches_beg0)
# 
# letter_all_matches_end0 <- sapply(letters_all, "[", 4)
# letter_all_matches_end <- rbindlist(letter_all_matches_end0,fill=TRUE,use.names=TRUE)
# rm(letter_all_matches_end0)
# 
# letter_all_matches_pos0 <- sapply(letters_all, "[", 5)
# letter_all_matches_pos <- rbindlist(letter_all_matches_pos0,fill=TRUE,use.names=TRUE)
# rm(letter_all_matches_pos0)
# 
# letter_all_matches_sign0 <- sapply(letters_all, "[", 6)
# letter_all_matches_sign <- rbindlist(letter_all_matches_sign0,fill=TRUE,use.names=TRUE)
# rm(letter_all_matches_sign0)

# 
# ###############################################################################
# cat("Output Combined Files \n")
# ###############################################################################
# 
# #Check to see if yr folder exists.  If not, create it.
# out_folder_path <- paste(download_folder_path, txtfolder_out, sep = "\\", collapse = "\\")   
# create_directory(out_folder_path,remove=1)
# 
# write.table(letter_all_comb,file=paste(out_folder_path,"\\","letter_all_comb",".csv",sep=""), append=FALSE, na="NA", 
#             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
# 
# write.table(letter_all_matches_summary,file=paste(out_folder_path,"\\","letter_all_matches_summary",".csv",sep=""), append=FALSE, na="NA", 
#             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
# 
# write.table(letter_all_matches_beg,file=paste(out_folder_path,"\\","letter_all_matches_beg",".csv",sep=""), append=FALSE, na="NA", 
#             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
# 
# write.table(letter_all_matches_end,file=paste(out_folder_path,"\\","letter_all_matches_end",".csv",sep=""), append=FALSE, na="NA", 
#             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
# 
# write.table(letter_all_matches_pos,file=paste(out_folder_path,"\\","letter_all_matches_pos",".csv",sep=""), append=FALSE, na="NA", 
#             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
# 
# write.table(letter_all_matches_sign,file=paste(out_folder_path,"\\","letter_all_matches_sign",".csv",sep=""), append=FALSE, na="NA", 
#             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
