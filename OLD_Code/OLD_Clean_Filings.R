# TODO: Add comment
# 
# Author:  Brad
# File:    Clean_Filings.R
# Version: 1.0
# Date:    06.10.2014
# Purpose: Extract individual filing
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

###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("gdata","plyr","XML")
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
downloadfolder <- "MF_Shareholder_Reports_N-CSR-A"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS-A"

filetype <- c("N-CSR","N-CSR/A","N-CSRS","N-CSRS/A")

#The sub directory where the downloaded filings are
headerfolder <- "txt"

#The sub directory where the combined filings will be located
headercombfolder <- "txt"

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
cat("Import HTML tags \n")
###############################################################################

#html_tags0 <- read.csv(file=paste(output_directory,"HTML_tags.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
html_tags0 <- read.table(file=paste(output_directory,"HTML_tags.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                         sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#Clean
html_tags_clean <- html_tags0

for(i in which(sapply(html_tags_clean,class)=="character"))
{
  html_tags_clean[[i]] = trim(html_tags_clean[[i]])
}
rm(i)

for (i in 1:ncol(html_tags_clean))
{
  html_tags_clean[,i] <- unknownToNA(html_tags_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                    NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                    NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  html_tags_clean[,i] <- ifelse(is.na(html_tags_clean[,i]),NA, html_tags_clean[,i])
} 
rm(i)

html_tags1 <- html_tags_clean[!is.na(html_tags_clean[,"START_TAG"]),]
row.names(html_tags1) <- seq(nrow(html_tags1))

html_tags <- html_tags1[,c("START_TAG","END_TAG","TAG_SHORT","IGNORE_CLOSING")]
#html_tags[,"START_TAG"] <- gsub("-","_",html_tags[,"START_TAG"])
#html_tags[,"END_TAG"] <- gsub("-","_",html_tags[,"END_TAG"])
#html_tags[,"TAG_SHORT"] <- gsub("-","_",html_tags[,"TAG_SHORT"])
html_tags <- unique(html_tags)
row.names(html_tags) <- seq(nrow(html_tags))

rm(html_tags0,html_tags1,html_tags_clean)


###############################################################################
cat("Import SEC tags \n")
###############################################################################

#sec_tags0 <- read.csv(file=paste(output_directory,"SEC_tags.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
sec_tags0 <- read.table(file=paste(output_directory,"SEC_tags.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                        sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#Clean
sec_tags_clean <- sec_tags0

for(i in which(sapply(sec_tags_clean,class)=="character"))
{
  sec_tags_clean[[i]] = trim(sec_tags_clean[[i]])
}
rm(i)

for (i in 1:ncol(sec_tags_clean))
{
  sec_tags_clean[,i] <- unknownToNA(sec_tags_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                  NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                  NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  sec_tags_clean[,i] <- ifelse(is.na(sec_tags_clean[,i]),NA, sec_tags_clean[,i])
} 
rm(i)

sec_tags1 <- sec_tags_clean[!is.na(sec_tags_clean[,"START_TAG"]),]
row.names(sec_tags1) <- seq(nrow(sec_tags1))

sec_tags <- sec_tags1[,c("START_TAG","END_TAG","TAG_SHORT","IGNORE_CLOSING")]
#sec_tags[,"START_TAG"] <- gsub("-","_",sec_tags[,"START_TAG"])
#sec_tags[,"END_TAG"] <- gsub("-","_",sec_tags[,"END_TAG"])
#sec_tags[,"TAG_SHORT"] <- gsub("-","_",sec_tags[,"TAG_SHORT"])
sec_tags <- unique(sec_tags)
row.names(sec_tags) <- seq(nrow(sec_tags))

rm(sec_tags0,sec_tags1,sec_tags_clean)


###############################################################################
cat("Get header information \n")
###############################################################################

filings_header_info <- dlply(.data=filings_trim2, .variables=c("yr"), 
                             .fun = function(x, path_output,subfolder,entity_encoding,html_tags,sec_tags){
                               
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2003),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2004),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2005),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2006),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2007),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2008),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2009),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2010),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2011),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2012),]
                               #x <- filings_trim2[(filings_trim2[,"yr"]==2013),]
                               #path_output <- paste(output_directory,downloadfolder,sep=slash)
                               #subfolder <- headerfolder
                               #entity_encoding <- entity_encoding
                               #filetype <- filetype
                               #html_tags <- html_tags
                               #sec_tags <- sec_tags
                               
                               filings_trim2_short <- x[,!(colnames(x) %in% c("file_header","file_index_htm"))]
                               
                               yr <-  unique(x[,"yr"])
                               
                               cat("\n",yr,"\n")
                               
                               #Check to see if yr folder exists.  If not, create it.
                               yr_folder_path <- paste(path_output, yr, sep = "\\", collapse = "\\")   
                               create_directory(yr_folder_path,remove=1)
                               
                               sub_folder_path <- paste(yr_folder_path, subfolder, sep = "\\", collapse = "\\")   
                               create_directory(sub_folder_path,remove=1)
                               
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
                               
                               trim_headings <- dlply(.data=downloaded_files3, .variables=c("yr_id"), 
                                                      .fun = function(y,entity_encoding,filetype,html_tags,sec_tags){
                                                        
                                                        #y <- downloaded_files3[(downloaded_files3[,"file"]=="0000072760-03-000038.txt"),]
                                                        #y <- downloaded_files3[(downloaded_files3[,"file"]=="0000088053-03-000790.txt"),]
                                                        #y <- downloaded_files3[(downloaded_files3[,"file"]=="0001017062-03-000375.txt"),]
                                                        
                                                        #entity_encoding <- entity_encoding
                                                        #filetype <- filetype
                                                        #html_tags <- html_tags
                                                        #sec_tags <- sec_tags
                                                        
                                                        file <- unique(y[,"file"])
                                                        filepath <- unique(y[,"filepath"])
                                                        
                                                        cat(file,"\n")
                                                        
                                                        webpage_df_xml_only_comb <- import_local_edgar_file(filepath)
                                                        
                                                        webpage_df_xml_only_df0 <- sapply(webpage_df_xml_only_comb, "[", 1)
                                                        webpage_df_xml_only_df1 <- do.call(cbind,webpage_df_xml_only_df0)
                                                        webpage_df_xml_only_df  <- as.data.frame(webpage_df_xml_only_df1,stringsAsFactors=FALSE)
                                                        colnames(webpage_df_xml_only_df) <- c("raw","trash")
                                                        webpage_df_xml_only_df[,"raw"] <- gsub("&NBSP;"," ",webpage_df_xml_only_df[,"raw"])
                                                        
                                                        webpage_df_xml_only_df[,"raw"]  <- gsub(" {2,}", " ", webpage_df_xml_only_df[,"raw"] )
                                                        webpage_df_xml_only_df[,"raw"]  <- gsub("^\\s+|\\s+$", "", webpage_df_xml_only_df[,"raw"] )
                                                        #webpage_df_xml_only_df <- webpage_df_xml_only_df[!(is.na(webpage_df_xml_only_df[,"raw"]) | webpage_df_xml_only_df[,"raw"]==""),]
                                                        
                                                        rm(webpage_df_xml_only_df0,webpage_df_xml_only_df1)
                                                        
                                                        bad_tags_df0 <- sapply(webpage_df_xml_only_comb, "[", 2)
                                                        bad_tags_df1 <- do.call(cbind,bad_tags_df0)
                                                        bad_tags_df  <- as.data.frame(bad_tags_df1,stringsAsFactors=FALSE)
                                                        colnames(bad_tags_df) <- c("filepath","trash")
                                                        #bad_tags_df_trim <- bad_tags_df[!is.na(bad_tags_df[,1]),]
                                                        bad_tags_df_trim <- bad_tags_df
                                                        
                                                        rm(bad_tags_df0,bad_tags_df1,bad_tags_df)
                                                        rm(webpage_df_xml_only_comb)
                                                        
                                                        #Clean Tags
                                                        webpage_tags_clean <- clean_edgar_tags(webpage_df_xml_only_df[,"raw"])
                                                        rm(webpage_df_xml_only_df)
                                                        
                                                        #Expand Tags
                                                        webpage_tags_expand <- expand_edgar_tags(webpage_tags_clean[,"raw"],html_tags,sec_tags)
                                                        rm(webpage_tags_clean)
                                                        
                                                        #Fix Edgar Tags
                                                        webpage_tags_comb <- fix_edgar_tags(webpage_tags_expand,entity_encoding,html_tags,sec_tags)
                                                        rm(webpage_tags_expand)
                                                        
                                                        webpage_tags0 <- webpage_tags_comb[[1]]
                                                        webpage_tags  <- as.data.frame(webpage_tags0,stringsAsFactors=FALSE)
                                                        rm(webpage_tags0)
                                                        
                                                        bad_tags0 <- webpage_tags_comb[[2]]
                                                        bad_tags  <- as.data.frame(bad_tags0,stringsAsFactors=FALSE)
                                                        rm(bad_tags0)
                                                        
                                                        rm(webpage_tags_comb)
                                                        
                                                        webpage_sep <- webpage_tags
                                                        webpage_sep[,"Final_tag"] <- ifelse(grepl("<SEC_DOCUMENT>", webpage_sep[,"Final_tag"]), "<SEC_DOCUMENT>", webpage_sep[,"Final_tag"])
                                                        
                                                        rm(webpage_tags)
                                                        
                                                        webpage_sep <- data.frame(webpage_sep,tag_status_open=NA,tag_status_close=NA,stringsAsFactors=FALSE)
                                                        
                                                        #Find all possible tags
                                                        tags <-  c("SEC_DOCUMENT","SEC_HEADER","DOCUMENT","TYPE","SEQUENCE","FILENAME","DESCRIPTION","TEXT","TABLE")      
                                                        #tags <- unique(webpage_sep[,"col01_tag_short"])
                                                        sep_tags1 <- find_individual_tags(data=webpage_sep,tag_raw_col="Final_tag",tags=tags,
                                                                                          tag_open_col="tag_status_open",tag_close_col="tag_status_close")
                                                        sep_tags1 <- sep_tags1[!(sep_tags1[,"open_count"]==0),]
                                                        sep_tags2 <- sep_tags1[,"cleaned"]
                                                        rm(tags)
                                                        
                                                        index_temp <- llply(.data=sep_tags2, create_tag_index,data=webpage_sep, tag_raw_col="Final_tag",
                                                                            .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
                                                        index <- do.call(cbind, index_temp)
                                                        index <- as.data.frame(index,stringsAsFactors=FALSE)
                                                        colnames(index) <- paste(sep_tags2,"INDEX",sep="_")
                                                        rm(index_temp)
                                                        
                                                        webpage_sep_index0 <- data.frame(file=file, Final_tag_temp=webpage_sep[,"Final_tag"],tag_short_temp=webpage_sep[,"col01_tag_short"],index,stringsAsFactors=FALSE)
                                                        colnames(webpage_sep_index0)[match("Final_tag_temp",names(webpage_sep_index0))] <- "Final_tag"
                                                        colnames(webpage_sep_index0)[match("tag_short_temp",names(webpage_sep_index0))] <- "tag_short"
                                                        rm(index)
                                                        
                                                        #Remove Header Info
                                                        if(c("SEC_DOCUMENT_INDEX") %in% colnames(webpage_sep_index0)) {
                                                          
                                                          webpage_sep_index1 <- webpage_sep_index0[!(webpage_sep_index0[,c("SEC_DOCUMENT_INDEX")]==0),]
                                                          
                                                        } else {
                                                          
                                                          #cat("NO SEC_DOCUMENT SECTION","\n")
                                                          
                                                          webpage_sep_index1 <- webpage_sep_index0
                                                          
                                                        }
                                                        
                                                        webpage_sep_index <- webpage_sep_index1
                                                        rm(webpage_sep_index0,webpage_sep_index1)
                                                        
                                                        #Document Section - Setup
                                                        document_index_val <- "DOCUMENT"
                                                        document_index_val2 <- paste(document_index_val,"INDEX",sep="_")
                                                        document_index_flag <- (document_index_val %in% sep_tags2)
                                                        
                                                        #Document Section - Filing Info
                                                        document_filing_info_sub_index_val <- "TEXT"
                                                        document_filing_info_sub_index_val2 <- paste(document_filing_info_sub_index_val,"INDEX",sep="_")
                                                        
                                                        #cat("FILING INFO","\n")
                                                        
                                                        
                                                        if(document_index_flag) {
                                                          
                                                          document_data_temp <- webpage_sep_index[!(webpage_sep_index[,document_index_val2] %in% c(0)),]
                                                          row.names(document_data_temp) <- seq(nrow(document_data_temp))
                                                          
                                                          document_filing_info_merge1 <- dlply(.data=document_data_temp, .variables=c("file",document_index_val2), extract_filing_section_by_drop,
                                                                                               xml_col="Final_tag",tag_col="tag_short",index_col=document_index_val,
                                                                                               sub_index_col=document_filing_info_sub_index_val, index_flag=document_index_flag,file=file)
                                                          
                                                          document_filing_info_merge2 <- do.call(rbind.fill,document_filing_info_merge1)
                                                          
                                                          document_filing_info_merge <-  create_sub_index_sequence(data=document_filing_info_merge2,index_val=document_index_val2,
                                                                                                                   sub_index_val=document_filing_info_sub_index_val2,nonindex_prefix="")
                                                          
                                                          rm(document_data_temp,document_filing_info_merge1,document_filing_info_merge2)
                                                          
                                                        } else {
                                                          
                                                          document_filing_info_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,stringsAsFactors=FALSE)
                                                          colnames(document_filing_info_merge) <- c("file",document_index_val2,head(document_filing_info_sub_index_val2,1))
                                                          
                                                        }
                                                        
                                                        document_filing_info_merge_trim <- document_filing_info_merge[,!(colnames(document_filing_info_merge) %in% c(document_filing_info_sub_index_val2))]                                                          
                                                        
                                                        rm(document_filing_info_sub_index_val,document_filing_info_sub_index_val2,document_filing_info_merge)
                                                        
                                                        #cat("FILING TEXT","\n")
                                                        
                                                        #Document Section - Filing Text
                                                        document_filing_text_sub_index_val <- "TEXT"
                                                        document_filing_text_sub_index_val2 <- paste(document_filing_text_sub_index_val,"INDEX",sep="_")
                                                        
                                                        if(document_index_flag) {
                                                          
                                                          document_data_temp <- webpage_sep_index[!(webpage_sep_index[,document_index_val2] %in% c(0)),]
                                                          row.names(document_data_temp) <- seq(nrow(document_data_temp))
                                                          
                                                          document_filing_text_merge1 <- dlply(.data=document_data_temp, .variables=c("file",document_index_val2), extract_filing_section_by_keep_no_xml,
                                                                                               xml_col="Final_tag",tag_col="tag_short",index_col=document_index_val,
                                                                                               sub_index_col=document_filing_text_sub_index_val, index_flag=document_index_flag,file=file)
                                                          document_filing_text_merge2 <- do.call(rbind.fill,document_filing_text_merge1)
                                                          
                                                          document_filing_text_merge <-  create_sub_index_sequence(data=document_filing_text_merge2,index_val=document_index_val2,
                                                                                                                   sub_index_val=document_filing_text_sub_index_val2,nonindex_prefix="")
                                                          
                                                          rm(document_data_temp,document_filing_text_merge1,document_filing_text_merge2)
                                                          
                                                        } else {
                                                          
                                                          document_filing_text_merge <- data.frame(file=file,index_col=NA,sub_index_col=NA,temp_ret=NA,stringsAsFactors=FALSE)
                                                          colnames(document_filing_text_merge) <- c("file",document_index_val2,head(document_filing_text_sub_index_val2,1),sub_index_col)
                                                          
                                                        }
                                                        #colnames(document_filing_text_merge)[match("merge1",names(document_filing_text_merge))] <- sub_index_col
                                                        
                                                        document_filing_text_merge_trim <- document_filing_text_merge[,!(colnames(document_filing_text_merge) %in% c(document_filing_text_sub_index_val2))]                                                          
                                                        
                                                        rm(document_filing_text_sub_index_val,document_filing_text_sub_index_val2,document_filing_text_merge)
                                                        
                                                        
                                                        document_filing_info_comb <- merge(document_filing_info_merge_trim, document_filing_text_merge_trim, 
                                                                                           by.x=c("file",document_index_val2), by.y=c("file",document_index_val2), 
                                                                                           all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
                                                        
                                                        rm(document_filing_info_merge_trim,document_filing_text_merge_trim)
                                                        rm(document_index_val,document_index_val2,document_index_flag)
                                                        
                                                        df_comb_list <- list(bad_tags,document_filing_info_comb)
                                                        
                                                        rm(bad_tags_df_trim,bad_tags,document_filing_info_comb)
                                                        rm(file,filepath)
                                                        rm(sep_tags1,sep_tags2,webpage_sep,webpage_sep_index)
                                                        
                                                        return(df_comb_list)
                                                        
                                                      },
                                                      entity_encoding=entity_encoding,filetype=filetype,html_tags=html_tags,sec_tags=sec_tags,
                                                      .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                               
                               
                               
                               
                               
                               
                               
                               
                               #Only keep forms of interest
                               text_expand_trim1 <- text_expand[(text_expand[,c("TYPE")] %in% filetype),]
                               
                               
                               
                               
                               
                               
                               bad_files_comb0 <- sapply(trim_headings, "[", 1)
                               bad_files_comb1 <- do.call(rbind,bad_files_comb0)
                               bad_files_comb  <- as.data.frame(bad_files_comb1,stringsAsFactors=FALSE)
                               colnames(bad_files_comb) <- c("filepath","trash")
                               row.names(bad_files_comb) <- seq(nrow(bad_files_comb))
                               rm(bad_files_comb0,bad_files_comb1)
                               
                               header_intro_comb <- create_comb_df(list=trim_headings,list_pos=2,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_company_data_comb <- create_comb_df(list=trim_headings,list_pos=3,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_filing_values_comb <- create_comb_df(list=trim_headings,list_pos=4,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_business_address_comb <- create_comb_df(list=trim_headings,list_pos=5,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_mail_address_comb <- create_comb_df(list=trim_headings,list_pos=6,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               filer_former_company_comb <- create_comb_df(list=trim_headings,list_pos=7,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               series_other_comb <- create_comb_df(list=trim_headings,list_pos=8,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               series_class_contract_comb <- create_comb_df(list=trim_headings,list_pos=9,list_file_col="file",filing_info=filings_trim2_short,filing_info_file_col="file_header")
                               
                               rm(trim_headings)
                               
                               df_comb_list_all <- list(bad_files_comb,header_intro_comb,
                                                        filer_company_data_comb,filer_filing_values_comb,filer_business_address_comb, filer_mail_address_comb,filer_former_company_comb,
                                                        series_other_comb,series_class_contract_comb)
                               
                               rm(bad_files_comb,header_intro_comb)
                               rm(filer_company_data_comb,filer_filing_values_comb,filer_business_address_comb,filer_mail_address_comb,filer_former_company_comb)
                               rm(series_other_comb,series_class_contract_comb)
                               rm(yr,yr_folder_path,sub_folder_path,downloaded_files3)
                               
                               return(df_comb_list_all)
                               
                             },
                             path_output=paste(output_directory,downloadfolder,sep=slash),
                             subfolder=headerfolder,entity_encoding=entity_encoding,filetype=filetype,
                             html_tags=html_tags,sec_tags=sec_tags,
                             .progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)