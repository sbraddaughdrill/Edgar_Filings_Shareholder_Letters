# TODO: Add comment
# 
# Author:  Brad
# File:    Clean_Filings.R
# Version: 1.0
# Date:    06.10.2014
# Purpose: Clean individual filings
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
downloadfolder <- "MF_Shareholder_Reports_N-CSR-A"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS"
#downloadfolder <- "MF_Shareholder_Reports_N-CSRS-A"

#The sub directory where the downloaded filings are
txtfolder <- "txt_extract"

#The sub directory where the combined filings will be located
txtfolder_clean <- "txt_clean"

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
cat("Clean Files \n")
###############################################################################

filings_header_info <- dlply(.data=filings_trim2, .variables=c("yr"), 
                             .fun = function(x, path_output,subfolder,subfolder_output){
                               
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
                               #  subfolder <- txtfolder
                               #  subfolder_output <- txtfolder_clean
                               
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
                               
                               downloaded_files2 <- downloaded_files2[order(downloaded_files2[,"filepath"]),]
                               row.names(downloaded_files2) <- seq(nrow(downloaded_files2))
                               
                               downloaded_files3 <- data.frame(yr_id=NA,downloaded_files2,stringsAsFactors=FALSE)
                               downloaded_files3[,"yr_id"] <- seq(1,nrow(downloaded_files3),1)
                               
                               rm(downloaded_files2)
                               
                               output_files <- dlply(.data=downloaded_files3, .variables=c("yr_id"), 
                                                     .fun = function(y,sub_folder_output_path){
                                                       
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000072760-03-000038.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000088053-03-000790.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0001017062-03-000375.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000820027-03-000786.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000950136-03-003115.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000811860-04-000012.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000038403-04-000029.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000763897-04-000004.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0001121624-04-000011.csv"),]
                                                       # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000038403-05-000033.csv"),]
                                                       
                                                       
                                                       # sub_folder_output_path <- sub_folder_output_path
                                                       
                                                       file <- unique(y[,"file"])
                                                       filepath <- unique(y[,"filepath"])
                                                       
                                                       file_out <- file
                                                       filepath_out <- paste(sub_folder_output_path,file_out,sep="\\")
                                                       
                                                       cat(file,"\n")
                                                       
                                                       
                                                       filing <- read.table(file=filepath, header = TRUE, na.strings="",stringsAsFactors=FALSE, 
                                                                            sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
                                                       
                                                       filing_no_text0 <- filing[,!(colnames(filing) %in% c("TEXT"))]
                                                       filing_no_text1 <- unique(filing_no_text0)
                                                       filing_no_text2 <- data.frame(file=NA,filing_no_text1,stringsAsFactors=FALSE)
                                                       filing_no_text2[,"file"] <- file
                                                       rm(filing_no_text0,filing_no_text1)
                                                       
                                                       filing_text0 <- filing[,(colnames(filing) %in% c("DOCUMENT_INDEX","TEXT"))]
                                                       filing_text1 <- data.frame(file=NA,filing_text0,stringsAsFactors=FALSE)
                                                       filing_text1[,"file"] <- file
                                                       rm(filing_text0)
                                                       
                                                       rm(filing)
                                                       
                                                       
                                                       #REMOVE TABLES    
                                                       
                                                       #Remove space after beginning and before end of tags
                                                       filing_text_table_clean <- filing_text1
                                                       filing_text_table_clean[,c("TEXT")] <- gsub("<\\s*(?!$)","<", filing_text_table_clean[,c("TEXT")],perl=TRUE)
                                                       filing_text_table_clean[,c("TEXT")] <- gsub("</\\s*(?!$)","</", filing_text_table_clean[,c("TEXT")],perl=TRUE)
                                                       filing_text_table_clean[,c("TEXT")] <- gsub("\\s*>",">", filing_text_table_clean[,c("TEXT")],perl=TRUE)
                                                       rm(filing_text1)
                                                       
                                                       for (i in 1:ncol(filing_text_table_clean))
                                                       {
                                                         filing_text_table_clean[,i] <- unknownToNA(filing_text_table_clean[,i], unknown=c("",".","n/a","na","<NA>","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                                                                           NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                                                                           NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
                                                         filing_text_table_clean[,i] <- ifelse(is.na(filing_text_table_clean[,i]),"", filing_text_table_clean[,i])
                                                       } 
                                                       rm(i)
                                                       
                                                       
                                                       tags_sep_table <- c("TABLE")
                                                       filing_text_table_sep <- ddply(.data=filing_text_table_clean,  .variables=c("file","DOCUMENT_INDEX"), 
                                                                                      .fun = function(x,bycol,xmlcol,tags){
                                                                                        
                                                                                        # x <- filing_text_table_clean[filing_text_table_clean[,"DOCUMENT_INDEX"]==1,]
                                                                                        # bycol <- c("file","DOCUMENT_INDEX")
                                                                                        # xmlcol <- "TEXT"
                                                                                        # tags <- tags_sep_table
                                                                                        
                                                                                        file_temp <- unique(x[,"file"])
                                                                                        index_temp <- unique(x[,"DOCUMENT_INDEX"])
                                                                                        
                                                                                        #x_temp <- data.frame(x,id=NA,stringsAsFactors=FALSE)
                                                                                        #colnames(x_temp) <- c(colnames(x),"id")
                                                                                        #x_temp[,"id"] <- seq(1,nrow(x_temp),1)
                                                                                        
                                                                                        x_temp_split <- split_by_tag(x,xmlcol,tags)
                                                                                        x_temp_split[,xmlcol] <- ifelse(x_temp_split[,xmlcol]==""," ",x_temp_split[,xmlcol])
                                                                                        
                                                                                        x_temp_split <- data.table(x_temp_split, key = "id")
                                                                                        sep_temp <- x_temp_split[, list(TEXT = unlist(strsplit(TEXT, '\n', fixed=TRUE))), by = id]
                                                                                        
                                                                                        rm(x_temp_split)
                                                                                        
                                                                                        sep_temp <- sep_temp[, expand_id := sequence(.N), by = "id"]
                                                                                        sep_temp <- as.data.frame(sep_temp,stringsAsFactors=FALSE)
                                                                                        sep_temp <- sep_temp[,c("id","expand_id",xmlcol)]
                                                                                        
                                                                                        sep_temp <- sep_temp[,!(colnames(sep_temp) %in% c("id","expand_id"))]
                                                                                        sep_temp_out <- data.frame(file=NA,index=NA,sep_temp,stringsAsFactors=FALSE)
                                                                                        rm(sep_temp)
                                                                                        
                                                                                        sep_temp_out[,"file"] <- file_temp
                                                                                        sep_temp_out[,"index"] <- index_temp
                                                                                        colnames(sep_temp_out) <- c(bycol,xmlcol)
                                                                                        
                                                                                        return(sep_temp_out)
                                                                                        
                                                                                      }, bycol=c("file","DOCUMENT_INDEX"),xmlcol="TEXT", tags=tags_sep_table,
                                                                                      .progress = "none", .inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)
                                                       
                                                       rm(filing_text_table_clean)
                                                       
                                                       filing_text_table_id <- data.frame(filing_text_table_sep,table_tag_temp=NA,stringsAsFactors=FALSE)
                                                       colnames(filing_text_table_id)[match("table_tag_temp",names(filing_text_table_id))] <- "TABLE_INDEX"
                                                       rm(filing_text_table_sep)
                                                       
                                                       filing_text_table_id[,"TABLE_INDEX"] <- ifelse(grepl("<TABLE", filing_text_table_id[,"TEXT"]), "<TABLE>", filing_text_table_id[,"TABLE_INDEX"])
                                                       filing_text_table_id[,"TABLE_INDEX"] <- ifelse(grepl("</TABLE", filing_text_table_id[,"TEXT"]), "</TABLE>", filing_text_table_id[,"TABLE_INDEX"])
                                                       
                                                       index_table_temp <- llply(.data=tags_sep_table, create_tag_index,data=filing_text_table_id, tag_raw_col="TABLE_INDEX",
                                                                                 .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
                                                       index_table <- do.call(cbind, index_table_temp)
                                                       index_table <- as.data.frame(index_table,stringsAsFactors=FALSE)
                                                       colnames(index_table) <- paste(tags_sep_table,"INDEX",sep="_")
                                                       colnames(index_table) <- gsub("-","_", colnames(index_table))
                                                       rm(index_table_temp)
                                                       
                                                       filing_text_table_id[,"TABLE_INDEX"] <- index_table
                                                       rm(index_table)
                                                       
                                                       if(c("TABLE_INDEX") %in% colnames(filing_text_table_id)) {
                                                         
                                                         filing_text_table_id_trim <- filing_text_table_id[(filing_text_table_id[,c("TABLE_INDEX")]==0),]
                                                         
                                                       } else {
                                                         
                                                         #cat("NO TABLE SECTION","\n")
                                                         
                                                         filing_text_table_id_trim <- filing_text_table_id
                                                         
                                                       }
                                                       rm(filing_text_table_id)
                                                       
                                                       filing_text_table_id_trim <- filing_text_table_id_trim[,!(colnames(filing_text_table_id_trim) %in% c("TABLE_INDEX"))]
                                                       
                                                       
                                                       
                                                       #REMOVE PDFS    
                                                       
                                                       #Remove space after beginning and before end of tags
                                                       filing_text_pdf_clean <- filing_text_table_id_trim
                                                       filing_text_pdf_clean[,c("TEXT")] <- gsub("<\\s*(?!$)","<", filing_text_pdf_clean[,c("TEXT")],perl=TRUE)
                                                       filing_text_pdf_clean[,c("TEXT")] <- gsub("</\\s*(?!$)","</", filing_text_pdf_clean[,c("TEXT")],perl=TRUE)
                                                       filing_text_pdf_clean[,c("TEXT")] <- gsub("\\s*>",">", filing_text_pdf_clean[,c("TEXT")],perl=TRUE)
                                                       rm(filing_text_table_id_trim)
                                                       
                                                       for (i in 1:ncol(filing_text_pdf_clean))
                                                       {
                                                         filing_text_pdf_clean[,i] <- unknownToNA(filing_text_pdf_clean[,i], unknown=c("",".","n/a","na","<NA>","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                                                                       NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                                                                       NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
                                                         filing_text_pdf_clean[,i] <- ifelse(is.na(filing_text_pdf_clean[,i]),"", filing_text_pdf_clean[,i])
                                                       } 
                                                       rm(i)
                                                       
                                                       
                                                       tags_sep_pdf <- c("PDF")
                                                       #                                                        filing_text_pdf_sep <- ddply(.data=filing_text_pdf_clean,  .variables=c("file","DOCUMENT_INDEX"), 
                                                       #                                                                                     .fun = function(x,bycol,xmlcol,tags){
                                                       #                                                                                       
                                                       #                                                                                       # x <- filing_text_pdf_clean[filing_text_pdf_clean[,"DOCUMENT_INDEX"]==1,]
                                                       #                                                                                       # bycol <- c("file","DOCUMENT_INDEX")
                                                       #                                                                                       # xmlcol <- "TEXT"
                                                       #                                                                                       # tags <- tags_sep_pdf
                                                       #                                                                                       
                                                       #                                                                                       file_temp <- unique(x[,"file"])
                                                       #                                                                                       index_temp <- unique(x[,"DOCUMENT_INDEX"])
                                                       #                                                                                       
                                                       #                                                                                       #x_temp <- data.frame(x,id=NA,stringsAsFactors=FALSE)
                                                       #                                                                                       #colnames(x_temp) <- c(colnames(x),"id")
                                                       #                                                                                       #x_temp[,"id"] <- seq(1,nrow(x_temp),1)
                                                       #                                                                                       
                                                       #                                                                                       x_temp_split <- split_by_tag(x,xmlcol,tags)
                                                       #                                                                                       x_temp_split[,xmlcol] <- ifelse(x_temp_split[,xmlcol]==""," ",x_temp_split[,xmlcol])
                                                       #                                                                                       
                                                       #                                                                                       x_temp_split <- data.table(x_temp_split, key = "id")
                                                       #                                                                                       sep_temp <- x_temp_split[, list(TEXT = unlist(strsplit(TEXT, '\n', fixed=TRUE))), by = id]
                                                       #                                                                                       
                                                       #                                                                                       rm(x_temp_split)
                                                       #                                                                                       
                                                       #                                                                                       sep_temp <- sep_temp[, expand_id := sequence(.N), by = "id"]
                                                       #                                                                                       sep_temp <- as.data.frame(sep_temp,stringsAsFactors=FALSE)
                                                       #                                                                                       sep_temp <- sep_temp[,c("id","expand_id",xmlcol)]
                                                       #                                                                                       
                                                       #                                                                                       sep_temp <- sep_temp[,!(colnames(sep_temp) %in% c("id","expand_id"))]
                                                       #                                                                                       sep_temp_out <- data.frame(file=NA,index=NA,sep_temp,stringsAsFactors=FALSE)
                                                       #                                                                                       rm(sep_temp)
                                                       #                                                                                       
                                                       #                                                                                       sep_temp_out[,"file"] <- file_temp
                                                       #                                                                                       sep_temp_out[,"index"] <- index_temp
                                                       #                                                                                       colnames(sep_temp_out) <- c(bycol,xmlcol)
                                                       #                                                                                       
                                                       #                                                                                       return(sep_temp_out)
                                                       #                                                                                       
                                                       #                                                                                     }, bycol=c("file","DOCUMENT_INDEX"),xmlcol="TEXT", tags=tags_sep_pdf,
                                                       #                                                                                     .progress = "none", .inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)
                                                       #                                                     
                                                       filing_text_pdf_sep <- filing_text_pdf_clean
                                                       rm(filing_text_pdf_clean)
                                                       
                                                       filing_text_pdf_id <- data.frame(filing_text_pdf_sep,pdf_tag_temp=NA,stringsAsFactors=FALSE)
                                                       colnames(filing_text_pdf_id)[match("pdf_tag_temp",names(filing_text_pdf_id))] <- "PDF_INDEX"
                                                       rm(filing_text_pdf_sep)
                                                       
                                                       filing_text_pdf_id[,"PDF_INDEX"] <- ifelse(grepl("<PDF>", filing_text_pdf_id[,"TEXT"]), "<PDF>", filing_text_pdf_id[,"PDF_INDEX"])
                                                       filing_text_pdf_id[,"PDF_INDEX"] <- ifelse(grepl("</PDF>", filing_text_pdf_id[,"TEXT"]), "</PDF>", filing_text_pdf_id[,"PDF_INDEX"])
                                                       
                                                       index_pdf_temp <- llply(.data=tags_sep_pdf, create_tag_index,data=filing_text_pdf_id, tag_raw_col="PDF_INDEX",
                                                                               .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
                                                       index_pdf <- do.call(cbind, index_pdf_temp)
                                                       index_pdf <- as.data.frame(index_pdf,stringsAsFactors=FALSE)
                                                       colnames(index_pdf) <- paste(tags_sep_pdf,"INDEX",sep="_")
                                                       colnames(index_pdf) <- gsub("-","_", colnames(index_pdf))
                                                       rm(index_pdf_temp)
                                                       
                                                       filing_text_pdf_id[,"PDF_INDEX"] <- index_pdf
                                                       rm(index_pdf)
                                                       
                                                       if(c("PDF_INDEX") %in% colnames(filing_text_pdf_id)) {
                                                         
                                                         filing_text_pdf_id_trim <- filing_text_pdf_id[(filing_text_pdf_id[,c("PDF_INDEX")]==0),]
                                                         
                                                       } else {
                                                         
                                                         #cat("NO PDF SECTION","\n")
                                                         
                                                         filing_text_pdf_id_trim <- filing_text_pdf_id
                                                         
                                                       }
                                                       rm(filing_text_pdf_id)
                                                       
                                                       filing_text_pdf_id_trim <- filing_text_pdf_id_trim[,!(colnames(filing_text_pdf_id_trim) %in% c("PDF_INDEX"))]
                                                       
                                                       
                                                       
                                                       #PARSE HTML
                                                       filing_text_parse <- ddply(.data=filing_text_pdf_id_trim, .variables=c("file","DOCUMENT_INDEX"), 
                                                                                  .fun = function(x,bycol,xmlcol){
                                                                                    
                                                                                    # x <- filing_text_pdf_id_trim[filing_text_pdf_id_trim[,"DOCUMENT_INDEX"]==1,]
                                                                                    # bycol <- c("file","DOCUMENT_INDEX")
                                                                                    # xmlcol <- "TEXT"
                                                                                    
                                                                                    file_temp <- unique(x[,"file"])
                                                                                    index_temp <- unique(x[,"DOCUMENT_INDEX"])
                                                                                    
                                                                                    html_flag_open0 <- ifelse(grepl("<HTML>", x[,xmlcol],ignore.case = TRUE), 1, 0)
                                                                                    html_flag_open1 <- sum(html_flag_open0, na.rm = TRUE)
                                                                                    
                                                                                    html_flag_close0 <- ifelse(grepl("</HTML>", x[,xmlcol],ignore.case = TRUE), 1, 0)
                                                                                    html_flag_close1 <- sum(html_flag_close0, na.rm = TRUE)
                                                                                    
                                                                                    html_flag_both <-  html_flag_open1 + html_flag_close1
                                                                                    
                                                                                    
                                                                                    if(html_flag_both==2) {
                                                                                      
                                                                                      #cat("HTML DOCUMENT","\n")
                                                                                      
                                                                                      #pagetree  <- htmlParse(x[,xmlcol], asText=TRUE, options = HUGE)
                                                                                      #plain.text1 <- xpathSApply(pagetree , "//p", xmlValue)
                                                                                      
                                                                                      pagetree  <- htmlTreeParse( x[,xmlcol], useInternal = TRUE, options = HUGE)
                                                                                      plain.text1 <- xpathApply(pagetree , '//p', xmlValue)
                                                                                      plain.text1 <- unlist(plain.text1)
                                                                                      
                                                                                      plain.text2 <- xpathSApply(pagetree , "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
                                                                                      
                                                                                      plain.text3 <- xpathSApply(pagetree , "//text()", xmlValue)
                                                                                      
                                                                                      if(length(plain.text1)!=0) {
                                                                                        
                                                                                        plain.text.final <- as.data.frame(plain.text1,stringsAsFactors=FALSE)
                                                                                        colnames(plain.text.final) <- xmlcol
                                                                                        
                                                                                      } else if (length(plain.text2)!=0) {
                                                                                        
                                                                                        plain.text.final <- as.data.frame(plain.text2,stringsAsFactors=FALSE)
                                                                                        colnames(plain.text.final) <- xmlcol
                                                                                        
                                                                                      } else if (length(plain.text3)!=0) {
                                                                                        
                                                                                        plain.text.final <- as.data.frame(plain.text3,stringsAsFactors=FALSE)
                                                                                        colnames(plain.text.final) <- xmlcol
                                                                                        
                                                                                      } else {
                                                                                        
                                                                                        #cat("CANNOT PARSE HTML DOCUMENT","\n")
                                                                                        
                                                                                        plain.text.final <- as.data.frame(x[,xmlcol],stringsAsFactors=FALSE)
                                                                                        colnames(plain.text.final) <- xmlcol
                                                                                        
                                                                                      }
                                                                                      rm(pagetree,plain.text1,plain.text2,plain.text3)
                                                                                      
                                                                                      plain.text.final_temp  <- data.frame(id=NA,plain.text.final,stringsAsFactors=FALSE)
                                                                                      plain.text.final_temp[,"id"] <- seq(1,nrow(plain.text.final_temp))
                                                                                      plain.text.final_temp[,xmlcol] <- ifelse(plain.text.final_temp[,xmlcol]==""," ",plain.text.final_temp[,xmlcol])
                                                                                      rm(plain.text.final)
                                                                                      
                                                                                      plain.text.final2_dt <- data.table(plain.text.final_temp, key = "id")
                                                                                      plain.text.final2_split <- plain.text.final2_dt[, list(TEXT = unlist(strsplit(TEXT, '\n', fixed=TRUE))), by = id]
                                                                                      
                                                                                      rm(plain.text.final_temp,plain.text.final2_dt)
                                                                                      
                                                                                      plain.text.final2_split <- plain.text.final2_split[, sep_id := sequence(.N), by = "id"]
                                                                                      plain.text.final2_split <- as.data.frame(plain.text.final2_split,stringsAsFactors=FALSE)
                                                                                      plain.text.final2_split <- plain.text.final2_split[,c("id","sep_id",xmlcol)]
                                                                                      
                                                                                      plain.text.final2_split[,xmlcol] <- gsub("\\n", " ", plain.text.final2_split[,xmlcol])
                                                                                      plain.text.final2_split <- plain.text.final2_split[,!(colnames(plain.text.final2_split) %in% c("id","sep_id"))]
                                                                                      
                                                                                      plain.text.final2 <- as.data.frame(plain.text.final2_split,stringsAsFactors=FALSE)
                                                                                      colnames(plain.text.final2) <- xmlcol
                                                                                      rm(plain.text.final2_split)
                                                                                      
                                                                                      parsed_text <- data.frame(temp_file=NA,temp_index=NA,plain.text.final2,stringsAsFactors=FALSE)
                                                                                      parsed_text[,"temp_file"] <- file_temp
                                                                                      parsed_text[,"temp_index"] <- index_temp
                                                                                      colnames(parsed_text) <- c(bycol,xmlcol)
                                                                                      
                                                                                      rm(plain.text.final2)
                                                                                      
                                                                                    } else if (html_flag_both==0) {
                                                                                      
                                                                                      #cat("NON-HTML DOCUMENT","\n")
                                                                                      
                                                                                      parsed_text <- data.frame(temp_file=NA,temp_index=NA,x[,xmlcol],stringsAsFactors=FALSE)
                                                                                      parsed_text[,"temp_file"] <- file_temp
                                                                                      parsed_text[,"temp_index"] <- index_temp
                                                                                      colnames(parsed_text) <- c(bycol,xmlcol)
                                                                                      
                                                                                      
                                                                                    } else if (html_flag_both==1) {
                                                                                      
                                                                                      cat("OPEN/CLOSING TAG MISMATCH","\n")
                                                                                      
                                                                                      parsed_text <- data.frame(temp_file=NA,temp_index=NA,plain.text2="",stringsAsFactors=FALSE)
                                                                                      parsed_text[,"temp_file"] <- file_temp
                                                                                      parsed_text[,"temp_index"] <- index_temp
                                                                                      colnames(parsed_text) <- c(bycol,xmlcol)
                                                                                      
                                                                                    } else {
                                                                                      
                                                                                      cat("ERROR FINDING HTML TAGS", "\n")
                                                                                      
                                                                                      parsed_text <- data.frame(temp_file=NA,temp_index=NA,plain.text2="",stringsAsFactors=FALSE)
                                                                                      parsed_text[,"temp_file"] <- file_temp
                                                                                      parsed_text[,"temp_index"] <- index_temp
                                                                                      colnames(parsed_text) <- c(bycol,xmlcol)
                                                                                      
                                                                                    }
                                                                                    
                                                                                    rm(html_flag_open0,html_flag_open1,html_flag_close0,html_flag_close1,html_flag_both)
                                                                                    rm(file_temp,index_temp)
                                                                                    
                                                                                    return(parsed_text)
                                                                                    
                                                                                  }, bycol=c("file","DOCUMENT_INDEX"),xmlcol="TEXT",
                                                                                  .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                                                       
                                                       rm(filing_text_pdf_id_trim)
                                                       
                                                       
                                                       #CREATE FINAL DATA AND OUTPUT
                                                       filing_text_comb <- merge(filing_no_text2, filing_text_parse, 
                                                                                    by.x=c("file","DOCUMENT_INDEX"), by.y=c("file","DOCUMENT_INDEX"), 
                                                                                    all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
                                                       
                                                       rm(filing_no_text2,filing_text_parse)
                                                       
                                                       write.table(filing_text_comb,file=filepath_out, append=FALSE, na="", 
                                                                   sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
                                                       
                                                       
                                                       filing_text_parse_clean_trim2 <- filing_text_comb
                                                       filing_text_parse_clean_trim2 <- unique(filing_text_parse_clean_trim2[,!(colnames(filing_text_parse_clean_trim2) %in% c("TEXT"))])
                                                       row.names(filing_text_parse_clean_trim2) <- seq(nrow(filing_text_parse_clean_trim2))
                                                       rm(filing_text_comb)
                                                       
                                                       df_comb_list <- list(filing_text_parse_clean_trim2)
                                                       
                                                       rm(filing_text_parse_clean_trim2)
                                                       rm(file,filepath,file_out,filepath_out)
                                                       
                                                       return(df_comb_list)
                                                       
                                                     },
                                                     sub_folder_output_path=sub_folder_output_path,
                                                     .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                               
                               
                               output_files_comb0 <- sapply(output_files, "[", 1)
                               #output_files_comb1 <- do.call(rbind,output_files_comb0)
                               output_files_comb1 <- rbindlist(output_files_comb0,fill=TRUE,use.names=TRUE)
                               output_files_comb  <- as.data.frame(output_files_comb1,stringsAsFactors=FALSE) 
                               rm(output_files_comb0,output_files_comb1)
                               
                               output_files_comb_final <- data.frame(yr=NA,output_files_comb,stringsAsFactors=FALSE) 
                               output_files_comb_final[,"yr"] <- yr
                               rm(output_files_comb)
                               
                               df_comb_list_all <- list(output_files_comb_final)
                               
                               rm(output_files_comb_final)
                               rm(yr,yr_folder_path,sub_folder_path,sub_folder_output_path,downloaded_files3)
                               
                               return(df_comb_list_all)
                               
                             },
                             path_output=paste(output_directory,downloadfolder,sep=slash),
                             subfolder=txtfolder,subfolder_output=txtfolder_clean,
                             .progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


