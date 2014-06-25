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
txtfolder_clean <- "txt_clean"

#The sub directory where the combined filings will be located
txtfolder_section <- "txt_section"

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
cat("Import Letter Beginnings \n")
###############################################################################

#letter_beginning0 <- read.csv(file=paste(output_directory,"Letter_Beginning.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
letter_beginning0 <- read.table(file=paste(output_directory,"Letter_Beginning.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                                sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#Clean
letter_beginning_clean <- letter_beginning0

for(i in which(sapply(letter_beginning_clean,class)=="character"))
{
  letter_beginning_clean[[i]] <- gsub(" {2,}", " ", letter_beginning_clean[[i]])
  letter_beginning_clean[[i]] <- trim(letter_beginning_clean[[i]])
}
rm(i)

for (i in 1:ncol(letter_beginning_clean))
{
  letter_beginning_clean[,i] <- unknownToNA(letter_beginning_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                  NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                  NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  letter_beginning_clean[,i] <- ifelse(is.na(letter_beginning_clean[,i]),NA, letter_beginning_clean[,i])
} 
rm(i)

letter_beginning1 <- data.frame(letter_beginning_clean,regex=NA,stringsAsFactors=FALSE)
letter_beginning1 <- letter_beginning1[!is.na(letter_beginning1[,"BEGINNINGS"]),]
letter_beginning1 <- unique(letter_beginning1)
row.names(letter_beginning1) <- seq(nrow(letter_beginning1))

letter_beginning1[,"regex"] <- letter_beginning1[,"BEGINNINGS"]
letter_beginning1[,"regex"]   <- gsub("^\\s+|\\s+$", "", letter_beginning1[,"regex"] )





letter_beginning <- letter_beginning1

rm(letter_beginning0,letter_beginning1,letter_beginning_clean)


###############################################################################
cat("Import Letter Endings \n")
###############################################################################

#letter_ending0 <- read.csv(file=paste(output_directory,"Letter_Ending.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
letter_ending0 <- read.table(file=paste(output_directory,"Letter_Ending.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                             sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#Clean
letter_ending_clean <- letter_ending0

for(i in which(sapply(letter_ending_clean,class)=="character"))
{
  letter_ending_clean[[i]] <- gsub(" {2,}", " ", letter_ending_clean[[i]])
  letter_ending_clean[[i]] <- trim(letter_ending_clean[[i]])
}
rm(i)

for (i in 1:ncol(letter_ending_clean))
{
  letter_ending_clean[,i] <- unknownToNA(letter_ending_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                            NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                            NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  letter_ending_clean[,i] <- ifelse(is.na(letter_ending_clean[,i]),NA, letter_ending_clean[,i])
} 
rm(i)

letter_ending1 <- letter_ending_clean[!is.na(letter_ending_clean[,"ENDINGS"]),]
letter_ending1 <- as.data.frame(letter_ending1, stringsAsFactors=FALSE)
colnames(letter_ending1) <- "ENDINGS"
row.names(letter_ending1) <- seq(nrow(letter_ending1))

letter_ending <- letter_ending1
letter_ending <- unique(letter_ending)
letter_ending <- as.data.frame(letter_ending, stringsAsFactors=FALSE)
colnames(letter_ending) <- "ENDINGS"
row.names(letter_ending) <- seq(nrow(letter_ending))

rm(letter_ending0,letter_ending1,letter_ending_clean)


###############################################################################
cat("Import Corporate Positions \n")
###############################################################################

#corporate_positions0 <- read.csv(file=paste(output_directory,"Corporate_Positions.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
corporate_positions0 <- read.table(file=paste(output_directory,"Corporate_Positions.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                                   sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

#Clean
corporate_positions_clean <- corporate_positions0

for(i in which(sapply(corporate_positions_clean,class)=="character"))
{
  corporate_positions_clean[[i]] <- gsub(" {2,}", " ", corporate_positions_clean[[i]])
  corporate_positions_clean[[i]] <- trim(corporate_positions_clean[[i]])
}
rm(i)

for (i in 1:ncol(corporate_positions_clean))
{
  corporate_positions_clean[,i] <- unknownToNA(corporate_positions_clean[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                        NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                        NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
  corporate_positions_clean[,i] <- ifelse(is.na(corporate_positions_clean[,i]),NA, corporate_positions_clean[,i])
} 
rm(i)

corporate_positions1 <- corporate_positions_clean[!is.na(corporate_positions_clean[,"POSITIONS"]),]
corporate_positions1 <- as.data.frame(corporate_positions1, stringsAsFactors=FALSE)
colnames(corporate_positions1) <- "POSITIONS"
row.names(corporate_positions1) <- seq(nrow(corporate_positions1))

corporate_positions <- corporate_positions1
corporate_positions <- unique(corporate_positions)
corporate_positions <- as.data.frame(corporate_positions, stringsAsFactors=FALSE)
colnames(corporate_positions) <- "POSITIONS"
row.names(corporate_positions) <- seq(nrow(corporate_positions))

rm(corporate_positions0,corporate_positions1,corporate_positions_clean)


###############################################################################
cat("Clean Files \n")
###############################################################################

filings_header_info <- dlply(.data=filings_trim2, .variables=c("yr"), 
                             .fun = function(x, path_output,subfolder,subfolder_output,entity_encoding,letter_beginning,letter_ending,corporate_positions){
                               
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
                               #  corporate_positions <- corporate_positions
                               
                               
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
                               
                               bad_tags <- dlply(.data=downloaded_files3, .variables=c("yr_id"), 
                                                 .fun = function(y,sub_folder_output_path,entity_encoding,letter_beginning,letter_ending,corporate_positions){
                                                   
                                                   
                                                   # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000072760-03-000038.csv"),]
                                                   # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000088053-03-000790.csv"),]
                                                   # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000820027-03-000786.csv"),]
                                                   # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000950136-03-003115.csv"),]
                                                   # y <- downloaded_files3[(downloaded_files3[,"file"]=="0001017062-03-000375.csv"),]
                                                   # y <- downloaded_files3[(downloaded_files3[,"file"]=="0000811860-04-000012.csv"),]
                                                   
                                                   #entity_encoding <- entity_encoding
                                                   #  letter_beginning <- letter_beginning
                                                   #  letter_ending <- letter_ending
                                                   #  corporate_positions <- corporate_positions
                                                   
                                                   file <- unique(y[,"file"])
                                                   filepath <- unique(y[,"filepath"])
                                                   
                                                   file_out <- gsub(".txt",".csv",file)
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
                                                   
                                                   #Clean Tags
                                                   filing_text_clean <- filing_text1
                                                   
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
                                                   
                                                   #SUBSTITUTE
                                                   filing_text_sub <- filing_text_clean
                                                   filing_text_sub[,c("TEXT")] <- gsub("CO OWNER", "COOWNER", filing_text_sub[,c("TEXT")])
                                                   filing_text_sub[,c("TEXT")] <- gsub("CO-OWNER", "COOWNER", filing_text_sub[,c("TEXT")])
                                                   filing_text_sub[,c("TEXT")] <- gsub("DEPUTY PRESIDENT", "DEPUTY-PRESIDENT", filing_text_sub[,c("TEXT")])
                                                   filing_text_sub[,c("TEXT")] <- gsub("NON EXECUTIVE", "NON-EXECUTIVE", filing_text_sub[,c("TEXT")])
                                                   filing_text_sub[,c("TEXT")] <- gsub("SOLE PROPRIETOR", "SOLE-PROPRIETOR", filing_text_sub[,c("TEXT")])
                                                   filing_text_sub[,c("TEXT")] <- gsub("VICE CHAIR", "VICE-CHAIR", filing_text_sub[,c("TEXT")])
                                                   filing_text_sub[,c("TEXT")] <- gsub("VICE CHAIRMAN", "VICE-CHAIRMAN", filing_text_sub[,c("TEXT")])
                                                   filing_text_sub[,c("TEXT")] <- gsub("VICE PRESIDENT", "VICE-PRESIDENT", filing_text_sub[,c("TEXT")])
                                                   filing_text_sub[,c("TEXT")] <- gsub("\\(S\\)", "S", filing_text_sub[,c("TEXT")])
                                                   rm(filing_text_clean)
                                                   
                                                   #COLLAPSE AT DOUBLE /N's
                                                   filing_text_collapse <- filing_text_sub
                                                   rm(filing_text_sub)
                                                   
                                                   
                                                   #EXTRACT LETTER
                                                   filing_text_letter <- ddply(.data=filing_text_collapse, .variables=c("file","DOCUMENT_INDEX"), 
                                                                               .fun = function(x,bycol,xmlcol,letter_beginning,letter_ending,corporate_positions){
                                                                                 
                                                                                 # x <- filing_text_collapse[filing_text_collapse[,"DOCUMENT_INDEX"]==1,]
                                                                                 # bycol <- c("file","DOCUMENT_INDEX")
                                                                                 # xmlcol <- "TEXT"
                                                                                 
                                                                                 file_temp <- unique(x[,"file"])
                                                                                 index_temp <- unique(x[,"DOCUMENT_INDEX"])
                                                                                 
                                                                                 filing_text_letter0 <- data.frame(x,text_trim=NA,index_temp=NA,letter_open=NA,letter_close=NA,stringsAsFactors=FALSE)
                                                                                 colnames(filing_text_letter0)[match("text_trim",names(filing_text_letter0))] <- paste(xmlcol,"TRIM",sep="_")
                                                                                 colnames(filing_text_letter0)[match("index_temp",names(filing_text_letter0))] <- "LETTER_INDEX"
                                                                                 
                                                                                 filing_text_letter0[,c(paste(xmlcol,"TRIM",sep="_"))] <-  filing_text_letter0[,xmlcol] 
                                                                                 filing_text_letter0[,c(paste(xmlcol,"TRIM",sep="_"))] <- gsub(" {2,}", " ",filing_text_letter0[,c(paste(xmlcol,"TRIM",sep="_"))])
                                                                                 filing_text_letter0[,c(paste(xmlcol,"TRIM",sep="_"))]  <- gsub("^\\s+|\\s+$", "", filing_text_letter0[,c(paste(xmlcol,"TRIM",sep="_"))])
                                                                                 
                                                                                 
                                                                                 
                                                                                 filing_text_letter0[,"letter_open"] <- ifelse(grepl("DEAR ", filing_text_letter0[,c(paste(xmlcol,"TRIM",sep="_"))],ignore.case = TRUE), 1, filing_text_letter0[,"letter_open"])
                                                                                 
                                                                                 filing_text_letter0[,"letter_close"] <- ifelse(grepl("SINCERELY", filing_text_letter0[,c(paste(xmlcol,"TRIM",sep="_"))],ignore.case = TRUE), 1, filing_text_letter0[,"letter_close"])
                                                                                 filing_text_letter0[,"letter_close"] <- ifelse(grepl("RESPECTFULLY", filing_text_letter0[,c(paste(xmlcol,"TRIM",sep="_"))],ignore.case = TRUE), 1, filing_text_letter0[,"letter_close"])
                                                                                 
                                                                                 
                                                                                 filing_text_letter0[,"letter_open"] <- ifelse(is.na(filing_text_letter0[,"letter_open"]), 0, filing_text_letter0[,"letter_open"])
                                                                                 filing_text_letter0[,"letter_close"] <- ifelse(is.na(filing_text_letter0[,"letter_close"]), 0, filing_text_letter0[,"letter_close"])
                                                                                 
                                                                                 letter_flag_open1 <- sum(filing_text_letter0[,"letter_open"], na.rm = TRUE)
                                                                                 letter_flag_close1 <- sum(filing_text_letter0[,"letter_close"], na.rm = TRUE)
                                                                                 
                                                                                 
                                                                                 if(letter_flag_open1==letter_flag_close1) {
                                                                                   
                                                                                   #cat("HTML DOCUMENT","\n")
                                                                                   
                                                                                   filing_text_letter0[,"LETTER_INDEX"] <- ifelse(filing_text_letter0[,"letter_open"]!=0, "<LETTER>", filing_text_letter0[,"LETTER_INDEX"])
                                                                                   filing_text_letter0[,"LETTER_INDEX"] <- ifelse(filing_text_letter0[,"letter_close"]!=0, "</LETTER>", filing_text_letter0[,"LETTER_INDEX"])
                                                                                   
                                                                                   tags_extract <- "LETTER"
                                                                                   index_extract_temp <- llply(.data=tags_extract, create_tag_index,data=filing_text_letter0, tag_raw_col="LETTER_INDEX",
                                                                                                               .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
                                                                                   index_extract <- do.call(cbind, index_extract_temp)
                                                                                   index_extract <- as.data.frame(index_extract,stringsAsFactors=FALSE)
                                                                                   colnames(index_extract) <- paste(tags_extract,"INDEX",sep="_")
                                                                                   colnames(index_extract) <- gsub("-","_", colnames(index_extract))
                                                                                   rm(index_extract_temp)
                                                                                   
                                                                                   filing_text_letter0[,"LETTER_INDEX"] <- index_extract
                                                                                   rm(tags_extract,index_extract)
                                                                                   
                                                                                   if(c("LETTER_INDEX") %in% colnames(filing_text_letter0)) {
                                                                                     
                                                                                     filing_text_letter_trim1 <- filing_text_letter0[!(filing_text_letter0[,c("LETTER_INDEX")]==0),]
                                                                                     
                                                                                   } else {
                                                                                     
                                                                                     cat("NO LETTER SECTION","\n")
                                                                                     
                                                                                     filing_text_letter_trim1 <- filing_text_letter0
                                                                                     
                                                                                   } 
                                                                                   
                                                                                 }  else {
                                                                                   
                                                                                   cat("OPEN/CLOSING TAG MISMATCH","\n")
                                                                                   
                                                                                   filing_text_letter_trim1 <- filing_text_letter0
                                                                                   
                                                                                 }
                                                                                 
                                                                                 #filing_text_letter_trim2 <- filing_text_letter_trim1[,!(colnames(filing_text_letter_trim1) %in% c("LETTER_INDEX","letter_open","letter_close",paste(xmlcol,"TRIM",sep="_")))]
                                                                                 filing_text_letter_trim2 <- filing_text_letter_trim1[,!(colnames(filing_text_letter_trim1) %in% c("letter_open","letter_close",paste(xmlcol,"TRIM",sep="_")))]
                                                                                 
                                                                                 rm(filing_text_letter0,filing_text_letter_trim1)
                                                                                 rm(file_temp,index_temp,letter_flag_open1,letter_flag_close1)
                                                                                 
                                                                                 return(filing_text_letter_trim2)
                                                                                 
                                                                               }, bycol=c("file","DOCUMENT_INDEX"),xmlcol="TEXT",
                                                                               letter_beginning=letter_beginning,letter_ending=letter_ending,corporate_positions=corporate_positions,
                                                                               .progress = "none", .inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)
                                                   
                                                   rm(filing_text_collapse)
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   #CLEAN TEXT
                                                   filing_text_parse_clean <- ddply(.data=filing_text_letter, .variables=c("file","DOCUMENT_INDEX","LETTER_INDEX"), 
                                                                                    .fun = function(x,bycol,xmlcol){
                                                                                      
                                                                                      # x <- filing_text_letter[filing_text_letter[,"DOCUMENT_INDEX"]==1,]
                                                                                      # bycol <- c("file","DOCUMENT_INDEX")
                                                                                      # xmlcol <- "TEXT"
                                                                                      
                                                                                      file_temp <- unique(x[,"file"])
                                                                                      index_temp <- unique(x[,"DOCUMENT_INDEX"])
                                                                                      letter_temp <- unique(x[,"LETTER_INDEX"])
                                                                                      
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
                                                                                      
                                                                                      
                                                                                      x_trim <- x[!(x[,xmlcol]==""),]
                                                                                      
                                                                                      #text_collapse <- paste(x_trim[,xmlcol], collapse = " ")     
                                                                                      text_collapse <- paste(x_trim[,xmlcol], collapse = "\n")
                                                                                      rm(x_trim)
                                                                                      
                                                                                      text_collapse2 <- data.frame(temp_file=NA,temp_index=NA,temp_letter=NA,text_collapse,stringsAsFactors=FALSE)
                                                                                      text_collapse2[,"temp_file"] <- file_temp
                                                                                      text_collapse2[,"temp_index"] <- index_temp
                                                                                      text_collapse2[,"temp_letter"] <- letter_temp
                                                                                      colnames(text_collapse2) <- c(bycol,xmlcol)
                                                                                      
                                                                                      rm(file_temp,index_temp,text_collapse)
                                                                                      
                                                                                      return(text_collapse2)
                                                                                      
                                                                                    }, bycol=c("file","DOCUMENT_INDEX","LETTER_INDEX"),xmlcol="TEXT",
                                                                                    .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                                                   
                                                   rm(filing_text_letter)
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   #CLEAN EMPTY ROWS
                                                   filing_text_parse_clean <- data.frame(filing_text_parse,bad_row=NA,stringsAsFactors=FALSE)
                                                   rm(filing_text_parse)
                                                   
                                                   filing_text_parse_clean[,"bad_row"] <- ifelse(grepl("^\\s*$", filing_text_parse_clean[,"TEXT"]), 1, 0)
                                                   
                                                   filing_text_parse_clean_trim <- filing_text_parse_clean[!(filing_text_parse_clean[,"bad_row"]==1),]
                                                   filing_text_parse_clean_trim <- filing_text_parse_clean_trim[,!(colnames(filing_text_parse_clean_trim) %in% c("bad_row"))]
                                                   row.names(filing_text_parse_clean_trim) <- seq(nrow(filing_text_parse_clean_trim))
                                                   rm(filing_text_parse_clean)
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   webpage_sep_index2_comb <- merge(webpage_sep_filings_keep_trim, filing_text_trim, 
                                                                                    by.x=c("file","DOCUMENT_INDEX"), 
                                                                                    by.y=c("file","DOCUMENT_INDEX"), 
                                                                                    all.x=TRUE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
                                                   
                                                   rm(webpage_sep_filings_keep_trim,filing_text_trim)
                                                   
                                                   
                                                   webpage_sep_index2_comb_trim <- webpage_sep_index2_comb[,!(colnames(webpage_sep_index2_comb) %in% c("file"))]
                                                   rm(webpage_sep_index2_comb)
                                                   
                                                   write.table(webpage_sep_index2_comb_trim,file=filepath_out, append=FALSE, na="", 
                                                               sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
                                                   
                                                   df_comb_list <- list(webpage_sep_index2_no_text1_tags_bad)
                                                   #df_comb_list <- list(webpage_sep_index2_no_text1_tags_bad,webpage_sep_index2_comb_trim)
                                                   
                                                   rm(webpage_sep_index2_no_text1_tags_bad,webpage_sep_index2_comb_trim)
                                                   rm(file,filepath)
                                                   rm(sep_tags1,sep_tags2)
                                                   
                                                   return(df_comb_list)
                                                   
                                                 },
                                                 sub_folder_output_path=sub_folder_output_path,entity_encoding=entity_encoding,filetype=filetype,
                                                 letter_beginning=letter_beginning,letter_ending=letter_ending,corporate_positions=corporate_positions,
                                                 .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
                               
                               
                               bad_files_comb0 <- sapply(bad_tags, "[", 1)
                               #bad_files_comb1 <- do.call(rbind,bad_files_comb0)
                               bad_files_comb1 <- rbindlist(bad_files_comb0,fill=TRUE,use.names=TRUE)
                               bad_files_comb  <- as.data.frame(bad_files_comb1,stringsAsFactors=FALSE) 
                               rm(bad_files_comb0,bad_files_comb1)
                               
                               bad_files_comb_final <- data.frame(yr=NA,bad_files_comb,stringsAsFactors=FALSE) 
                               bad_files_comb_final[,"yr"] <- yr
                               rm(bad_files_comb)
                               
                               df_comb_list_all <- list(bad_files_comb_final)
                               
                               rm(bad_files_comb_final)
                               rm(yr,yr_folder_path,sub_folder_path,sub_folder_output_path,downloaded_files3)
                               
                               return(df_comb_list_all)
                               
                             },
                             path_output=paste(output_directory,downloadfolder,sep=slash),
                             subfolder=txtfolder_clean,subfolder_output=txtfolder_section,entity_encoding=entity_encoding,
                             letter_beginning=letter_beginning,letter_ending=letter_ending,corporate_positions=corporate_positions,
                             .progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
