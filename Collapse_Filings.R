# TODO: Add comment
# 
# Author:  Brad
# File:    Collapse_Filings.R
# Version: 1.0
# Date:    08.06.2014
# Purpose: Collapse Filings into One Row Per Filing
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
#startyear <- 2003

#Last year you want index files for:
#endyear <- 2004
#endyear <- 2013

#First qtr you want index files for (usually 1):
#startqtr <- 1

#Last qtr you want index files for (usually 4):
#endqtr <- 4

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
txtfolder_in <- "letter"

#The sub directory where the output filings will go
txtfolder_out <- "letter"

#The file that will contain the filings you want to download.
#infile <- "filings_list_comb.csv"

#yr_qtr_comb <- expand.grid(yr = seq(startyear, endyear, 1), qtr = seq(1, 4, 1))

#yr_qtr_comb <- yr_qtr_comb[order(yr_qtr_comb[,"yr"],yr_qtr_comb[,"qtr"]),]
#row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

#yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==startyear & yr_qtr_comb[,"qtr"] < startqtr),NA,yr_qtr_comb[,"qtr"])
#yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==endyear & yr_qtr_comb[,"qtr"] > endqtr),NA,yr_qtr_comb[,"qtr"])

#rm(startyear,startqtr,endyear,endqtr)

#yr_qtr_comb <- yr_qtr_comb[(!is.na(yr_qtr_comb[,"qtr"])),]
#row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

#yr_qtr_comb2 <- data.frame(yr_qtr_comb,yr_qtr=NA,stringsAsFactors=FALSE)

#rm(yr_qtr_comb)

#yr_qtr_comb2[,"yr_qtr"] <- paste(yr_qtr_comb2[,"yr"],yr_qtr_comb2[,"qtr"],sep="_")

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)

#Check to see if download folder exists.  If not, create it.
download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(download_folder_path,remove=1)

#Check to see if output folder exists.  If not, create it.
out_folder_path <- paste(download_folder_path, txtfolder_out, sep = "\\", collapse = "\\")   
create_directory(out_folder_path,remove=1)

###############################################################################
cat("Input Letters \n")
###############################################################################

letters <- read.table(file=paste(out_folder_path,"\\","letter_all_comb",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

###############################################################################
cat("Clean Files \n")
###############################################################################

xmlcol <- "TEXT"
xmltrim_col <- "TEXT_TRIM"

letters_trim <- letters[,!(colnames(letters) %in% c(xmltrim_col))]

#CONVERT TO ASCII ENCODING
letters_trim[,c(xmlcol)] <- iconv(letters_trim[,c(xmlcol)], "latin1", "ASCII", sub=" ")


###############################################################################
cat("Collaspe Files \n")
###############################################################################

#CLEAN TEXT
letters_collapse <- ddply(.data=letters_trim, .variables=c("yr","file","DOCUMENT_INDEX","LETTER_INDEX"), 
                          .fun = function(x,bycol,xmlcol){
                            
                            # x <- letters_trim[(letters_trim[,"file"]=="0000949377-03-000772.csv" & letters_trim[,"DOCUMENT_INDEX"]==1 & letters_trim[,"LETTER_INDEX"]==2 ),]
                            # bycol <- c("yr","file","DOCUMENT_INDEX","LETTER_INDEX")
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
                          .progress = "text", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)




###############################################################################
cat("Check for strings that are too long \n")
###############################################################################

letters_collapse <- letters_collapse[order(letters_collapse[,"yr"],
                                           letters_collapse[,"file"],
                                           letters_collapse[,"DOCUMENT_INDEX"],
                                           letters_collapse[,"LETTER_INDEX"]),]
row.names(letters_collapse) <- seq(nrow(letters_collapse))

letters_collapse_length <- data.frame(letters_collapse,char_length=NA,stringsAsFactors=FALSE)
letters_collapse_length[,"char_length"] <- nchar(letters_collapse_length[,xmlcol])

letters_collapse_blanks <- letters_collapse_length[letters_collapse_length[,"char_length"] == 0,]

letters_collapse_good <- letters_collapse_length[(letters_collapse_length[,"char_length"] >=1 
                                                 & letters_collapse_length[,"char_length"] < 32767),]
letters_collapse_bad <- letters_collapse_length[letters_collapse_length[,"char_length"] >= 32767,]


###############################################################################
cat("Output Collpased Files \n")
###############################################################################

write.table(letters_collapse_blanks,file=paste(out_folder_path,"\\","letters_collapse_blanks",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letters_collapse_good,file=paste(out_folder_path,"\\","letters_collapse_good",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letters_collapse_bad,file=paste(out_folder_path,"\\","letters_collapse_bad",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)




