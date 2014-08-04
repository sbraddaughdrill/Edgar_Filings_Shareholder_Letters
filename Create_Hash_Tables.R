# TODO: Add comment
# 
# Author:  Brad
# File:    Create_Hash_Tables.R
# Version: 1.0
# Date:    07.11.2014
# Purpose: Create Lookup Tables
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
#source(file=paste(function_directory,"functions_text_parse.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)


clean_replacement_lookup <- function(file,cols_to_clean) {
  
  #  file <- contractions0
  #  cols_to_clean <- c("PATTERN","REPLACEMENT")
  
  require(gdata)
  
  #Clean
  letter_temp_clean <- file
  
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
  
  letter_temp1 <- data.frame(letter_temp_clean,stringsAsFactors=FALSE)
  
  
  letter_temp1 <- letter_temp1[letter_temp1[,"INCLUDE"]=="YES",]  
  
  for (i in cols_to_clean)
  {
    # i <- 1
    
    letter_temp1 <- letter_temp1[!is.na(letter_temp1[,i]),]
    letter_temp1 <- letter_temp1[!(letter_temp1[,i]==""),]
    
    letter_temp1[,i] <- gsub(" {2,}", " ", letter_temp1[,i])
    letter_temp1[,i] <- gsub("^\\s+|\\s+$", "", letter_temp1[,i])
    letter_temp1 <- letter_temp1[!(letter_temp1[,i]==""),]
    
  } 
  rm(i)
  
  letter_temp1 <- unique(letter_temp1)
  row.names(letter_temp1) <- seq(nrow(letter_temp1))
  
  letter_temp <- letter_temp1[,!(colnames(letter_temp1) %in% c("INCLUDE"))]
  
  letter_temp <- as.data.frame(letter_temp,stringsAsFactors=FALSE)
  colnames(letter_temp) <- cols_to_clean
  
  rm(letter_temp1,letter_temp_clean)
  
  return(letter_temp)
  
}

clean_strings <- function(file,str_col,backslash_flag,apostrophe_flag) {
  
  #  file <- letter_beginning1
  #  str_col <- "BEGINNINGS"
  
  #  file <- letter_closing1
  #  str_col <- "CLOSINGS"
  
  #  file <- letter_position1
  #  str_col <- "POSITIONS"
  
  #  backslash_flag <- 1
  #  apostrophe_flag <- 1
  
  
  
  require(gdata)
  
  #Clean
  letter_temp_clean <- file
  
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
  
  letter_temp1[,"regex"] <- gsub(",", " ", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub(":", " ", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub("--", "-", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub(" -", "-", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub("- ", "-", letter_temp1[,"regex"])
  
  if(backslash_flag) { letter_temp1[,"regex"] <- gsub("/", " ", letter_temp1[,"regex"]) }
  if(apostrophe_flag) { letter_temp1[,"regex"] <- gsub("'", "", letter_temp1[,"regex"]) }
  
  letter_temp1[,"regex"] <- gsub("\\(", " ", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub("\\)", " ", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub("&", " AND ", letter_temp1[,"regex"])
  
  letter_temp1[,"regex"] <- gsub(" {2,}", " ", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub("^\\s+|\\s+$", "", letter_temp1[,"regex"])
  letter_temp1 <- letter_temp1[!(letter_temp1[,str_col]==""),]
  
  #letter_temp1[,"regex"] <- gsub(" ", ".*", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- gsub(" ", "\\\\s*", letter_temp1[,"regex"])
  letter_temp1[,"regex"] <- ifelse(letter_temp1[,"PAD"]=="YES",paste(" ", letter_temp1[,"regex"], " ", sep=""), letter_temp1[,"regex"])
  
  letter_temp1[,"PRIORITY"] <- NA
  
  letter_temp1 <- unique(letter_temp1)
  row.names(letter_temp1) <- seq(nrow(letter_temp1))
  
  letter_temp1[,"PRIORITY"] <- seq(nrow(letter_temp1))
  
  letter_temp <- letter_temp1[,!(colnames(letter_temp1) %in% c("PAD","INCLUDE"))]
  
  rm(letter_temp1,letter_temp_clean)
  
  return(letter_temp)
  
}

expand_patterns <- function(x){
  
  # x <- punctuation_words1
  # x <- bb0a
  
  x_expand <- sapply(x, rep.int, times=2)
  x_expand <- as.data.frame(x_expand,stringsAsFactors=FALSE)
  
  x_expand[((nrow(x)*1)+1):(nrow(x)*2),"PATTERN"] <- x_expand[1:nrow(x),"REPLACEMENT"]
  
  return(x_expand)
  
}

expand_patterns_full <- function(x){
  
  # x <- shortened_words1
  # x <- bb0a
  
  x_expand <- sapply(x, rep.int, times=3)
  x_expand <- as.data.frame(x_expand,stringsAsFactors=FALSE)
  
  x_expand[((nrow(x)*1)+1):(nrow(x)*2),"PATTERN"] <- x_expand[1:nrow(x),"REPLACEMENT"]
  x_expand[((nrow(x)*2)+1):(nrow(x)*3),"REPLACEMENT"] <- x_expand[1:nrow(x),"PATTERN"]
  
  return(x_expand)
  
}

expand_patterns_all <- function(x){
  
  # x <- shortened_words1
  # x <- bb0a
  
  x_expand <- sapply(x, rep.int, times=4)
  x_expand <- as.data.frame(x_expand,stringsAsFactors=FALSE)
  
  x_expand[((nrow(x)*1)+1):(nrow(x)*2),"PATTERN"] <- x_expand[1:nrow(x),"REPLACEMENT"]
  x_expand[((nrow(x)*2)+1):(nrow(x)*3),"REPLACEMENT"] <- x_expand[1:nrow(x),"PATTERN"]
  
  x_expand[((nrow(x)*3)+1):(nrow(x)*4),"REPLACEMENT"] <- x_expand[1:nrow(x),"PATTERN"]
  x_expand[((nrow(x)*3)+1):(nrow(x)*4),"PATTERN"] <- x_expand[1:nrow(x),"REPLACEMENT"]
  
  return(x_expand)
  
}

expand_strings <- function(x){
  
  # x <- bb0a[303,]
  # x <- bb0a[417,]
  
  x_expand <- sapply(x, rep.int, times=4)
  
  x_expand[2,"PATTERN"] <- gsub("\\s+","-",x_expand[2,"PATTERN"])
  
  x_expand[3,"PATTERN"] <- gsub("'+","",x_expand[3,"PATTERN"])
  
  x_expand[4,"PATTERN"] <- gsub("\\s+","-",x_expand[4,"PATTERN"])
  x_expand[4,"PATTERN"] <- gsub("'+","",x_expand[4,"PATTERN"])
  
  x_expand[,"REPLACEMENT"] <- x_expand[,"PATTERN"]
  
  x_expand2 <- rbind(x,x_expand)
  
  x_expand2[,"REPLACEMENT"] <- gsub("\\s+","-",x_expand2[,"REPLACEMENT"])
  x_expand2[,"REPLACEMENT"] <- gsub("'+","",x_expand2[,"REPLACEMENT"])
  
  x_expand2 <- x_expand2[(x_expand2[,"REPLACEMENT"] != x_expand2[,"PATTERN"]),]
  x_expand2 <- unique(x_expand2)
  
  x_expand2 <- as.data.frame(x_expand2,stringsAsFactors=FALSE)
  
  return(x_expand2)
  
}

word2num <- function(word){
  
  # word <- "four"
  # word <- "aaaa"
  
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  
  wsplit <- strsplit(tolower(word)," ")[[1]]
  
  while(i <= length(wsplit)){
    j <- 1
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
    rm(j,temp)
  }
  
  rm(i,wsplit,one_digits,teens,ten_digits,doubles)
  
  return(list(word,out))
}


###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","gdata","gsubfn","plyr","qdap","stringr","XML")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(external_packages,installed_packages,repo)


###############################################################################
cat("Import Data \n")
###############################################################################

contractions0 <- read.table(file=paste(input_directory,"contractions.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
abbreviations0 <- read.table(file=paste(input_directory,"abbreviations.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
shortened_words0 <- read.table(file=paste(input_directory,"shortened_words.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
punctuation_words0 <- read.table(file=paste(input_directory,"punctuation_words.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

ordinal_lookup0 <- read.table(file=paste(input_directory,"\\","ordinal_lookup",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

letter_beginning0 <- read.table(file=paste(input_directory,"letter_beginning.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_ending0 <- read.table(file=paste(input_directory,"letter_ending.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_position0 <- read.table(file=paste(input_directory,"letter_position.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_signature0 <- read.table(file=paste(input_directory,"letter_signature.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_closing0 <- read.table(file=paste(input_directory,"letter_closing.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")


###############################################################################
cat("Clean Lookup Data \n")
###############################################################################

contractions1 <- clean_replacement_lookup(file=contractions0,cols_to_clean=c("PATTERN","REPLACEMENT"))
contractions1[,"PATTERN"] <- toupper(contractions1[,c("PATTERN")])
contractions1[,"REPLACEMENT"] <- toupper(contractions1[,c("REPLACEMENT")])
contractions <- contractions1
contractions[,c("PATTERN")] <- paste(" ",contractions[,c("PATTERN")]," ",sep="")
contractions[,c("REPLACEMENT")] <- paste(" ",contractions[,c("REPLACEMENT")]," ",sep="")

abbreviations1 <- clean_replacement_lookup(file=abbreviations0,cols_to_clean=c("PATTERN","REPLACEMENT"))
abbreviations1[,"PATTERN"] <- toupper(abbreviations1[,c("PATTERN")])
abbreviations1[,"REPLACEMENT"] <- toupper(abbreviations1[,c("REPLACEMENT")])
abbreviations <- abbreviations1
abbreviations[,c("PATTERN")] <- paste(" ",abbreviations[,c("PATTERN")]," ",sep="")
abbreviations[,c("REPLACEMENT")] <- paste(" ",abbreviations[,c("REPLACEMENT")]," ",sep="")

shortened_words1 <- clean_replacement_lookup(file=shortened_words0,cols_to_clean=c("PATTERN","REPLACEMENT"))
shortened_words1[,"PATTERN"] <- toupper(shortened_words1[,c("PATTERN")])
shortened_words1[,"REPLACEMENT"] <- toupper(shortened_words1[,c("REPLACEMENT")])
shortened_words <- shortened_words1
shortened_words[,c("PATTERN")] <- paste(" ",shortened_words[,c("PATTERN")]," ",sep="")
shortened_words[,c("REPLACEMENT")] <- paste(" ",shortened_words[,c("REPLACEMENT")]," ",sep="")

punctuation_words1 <- clean_replacement_lookup(file=punctuation_words0,cols_to_clean=c("WORD"))
punctuation_words1[,"WORD"] <- toupper(punctuation_words1[,c("WORD")])
punctuation_words <- punctuation_words1
punctuation_words[,c("WORD")] <- paste(" ",punctuation_words[,c("WORD")]," ",sep="")

ordinal_lookup1 <- clean_replacement_lookup(file=ordinal_lookup0,cols_to_clean=c("PATTERN","REPLACEMENT"))
ordinal_lookup1[,"PATTERN"] <- toupper(ordinal_lookup1[,c("PATTERN")])
ordinal_lookup1[,"REPLACEMENT"] <- toupper(ordinal_lookup1[,c("REPLACEMENT")])
ordinal_lookup <- ordinal_lookup1
ordinal_lookup[,c("PATTERN")] <- paste(" ",ordinal_lookup[,c("PATTERN")]," ",sep="")
ordinal_lookup[,c("REPLACEMENT")] <- paste(" ",ordinal_lookup[,c("REPLACEMENT")]," ",sep="")

rm(abbreviations0,contractions0,shortened_words0,punctuation_words0,ordinal_lookup0)
#rm(abbreviations1,contractions1,shortened_words1,punctuation_words1,ordinal_lookup1)


###############################################################################
cat("Expand Abbreviations \n")
###############################################################################

abbreviations1_expand0 <- expand_patterns(abbreviations1)
abbreviations1_expand0[,"PATTERN"] <- gsub("-"," ",abbreviations1_expand0[,"PATTERN"])
abbreviations1_expand0 <- unique(abbreviations1_expand0)
abbreviations1_expand0 <- abbreviations1_expand0[order(abbreviations1_expand0[,"PATTERN"],abbreviations1_expand0[,"REPLACEMENT"]),]

abbreviations1_expand1 <- expand_patterns(abbreviations1_expand0)
abbreviations1_expand1[,"REPLACEMENT"] <- gsub(" ","-",abbreviations1_expand1[,"REPLACEMENT"])
abbreviations1_expand1 <- unique(abbreviations1_expand1)
abbreviations1_expand1 <- abbreviations1_expand1[order(abbreviations1_expand1[,"PATTERN"],abbreviations1_expand1[,"REPLACEMENT"]),]

abbreviations1_expand2 <- expand_patterns(abbreviations1_expand1)
abbreviations1_expand2[,"PATTERN"] <- gsub("-"," ",abbreviations1_expand2[,"PATTERN"])
abbreviations1_expand2[,"REPLACEMENT"] <- gsub(" ","-",abbreviations1_expand2[,"REPLACEMENT"])
abbreviations1_expand2 <- unique(abbreviations1_expand2)
abbreviations1_expand2 <- abbreviations1_expand2[order(abbreviations1_expand2[,"PATTERN"],abbreviations1_expand2[,"REPLACEMENT"]),]

#abbreviations1_expand <- rbind(abbreviations1_expand0,abbreviations1_expand1)
abbreviations1_expand <- rbindlist(l=list(abbreviations1_expand1,abbreviations1_expand2), use.names=TRUE, fill=FALSE)
abbreviations1_expand <- as.data.frame(abbreviations1_expand,stringsAsFactors=FALSE)

abbreviations1_expand <- unique(abbreviations1_expand)
abbreviations1_expand <- abbreviations1_expand[(abbreviations1_expand[,"REPLACEMENT"] != abbreviations1_expand[,"PATTERN"]),]
abbreviations1_expand <- abbreviations1_expand[order(abbreviations1_expand[,"PATTERN"],abbreviations1_expand[,"REPLACEMENT"]),]
row.names(abbreviations1_expand) <- seq(nrow(abbreviations1_expand))

abbreviations_expand <- abbreviations1_expand
abbreviations_expand[,c("PATTERN")] <- paste(" ",abbreviations_expand[,c("PATTERN")]," ",sep="")
abbreviations_expand[,c("REPLACEMENT")] <- paste(" ",abbreviations_expand[,c("REPLACEMENT")]," ",sep="")

rm(abbreviations1_expand0,abbreviations1_expand1,abbreviations1_expand2)


###############################################################################
cat("Expand Punctuation \n")
###############################################################################

punct_u0 <- punctuation_words1

punct_u0[,"WORD"] <- gsub("[^[:punct:]]+","",punct_u0[,"WORD"])
#punct_u0[,"WORD"] <- gsub("[[:alpha:]]+","",punct_u0[,"WORD"])
#punct_u0[,"WORD"] <- gsub("[[:digit:]]+","",punct_u0[,"WORD"])

punct_u1 <- unique(punct_u0[,"WORD"])
punct_u2 <- paste(punct_u1,sep="",collapse=" ")

punct_u3 <- unlist(strsplit(punct_u2, "*"))

punct_u4 <- unique(punct_u3)

rm(punct_u0,punct_u1,punct_u2,punct_u3)

punct_u_trim0 <- punct_u4
punct_u_trim0 <- gsub(" {2,}", " ",punct_u_trim0)
punct_u_trim0 <- gsub("^\\s+|\\s+$", "", punct_u_trim0)
punct_u_trim0 <- gsub(" ", "", punct_u_trim0)

punct_u_trim <- punct_u_trim0[punct_u_trim0!=""]

punct_u_expand <- expand.grid(rep(list(punct_u_trim), length(punct_u_trim)),KEEP.OUT.ATTRS = TRUE, stringsAsFactors = FALSE)

i <- sapply(punct_u_expand, is.factor)
punct_u_expand[i] <- lapply(punct_u_expand[i], as.character)
rm(i)


#punct_u_expand_id <- data.frame(ID=NA,punct_u_expand,stringsAsFactors=FALSE)
#punct_u_expand_id[,"ID"] <- seq(1,nrow(punct_u_expand_id),1)
punct_u_expand_id <- punct_u_expand

rm(punct_u_expand)

cols    <- names(punct_u_expand_id)[grepl('^Var[[:digit:]]+$',names(punct_u_expand_id))]
cols_str <- paste(cols,sep="",collapse=",")
paste_str <- paste("paste(",cols_str,",sep='')")

#punct_u_expand_dt <- data.table(punct_u_expand_id, key="ID")
punct_u_expand_dt <- data.table(punct_u_expand_id)

rm(punct_u_expand_id)

temp_col <- "col_collapse"
myexp <- parse(text=paste0("list(",temp_col,"=",paste_str,")"))

punct_u_expand_dt2 <- punct_u_expand_dt[, eval(myexp), by=1:nrow(punct_u_expand_dt)]

rm(cols,cols_str,paste_str)
rm(myexp,punct_u_expand_dt)

temp_col2 <- "col_trim"
myexp2 <- parse(text=paste0("list(",temp_col2,"=","paste(sort(unique(strsplit(",temp_col,", NULL)[[1]])),collapse='|')",")"))

punct_u_expand_dt3 <- punct_u_expand_dt2[, eval(myexp2), by=1:nrow(punct_u_expand_dt2)]

rm(temp_col,punct_u_expand_dt2)

punct_u_expand_trim <- as.data.frame(punct_u_expand_dt3,stringsAsFactors=FALSE)
punct_u_expand_trim2 <- unique(punct_u_expand_trim[,temp_col2])

rm(temp_col2,myexp2,punct_u_expand_dt3,punct_u_expand_trim)

punct_u_expand_trim2 <- as.data.frame(punct_u_expand_trim2,stringsAsFactors=FALSE)
colnames(punct_u_expand_trim2) <- "patterns"

punct_u_expand_trim2 <- punct_u_expand_trim2[order(punct_u_expand_trim2[,"patterns"]),]

punct_u_expand_trim3 <- data.frame(PATTERN=c("",punct_u_expand_trim2),REPLACEMENT_blank=NA,REPLACEMENT_space=NA,stringsAsFactors=FALSE)

punct_u_expand_trim3[,"REPLACEMENT_blank"] <- punct_u_expand_trim3[,"PATTERN"]
punct_u_expand_trim3[,"REPLACEMENT_blank"] <- paste('gsub("[',punct_u_expand_trim3[,"PATTERN"],']","",x)',sep="")

punct_u_expand_trim3[,"REPLACEMENT_space"] <- punct_u_expand_trim3[,"PATTERN"]
punct_u_expand_trim3[,"REPLACEMENT_space"] <- paste('gsub("[',punct_u_expand_trim3[,"PATTERN"],']"," ",x)',sep="")

punct_u_expand_trim3[,"REPLACEMENT_blank"] <- ifelse(punct_u_expand_trim3[,"PATTERN"]=="","gsub('','',x)",punct_u_expand_trim3[,"REPLACEMENT_blank"])
punct_u_expand_trim3[,"REPLACEMENT_space"] <- ifelse(punct_u_expand_trim3[,"PATTERN"]=="","gsub('','',x)",punct_u_expand_trim3[,"REPLACEMENT_space"])

rm(punct_u_expand_trim2)

punctuation_words1_df <- data.frame(PATTERN=punctuation_words1[,"WORD"],REPLACEMENT=NA,stringsAsFactors=FALSE)
punctuation_words1_df[,"REPLACEMENT"] <- punctuation_words1_df[,"PATTERN"]

punctuation_words1_df_expand <- expand_patterns(punctuation_words1_df)
punctuation_words1_df_expand <- unique(punctuation_words1_df_expand)
punctuation_words1_df_expand <- punctuation_words1_df_expand[order(punctuation_words1_df_expand[,"REPLACEMENT"],punctuation_words1_df_expand[,"PATTERN"]),]
row.names(punctuation_words1_df_expand) <- seq(nrow(punctuation_words1_df_expand))

punctuation_words1_df_expand_pad <- punctuation_words1_df_expand
punctuation_words1_df_expand_pad[,"REPLACEMENT"] <- paste(" ",punctuation_words1_df_expand_pad[,"REPLACEMENT"]," ",sep="")
punctuation_words1_df_expand_pad[,"PATTERN"] <- paste(" ",punctuation_words1_df_expand_pad[,"PATTERN"]," ",sep="")

rm(punctuation_words1_df)


###############################################################################
cat("REPLACE BRACKETS \n")
###############################################################################

#punctuation_words_bracket_replace0 <- punctuation_words_contraction_replace
#punctuation_words_bracket_replace1 <- punctuation_words_contraction_replace

punctuation_words_bracket_replace0 <- punctuation_words1_df_expand_pad
punctuation_words_bracket_replace1 <- punctuation_words1_df_expand_pad

punctuation_words_bracket_replace1[,c("REPLACEMENT")] <- bracketX(clean(text.var=punctuation_words_bracket_replace1[,c("REPLACEMENT")]), bracket = "all", missing = "", names = FALSE)

punctuation_words_bracket_replace1[,c("REPLACEMENT")] <- toupper(punctuation_words_bracket_replace1[,c("REPLACEMENT")])
punctuation_words_bracket_replace1[,c("REPLACEMENT")] <- paste(" ",punctuation_words_bracket_replace1[,c("REPLACEMENT")]," ",sep="")

punctuation_words_bracket_replace <- expand_patterns(punctuation_words_bracket_replace1)

punctuation_words_bracket_replace <- unique(punctuation_words_bracket_replace)
punctuation_words_bracket_replace <- as.data.frame(punctuation_words_bracket_replace,stringsAsFactors=FALSE)
punctuation_words_bracket_replace <- punctuation_words_bracket_replace[order(punctuation_words_bracket_replace[,"PATTERN"],punctuation_words_bracket_replace[,"REPLACEMENT"]),]
row.names(punctuation_words_bracket_replace) <- seq(nrow(punctuation_words_bracket_replace))

rm(punctuation_words_bracket_replace0,punctuation_words_bracket_replace1)


###############################################################################
cat("Convert ordinal numbers to numeric \n")
###############################################################################

#punctuation_words_ordinal_replace0 <- punctuation_words1_df_expand_pad
#punctuation_words_ordinal_replace1 <- punctuation_words1_df_expand_pad

punctuation_words_ordinal_replace0 <- punctuation_words_bracket_replace
punctuation_words_ordinal_replace1 <- punctuation_words_bracket_replace

punctuation_words_ordinal_replace1 <- data.table(punctuation_words_ordinal_replace1)

for(k in 1:nrow(ordinal_lookup1))
{
  # k <- 1
  
  set(punctuation_words_ordinal_replace1, i=NULL, j="REPLACEMENT", value=gsub(ordinal_lookup1[k,c("PATTERN")], ordinal_lookup1[k,c("REPLACEMENT")], punctuation_words_ordinal_replace1[["REPLACEMENT"]], ignore.case = TRUE, perl=TRUE))
  
}
rm(k)


punctuation_words_ordinal_replace1 <- as.data.frame(punctuation_words_ordinal_replace1,stringsAsFactors=FALSE)

#punctuation_words_ordinal_replace <- rbindlist(l=list(punctuation_words_ordinal_replace0,punctuation_words_ordinal_replace1), use.names=TRUE, fill=FALSE)
punctuation_words_ordinal_replace <- expand_patterns(punctuation_words_ordinal_replace1)

punctuation_words_ordinal_replace <- unique(punctuation_words_ordinal_replace)
punctuation_words_ordinal_replace <- as.data.frame(punctuation_words_ordinal_replace,stringsAsFactors=FALSE)
punctuation_words_ordinal_replace <- punctuation_words_ordinal_replace[order(punctuation_words_ordinal_replace[,"PATTERN"],punctuation_words_ordinal_replace[,"REPLACEMENT"]),]
row.names(punctuation_words_ordinal_replace) <- seq(nrow(punctuation_words_ordinal_replace))

rm(punctuation_words_ordinal_replace0,punctuation_words_ordinal_replace1)


###############################################################################
cat("REPLACE NUMBER \n")
###############################################################################

punctuation_words_num_replace0 <- punctuation_words_ordinal_replace
punctuation_words_num_replace1 <- punctuation_words_ordinal_replace

#punctuation_words_num_replace1[,c("REPLACEMENT")] <- gsub("0","ZERO",punctuation_words_num_replace1[,c("REPLACEMENT")])
punctuation_words_num_replace1[,c("REPLACEMENT")] <- gsub("[^0-9]+(0)+[^0-9]","ZERO",punctuation_words_num_replace1[,c("REPLACEMENT")])
punctuation_words_num_replace1[,c("REPLACEMENT")] <- replace_number(text.var=punctuation_words_num_replace1[,c("REPLACEMENT")], num.paste = TRUE)

punctuation_words_num_replace1[,c("REPLACEMENT")] <- toupper(punctuation_words_num_replace1[,c("REPLACEMENT")])
punctuation_words_num_replace1[,c("REPLACEMENT")] <- paste(" ",punctuation_words_num_replace1[,c("REPLACEMENT")]," ",sep="")

punctuation_words_num_replace <- expand_patterns(punctuation_words_num_replace1)

punctuation_words_num_replace <- unique(punctuation_words_num_replace)
punctuation_words_num_replace <- as.data.frame(punctuation_words_num_replace,stringsAsFactors=FALSE)
punctuation_words_num_replace <- punctuation_words_num_replace[order(punctuation_words_num_replace[,"PATTERN"],punctuation_words_num_replace[,"REPLACEMENT"]),]
row.names(punctuation_words_num_replace) <- seq(nrow(punctuation_words_num_replace))

rm(punctuation_words_num_replace0,punctuation_words_num_replace1)


###############################################################################
cat("REPLACE CONTRACTIONS \n")
###############################################################################

#contractions_expand0 <- contractions1
#contractions_expand0[,"PATTERN"] <- gsub("'","",contractions_expand0[,"PATTERN"])
#contractions_expand <- rbind(contractions1,contractions_expand0)

#contractions_expand <- unique(contractions_expand)
#contractions_expand <- as.data.frame(contractions_expand,stringsAsFactors=FALSE)
#contractions_expand <- contractions_expand[order(contractions_expand[,"PATTERN"],contractions_expand[,"REPLACEMENT"]),]
#row.names(contractions_expand) <- seq(nrow(contractions_expand))

punctuation_words_contraction_replace0 <- punctuation_words_num_replace
punctuation_words_contraction_replace1 <- punctuation_words_num_replace

punctuation_words_contraction_replace1 <- data.table(punctuation_words_contraction_replace1)

for(k in 1:nrow(contractions1))
{
  # k <- 1
  
  set(punctuation_words_contraction_replace1, i=NULL, j="REPLACEMENT", value=gsub(contractions1[k,c("PATTERN")], contractions1[k,c("REPLACEMENT")], punctuation_words_contraction_replace1[["REPLACEMENT"]], ignore.case = TRUE, perl=TRUE))
   
}
rm(k)


punctuation_words_contraction_replace1 <- as.data.frame(punctuation_words_contraction_replace1,stringsAsFactors=FALSE)

punctuation_words_contraction_replace <- expand_patterns(punctuation_words_contraction_replace1)

punctuation_words_contraction_replace <- unique(punctuation_words_contraction_replace)
punctuation_words_contraction_replace <- as.data.frame(punctuation_words_contraction_replace,stringsAsFactors=FALSE)
punctuation_words_contraction_replace <- punctuation_words_contraction_replace[order(punctuation_words_contraction_replace[,"PATTERN"],punctuation_words_contraction_replace[,"REPLACEMENT"]),]
row.names(punctuation_words_contraction_replace) <- seq(nrow(punctuation_words_contraction_replace))

rm(punctuation_words_contraction_replace0,punctuation_words_contraction_replace1)


# ###############################################################################
# cat("REPLACE BRACKETS \n")
# ###############################################################################
# 
# punctuation_words_bracket_replace0 <- punctuation_words_contraction_replace
# punctuation_words_bracket_replace1 <- punctuation_words_contraction_replace
# 
# punctuation_words_bracket_replace1[,c("REPLACEMENT")] <- bracketX(clean(text.var=punctuation_words_bracket_replace1[,c("REPLACEMENT")]), bracket = "all", missing = "", names = FALSE)
# 
# punctuation_words_bracket_replace1[,c("REPLACEMENT")] <- toupper(punctuation_words_bracket_replace1[,c("REPLACEMENT")])
# punctuation_words_bracket_replace1[,c("REPLACEMENT")] <- paste(" ",punctuation_words_bracket_replace1[,c("REPLACEMENT")]," ",sep="")
# 
# punctuation_words_bracket_replace <- expand_patterns(punctuation_words_bracket_replace1)
# 
# punctuation_words_bracket_replace <- unique(punctuation_words_bracket_replace)
# punctuation_words_bracket_replace <- as.data.frame(punctuation_words_bracket_replace,stringsAsFactors=FALSE)
# punctuation_words_bracket_replace <- punctuation_words_bracket_replace[order(punctuation_words_bracket_replace[,"PATTERN"],punctuation_words_bracket_replace[,"REPLACEMENT"]),]
# row.names(punctuation_words_bracket_replace) <- seq(nrow(punctuation_words_bracket_replace))
# 
# rm(punctuation_words_bracket_replace0,punctuation_words_bracket_replace1)


###############################################################################
cat("REPLACE ABBREVIATIONS \n")
###############################################################################

#punctuation_words_abbreviation_replace0 <- punctuation_words_bracket_replace
#punctuation_words_abbreviation_replace1 <- punctuation_words_bracket_replace

punctuation_words_abbreviation_replace0 <- punctuation_words_contraction_replace
punctuation_words_abbreviation_replace1 <- punctuation_words_contraction_replace

punctuation_words_abbreviation_replace1 <- data.table(punctuation_words_abbreviation_replace1)

for(k in 1:nrow(abbreviations_expand))
{
  # k <- 1
  
  set(punctuation_words_abbreviation_replace1, i=NULL, j="REPLACEMENT", value=gsub(abbreviations_expand[k,c("PATTERN")], abbreviations_expand[k,c("REPLACEMENT")], punctuation_words_abbreviation_replace1[["REPLACEMENT"]], ignore.case = TRUE, perl=TRUE))
  
}
rm(k)

punctuation_words_abbreviation_replace1 <- as.data.frame(punctuation_words_abbreviation_replace1,stringsAsFactors=FALSE)

punctuation_words_abbreviation_replace <- expand_patterns(punctuation_words_abbreviation_replace1)

punctuation_words_abbreviation_replace <- unique(punctuation_words_abbreviation_replace)
punctuation_words_abbreviation_replace <- as.data.frame(punctuation_words_abbreviation_replace,stringsAsFactors=FALSE)
punctuation_words_abbreviation_replace <- punctuation_words_abbreviation_replace[order(punctuation_words_abbreviation_replace[,"PATTERN"],punctuation_words_abbreviation_replace[,"REPLACEMENT"]),]
row.names(punctuation_words_abbreviation_replace) <- seq(nrow(punctuation_words_abbreviation_replace))

rm(punctuation_words_abbreviation_replace0,punctuation_words_abbreviation_replace1,abbreviations_expand)


###############################################################################
cat("REPLACE SYMBOLS \n")
###############################################################################

punctuation_words_symbol_replace0 <- punctuation_words_abbreviation_replace
punctuation_words_symbol_replace1 <- punctuation_words_abbreviation_replace

punctuation_words_symbol_replace1[,c("REPLACEMENT")] <- replace_symbol(text.var=punctuation_words_symbol_replace1[,c("REPLACEMENT")], 
                                                                       dollar = TRUE, percent = TRUE, pound = TRUE,at = TRUE, and = TRUE, with = TRUE)

punctuation_words_symbol_replace1[,c("REPLACEMENT")] <- toupper(punctuation_words_symbol_replace1[,c("REPLACEMENT")])
punctuation_words_symbol_replace1[,c("REPLACEMENT")] <- paste(" ",punctuation_words_symbol_replace1[,c("REPLACEMENT")]," ",sep="")

punctuation_words_symbol_replace <- expand_patterns(punctuation_words_symbol_replace1)

punctuation_words_symbol_replace <- unique(punctuation_words_symbol_replace)
punctuation_words_symbol_replace <- as.data.frame(punctuation_words_symbol_replace,stringsAsFactors=FALSE)
punctuation_words_symbol_replace <- punctuation_words_symbol_replace[order(punctuation_words_symbol_replace[,"PATTERN"],punctuation_words_symbol_replace[,"REPLACEMENT"]),]
row.names(punctuation_words_symbol_replace) <- seq(nrow(punctuation_words_symbol_replace))

rm(punctuation_words_symbol_replace0,punctuation_words_symbol_replace1)


###############################################################################
cat("TRIM AND SCRUB \n")
###############################################################################

punctuation_words_scrub0 <- punctuation_words_symbol_replace
punctuation_words_scrub1 <- punctuation_words_symbol_replace

punctuation_words_scrub1[,c("PATTERN")] <- Trim(scrubber(punctuation_words_scrub1[,c("PATTERN")]))
punctuation_words_scrub1[,c("PATTERN")] <- gsub(" {2,}", " ",punctuation_words_scrub1[,c("PATTERN")])
punctuation_words_scrub1[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "",punctuation_words_scrub1[,c("PATTERN")])

punctuation_words_scrub1[,c("REPLACEMENT")] <- Trim(scrubber(punctuation_words_scrub1[,c("REPLACEMENT")]))
punctuation_words_scrub1[,c("REPLACEMENT")] <- gsub(" {2,}", " ",punctuation_words_scrub1[,c("REPLACEMENT")])
punctuation_words_scrub1[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "",punctuation_words_scrub1[,c("REPLACEMENT")])

punctuation_words_scrub1[,c("REPLACEMENT")] <- gsub("-+", "-",punctuation_words_scrub1[,c("REPLACEMENT")])
punctuation_words_scrub1[,c("REPLACEMENT")] <- gsub("- ", "-",punctuation_words_scrub1[,c("REPLACEMENT")])
punctuation_words_scrub1[,c("REPLACEMENT")] <- gsub(" -", "-",punctuation_words_scrub1[,c("REPLACEMENT")])

punctuation_words_scrub <- expand_patterns(punctuation_words_scrub1)

punctuation_words_scrub[,c("REPLACEMENT")] <- gsub(" ", "-",punctuation_words_scrub[,c("REPLACEMENT")])

punctuation_words_scrub <- unique(punctuation_words_scrub)
punctuation_words_scrub <- as.data.frame(punctuation_words_scrub,stringsAsFactors=FALSE)
punctuation_words_scrub <- punctuation_words_scrub[order(punctuation_words_scrub[,"PATTERN"],punctuation_words_scrub[,"REPLACEMENT"]),]
row.names(punctuation_words_scrub) <- seq(nrow(punctuation_words_scrub))

rm(punctuation_words_scrub0,punctuation_words_scrub1)


###############################################################################
cat("Create Combinations of input datasets \n")
###############################################################################

#punctuation_data <- punctuation_words_num_replace
punctuation_data <- punctuation_words_scrub

contractions_expand1 <- expand_patterns(contractions1)
#contractions_expand <- unique(contractions_expand1[,1])
#contractions_expand <- c(contractions1[,"PATTERN"],contractions1[,"REPLACEMENT"])

#shortened_words_expand1 <- expand_patterns(shortened_words1)
shortened_words_expand1 <- expand_patterns_full(shortened_words1)
#shortened_words_expand <- unique(shortened_words_expand1[,1])
#shortened_words_expand <- c(shortened_words1[,"PATTERN"],shortened_words1[,"REPLACEMENT"])

#contractions_shortened_words_expand1 <- rbind(contractions_expand1,shortened_words_expand1)
contractions_shortened_words_expand1 <- rbindlist(l=list(contractions_expand1,shortened_words_expand1), use.names=TRUE, fill=FALSE)
contractions_shortened_words_expand1 <- as.data.frame(contractions_shortened_words_expand1,stringsAsFactors=FALSE)

rm(contractions_expand1,shortened_words_expand1)

#contractions_shortened_words_expand1[,c("PATTERN")] <- gsub(" {2,}", " ",contractions_shortened_words_expand1[,c("PATTERN")])
#contractions_shortened_words_expand1[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", contractions_shortened_words_expand1[,c("PATTERN")])

#contractions_shortened_words_expand1[,c("REPLACEMENT")] <- gsub(" {2,}", " ",contractions_shortened_words_expand1[,c("REPLACEMENT")])
#contractions_shortened_words_expand1[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", contractions_shortened_words_expand1[,c("REPLACEMENT")])

#all_words_expand1 <- c(punctuation_data[,"WORD"],contractions_shortened_words_expand1[,1])
#all_words_expand1 <- as.data.frame(all_words_expand1,stringsAsFactors=FALSE)
#colnames(all_words_expand1) <- "WORD"

#all_words <- rbindlist(l=list(expand_patterns(punctuation_data),contractions_shortened_words_expand1), use.names=TRUE, fill=FALSE)
all_words <- rbindlist(l=list(punctuation_data,contractions_shortened_words_expand1), use.names=TRUE, fill=FALSE)
all_words <- as.data.frame(all_words,stringsAsFactors=FALSE)


all_words_expand1 <- expand_patterns(all_words)

#punct_expand_blanks0[,c("PATTERN")] <- gsub(" {2,}", " ",punct_expand_blanks0[,c("PATTERN")])
#punct_expand_blanks0[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", punct_expand_blanks0[,c("PATTERN")])

#punct_expand_blanks0[,c("REPLACEMENT")] <- gsub(" {2,}", " ",punct_expand_blanks0[,c("REPLACEMENT")])
#punct_expand_blanks0[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", punct_expand_blanks0[,c("REPLACEMENT")])

#words1 <- as.vector(punctuation_words[,"WORD"])
#words1 <- as.vector(all_words_expand1[,"WORD"])

punct_expand_blanks0 <- ldply(.data=punct_u_expand_trim3[,"REPLACEMENT_blank"], .fun = function(y,data){
  
  # y <- punct_u_expand_trim3[2,"REPLACEMENT_blank"]
  
  require(data.table)
  
  myexp <- parse(text=paste0(y))
  
  data_expand <- sapply(data, rep.int, times=4)
  data_expand <- as.data.frame(data_expand,stringsAsFactors=FALSE)
  
  x <- data_expand[((nrow(data)*1)+1):(nrow(data)*2),"PATTERN"]
  x_replaced <- eval(myexp)
  data_expand[((nrow(data)*1)+1):(nrow(data)*2),"PATTERN"] <- x_replaced
  
  x <- data_expand[((nrow(data)*2)+1):(nrow(data)*3),"REPLACEMENT"]
  x_replaced <- eval(myexp)
  data_expand[((nrow(data)*2)+1):(nrow(data)*3),"REPLACEMENT"] <- x_replaced
  
  x <- data_expand[((nrow(data)*3)+1):(nrow(data)*4),"PATTERN"]
  x_replaced <- eval(myexp)
  data_expand[((nrow(data)*3)+1):(nrow(data)*4),"PATTERN"] <- x_replaced
  
  x <- data_expand[((nrow(data)*3)+1):(nrow(data)*4),"REPLACEMENT"]
  x_replaced <- eval(myexp)
  data_expand[((nrow(data)*3)+1):(nrow(data)*4),"REPLACEMENT"] <- x_replaced
  
  #x_expand2 <- rbind(x,x_expand)
  x_expand2 <- rbindlist(l=list(data,data_expand), use.names=TRUE, fill=FALSE)
  x_expand2 <- as.data.frame(x_expand2,stringsAsFactors=FALSE)
  
  #comb <- data.frame(PATTERN=x_replaced,REPLACEMENT=x,stringsAsFactors=FALSE)
  
  return(x_expand2)
  
}, data=all_words_expand1, .progress = "text", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)

# punct_expand_blanks0 <- adply(.data=punct_u_expand_trim3[,"REPLACEMENT_blank"], .fun = function(y,x){
#   
#   x_expand <- sapply(x, rep.int, times=4)
#   
#   x_expand[2,"PATTERN"] <- gsub("\\s+","-",x_expand[2,"PATTERN"])
#   
#   x_expand[3,"PATTERN"] <- gsub("'+","",x_expand[3,"PATTERN"])
#   
#   x_expand[4,"PATTERN"] <- gsub("\\s+","-",x_expand[4,"PATTERN"])
#   x_expand[4,"PATTERN"] <- gsub("'+","",x_expand[4,"PATTERN"])
#   
#   x_expand[,"REPLACEMENT"] <- x_expand[,"PATTERN"]
#   
#   x_expand2 <- rbind(x,x_expand)
#   
#   
#   myexp <- parse(text=paste0(y))
#   x_replaced <- eval(myexp)
#   comb <- data.frame(PATTERN=x_replaced,REPLACEMENT=x,stringsAsFactors=FALSE)
#   
#   return(comb)
#   
# }, x=words1, .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL, .id = NA)


#punct_expand_blanks0[,c("PATTERN")] <- gsub(" {2,}", " ",punct_expand_blanks0[,c("PATTERN")])
#punct_expand_blanks0[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", punct_expand_blanks0[,c("PATTERN")])

#punct_expand_blanks0[,c("REPLACEMENT")] <- gsub(" {2,}", " ",punct_expand_blanks0[,c("REPLACEMENT")])
#punct_expand_blanks0[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", punct_expand_blanks0[,c("REPLACEMENT")])

#punct_expand_blanks <- rbindlist(l=list(punct_expand_blanks0,contractions_shortened_words_expand1), use.names=TRUE, fill=FALSE)
punct_expand_blanks <- rbindlist(l=list(punct_expand_blanks0,all_words_expand1), use.names=TRUE, fill=FALSE)
punct_expand_blanks <- as.data.frame(punct_expand_blanks,stringsAsFactors=FALSE)

punct_expand_blanks <- unique(punct_expand_blanks)
#punct_expand_blanks <- punct_expand_blanks[order(punct_expand_blanks[,"PATTERN"],punct_expand_blanks[,"REPLACEMENT"]),]
punct_expand_blanks <- punct_expand_blanks[order(punct_expand_blanks[,"REPLACEMENT"],punct_expand_blanks[,"PATTERN"]),]
#punct_expand_blanks <- punct_expand_blanks[(punct_expand_blanks[,"REPLACEMENT"] != punct_expand_blanks[,"PATTERN"]),]
row.names(punct_expand_blanks) <- seq(nrow(punct_expand_blanks))


#words2a <- expand_patterns(punct_expand_blanks)
#words2a <- punct_expand_blanks
#words2b <- as.vector(words2a[,"REPLACEMENT"])

all_words_expand2 <- expand_patterns(punct_expand_blanks)

punct_expand_space0 <- ldply(.data=punct_u_expand_trim3[,"REPLACEMENT_space"], .fun = function(y,data){
  
  # y <- punct_u_expand_trim3[2,"REPLACEMENT_space"]
  
  require(data.table)
  
  myexp <- parse(text=paste0(y))
  
  data_expand <- sapply(data, rep.int, times=4)
  data_expand <- as.data.frame(data_expand,stringsAsFactors=FALSE)
  
  x <- data_expand[((nrow(data)*1)+1):(nrow(data)*2),"PATTERN"]
  x_replaced <- eval(myexp)
  data_expand[((nrow(data)*1)+1):(nrow(data)*2),"PATTERN"] <- x_replaced
  
  x <- data_expand[((nrow(data)*2)+1):(nrow(data)*3),"REPLACEMENT"]
  x_replaced <- eval(myexp)
  data_expand[((nrow(data)*2)+1):(nrow(data)*3),"REPLACEMENT"] <- x_replaced
  
  x <- data_expand[((nrow(data)*3)+1):(nrow(data)*4),"PATTERN"]
  x_replaced <- eval(myexp)
  data_expand[((nrow(data)*3)+1):(nrow(data)*4),"PATTERN"] <- x_replaced
  
  x <- data_expand[((nrow(data)*3)+1):(nrow(data)*4),"REPLACEMENT"]
  x_replaced <- eval(myexp)
  data_expand[((nrow(data)*3)+1):(nrow(data)*4),"REPLACEMENT"] <- x_replaced
  
  #x_expand2 <- rbind(x,x_expand)
  x_expand2 <- rbindlist(l=list(data,data_expand), use.names=TRUE, fill=FALSE)
  x_expand2 <- as.data.frame(x_expand2,stringsAsFactors=FALSE)
  
  #comb <- data.frame(PATTERN=x_replaced,REPLACEMENT=x,stringsAsFactors=FALSE)
  
  return(x_expand2)
  
}, data=all_words_expand2, .progress = "text", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)

#punct_expand_space0[,c("PATTERN")] <- gsub(" {2,}", " ",punct_expand_space0[,c("PATTERN")])
#punct_expand_space0[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", punct_expand_space0[,c("PATTERN")])

#punct_expand_space0[,c("REPLACEMENT")] <- gsub(" {2,}", " ",punct_expand_space0[,c("REPLACEMENT")])
#punct_expand_space0[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", punct_expand_space0[,c("REPLACEMENT")])

punct_expand_space <- rbindlist(l=list(punct_expand_space0,all_words_expand2), use.names=TRUE, fill=FALSE)
punct_expand_space <- as.data.frame(punct_expand_space,stringsAsFactors=FALSE)

punct_expand_space <- unique(punct_expand_space)
punct_expand_space <- punct_expand_space[order(punct_expand_space[,"PATTERN"],punct_expand_space[,"REPLACEMENT"]),]
#punct_expand_space <- punct_expand_space[(punct_expand_space[,"REPLACEMENT"] != punct_expand_space[,"PATTERN"]),]
row.names(punct_expand_space) <- seq(nrow(punct_expand_space))

punct_expand_full <- punct_expand_space

rm(all_words,all_words_expand1,all_words_expand2)
rm(punctuation_data,contractions_shortened_words_expand1)
rm(punct_expand_blanks0,punct_expand_blanks)
rm(punct_expand_space0,punct_expand_space)
rm(punctuation_words_bracket_replace,punctuation_words_ordinal_replace,punctuation_words_num_replace)
rm(punctuation_words_contraction_replace,punctuation_words_abbreviation_replace,punctuation_words_symbol_replace,punctuation_words_scrub)
rm(punctuation_words1_df_expand_pad)


###############################################################################
cat("Substitute Contractions and Shortned Words \n")
###############################################################################

punct_sub_contraction0 <- expand_patterns(punct_expand_full)
#punct_sub_contraction0 <- unique(punct_sub_contraction0)
#row.names(punct_sub_contraction0) <- seq(nrow(punct_sub_contraction0))
punct_sub_contraction_dt <- data.table(punct_sub_contraction0)

punct_sub_contraction_dt[,c("PATTERN")] <- gsub(" {2,}", " ",punct_sub_contraction_dt[,c("PATTERN")])
punct_sub_contraction_dt[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", punct_sub_contraction_dt[,c("PATTERN")])

punct_sub_contraction_dt[,c("REPLACEMENT")] <- gsub(" {2,}", " ",punct_sub_contraction_dt[,c("REPLACEMENT")])
punct_sub_contraction_dt[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", punct_sub_contraction_dt[,c("REPLACEMENT")])

for(k in 1:nrow(contractions1))
{
  # k <- 1
  
  set(punct_sub_contraction_dt, i=NULL, j="REPLACEMENT", value=gsub(contractions1[k,c("PATTERN")], contractions1[k,c("REPLACEMENT")], punct_sub_contraction_dt[["REPLACEMENT"]], perl=TRUE))
  
}
rm(k)

punct_sub_contraction <- rbindlist(l=list(punct_sub_contraction_dt,punct_sub_contraction0), use.names=TRUE, fill=FALSE)
punct_sub_contraction <- as.data.frame(punct_sub_contraction,stringsAsFactors=FALSE)

#punct_sub_contraction <- punct_sub_contraction[order(punct_sub_contraction[,"PATTERN"],punct_sub_contraction[,"REPLACEMENT"]),]

punct_sub_contraction_expand <- expand_patterns(punct_sub_contraction)

#punct_sub_contraction_expand[,c("PATTERN")] <- gsub(" {2,}", " ",punct_sub_contraction_expand[,c("PATTERN")])
#punct_sub_contraction_expand[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", punct_sub_contraction_expand[,c("PATTERN")])

#punct_sub_contraction_expand[,c("REPLACEMENT")] <- gsub(" {2,}", " ",punct_sub_contraction_expand[,c("REPLACEMENT")])
#punct_sub_contraction_expand[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", punct_sub_contraction_expand[,c("REPLACEMENT")])

punct_sub_contraction_expand <- unique(punct_sub_contraction_expand)
punct_sub_contraction_expand <- punct_sub_contraction_expand[order(punct_sub_contraction_expand[,"REPLACEMENT"],punct_sub_contraction_expand[,"PATTERN"]),]
row.names(punct_sub_contraction_expand) <- seq(nrow(punct_sub_contraction_expand))


punct_sub_shortened0 <- expand_patterns(punct_sub_contraction_expand)
#punct_sub_shortened0 <- unique(punct_sub_shortened0)
#row.names(punct_sub_shortened0) <- seq(nrow(punct_sub_shortened0))
punct_sub_shortened_dt <- data.table(punct_sub_shortened0)

punct_sub_shortened_dt[,c("PATTERN")] <- gsub(" {2,}", " ",punct_sub_shortened_dt[,c("PATTERN")])
punct_sub_shortened_dt[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", punct_sub_shortened_dt[,c("PATTERN")])

punct_sub_shortened_dt[,c("REPLACEMENT")] <- gsub(" {2,}", " ",punct_sub_shortened_dt[,c("REPLACEMENT")])
punct_sub_shortened_dt[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", punct_sub_shortened_dt[,c("REPLACEMENT")])

for(k in 1:nrow(shortened_words1))
{
  # k <- 1
  
  set(punct_sub_shortened_dt, i=NULL, j="REPLACEMENT", value=gsub(shortened_words1[k,c("PATTERN")], shortened_words1[k,c("REPLACEMENT")], punct_sub_shortened_dt[["REPLACEMENT"]], perl=TRUE))
  
}
rm(k)

punct_sub_shortened <- rbindlist(l=list(punct_sub_shortened_dt,punct_sub_shortened0), use.names=TRUE, fill=FALSE)
punct_sub_shortened <- as.data.frame(punct_sub_shortened,stringsAsFactors=FALSE)

#punct_sub_shortened <- punct_sub_shortened[order(punct_sub_shortened[,"PATTERN"],punct_sub_shortened[,"REPLACEMENT"]),]

punct_sub_shortened_expand <- expand_patterns(punct_sub_shortened)

#punct_sub_shortened_expand[,c("PATTERN")] <- gsub(" {2,}", " ",punct_sub_shortened_expand[,c("PATTERN")])
#punct_sub_shortened_expand[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", punct_sub_shortened_expand[,c("PATTERN")])

#punct_sub_shortened_expand[,c("REPLACEMENT")] <- gsub(" {2,}", " ",punct_sub_shortened_expand[,c("REPLACEMENT")])
#punct_sub_shortened_expand[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", punct_sub_shortened_expand[,c("REPLACEMENT")])

punct_sub_shortened_expand <- unique(punct_sub_shortened_expand)
#punct_sub_shortened_expand <- punct_sub_shortened_expand[order(punct_sub_shortened_expand[,"PATTERN"],punct_sub_shortened_expand[,"REPLACEMENT"]),]
punct_sub_shortened_expand <- punct_sub_shortened_expand[order(punct_sub_shortened_expand[,"REPLACEMENT"],punct_sub_shortened_expand[,"PATTERN"]),]
row.names(punct_sub_shortened_expand) <- seq(nrow(punct_sub_shortened_expand))

punct_sub_full <- punct_sub_shortened_expand

rm(punct_expand_full)
rm(punct_sub_contraction0,punct_sub_contraction_dt,punct_sub_contraction,punct_sub_contraction_expand)
rm(punct_sub_shortened0,punct_sub_shortened_dt,punct_sub_shortened,punct_sub_shortened_expand)


###############################################################################
cat("Create hash table pattern \n")
###############################################################################

hash_pattern_trim0 <- punct_u_trim
hash_pattern_trim1 <- hash_pattern_trim0[hash_pattern_trim0!="-"]
hash_pattern_trim2 <- hash_pattern_trim1[hash_pattern_trim1!=" "]
hash_pattern_trim3 <- hash_pattern_trim2[hash_pattern_trim2!="'"]
hash_pattern0 <- paste(hash_pattern_trim3,collapse="")
hash_pattern <- paste("[",hash_pattern0,"]",sep="")


###############################################################################
cat("Create small hash table \n")
###############################################################################

hash_table_small1 <- rbindlist(l=list(ordinal_lookup1,abbreviations1_expand,contractions1,shortened_words1), use.names=TRUE, fill=FALSE)
hash_table_small1 <- as.data.frame(hash_table_small1,stringsAsFactors=FALSE)


###############################################################################
cat("Create large hash table \n")
###############################################################################

hash_table_large0 <- expand_patterns(punct_sub_full)
hash_table_large0 <- as.data.frame(hash_table_large0,stringsAsFactors=FALSE)

hash_table_large1 <- rbindlist(l=list(hash_table_large0,hash_table_small1), use.names=TRUE, fill=FALSE)
hash_table_large1 <- as.data.frame(hash_table_large1,stringsAsFactors=FALSE)

#words <- c(punctuation_words1_df_expand[,"REPLACEMENT"],contractions1[,"REPLACEMENT"],shortened_words1[,"REPLACEMENT"])
#words_u <- unique(words)


###############################################################################
cat("Trim hash tables \n")
###############################################################################

#Small Hash Table

hash_table_small1[,c("PATTERN")] <- gsub(" {2,}", " ",hash_table_small1[,c("PATTERN")])
hash_table_small1[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", hash_table_small1[,c("PATTERN")])

hash_table_small1[,c("REPLACEMENT")] <- gsub(" {2,}", " ",hash_table_small1[,c("REPLACEMENT")])
hash_table_small1[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", hash_table_small1[,c("REPLACEMENT")])

hash_table_small1[,c("REPLACEMENT")] <- gsub(hash_pattern, "",hash_table_small1[,c("REPLACEMENT")])
#hash_table_small1[,c("REPLACEMENT")] <- gsub("[[:punct:]]", "",hash_table_small1[,c("REPLACEMENT")])
#hash_table_small1[,c("REPLACEMENT")] <- gsub(" ", "",hash_table_small1[,c("REPLACEMENT")])


#hash_table_small1 <- hash_table_small1[order(hash_table_small1[,"REPLACEMENT"],hash_table_small1[,"PATTERN"]),]
hash_table_small1 <- hash_table_small1[order(hash_table_small1[,"PATTERN"],hash_table_small1[,"REPLACEMENT"]),]
hash_table_small1 <- unique(hash_table_small1)
row.names(hash_table_small1) <- seq(nrow(hash_table_small1))

hash_table_small2 <- expand_patterns(hash_table_small1)
hash_table_small2 <- as.data.frame(hash_table_small2,stringsAsFactors=FALSE)
hash_table_small2 <- hash_table_small2[order(hash_table_small2[,"PATTERN"],hash_table_small2[,"REPLACEMENT"]),]
#hash_table_small2 <- hash_table_small2[(hash_table_small2[,"REPLACEMENT"] != hash_table_small2[,"PATTERN"]),]
hash_table_small2 <- unique(hash_table_small2)
row.names(hash_table_small2) <- seq(nrow(hash_table_small2))


#Large Hash Table

hash_table_large1[,c("PATTERN")] <- gsub(" {2,}", " ",hash_table_large1[,c("PATTERN")])
hash_table_large1[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", hash_table_large1[,c("PATTERN")])

hash_table_large1[,c("REPLACEMENT")] <- gsub(" {2,}", " ",hash_table_large1[,c("REPLACEMENT")])
hash_table_large1[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", hash_table_large1[,c("REPLACEMENT")])

hash_table_large1[,c("REPLACEMENT")] <- gsub(hash_pattern, "",hash_table_large1[,c("REPLACEMENT")])
#hash_table_large1[,c("REPLACEMENT")] <- gsub("[[:punct:]]", "",hash_table_large1[,c("REPLACEMENT")])
#hash_table_large1[,c("REPLACEMENT")] <- gsub(" ", "",hash_table_large1[,c("REPLACEMENT")])


#hash_table_large1 <- hash_table_large1[order(hash_table_large1[,"REPLACEMENT"],hash_table_large1[,"PATTERN"]),]
hash_table_large1 <- hash_table_large1[order(hash_table_large1[,"PATTERN"],hash_table_large1[,"REPLACEMENT"]),]
hash_table_large1 <- unique(hash_table_large1)
row.names(hash_table_large1) <- seq(nrow(hash_table_large1))

hash_table_large2 <- expand_patterns(hash_table_large1)
hash_table_large2 <- as.data.frame(hash_table_large2,stringsAsFactors=FALSE)
hash_table_large2 <- hash_table_large2[order(hash_table_large2[,"PATTERN"],hash_table_large2[,"REPLACEMENT"]),]
#hash_table_large2 <- hash_table_large2[(hash_table_large2[,"REPLACEMENT"] != hash_table_large2[,"PATTERN"]),]
hash_table_large2 <- unique(hash_table_large2)
row.names(hash_table_large2) <- seq(nrow(hash_table_large2))


###############################################################################
cat("Remove hash tables spaces \n")
###############################################################################

#Small Hash Table

hash_table_small3 <- hash_table_small2
hash_table_small3[,c("REPLACEMENT")] <- gsub(" ", "",hash_table_small3[,c("REPLACEMENT")])
hash_table_small3 <- unique(hash_table_small3)
row.names(hash_table_small3) <- seq(nrow(hash_table_small3))

hash_table_small4 <- expand_patterns(hash_table_small3)
hash_table_small4 <- as.data.frame(hash_table_small4,stringsAsFactors=FALSE)
hash_table_small4 <- hash_table_small4[order(hash_table_small4[,"PATTERN"],hash_table_small4[,"REPLACEMENT"]),]
#hash_table_small4 <- hash_table_small4[(hash_table_small4[,"REPLACEMENT"] != hash_table_small4[,"PATTERN"]),]
hash_table_small4 <- unique(hash_table_small4)
row.names(hash_table_small4) <- seq(nrow(hash_table_small4))


#Large Hash Table

hash_table_large3 <- hash_table_large2
hash_table_large3[,c("REPLACEMENT")] <- gsub(" ", "",hash_table_large3[,c("REPLACEMENT")])
hash_table_large3 <- unique(hash_table_large3)
row.names(hash_table_large3) <- seq(nrow(hash_table_large3))

hash_table_large4 <- expand_patterns(hash_table_large3)
hash_table_large4 <- as.data.frame(hash_table_large4,stringsAsFactors=FALSE)
hash_table_large4 <- hash_table_large4[order(hash_table_large4[,"PATTERN"],hash_table_large4[,"REPLACEMENT"]),]
#hash_table_large4 <- hash_table_large4[(hash_table_large4[,"REPLACEMENT"] != hash_table_large4[,"PATTERN"]),]
hash_table_large4 <- unique(hash_table_large4)
row.names(hash_table_large4) <- seq(nrow(hash_table_large4))


###############################################################################
cat("Remove hash tables punctuation \n")
###############################################################################

#Small Hash Table

hash_table_small5 <- hash_table_small4
hash_table_small5[,c("REPLACEMENT")] <- gsub("-", "",hash_table_small5[,c("REPLACEMENT")])
hash_table_small5[,c("REPLACEMENT")] <- gsub("'", "",hash_table_small5[,c("REPLACEMENT")])
hash_table_small5 <- unique(hash_table_small5)
row.names(hash_table_small5) <- seq(nrow(hash_table_small5))

hash_table_small6 <- expand_patterns(hash_table_small5)
hash_table_small6 <- as.data.frame(hash_table_small6,stringsAsFactors=FALSE)
hash_table_small6 <- hash_table_small6[order(hash_table_small6[,"PATTERN"],hash_table_small6[,"REPLACEMENT"]),]
#hash_table_small6 <- hash_table_small6[(hash_table_small6[,"REPLACEMENT"] != hash_table_small6[,"PATTERN"]),]
hash_table_small6 <- unique(hash_table_small6)
row.names(hash_table_small6) <- seq(nrow(hash_table_small6))


#Large Hash Table

hash_table_large5 <- hash_table_large4
hash_table_large5[,c("REPLACEMENT")] <- gsub("-", "",hash_table_large5[,c("REPLACEMENT")])
hash_table_large5[,c("REPLACEMENT")] <- gsub("'", "",hash_table_large5[,c("REPLACEMENT")])
hash_table_large5 <- unique(hash_table_large5)
row.names(hash_table_large5) <- seq(nrow(hash_table_large5))

hash_table_large6 <- expand_patterns(hash_table_large5)
hash_table_large6 <- as.data.frame(hash_table_large6,stringsAsFactors=FALSE)
hash_table_large6 <- hash_table_large6[order(hash_table_large6[,"PATTERN"],hash_table_large6[,"REPLACEMENT"]),]
#hash_table_large6 <- hash_table_large6[(hash_table_large6[,"REPLACEMENT"] != hash_table_large6[,"PATTERN"]),]
hash_table_large6 <- unique(hash_table_large6)
row.names(hash_table_large6) <- seq(nrow(hash_table_large6))


###############################################################################
cat("Remove equal pattern and replacements in hash tables \n")
###############################################################################

#Small Hash Table

hash_table_small7 <- hash_table_small6
hash_table_small7 <- hash_table_small7[(hash_table_small7[,"REPLACEMENT"] != hash_table_small7[,"PATTERN"]),]
hash_table_small7 <- unique(hash_table_small7)
row.names(hash_table_small7) <- seq(nrow(hash_table_small7))

hash_table_full_small <- hash_table_small7


#Large Hash Table

hash_table_large7 <- hash_table_large6
hash_table_large7 <- hash_table_large7[(hash_table_large7[,"REPLACEMENT"] != hash_table_large7[,"PATTERN"]),]
hash_table_large7 <- unique(hash_table_large7)
row.names(hash_table_large7) <- seq(nrow(hash_table_large7))

hash_table_full_large <- hash_table_large7


###############################################################################
cat("Get hash tables counts \n")
###############################################################################

#Small Hash Table

hash_table_count_small <- ddply(.data=hash_table_full_small, .variables="PATTERN", .fun = function(x){
  x_count <- data.frame(x,count=nrow(x),stringsAsFactors=FALSE)
  return(x_count)
}, .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

hash_table_count_small <- hash_table_count_small[order(-hash_table_count_small[,"count"],hash_table_count_small[,"PATTERN"],hash_table_count_small[,"REPLACEMENT"]),]


#Large Hash Table

hash_table_count_large <- ddply(.data=hash_table_full_large, .variables="PATTERN", .fun = function(x){
  x_count <- data.frame(x,count=nrow(x),stringsAsFactors=FALSE)
  return(x_count)
}, .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

hash_table_count_large <- hash_table_count_large[order(-hash_table_count_large[,"count"],hash_table_count_large[,"PATTERN"],hash_table_count_large[,"REPLACEMENT"]),]


###############################################################################
cat("Only keep one pattern per replacement \n")
###############################################################################

#Small Hash Table

hash_table_final_small0 <- ddply(.data=hash_table_full_small, .variables="PATTERN", .fun = function(x){
  
  # x <- hash_table_full_small[hash_table_full_small[,"PATTERN"]=="12B-1",]
  
  x_trim0 <- data.frame(x,LEN=nchar(x[,"REPLACEMENT"]),stringsAsFactors=FALSE)
  x_trim0 <- x_trim0[order(x_trim0[,"PATTERN"],x_trim0[,"LEN"]),]
  #  x_trim1 <- x_trim0[x_trim0[,"LEN"]==min(x_trim0[,"LEN"]),]
  x_trim1 <- x_trim0[x_trim0[,"LEN"]==max(x_trim0[,"LEN"]),]
  x_trim2 <- x_trim1[,!(colnames(x_trim1) %in% c("LEN"))]
  
  x_trim <- tail(x_trim2,1)
  
  return(x_trim)
}, .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

#hash_table_final_small0 <- hash_table_final_small0[order(hash_table_final_small0[,"PATTERN"],hash_table_final_small0[,"REPLACEMENT"]),]
hash_table_final_small0 <- hash_table_final_small0[order(hash_table_final_small0[,"REPLACEMENT"],hash_table_final_small0[,"PATTERN"]),]
row.names(hash_table_final_small0) <- seq(nrow(hash_table_final_small0))

hash_table_final_small <- hash_table_final_small0


#Large Hash Table

hash_table_final_large0 <- ddply(.data=hash_table_full_large, .variables="PATTERN", .fun = function(x){
  
  # x <- hash_table_full_large[hash_table_full_large[,"PATTERN"]=="12B-1",]
  
  x_trim0 <- data.frame(x,LEN=nchar(x[,"REPLACEMENT"]),stringsAsFactors=FALSE)
  x_trim0 <- x_trim0[order(x_trim0[,"PATTERN"],x_trim0[,"LEN"]),]
  #  x_trim1 <- x_trim0[x_trim0[,"LEN"]==min(x_trim0[,"LEN"]),]
  x_trim1 <- x_trim0[x_trim0[,"LEN"]==max(x_trim0[,"LEN"]),]
  x_trim2 <- x_trim1[,!(colnames(x_trim1) %in% c("LEN"))]
  
  x_trim <- tail(x_trim2,1)
  
  return(x_trim)
}, .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

#hash_table_final_large0 <- hash_table_final_large0[order(hash_table_final_large0[,"PATTERN"],hash_table_final_large0[,"REPLACEMENT"]),]
hash_table_final_large0 <- hash_table_final_large0[order(hash_table_final_large0[,"REPLACEMENT"],hash_table_final_large0[,"PATTERN"]),]
row.names(hash_table_final_large0) <- seq(nrow(hash_table_final_large0))

hash_table_final_large <- hash_table_final_large0


###############################################################################
cat("Add backslahses so that hashes can be used in gsub function \n")
###############################################################################

eres <- c("\\.","\\\\","\\|","\\(","\\)","\\[","\\{","\\^","\\$","\\*","\\+","\\?")

#Small Hash Table

for(k in 1:length(eres))
{
  #hash_table_final_small[,"PATTERN"] <- gsub("\\(","\\\\(",hash_table_final_small[,"PATTERN"])
  
  hash_table_final_small[,"PATTERN"] <- gsub(eres[k],paste("\\",eres[k],sep=""),hash_table_final_small[,"PATTERN"])
  hash_table_final_small[,"REPLACEMENT"] <- gsub(eres[k],paste("\\",eres[k],sep=""),hash_table_final_small[,"REPLACEMENT"])
}
rm(k)

hash_table_final_small[,c("PATTERN")] <- paste(" ",hash_table_final_small[,c("PATTERN")]," ",sep="")
hash_table_final_small[,c("REPLACEMENT")] <- paste(" ",hash_table_final_small[,c("REPLACEMENT")]," ",sep="")


#Large Hash Table

for(k in 1:length(eres))
{
  #hash_table_final_large[,"PATTERN"] <- gsub("\\(","\\\\(",hash_table_final_large[,"PATTERN"])
  
  hash_table_final_large[,"PATTERN"] <- gsub(eres[k],paste("\\",eres[k],sep=""),hash_table_final_large[,"PATTERN"])
  hash_table_final_large[,"REPLACEMENT"] <- gsub(eres[k],paste("\\",eres[k],sep=""),hash_table_final_large[,"REPLACEMENT"])
}
rm(k)

hash_table_final_large[,c("PATTERN")] <- paste(" ",hash_table_final_large[,c("PATTERN")]," ",sep="")
hash_table_final_large[,c("REPLACEMENT")] <- paste(" ",hash_table_final_large[,c("REPLACEMENT")]," ",sep="")

rm(eres)
rm(contractions,contractions1)
rm(shortened_words,shortened_words1)
rm(ordinal_lookup,ordinal_lookup1)
rm(abbreviations,abbreviations1,abbreviations1_expand)
rm(punctuation_words,punctuation_words1,punctuation_words1_df_expand,punct_u_expand_trim3)
rm(punct_sub_full,punct_u4,punct_u_trim0,punct_u_trim)
rm(hash_pattern_trim0,hash_pattern_trim1,hash_pattern_trim2,hash_pattern0)
rm(hash_table_small1,hash_table_small2,hash_table_small3)
rm(hash_table_small4,hash_table_small5,hash_table_small6,hash_table_small7)
rm(hash_table_large0,hash_table_large1,hash_table_large2,hash_table_large3)
rm(hash_table_large4,hash_table_large5,hash_table_large6,hash_table_large7)
rm(hash_table_count_small,hash_table_count_large)
rm(hash_table_final_small0,hash_table_final_large0)


###############################################################################
cat("Output Hash Tables \n")
###############################################################################

write.table(hash_table_final_small,file=paste(input_directory,"\\","hash_table_final_small",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(hash_table_final_large,file=paste(input_directory,"\\","hash_table_final_large",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)


###############################################################################
cat("Substitute External Text Data \n")
###############################################################################

letter_beginning0_dt <-letter_beginning0
letter_beginning0_dt[,c("BEGINNINGS")] <- paste(" ",letter_beginning0_dt[,c("BEGINNINGS")]," ",sep="")
letter_beginning0_dt[,c("BEGINNINGS")] <- gsub(hash_pattern," \\1 ",letter_beginning0_dt[,c("BEGINNINGS")])
letter_beginning0_dt <- data.table(letter_beginning0_dt)

letter_ending0_dt <-letter_ending0
letter_ending0_dt[,c("ENDINGS")] <- paste(" ",letter_ending0_dt[,c("ENDINGS")]," ",sep="")
letter_ending0_dt[,c("ENDINGS")] <- gsub(hash_pattern," \\1 ",letter_ending0_dt[,c("ENDINGS")])
letter_ending0_dt <- data.table(letter_ending0_dt)

letter_position0_dt <-letter_position0
letter_position0_dt[,c("POSITIONS")] <- paste(" ",letter_position0_dt[,c("POSITIONS")]," ",sep="")
letter_position0_dt[,c("POSITIONS")] <- gsub(hash_pattern," \\1 ",letter_position0_dt[,c("POSITIONS")])
letter_position0_dt <- data.table(letter_position0_dt)

letter_signature0_dt <-letter_signature0
letter_signature0_dt[,c("SIGNATURES")] <- paste(" ",letter_signature0_dt[,c("SIGNATURES")]," ",sep="")
letter_signature0_dt[,c("SIGNATURES")] <- gsub(hash_pattern," \\1 ",letter_signature0_dt[,c("SIGNATURES")])
letter_signature0_dt <- data.table(letter_signature0_dt)

letter_closing0_dt <-letter_closing0
letter_closing0_dt[,c("CLOSINGS")] <- paste(" ",letter_closing0_dt[,c("CLOSINGS")]," ",sep="")
letter_closing0_dt[,c("CLOSINGS")] <- gsub(hash_pattern," \\1 ",letter_closing0_dt[,c("CLOSINGS")])
letter_closing0_dt <- data.table(letter_closing0_dt)

#rm(letter_beginning0,letter_ending0,letter_position0,letter_signature0,letter_closing0)

hash_table_full <-hash_table_full_small
hash_table_final <- hash_table_final_small

#hash_table_full <-hash_table_full_large
#hash_table_final <- hash_table_final_large

for(k in 1:nrow(hash_table_final))
{
  # k <- 1
  # k <- 254
  # k <- 10038

  set(letter_beginning0_dt, i=NULL, j="BEGINNINGS", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_beginning0_dt[["BEGINNINGS"]]))
  set(letter_ending0_dt, i=NULL, j="ENDINGS", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_ending0_dt[["ENDINGS"]]))
  set(letter_position0_dt, i=NULL, j="POSITIONS", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_position0_dt[["POSITIONS"]]))
  set(letter_signature0_dt, i=NULL, j="SIGNATURES", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_signature0_dt[["SIGNATURES"]]))
  set(letter_closing0_dt, i=NULL, j="CLOSINGS", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_closing0_dt[["CLOSINGS"]]))
  
  #progress_function(k,1,nrow(hash_table_final),1,1,1)
}
#rm(hash_table_final,k)

  
letter_beginning1 <- as.data.frame(letter_beginning0_dt,stringsAsFactors=FALSE)
letter_ending1 <- as.data.frame(letter_ending0_dt,stringsAsFactors=FALSE)
letter_position1 <- as.data.frame(letter_position0_dt,stringsAsFactors=FALSE)
letter_signature1 <- as.data.frame(letter_signature0_dt,stringsAsFactors=FALSE)
letter_closing1 <- as.data.frame(letter_closing0_dt,stringsAsFactors=FALSE)

# for(k in 1:nrow(hash_table_final))
# {
#   # k <- 1
#   # k <- 254
#   # k <- 10038
# 
#   set(letter_beginning0_dt, i=NULL, j="BEGINNINGS", value=gsub(hash_table_final[k,c("REPLACEMENT")], hash_table_final[k,c("PATTERN")], letter_beginning0_dt[["BEGINNINGS"]]))
#   set(letter_ending0_dt, i=NULL, j="ENDINGS", value=gsub(hash_table_final[k,c("REPLACEMENT")], hash_table_final[k,c("PATTERN")], letter_ending0_dt[["ENDINGS"]]))
#   set(letter_position0_dt, i=NULL, j="POSITIONS", value=gsub(hash_table_final[k,c("REPLACEMENT")], hash_table_final[k,c("PATTERN")], letter_position0_dt[["POSITIONS"]]))
#   set(letter_signature0_dt, i=NULL, j="SIGNATURES", value=gsub(hash_table_final[k,c("REPLACEMENT")], hash_table_final[k,c("PATTERN")], letter_signature0_dt[["SIGNATURES"]]))
#   set(letter_closing0_dt, i=NULL, j="CLOSINGS", value=gsub(hash_table_final[k,c("REPLACEMENT")], hash_table_final[k,c("PATTERN")], letter_closing0_dt[["CLOSINGS"]]))
#   
#   #progress_function(k,1,nrow(hash_table_final),1,1,1)
# }
# #rm(hash_table_final,k)
# 
# letter_beginning1_dt <- letter_beginning0_dt
# letter_ending1_dt <- letter_ending0_dt
# letter_position1_dt <- letter_position0_dt
# letter_signature1_dt <- letter_signature0_dt
# letter_closing1_dt <- letter_closing0_dt
#   
# for(k in 1:nrow(hash_table_final))
# {
#   # k <- 1
#   # k <- 254
#   # k <- 10038
#   
#   set(letter_beginning1_dt, i=NULL, j="BEGINNINGS", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_beginning1_dt[["BEGINNINGS"]]))
#   set(letter_ending1_dt, i=NULL, j="ENDINGS", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_ending1_dt[["ENDINGS"]]))
#   set(letter_position1_dt, i=NULL, j="POSITIONS", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_position1_dt[["POSITIONS"]]))
#   set(letter_signature1_dt, i=NULL, j="SIGNATURES", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_signature1_dt[["SIGNATURES"]]))
#   set(letter_closing1_dt, i=NULL, j="CLOSINGS", value=gsub(hash_table_final[k,c("PATTERN")], hash_table_final[k,c("REPLACEMENT")], letter_closing1_dt[["CLOSINGS"]]))
#   
#   #progress_function(k,1,nrow(hash_table_final),1,1,1)
# }
#   
# letter_beginning1 <- as.data.frame(letter_beginning1_dt,stringsAsFactors=FALSE)
# letter_ending1 <- as.data.frame(letter_ending1_dt,stringsAsFactors=FALSE)
# letter_position1 <- as.data.frame(letter_position1_dt,stringsAsFactors=FALSE)
# letter_signature1 <- as.data.frame(letter_signature1_dt,stringsAsFactors=FALSE)
# letter_closing1 <- as.data.frame(letter_closing1_dt,stringsAsFactors=FALSE)

#rm(letter_beginning0_dt,letter_ending0_dt,letter_position0_dt,letter_signature0_dt,letter_closing0_dt)

#aa <- adply(.data=hash_table_final, .margins=1, .fun = function(x,data){
#
#  data[,"BEGINNINGS"] <- gsub(x[,"PATTERN"],x["REPLACEMENT"],data[,"BEGINNINGS"])
#  return(data)
#  
#}, data=letter_beginning1, .expand = FALSE, .progress = "text", .inform = FALSE, .parallel = FALSE,.paropts = NULL)


#bb <- ddply(.data=aa[,], .variables="BEGINNINGS", .fun = function(x){
#
#  p_u <- unique(x[,"PRIORITY"])
#  b_u <- unique(x[,"BEGINNINGS"])
#  
#  x_count <- unique(data.frame(PRIORITY=p_u,BEGINNINGS=b_u,count=nrow(x),stringsAsFactors=FALSE))
#  return(x_count)
#
#}, .progress = "text",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


###############################################################################
cat("Create Regex Strings External Text Data \n")
###############################################################################

#letter_beginning1[,c("BEGINNINGS")] <- gsub("'", "", letter_beginning1[,c("BEGINNINGS")])
#letter_beginning1[,c("BEGINNINGS")] <- gsub("-", "", letter_beginning1[,c("BEGINNINGS")])
letter_beginning_final <- clean_strings(file=letter_beginning1,str_col="BEGINNINGS",backslash_flag=1,apostrophe_flag=1)


#letter_ending1[,c("ENDINGS")] <- gsub("'", "", letter_ending1[,c("ENDINGS")])
#letter_ending1[,c("ENDINGS")] <- gsub("-", "", letter_ending1[,c("ENDINGS")])
letter_ending_final <- clean_strings(file=letter_ending1,str_col="ENDINGS",backslash_flag=1,apostrophe_flag=1)

#letter_position1[,c("POSITIONS")] <- gsub("'", "", letter_position1[,c("POSITIONS")])
#letter_position1[,c("POSITIONS")] <- gsub("-", "", letter_position1[,c("POSITIONS")])
letter_position_final <- clean_strings(file=letter_position1,str_col="POSITIONS",backslash_flag=1,apostrophe_flag=1)

#letter_signature1[,c("SIGNATURES")] <- gsub("'", "", letter_signature1[,c("SIGNATURES")])
#letter_signature1[,c("SIGNATURES")] <- gsub("-", "", letter_signature1[,c("SIGNATURES")])
letter_signature_final <- clean_strings(file=letter_signature1,str_col="SIGNATURES",backslash_flag=1,apostrophe_flag=1)

#letter_closing1[,c("CLOSINGS")] <- gsub("'", "", letter_closing1[,c("CLOSINGS")])
#letter_closing1[,c("CLOSINGS")] <- gsub("-", "", letter_closing1[,c("CLOSINGS")])
letter_closing_final <- clean_strings(file=letter_closing1,str_col="CLOSINGS",backslash_flag=1,apostrophe_flag=1)

rm(letter_beginning1,letter_ending1,letter_position1,letter_signature1,letter_closing1)


###############################################################################
cat("Output Subsitution Data \n")
###############################################################################

write.table(letter_beginning_final,file=paste(input_directory,"\\","letter_beginning_final",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_ending_final,file=paste(input_directory,"\\","letter_ending_final",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_position_final,file=paste(input_directory,"\\","letter_position_final",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_signature_final,file=paste(input_directory,"\\","letter_signature_final",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

write.table(letter_closing_final,file=paste(input_directory,"\\","letter_closing_final",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)


###############################################################################
cat("Update Not Used Words \n")
###############################################################################

punctuation_words_not_used_orginal <- read.table(file=paste(input_directory,"\\","punctuation_words_not_used",".csv",sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

punctuation_words_used0 <- expand_patterns(hash_table_full)
punctuation_words_used0 <- punctuation_words_used0[order(punctuation_words_used0[,"PATTERN"],punctuation_words_used0[,"REPLACEMENT"]),]
row.names(punctuation_words_used0) <- seq(nrow(punctuation_words_used0))

punctuation_words_used1 <- punctuation_words_used0[,!(colnames(punctuation_words_used0) %in% c("REPLACEMENT"))]
punctuation_words_used <- unique(punctuation_words_used1)

punctuation_words_used <- as.data.frame(punctuation_words_used,stringsAsFactors=FALSE)
colnames(punctuation_words_used) <- "WORD"


punctuation_words_not_used <- punctuation_words_not_used_orginal[!(punctuation_words_not_used_orginal[,"WORD"] %in% punctuation_words_used[,"WORD"]),]
punctuation_words_not_used <- unique(punctuation_words_not_used)
punctuation_words_not_used <- as.data.frame(punctuation_words_not_used,stringsAsFactors=FALSE)
colnames(punctuation_words_not_used) <- "WORD"

write.table(punctuation_words_not_used,file=paste(input_directory,"\\","punctuation_words_not_used",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

