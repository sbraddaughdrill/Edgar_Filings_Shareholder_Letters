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
external_packages <- c("data.table","gdata","gsubfn","plyr","stringr","XML")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(external_packages,installed_packages,repo)


###############################################################################
cat("Look and Clean Data \n")
###############################################################################

punctuation_words0 <- read.table(file=paste(input_directory,"punctuation_words.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
contractions0 <- read.table(file=paste(input_directory,"contractions.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
shortened_words0 <- read.table(file=paste(input_directory,"shortened_words.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

punctuation_words1 <- clean_replacement_lookup(file=punctuation_words0,cols_to_clean=c("WORD"))
punctuation_words <- punctuation_words1
punctuation_words[,c("WORD")] <- paste(" ",punctuation_words[,c("WORD")]," ",sep="")

contractions1 <- clean_replacement_lookup(file=contractions0,cols_to_clean=c("PATTERN","REPLACEMENT"))
contractions <- contractions1
contractions[,c("PATTERN")] <- paste(" ",contractions[,c("PATTERN")]," ",sep="")
contractions[,c("REPLACEMENT")] <- paste(" ",contractions[,c("REPLACEMENT")]," ",sep="")

shortened_words1 <- clean_replacement_lookup(file=shortened_words0,cols_to_clean=c("PATTERN","REPLACEMENT"))
shortened_words <- shortened_words1
shortened_words[,c("PATTERN")] <- paste(" ",shortened_words[,c("PATTERN")]," ",sep="")
shortened_words[,c("REPLACEMENT")] <- paste(" ",shortened_words[,c("REPLACEMENT")]," ",sep="")

rm(punctuation_words0,contractions0,shortened_words0)
#rm(punctuation_words1,contractions1,shortened_words1)


###############################################################################
cat("Expand Punctuation \n")
###############################################################################

punct_u0 <- punctuation_words1

punct_u0[,"WORD"] <- gsub("[[:alpha:]]+","",punct_u0[,"WORD"])
punct_u0[,"WORD"] <- gsub("[[:digit:]]+","",punct_u0[,"WORD"])

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


punct_u_expand_id <- data.frame(ID=NA,punct_u_expand,stringsAsFactors=FALSE)
punct_u_expand_id[,"ID"] <- seq(1,nrow(punct_u_expand_id),1)

rm(punct_u_expand)

cols    <- names(punct_u_expand_id)[grepl('^Var[[:digit:]]+$',names(punct_u_expand_id))]
cols_str <- paste(cols,sep="",collapse=",")
paste_str <- paste("paste(",cols_str,",sep='')")

punct_u_expand_dt <- data.table(punct_u_expand_id, key="ID")

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

rm(punctuation_words1_df)


###############################################################################
cat("Convert ordinal numbers to numeric \n")
###############################################################################

ordinal_lookup <- data.frame(matrix(NA, ncol=2, nrow=19, 
                                    dimnames=list(c(), c("PATTERN","REPLACEMENT"))), 
                             stringsAsFactors=FALSE)

ordinal_lookup[1,] <- c("FIRST","1ST")
ordinal_lookup[2,] <- c("SECOND","2ND")
ordinal_lookup[3,] <- c("THIRD","3RD")
ordinal_lookup[4,] <- c("FOURTH","4TH")
ordinal_lookup[5,] <- c("FIFTH","5TH")
ordinal_lookup[6,] <- c("SIXTH","6TH")
ordinal_lookup[7,] <- c("SEVENTH","7TH")
ordinal_lookup[8,] <- c("EIGHTH","8TH")
ordinal_lookup[9,] <- c("NINTH","9TH")
ordinal_lookup[10,] <- c("TENTH","10TH")
ordinal_lookup[11,] <- c("ELEVENTH","11TH")
ordinal_lookup[12,] <- c("TWELFTH","12TH")
ordinal_lookup[13,] <- c("THIRTEENTH","13TH")
ordinal_lookup[14,] <- c("FOURTEENTH","14TH")
ordinal_lookup[15,] <- c("FIFTEENTH","15TH")
ordinal_lookup[16,] <- c("SIXTEENTH","16TH")
ordinal_lookup[17,] <- c("SEVENTEENTH","17TH")
ordinal_lookup[18,] <- c("EIGHTEENTH","18TH")
ordinal_lookup[19,] <- c("NINETEENTH","19TH")

pattern0 <- paste(punct_u4,collapse="")
pattern1a <- paste("(?<=[",pattern0,"])",sep="")
#pattern1b <- paste("([^",pattern0,"]+)([",pattern0,"]|$)",sep="")
#pattern1b <- paste("([^",pattern0,"])([",pattern0,"]|$)",sep="")
#pattern1b <- paste("([^",pattern0,"]*)([",pattern0,"]*|$)",sep="")
pattern1b <- paste("([^",pattern0,"]*)([",pattern0,"]|$)",sep="")

#punctuation_words_ordinal_replace0 <- punctuation_words1[,"WORD"]
#punctuation_words_ordinal_replace0 <- punctuation_words1_df

punctuation_words_ordinal_replace0 <- punctuation_words1_df_expand

punctuation_words_ordinal_replace1 <- punctuation_words1_df_expand

punctuation_words_ordinal_replace1[,"REPLACEMENT"] <- ldply(.data=punctuation_words_ordinal_replace1[,"REPLACEMENT"], .fun = function(x,pattern1,pattern2,lookup){
  
  # x <- "first-://SECOND"
  # x <- punctuation_words_ordinal_replace1[803,"REPLACEMENT"]
  # x <- punctuation_words_ordinal_replace1[1990,"REPLACEMENT"]
  # x <- punctuation_words_ordinal_replace1[1991,"REPLACEMENT"]
  # pattern1 <- pattern1a
  # pattern2 <- pattern1b
  # lookup <- ordinal_lookup
  
  aa1a <- strsplit(x,pattern1,perl=TRUE)
  aa1b <- unlist(aa1a)
  
  aa2a <- strapply(aa1b, pattern2, ~ c(...), b= -2)
  aa2b <- unlist(aa2a)
  
  bb <- data.table(aa2b)
  setnames(bb,"WORD")
  
  for(k in 1:nrow(lookup))
  {
    # k <- 1
    
    set(bb, i=NULL, j="WORD", value=gsub(lookup[k,c("PATTERN")], lookup[k,c("REPLACEMENT")], bb[["WORD"]], ignore.case = TRUE, perl=TRUE))
    
  }
  rm(lookup,k)
  
  cc <- paste(unlist(bb),sep="",collapse="")
  
  return(cc)
  
}, pattern1=pattern1a, pattern2=pattern1b, lookup=ordinal_lookup,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL, .id = NA)

rm(ordinal_lookup,pattern0,pattern1a,pattern1b)

#punctuation_words_ordinal_replace1 <- as.data.frame(punctuation_words_ordinal_replace1,stringsAsFactors=FALSE)
#colnames(punctuation_words_ordinal_replace1) <- "WORD"
#punctuation_words_ordinal_replace1 <- punctuation_words_ordinal_replace1[order(punctuation_words_ordinal_replace1[,"WORD"]),]
#punctuation_words_ordinal_replace1 <- as.data.frame(punctuation_words_ordinal_replace1,stringsAsFactors=FALSE)
#colnames(punctuation_words_ordinal_replace1) <- "WORD"

#punctuation_words_ordinal_replace <- rbind(punctuation_words1,punctuation_words_ordinal_replace1)
#punctuation_words_ordinal_replace <- unique(punctuation_words_ordinal_replace)
#punctuation_words_ordinal_replace <- as.data.frame(punctuation_words_ordinal_replace,stringsAsFactors=FALSE)
#colnames(punctuation_words_ordinal_replace) <- "WORD"

#punctuation_words_ordinal_replace <- punctuation_words_ordinal_replace[order(punctuation_words_ordinal_replace[,"WORD"]),]
#punctuation_words_ordinal_replace <- as.data.frame(punctuation_words_ordinal_replace,stringsAsFactors=FALSE)
#colnames(punctuation_words_ordinal_replace) <- "WORD"


#punctuation_words_ordinal_replace <- rbindlist(l=list(punctuation_words_ordinal_replace0,punctuation_words_ordinal_replace1), use.names=TRUE, fill=FALSE)
punctuation_words_ordinal_replace <- expand_patterns(punctuation_words_ordinal_replace1)

punctuation_words_ordinal_replace <- unique(punctuation_words_ordinal_replace)
punctuation_words_ordinal_replace <- as.data.frame(punctuation_words_ordinal_replace,stringsAsFactors=FALSE)
punctuation_words_ordinal_replace <- punctuation_words_ordinal_replace[order(punctuation_words_ordinal_replace[,"PATTERN"],punctuation_words_ordinal_replace[,"REPLACEMENT"]),]
row.names(punctuation_words_ordinal_replace) <- seq(nrow(punctuation_words_ordinal_replace))

rm(punctuation_words_ordinal_replace0,punctuation_words_ordinal_replace1)


###############################################################################
cat("Convert written numbers to numeric \n")
###############################################################################

pattern0 <- paste(punct_u4,collapse="")
pattern1a <- paste("(?<=[",pattern0,"])",sep="")
#pattern1b <- paste("([^",pattern0,"]+)([",pattern0,"]|$)",sep="")
#pattern1b <- paste("([^",pattern0,"])([",pattern0,"]|$)",sep="")
#pattern1b <- paste("([^",pattern0,"]*)([",pattern0,"]*|$)",sep="")
pattern1b <- paste("([^",pattern0,"]*)([",pattern0,"]|$)",sep="")

#punctuation_words_num_replace0 <- punctuation_words_ordinal_replace
#punctuation_words_ordinal_replace0 <- punctuation_words1[,"WORD"]
#punctuation_words_num_replace0 <- punctuation_words_ordinal_replace

#punctuation_words_num_replace0 <- expand_patterns(punctuation_words_ordinal_replace)
#punctuation_words_num_replace0 <- unique(punctuation_words_num_replace0)

punctuation_words_num_replace0 <- punctuation_words_ordinal_replace

punctuation_words_num_replace1 <- punctuation_words_ordinal_replace

punctuation_words_num_replace1[,"REPLACEMENT"] <- ldply(.data=punctuation_words_num_replace1[,"REPLACEMENT"], .fun = function(x,pattern1,pattern2){
  
  # x <- "THIS IS A TEST ONE-/()%'TWO"
  # x <- punctuation_words_num_replace1[803,"REPLACEMENT"]
  # x <- punctuation_words_num_replace1[1990,"REPLACEMENT"]
  # x <- punctuation_words_num_replace1[1991,"REPLACEMENT"]
  # pattern1 <- pattern1a
  # pattern2 <- pattern1b
  
  aa1a <- strsplit(x,pattern1,perl=TRUE)
  aa1b <- unlist(aa1a)
  
  aa2a <- strapply(aa1b, pattern2, ~ c(...), b= -2)
  aa2b <- unlist(aa2a)
  
  bb <- ldply(.data=aa2b, .fun = function(y){
    
    # y <- aa[2]
    # y <- "one"
    # y <- ""
    # y <- "  "
    # y <- 1
    
    y_trim <- y
    y_trim <- gsub(" {2,}", " ",y_trim)
    y_trim <- gsub("^\\s+|\\s+$", "", y_trim)
    
    if(y_trim != "") 
    {
      bb2 <-  try(word2num(y),silent=TRUE)
      
      if(is(bb2,"try-error")) {
        
        output <- y
        
      } else {
        
        output <- bb2[[2]]
        
      }
      
    } else {
      output <- y
    }
    
    return(output)
    
  }, .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)
  
  cc <- paste(unlist(bb),sep="",collapse="")
  
  return(cc)
  
}, pattern1=pattern1a, pattern2=pattern1b, .progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL, .id = NA)

rm(pattern0,pattern1a,pattern1b)

#punctuation_words_num_replace1 <- as.data.frame(punctuation_words_num_replace1,stringsAsFactors=FALSE)
#colnames(punctuation_words_num_replace1) <- "WORD"
#punctuation_words_num_replace1 <- punctuation_words_num_replace1[order(punctuation_words_num_replace1[,"WORD"]),]
#punctuation_words_num_replace1 <- as.data.frame(punctuation_words_num_replace1,stringsAsFactors=FALSE)
#colnames(punctuation_words_num_replace1) <- "WORD"

#punctuation_words_num_replace <- rbind(punctuation_words_ordinal_replace,punctuation_words_num_replace1)
#punctuation_words_num_replace <- unique(punctuation_words_num_replace)
#punctuation_words_num_replace <- as.data.frame(punctuation_words_num_replace,stringsAsFactors=FALSE)
#colnames(punctuation_words_num_replace) <- "WORD"

#punctuation_words_num_replace <- punctuation_words_num_replace[order(punctuation_words_num_replace[,"WORD"]),]
#punctuation_words_num_replace <- as.data.frame(punctuation_words_num_replace,stringsAsFactors=FALSE)
#colnames(punctuation_words_num_replace) <- "WORD"


#punctuation_words_num_replace <- rbindlist(l=list(punctuation_words_num_replace0,punctuation_words_num_replace1), use.names=TRUE, fill=FALSE)
punctuation_words_num_replace <- expand_patterns(punctuation_words_num_replace1)

punctuation_words_num_replace <- unique(punctuation_words_num_replace)
punctuation_words_num_replace <- as.data.frame(punctuation_words_num_replace,stringsAsFactors=FALSE)
punctuation_words_num_replace <- punctuation_words_num_replace[order(punctuation_words_num_replace[,"PATTERN"],punctuation_words_num_replace[,"REPLACEMENT"]),]
row.names(punctuation_words_num_replace) <- seq(nrow(punctuation_words_num_replace))

rm(punctuation_words_num_replace0,punctuation_words_num_replace1)


###############################################################################
cat("Create Combinations of input datasets \n")
###############################################################################

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

#all_words_expand1 <- c(punctuation_words_num_replace[,"WORD"],contractions_shortened_words_expand1[,1])
#all_words_expand1 <- as.data.frame(all_words_expand1,stringsAsFactors=FALSE)
#colnames(all_words_expand1) <- "WORD"

#all_words <- rbindlist(l=list(expand_patterns(punctuation_words_num_replace),contractions_shortened_words_expand1), use.names=TRUE, fill=FALSE)
all_words <- rbindlist(l=list(punctuation_words_num_replace,contractions_shortened_words_expand1), use.names=TRUE, fill=FALSE)
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
rm(punctuation_words_ordinal_replace,punctuation_words_num_replace)
rm(contractions_shortened_words_expand1)
rm(punct_expand_blanks0,punct_expand_blanks)
rm(punct_expand_space0,punct_expand_space)


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
punct_sub_shortened_expand <- punct_sub_shortened_expand[order(punct_sub_shortened_expand[,"REPLACEMENT"],punct_sub_shortened_expand[,"PATTERN"]),]
row.names(punct_sub_shortened_expand) <- seq(nrow(punct_sub_shortened_expand))

punct_sub_full <- punct_sub_shortened_expand

rm(punct_expand_full)
rm(punct_sub_contraction0,punct_sub_contraction_dt,punct_sub_contraction,punct_sub_contraction_expand)
rm(punct_sub_shortened0,punct_sub_shortened_dt,punct_sub_shortened,punct_sub_shortened_expand)


###############################################################################
cat("Create final hash table \n")
###############################################################################

hash_table0 <- expand_patterns(punct_sub_full)

hash_table1 <- hash_table0

words <- c(punctuation_words1_df_expand[,"REPLACEMENT"],contractions1[,"REPLACEMENT"],shortened_words1[,"REPLACEMENT"])
words_u <- unique(words)

hash_pattern_trim0 <- punct_u_trim
hash_pattern_trim1 <- hash_pattern_trim0[hash_pattern_trim0!="-"]
hash_pattern_trim2 <- hash_pattern_trim1[hash_pattern_trim1!=" "]
hash_pattern0 <- paste(hash_pattern_trim2,collapse="")
hash_pattern <- paste("[",hash_pattern0,"]",sep="")

hash_table1[,c("PATTERN")] <- gsub(" {2,}", " ",hash_table1[,c("PATTERN")])
hash_table1[,c("PATTERN")] <- gsub("^\\s+|\\s+$", "", hash_table1[,c("PATTERN")])

hash_table1[,c("REPLACEMENT")] <- gsub(" {2,}", " ",hash_table1[,c("REPLACEMENT")])
hash_table1[,c("REPLACEMENT")] <- gsub("^\\s+|\\s+$", "", hash_table1[,c("REPLACEMENT")])

hash_table1 <- hash_table1[hash_table1[,c("REPLACEMENT")] %in% words_u,]

hash_table1[,c("REPLACEMENT")] <- gsub(hash_pattern, "",hash_table1[,c("REPLACEMENT")])
#hash_table1[,c("REPLACEMENT")] <- gsub("[[:punct:]]", "",hash_table1[,c("REPLACEMENT")])
#hash_table1[,c("REPLACEMENT")] <- gsub(" ", "",hash_table1[,c("REPLACEMENT")])


#hash_table1 <- hash_table1[order(hash_table1[,"REPLACEMENT"],hash_table1[,"PATTERN"]),]
hash_table1 <- hash_table1[order(hash_table1[,"PATTERN"],hash_table1[,"REPLACEMENT"]),]
hash_table1 <- hash_table1[(hash_table1[,"REPLACEMENT"] != hash_table1[,"PATTERN"]),]
hash_table1 <- unique(hash_table1)
row.names(hash_table1) <- seq(nrow(hash_table1))


#### TEST ####
hash_table1_count <- ddply(.data=hash_table1, .variables="PATTERN", .fun = function(x){
  x_count <- data.frame(x,count=nrow(x),stringsAsFactors=FALSE)
  return(x_count)
}, .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
hash_table1_count <- hash_table1_count[order(-hash_table1_count[,"count"],hash_table1_count[,"PATTERN"],hash_table1_count[,"REPLACEMENT"]),]
rm(hash_table1_count)

#### END TEST ####

hash_table <- ddply(.data=hash_table1, .variables="PATTERN", .fun = function(x){
  
  # x <- hash_table1[hash_table1[,"PATTERN"]=="GOTTA",]
  
  x_trim0 <- data.frame(x,len=nchar(x[,"REPLACEMENT"]),stringsAsFactors=FALSE)
  x_trim1 <- x_trim0[x_trim0[,"len"]==min(x_trim0[,"len"]),]
  x_trim2 <- x_trim1[,!(colnames(x_trim1) %in% c("len"))]
  
  x_trim <- tail(x_trim2,1)
  
  return(x_trim)
}, .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

#hash_table <- hash_table[order(hash_table[,"PATTERN"],hash_table[,"REPLACEMENT"]),]
hash_table <- hash_table[order(hash_table[,"REPLACEMENT"],hash_table[,"PATTERN"]),]
row.names(hash_table) <- seq(nrow(hash_table))

hash_table_final <- hash_table

eres <- c("\\.","\\\\","\\|","\\(","\\)","\\[","\\{","\\^","\\$","\\*","\\+","\\?")

for(k in 1:length(eres))
{
  #hash_table_final[,"PATTERN"] <- gsub("\\(","\\\\(",hash_table_final[,"PATTERN"])
  
  hash_table_final[,"PATTERN"] <- gsub(eres[k],paste("\\",eres[k],sep=""),hash_table_final[,"PATTERN"])
  hash_table_final[,"REPLACEMENT"] <- gsub(eres[k],paste("\\",eres[k],sep=""),hash_table_final[,"REPLACEMENT"])
}
rm(eres,k)

hash_table_final[,c("PATTERN")] <- paste(" ",hash_table_final[,c("PATTERN")]," ",sep="")
hash_table_final[,c("REPLACEMENT")] <- paste(" ",hash_table_final[,c("REPLACEMENT")]," ",sep="")

rm(contractions,contractions1)
rm(shortened_words,shortened_words1)
rm(punctuation_words,punctuation_words1,punctuation_words1_df_expand,punct_u_expand_trim3)
rm(punct_sub_full,words,words_u)
rm(punct_u4,punct_u_trim0,punct_u_trim)
rm(hash_pattern_trim0,hash_pattern_trim1,hash_pattern_trim2,hash_pattern0,hash_pattern)
rm(hash_table0,hash_table1)


###############################################################################
cat("Import External Text Data \n")
###############################################################################

#letter_beginning0 <- read.csv(file=paste(input_directory,"letter_beginning.csv",sep="\\"),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
letter_beginning0 <- read.table(file=paste(input_directory,"letter_beginning.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_ending0 <- read.table(file=paste(input_directory,"letter_ending.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_position0 <- read.table(file=paste(input_directory,"letter_position.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_signature0 <- read.table(file=paste(input_directory,"letter_signature.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
letter_closing0 <- read.table(file=paste(input_directory,"letter_closing.csv",sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

###############################################################################
cat("Substitute External Text Data \n")
###############################################################################

letter_beginning0[,c("BEGINNINGS")] <- paste(" ",letter_beginning0[,c("BEGINNINGS")]," ",sep="")
letter_beginning0[,c("BEGINNINGS")] <- gsub("([[:punct:]])"," \\1 ",letter_beginning0[,c("BEGINNINGS")])
letter_beginning0_dt <- data.table(letter_beginning0)

letter_ending0[,c("ENDINGS")] <- paste(" ",letter_ending0[,c("ENDINGS")]," ",sep="")
letter_ending0[,c("ENDINGS")] <- gsub("([[:punct:]])"," \\1 ",letter_ending0[,c("ENDINGS")])
letter_ending0_dt <- data.table(letter_ending0)

letter_position0[,c("POSITIONS")] <- paste(" ",letter_position0[,c("POSITIONS")]," ",sep="")
letter_position0[,c("POSITIONS")] <- gsub("([[:punct:]])"," \\1 ",letter_position0[,c("POSITIONS")])
letter_position0_dt <- data.table(letter_position0)

letter_signature0[,c("SIGNATURES")] <- paste(" ",letter_signature0[,c("SIGNATURES")]," ",sep="")
letter_signature0[,c("SIGNATURES")] <- gsub("([[:punct:]])"," \\1 ",letter_signature0[,c("SIGNATURES")])
letter_signature0_dt <- data.table(letter_signature0)

letter_closing0[,c("CLOSINGS")] <- paste(" ",letter_closing0[,c("CLOSINGS")]," ",sep="")
letter_closing0[,c("CLOSINGS")] <- gsub("([[:punct:]])"," \\1 ",letter_closing0[,c("CLOSINGS")])
letter_closing0_dt <- data.table(letter_closing0)


#rm(letter_beginning0,letter_ending0,letter_position0,letter_signature0,letter_closing0)

for(k in 1:nrow(hash_table_final))
{
  # k <- 1
  # k <- 254

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

#rm(letter_beginning0_dt,letter_ending0_dt,letter_position0_dt,letter_signature0_dt,letter_closing0_dt)

aa <- adply(.data=hash_table_final, .margins=1, .fun = function(x,data){
  
  # x <- hash_table_final[1,]
  
  data[,"BEGINNINGS"] <- gsub(x[,"PATTERN"],x["REPLACEMENT"],data[,"BEGINNINGS"])
  return(data)
  
}, data=letter_beginning1, .expand = FALSE, .progress = "text", .inform = FALSE, .parallel = FALSE,.paropts = NULL)


bb <- ddply(.data=aa[,], .variables="BEGINNINGS", .fun = function(x){
  # x <- aa[aa[,"BEGINNINGS"]==" ALL CLIENT ",]
  
  p_u <- unique(x[,"PRIORITY"])
  b_u <- unique(x[,"BEGINNINGS"])
  
  x_count <- unique(data.frame(PRIORITY=p_u,BEGINNINGS=b_u,count=nrow(x),stringsAsFactors=FALSE))
  return(x_count)
}, .progress = "text",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


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
cat("Output Data \n")
###############################################################################

write.table(hash_table_final,file=paste(input_directory,"\\","hash_table_final",".csv",sep=""), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

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
