#### LIBRARIES ####

library(data.table)
library(gdata)
library(plyr)
library(reshape2)
library(stringr)


#### FUNCTIONS ####

clean_data <- function(x){
  
  require(gdata)
  
  extract_stacked <- x
  
  extract_stacked[,1] <- gsub(" {2,}", " ",extract_stacked[,1])
  extract_stacked[,1] <- gsub("^\\s+|\\s+$", "", extract_stacked[,1])
  
  extract_stacked[,1] <- gsub("\\.+","\\.",extract_stacked[,1])
  #extract_stacked[,1] <- gsub(" \\.","\\.",extract_stacked[,1])
  #extract_stacked[,1] <- gsub("\\. ","\\.",extract_stacked[,1])
  #extract_stacked[,1] <- gsub("\\.\\.","\\.",extract_stacked[,1])
  
  extract_stacked[,1] <- gsub("-+","-",extract_stacked[,1])
  extract_stacked[,1] <- gsub(" -","-",extract_stacked[,1])
  extract_stacked[,1] <- gsub("- ","-",extract_stacked[,1])
  extract_stacked[,1] <- gsub("-+","-",extract_stacked[,1])
  
  extract_stacked[,1] <- gsub(" {2,}", " ",extract_stacked[,1])
  extract_stacked[,1] <- gsub("^\\s+|\\s+$", "", extract_stacked[,1])
  
  
  for (i in 1:ncol(extract_stacked))
  {
    extract_stacked[,i] <- unknownToNA(extract_stacked[,i], unknown=c("",".","n/a","na","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                      NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                      NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
    extract_stacked[,i] <- ifelse(is.na(extract_stacked[,i]),NA, extract_stacked[,i])
  } 
  rm(i)
  
  return(extract_stacked)
  
}

#### PARAMETERS ####

directory <- "C:\\Users\\S.Brad\\Desktop"
#in_file <- "temp1.csv"
#out_file <- "temp1_out.csv"
in_file <- "temp2.csv"
out_file <- "temp2_out.csv"


#### INPUT DATA ####

existing_hyphens <- 

#strings <-  data.frame(matrix(NA, ncol=2, nrow=5, dimnames=list(c(), c("STRINGS1","STRINGS"))), stringsAsFactors=FALSE)
#strings[1,] <-  c("trash one-hyphen two-hyphen nothing", "ROW 1, SENTENCE TWO")
#strings[2,] <-  c("one-hyphen", "    ROW 2 ---- SENTENCE 2   ")
#strings[3,] <-  c("no matches", NA)
#strings[4,] <-  c("matches\n\none-match")
#strings[5,] <-  c(..)

strings <- read.table(file=paste(directory,in_file,sep="\\"), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

colnames(strings) <- paste("STRINGS",seq(1,ncol(strings),1),sep="")

#### TEXT SUBSTITUTION ####

for(i in 1:ncol(strings))
{
  strings[,i] <- iconv(strings[,i], "latin1", "ASCII", sub=" ")
  strings[,i] <- gsub("\\n+", " ", strings[,i])
  strings[,i] <- gsub("\\t+", " ", strings[,i])
  strings[,i] <- gsub('\"+', " ", strings[,i])
  strings[,i] <- gsub(":+", " ", strings[,i])
  strings[,i] <- gsub(";+", " ", strings[,i])
  strings[,i] <- gsub(",+", " ", strings[,i])
  strings[,i] <- gsub("\\?+", " ", strings[,i])
  strings[,i] <- gsub("!+", " ", strings[,i])
  strings[,i] <- gsub("\\*+", " ", strings[,i])
  strings[,i] <- gsub("=+", " ", strings[,i])
  strings[,i] <- toupper(gsub("=+", " ", strings[,i]))
}
rm(i)


#### STACK ALL COLUMNS ####

strings_id <-  data.frame(ID=NA,strings, stringsAsFactors=FALSE)
strings_id[,"ID"] <- seq(1,nrow(strings_id),1)
rm(strings)

strings_stacked <- melt(strings_id, id.vars=1)
rm(strings_id)

strings_stacked2 <- strings_stacked[,!(colnames(strings_stacked) %in% c("ID","variable"))]
strings_stacked2 <- as.data.frame(strings_stacked2, stringsAsFactors=FALSE)
colnames(strings_stacked2) <- "STRINGS"

rm(strings_stacked)


#### CLEAN DATA ####

strings_stacked2[,1] <- paste(" ",strings_stacked2[,1], " ", sep="")

strings_stacked3 <- clean_data(strings_stacked2)
rm(strings_stacked2)

strings_stacked3 <- strings_stacked3[!is.na(strings_stacked3[,"STRINGS"]),]
strings_stacked3 <- unique(strings_stacked3)
strings_stacked3 <- as.data.frame(strings_stacked3, stringsAsFactors=FALSE)
colnames(strings_stacked3) <- "STRINGS"


#### SPLIT DATA ####

strings_stacked3[,1] <- paste(" ",strings_stacked3[,1], " ", sep="")

strings_split <- strsplit(strings_stacked3[,1], " ", fixed = FALSE, perl = TRUE, useBytes = FALSE)
rm(strings_stacked3)


#### EXTRACT HYPEN WORDS ####

extract <- llply(.data=strings_split, .fun = function(x){
  
  #  x <- strings_split[1] 
  
  x_temp <- ifelse(grepl("-", x,ignore.case = TRUE, perl = TRUE), x, NA)
  x_temp2 <- x_temp[!is.na(x_temp)]
  
  return(x_temp2)
  
  
}, .progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
rm(strings_split)

extract_stacked <- unlist(extract)
extract_stacked <- as.data.frame(extract_stacked, stringsAsFactors=FALSE)
colnames(extract_stacked) <- "STRINGS"


#### CLEAN DATA ####

extract_stacked[,1] <- paste(" ",extract_stacked[,1], " ", sep="")

extract_stacked2 <- clean_data(extract_stacked)
rm(extract_stacked)

extract_stacked2 <- extract_stacked2[!is.na(extract_stacked2[,"STRINGS"]),]
extract_stacked2 <- unique(extract_stacked2)
extract_stacked2 <- as.data.frame(extract_stacked2, stringsAsFactors=FALSE)
colnames(extract_stacked2) <- "STRINGS"


#### REMOVE FILENAMES ####

extract_stacked_trim1 <- extract_stacked2
rm(extract_stacked2)

extract_stacked_trim1[,"STRINGS"] <- ifelse(grepl(".csv", extract_stacked_trim1[,"STRINGS"],ignore.case = TRUE, perl = TRUE), NA, extract_stacked_trim1[,"STRINGS"])
extract_stacked_trim1[,"STRINGS"] <- ifelse(grepl(".txt", extract_stacked_trim1[,"STRINGS"],ignore.case = TRUE, perl = TRUE), NA, extract_stacked_trim1[,"STRINGS"])
extract_stacked_trim1[,"STRINGS"] <- ifelse(grepl(".htm", extract_stacked_trim1[,"STRINGS"],ignore.case = TRUE, perl = TRUE), NA, extract_stacked_trim1[,"STRINGS"])

extract_stacked_trim1 <- extract_stacked_trim1[!is.na(extract_stacked_trim1[,"STRINGS"]),]

extract_stacked_trim1 <- unique(extract_stacked_trim1)
extract_stacked_trim1 <- as.data.frame(extract_stacked_trim1, stringsAsFactors=FALSE)
colnames(extract_stacked_trim1) <- "STRINGS"


#### REMOVE SELECTED END PUNCUATION ####

extract_stacked_trim2 <- extract_stacked_trim1
rm(extract_stacked_trim1)

extract_stacked_trim2[,1] <- gsub("[?.!,;]?$", "", extract_stacked_trim2[,1])

extract_stacked_trim2 <- unique(extract_stacked_trim2)
extract_stacked_trim2 <- as.data.frame(extract_stacked_trim2, stringsAsFactors=FALSE)
colnames(extract_stacked_trim2) <- "STRINGS"


#### OUTPUT DATA ####

extract_stacked_final <- extract_stacked_trim2
rm(extract_stacked_trim2)

extract_stacked_final <- extract_stacked_final[order(extract_stacked_final[,"STRINGS"]),]
extract_stacked_final <- as.data.frame(extract_stacked_final, stringsAsFactors=FALSE)
colnames(extract_stacked_final) <- "STRINGS"
row.names(extract_stacked_final) <- seq(nrow(extract_stacked_final))


write.table(extract_stacked_final,file=paste(directory,out_file,sep="\\"), append=FALSE, na="NA", 
            sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)



