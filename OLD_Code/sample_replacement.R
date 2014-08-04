
punctuation_words_ordinal_replace1[,"REPLACEMENT"] <- ldply(.data=punctuation_words_ordinal_replace1[,"REPLACEMENT"], .fun = function(x,pattern1,pattern2,lookup){
  
  # x <- "1st-://2ND"
  # x <- punctuation_words_ordinal_replace1[803,"REPLACEMENT"]
  # x <- punctuation_words_ordinal_replace1[1990,"REPLACEMENT"]
  # x <- punctuation_words_ordinal_replace1[1991,"REPLACEMENT"]
  # pattern1 <- pattern1a
  # pattern2 <- pattern1b
  # lookup <- ordinal_lookup1
  
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
  
}, pattern1=pattern1a, pattern2=pattern1b, lookup=ordinal_lookup1,.progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL, .id = NA)

rm(pattern0,pattern1a,pattern1b)









punctuation_words_contraction_replace1[,"REPLACEMENT"] <- ldply(.data=punctuation_words_contraction_replace1[,"REPLACEMENT"], .fun = function(x,pattern1,pattern2,lookup){
  
  # x <- punctuation_words_contraction_replace1[605,"REPLACEMENT"]
  # x <- punctuation_words_contraction_replace1[606,"REPLACEMENT"]
  # pattern1 <- pattern1a
  # pattern2 <- pattern1b
  # lookup <- contractions_expand
  
  
  aa1a <- strsplit(x,pattern1,perl=TRUE)
  aa1b <- unlist(aa1a)
  
  aa2a <- strapply(aa1b, pattern2, ~ c(...), b= -2)
  aa2b <- unlist(aa2a)
  
  #bb <- aa2b
  bb <- data.table(aa2b)
  setnames(bb,"WORD")
  
  #bb <- replace_contraction(text.var=bb, contraction = contractions_expand,replace = NULL, ignore.case = TRUE, sent.cap = TRUE)
  
  
  for(k in 1:nrow(lookup))
  {
    # k <- 1
    
    set(bb, i=NULL, j="WORD", value=gsub(lookup[k,c("PATTERN")], lookup[k,c("REPLACEMENT")], bb[["WORD"]], ignore.case = TRUE, perl=TRUE))
    
    #bb <- gsub(lookup[k,c("PATTERN")], lookup[k,c("REPLACEMENT")], bb, ignore.case = TRUE, perl=TRUE)
    #bb1a <- strsplit(bb,pattern1,perl=TRUE)
    #bb1b <- unlist(bb1a)
    #bb2a <- strapply(bb1b, pattern2, ~ c(...), b= -2)
    #bb2b <- unlist(bb2a)
    #bb <- bb2b
    
  }
  rm(k)
  
  cc <- paste(unlist(bb),sep="",collapse="")
  
  return(cc)
  
}, pattern1=pattern1a, pattern2=pattern1b,lookup=contractions_expand, .progress = "text", .inform = FALSE,.parallel = FALSE, .paropts = NULL, .id = NA)

rm(pattern0,pattern1a,pattern1b)




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



abbreviations_expand0 <- abbreviations

aa <- adply(.data=abbreviations1, .margins=1, .fun = function(x,data){
  
  # x <-  abbreviations1[1,]
  # data <- abbreviations_expand0
  
  #temp_pattern <- x[,"PATTERN"]
  #temp_replacement <- x[,"REPLACEMENT"]
  
  temp_pattern <- paste(" ",x[,"PATTERN"]," ",sep="")
  temp_replacement <- paste(" ",x[,"REPLACEMENT"]," ",sep="")
  
  data_expand <- sapply(data, rep.int, times=4)
  data_expand <- as.data.frame(data_expand,stringsAsFactors=FALSE)
  
  data_expand[((nrow(data)*1)+1):(nrow(data)*2),"PATTERN"] <- sub(temp_pattern,temp_replacement,data_expand[((nrow(data)*1)+1):(nrow(data)*2),"PATTERN"])
  #data_expand[((nrow(data)*2)+1):(nrow(data)*3),"REPLACEMENT"] <- sub(temp_pattern,temp_replacement,data_expand[((nrow(data)*1)+1):(nrow(data)*2),"REPLACEMENT"])
  
  #data_expand[((nrow(data)*3)+1):(nrow(data)*4),"PATTERN"] <- sub(temp_pattern,temp_replacement,data_expand[((nrow(data)*1)+1):(nrow(data)*2),"PATTERN"])
  #data_expand[((nrow(data)*3)+1):(nrow(data)*4),"REPLACEMENT"] <- sub(temp_pattern,temp_replacement,data_expand[((nrow(data)*1)+1):(nrow(data)*2),"REPLACEMENT"])
  
  data_expand[,"PATTERN"] <- gsub(" {2,}", " ", data_expand[,"PATTERN"])
  #data_expand[,"PATTERN"] <- gsub("^\\s+|\\s+$", "", data_expand[,"PATTERN"])
  
  data_expand[,"REPLACEMENT"] <- gsub(" {2,}", " ", data_expand[,"REPLACEMENT"])
  #data_expand[,"REPLACEMENT"] <- gsub("^\\s+|\\s+$", "", data_expand[,"REPLACEMENT"])
  
  data_expand <- data_expand[(data_expand[,"REPLACEMENT"] != data_expand[,"PATTERN"]),]
  
  data_expand <- unique(data_expand)
  data_expand <- as.data.frame(data_expand,stringsAsFactors=FALSE)
  data_expand <- data_expand[order(data_expand[,"PATTERN"],data_expand[,"REPLACEMENT"]),]
  row.names(data_expand) <- seq(nrow(data_expand))
  
  return(data_expand)
  
}, data=abbreviations_expand0, .expand = TRUE, .progress = "none", .inform = FALSE, .parallel = FALSE,.paropts = NULL)

aa[,"PATTERN"] <- gsub(" {2,}", " ", aa[,"PATTERN"])
aa[,"PATTERN"] <- gsub("^\\s+|\\s+$", "", aa[,"PATTERN"])

aa[,"REPLACEMENT"] <- gsub(" {2,}", " ", aa[,"REPLACEMENT"])
aa[,"REPLACEMENT"] <- gsub("^\\s+|\\s+$", "", aa[,"REPLACEMENT"])

aa <- aa[(aa[,"REPLACEMENT"] != aa[,"PATTERN"]),]

aa <- unique(aa)
aa <- as.data.frame(aa,stringsAsFactors=FALSE)
aa <- aa[order(aa[,"PATTERN"],aa[,"REPLACEMENT"]),]
row.names(aa) <- seq(nrow(aa))
