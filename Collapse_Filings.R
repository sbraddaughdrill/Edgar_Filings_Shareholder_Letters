
#CLEAN TEXT
filing_text_parse_clean <- ddply(.data=filing_text_letter8, .variables=c("file","DOCUMENT_INDEX","LETTER_INDEX"), 
                                 .fun = function(x,bycol,xmlcol){
                                   
                                   # x <- filing_text_letter8[filing_text_letter8[,"LETTER_INDEX"]==0,]
                                   # x <- filing_text_letter8[filing_text_letter8[,"LETTER_INDEX"]==1,]
                                   # x <- filing_text_letter8[filing_text_letter8[,"LETTER_INDEX"]==2,]
                                   # bycol <- c("file","DOCUMENT_INDEX","LETTER_INDEX")
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
                                   rm(x)
                                   
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
                                 .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)

rm(filing_text_letter8)










#THERE NEEDS TO BE A TEST TO CHECK IF CHARACTER COUNT IS LARGER THAN 32,767