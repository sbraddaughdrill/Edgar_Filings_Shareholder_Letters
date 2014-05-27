library(gdata)
library(sqldf)
library(XBRL)


ko = "http://www.sec.gov/Archives/edgar/data/21344/000002134413000050/ko-20130927.xml"
#ko = "http://www.sec.gov/Archives/edgar/data/1414040/000141404014000007/0001414040-14-000007.hdr.sgml"

xbrl.vars <- xbrlDoAll(ko, verbose=TRUE)

###combine things

xbrl_to_df = function(xbrl.vars){
  name_list = names(xbrl.vars)
  
  for(nom in name_list){
    
    eval(parse(text=paste0("df_",nom,"= xbrl.vars$",nom)))
    
  }
  query <- ""
  query <- paste(query, "select     a.elementid, a.contextid, a.unitid, a.fact, a.decimals, a.factid,         ", sep=" ")
  query <- paste(query, "           b.startdate, b.enddate, b.dimension1, b.value1, b.dimension2, b.value2,   ", sep=" ")
  query <- paste(query, "           b.dimension3, b.value3, b.dimension4, b.value4, c.footnoteString          ", sep=" ")
  query <- paste(query, "from       df_fact a                                                                 ", sep=" ")
  query <- paste(query, "left join  df_context b                                                              ", sep=" ")
  query <- paste(query, "on         a.contextid = b.contextid                                                 ", sep=" ")
  query <- paste(query, "left join  df_footnote c                                                             ", sep=" ")
  query <- paste(query, "on         c.factid=a.factid                                                         ", sep=" ")
  query <- gsub(" {2,}", " ", query)
  query <- gsub("^\\s+|\\s+$", "", query)
  
  big_data_frame <- sqldf(query)
  
  #big_data_frame = sqldf('select a.elementid, a.contextid, a.unitid, a.fact, a.decimals, a.factid, b.startdate,         
  #                       b.enddate, b.dimension1, b.value1, b.dimension2, b.value2, b.dimension3, b.value3, b.dimension4, 
  #                       b.value4,     c.footnoteString
  #                       from df_fact a left join df_context b on a.contextid = b.contextid left join df_footnote c on     c.factid=a.factid ')
  
  return(big_data_frame)
  
}

my_data <-  xbrl_to_df(xbrl.vars)