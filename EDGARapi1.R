# https://www.sec.gov/edgar/sec-api-documentation
# load packages
require("jsonlite"); require("data.table");require("httr");require("pbapply");require("stringr");require("plyr")
# assign user agent
PASS <- new.env()
assign("usrAgent","companyname.com email@companyName.com",env=PASS)
# ****************************************************************************************************************
#                                        Read in CIK Codes : 
# ****************************************************************************************************************
# read in list of CIK codes
INFO <- read_json("https://www.sec.gov/files/company_tickers.json")
INFO <- rbindlist(INFO)
# CIK numbers are 10 digits - we have to fill in with zeros
INFO$CIK = do.call(rbind, lapply(as.list(1:nrow(INFO)), function(ii){
  ZEROS = 10-as.numeric(str_count(INFO$cik_str[ii]))
  paste0(c(rep(0,ZEROS),INFO$cik_str[ii]), collapse = "")
}))
INFO <- as.data.frame(INFO)
# *****************************************************************************************************************
# *****************************************************************************************************************
# function to lookup CIK number by ticker
getCIK = function(symbol){
  subset(INFO, INFO$ticker == paste(symbol))$CIK
}
# *****************************************************************************************************************
# *****************************************************************************************************************
# gets all filing values for a specific ticker
getAllEDGAR = function(ticker)
{
  # get CIK # for ticker
  CIK = getCIK(ticker)
  # get data by passing in url & headers
  pg <- GET(url = paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK",CIK,".json"),
            config = httr::add_headers(`User-Agent` = PASS$usrAgent,
                                       `Accept-Encoding` = 'gzip, deflate'))
  
  # raw data
  data_raw <- try(content(pg, as="text", encoding="UTF-8") %>% fromJSON(pg, flatten=FALSE),silent = TRUE)
  # ********************************************************************************************************
  #                                                   DEI
  # ********************************************************************************************************
  N = length(data_raw$facts$dei)
  if(N >= 1)
  {
    DEI = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(rbindlist(data_raw$facts$dei[[ii]]$units[[1]],use.names = TRUE, fill = TRUE))
      # add description column
      tmp$desc <- names(data_raw$facts$dei)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      tmp
    }),use.names=TRUE, fill=TRUE)
  }else{
    DEI = NULL
  }
  # ********************************************************************************************************
  #                                                   INVEST
  # ********************************************************************************************************
  N = length(data_raw$facts$invest)
  if(N >= 1)
  {
    INVEST = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(rbindlist(data_raw$facts$invest[[ii]]$units[[1]],use.names = TRUE, fill = TRUE))
      # add description column
      tmp$desc <- names(data_raw$facts$invest)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      tmp
    }),use.names=TRUE, fill=TRUE)
  }else{
    INVEST = NULL
  }
  # ********************************************************************************************************
  #                                                   SRT
  # ********************************************************************************************************
  N = length(data_raw$facts$srt)
  if(N >= 1)
  {
    SRT = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(rbindlist(data_raw$facts$srt[[ii]]$units[[1]],use.names = TRUE, fill = TRUE))
      # add description column
      tmp$desc <- names(data_raw$facts$srt)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      tmp
    }),use.names=TRUE, fill=TRUE)
    
  }else{
    SRT = NULL
  }
  # ********************************************************************************************************
  #                                                   US-GAAP
  # ********************************************************************************************************
  N = length(data_raw$facts$`us-gaap`)
  if(N >= 1)
  {
    GAAP = rbindlist(lapply(as.list(1:N), function(ii){
      # extract data
      tmp = as.data.frame(rbindlist(data_raw$facts$`us-gaap`[[ii]]$units[[1]],use.names = TRUE, fill = TRUE))
      # add description column
      tmp$desc <- names(data_raw$facts$`us-gaap`)[ii]
      # delete duplicates
      tmp <- tmp[!duplicated(tmp$end),]
      # add ticker column
      tmp$symbol <- ticker
      # return df
      tmp
    }),use.names=TRUE, fill=TRUE)
    # re-order
    GAAP = GAAP[,c("start","end","val","accn","fy","fp","form","filed","frame","desc","symbol" )]
  }else{
    GAAP = NULL
  }
  # combine ALL data
  ALL <- rbind.fill(GAAP,DEI,SRT,INVEST)
  # return data frame
  ALL
}
# *****************************************************************************************************************
# *****************************************************************************************************************
# test function
df = getAllEDGAR(ticker="TSLA")

