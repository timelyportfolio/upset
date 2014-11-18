library(dplyr)
library(pipeR)
library(jsonlite)

##do it with data from research affiliates JOF article
#<blockquote>
#  <small><em style = "background-color:#E7E4E4;">data source:</em></small><br>
#  Arnott, Robert D., et al.<br>
#  <strong>The Surprising Alpha from Malkiel's Monkey and Upside-Down Strategies</strong><br>
#  The Journal of Portfolio Management 39.4 (2013): 91-105.
#</blockquote>
#read in the csv version of data
rebalStats <- read.csv(
  "../research_researchaffiliates/global rebalance allocation stats.csv",
  stringsAsFactors = F
)

# crude abbreviation of Strategy
rebalStats %>>%
  #filter( StrategyType == "Averages" ) %>>%
  #mutate( Strategy = sapply(
  #  Strategy
  #  , function(x){
  #    paste0(substring(unlist(strsplit(x, "[ ,( ]")),1,1) ,collapse="")
  #  }
  #))
  mutate(
    Geography_Global = as.numeric(Geography == "Global")
  )

rebalStats %>>%
  mutate(StrategyType = gsub(StrategyType,pattern="[=,-]",replacement="_")) %>>% 
 (rs~
   apply(
      rs
      , MARGIN = 1
      , function(x){
        unname(c(
          as.numeric(x["Geography"] == "Global")
          ,as.numeric(is.element(unique(rs$StrategyType),x["StrategyType"]))
          ,as.numeric(x["Return"])
        ))
      }
   ) %>>% t %>>% data.frame( stringsAsFactors = F ) %>>%
   structure(names = c("Global",unique(rebalStats$StrategyType),"Return"))
 ) %>>%
 (
    list(
      upset_df = .
      ,upset_json = list(
          file = "data/testR_jopm/testR.csv"
          ,name = "R JOPM Paper"
          ,header = 0
          ,separator = ","
          ,skip = 0
          ,meta = rbind(
            data.frame(type = "id", index = 0, name = "Name" )
            ,data.frame(type = "float", index = ncol(.), name = "Return" )
          )
          ,sets = data.frame(
            format = "binary", start = 1, end = ncol(.) - 1
          )
          ,description = "see post http://timelyportfolio.blogspot.com/2014/03/interactive-discovery-of-research.html"
      )
    )
  ) -> jopm

jopm %>>%
  (~ dir.create("data/testR_jopm") ) %>>%
  (~ write.csv(.$upset_df, file = "data/testR_jopm/testR.csv" ) ) %>>%
  (~ writeLines(
         jsonlite::toJSON(.$upset_json,auto_unbox=T)
         ,con="data/testR_jopm/testR.json"
       )
    )

 