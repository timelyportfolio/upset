# try to break apart a summary table
# into the data format required by vcg upset

# not sure of the proper title or name for this
# but in effect want a single line with 0 or 1
# for each intersection

# make a function for the conversion
convertToUpset <- function ( dat ){
  # force convert to data.frame and hope it works
  # no error checking for now
  dat = data.frame( dat )
  
  # break each factor into a column name so a very wide format
  columnNames = unlist(lapply(
    names(dat)
    ,function(x){
      if(is.factor(dat[[x]])) paste0(x,"_",levels(dat[[x]]))
    }
  ))
  
  # now we need to get 1 or 0 for the intersections
  upset_df <- structure(
    as.data.frame(
      matrix(
        unlist(apply(
          dat
          ,MARGIN=1
          ,function(x){
            rep(as.numeric(unname(is.element(
              columnNames
              , paste0(names(x)[-length(x)],"_",x[-length(x)])
            ))),x[length(x)])
          }
        ))
        ,ncol=length(columnNames),byrow=T
      )
    )
    ,names=columnNames
  )

  # set up json meta
  upset_json = list(
    file = "data/testR/testR.csv"
    ,name = "R Upsetted Table"
    ,header = 0
    ,separator = ","
    ,skip = 0
    ,meta = data.frame(
      type = "id", index = 0, name = "Name"
    )
    ,sets = data.frame(
      format = "binary", start = 1, end = ncol(upset_df)
    )
  )
  
  return(list(df = upset_df,json = upset_json))
}

# convert data
testDat  = convertToUpset( HairEyeColor )

dir.create("data/testR")

# write files for testing
write.csv(testDat$df, file = "data/testR/testR.csv")
writeLines(jsonlite::toJSON(testDat$json,auto_unbox=T),con="data/testR/testR.json")


# this function unlike the first
# only converts the first column to factor names
# and makes the rest attributes
convertToUpset2 <- function( dat ){
  # force to data.frame
  dat = data.frame( dat )
  
  columnNames = paste0(
    colnames(dat)[1]
    ,"_"
    , if(is.factor(dat[,1])){
      levels(dat[,1])
      } else unique(dat[,1])
  )
  
  dat %>>%
    #( grouped_df( .,vars = lapply(colnames(.)[-length(.)],function(x){x}) ) ) %>>%
    #( grouped_df( .,vars = list(colnames(.)[1]) ) ) %>>%
    (
      by_group ~
        apply(
          by_group
          ,MARGIN=1
          ,function(x){
            rep(
                c(as.numeric(is.element(
                  columnNames
                  , paste0(names(x)[1],"_",x[1])
                )),x[-c(1,length(x))])
              ,x[length(x)]
            )
          }
        )
    ) %>>%
    unlist %>>%
    matrix( byrow = T, ncol = length(columnNames) + ncol(dat) - 2 ) %>>%
    (structure(
      data.frame(.)
      ,names=c(columnNames,colnames(dat)[-c(1,ncol(dat))])
    )) -> upset_df
  
  # set up json meta
  upset_json = list(
    file = "data/testR2/testR.csv"
    ,name = "R Upsetted Table"
    ,header = 0
    ,separator = ","
    ,skip = 0
    ,meta = rbind(
      data.frame(type = "id", index = 0, name = "Name")
      ,data.frame(type = "string", index = length(columnNames) + 1, name = "Eye")
      ,data.frame(type = "string", index = length(columnNames) + 2, name = "Sex")
    )
    ,sets = data.frame(
      format = "binary", start = 1, end = length(columnNames)
    )
  )
  
  return(list(df = upset_df,json = upset_json))  
}

# convert data
testDat  = convertToUpset2( HairEyeColor )

dir.create("data/testR2")

# write files for testing
write.csv(testDat$df, file = "data/testR2/testR.csv")
writeLines(jsonlite::toJSON(testDat$json,auto_unbox=T),con="data/testR2/testR.json")




# this function unlike the first 2 will pass a frequency for each intersection
convertToUpset3 <- function( dat ){
  # force to data.frame
  dat = data.frame( dat )
  
  # break each factor into a column name so a very wide format
  columnNames = unlist(lapply(
    names(dat)
    ,function(x){
      if(is.factor(dat[[x]])) paste0(x,"_",levels(dat[[x]]))
    }
  ))
  
  # now we need to get 1 or 0 for the intersections
  upset_df <- structure(
    as.data.frame(
      matrix(
        unlist(apply(
          dat
          ,MARGIN=1
          ,function(x){
            c(as.numeric(unname(is.element(
              columnNames
              , paste0(names(x)[-length(x)],"_",x[-length(x)])
            ))),x[length(x)])
          }
        ))
        ,ncol=length(columnNames) + 1,byrow=T
      )
    )
    ,names=c(columnNames,"freq")
  )  
 
  # set up json meta
  upset_json = list(
    file = "data/testR3/testR.csv"
    ,name = "R Upsetted Table"
    ,header = 0
    ,separator = ","
    ,skip = 0
    ,meta = rbind(
      data.frame(type = "id", index = 0, name = "Name")
      ,data.frame(type = "integer", index = length(columnNames) + 1, name = "Freq")
    )
    ,sets = data.frame(
      format = "binary", start = 1, end = length(columnNames)
    )
  )
  
  return(list(df = upset_df,json = upset_json))  
}

# convert data
testDat  = convertToUpset3( HairEyeColor )

dir.create("data/testR3")

# write files for testing
write.csv(testDat$df, file = "data/testR3/testR.csv")
writeLines(jsonlite::toJSON(testDat$json,auto_unbox=T),con="data/testR3/testR.json")





# this function unlike the first 2 will pass a frequency for each intersection
convertToUpset4 <- function( dat ){
  # force to data.frame
  dat = data.frame( dat )
  
  # break each factor into a column name so a very wide format
  columnNames = unlist(lapply(
    names(dat)
    ,function(x){
      if(is.factor(dat[[x]])) paste0(x,"_",levels(dat[[x]]))
    }
  ))
  
  # just for testing
  # since last (sex) is true/false
  # remove that columnName
  columnNames = columnNames[-length(columnNames)]
  
  # now we need to get 1 or 0 for the intersections
  upset_df <- structure(
    as.data.frame(
      matrix(
        unlist(apply(
          dat
          ,MARGIN=1
          ,function(x){
            c(as.numeric(unname(is.element(
              columnNames
              , paste0(names(x)[-length(x)],"_",x[-length(x)])
            ))),x[length(x)])
          }
        ))
        ,ncol=length(columnNames) + 1,byrow=T
      )
    )
    ,names=c(columnNames,"freq")
  )  
  
  # set up json meta
  upset_json = list(
    file = "data/testR4/testR.csv"
    ,name = "R Upsetted Table"
    ,header = 0
    ,separator = ","
    ,skip = 0
    ,meta = rbind(
      data.frame(type = "id", index = 0, name = "Name")
      ,data.frame(type = "integer", index = length(columnNames) + 1, name = "Freq")
    )
    ,sets = data.frame(
      format = "binary", start = 1, end = length(columnNames)
    )
  )
  
  return(list(df = upset_df,json = upset_json))  
}

# convert data
testDat  = convertToUpset4( HairEyeColor )

dir.create("data/testR4")

# write files for testing
write.csv(testDat$df, file = "data/testR4/testR.csv")
writeLines(jsonlite::toJSON(testDat$json,auto_unbox=T),con="data/testR4/testR.json")










##### try with titanic
# convert data
testDat  = convertToUpset( Titanic )
testDat$json$name = "R Titanic"
testDat$json$file = "data/testR_titanic/testR.csv"
dir.create("data/testR_titanic")

# write files for testing
write.csv(testDat$df, file = "data/testR_titanic/testR.csv")
writeLines(jsonlite::toJSON(testDat$json,auto_unbox=T),con="data/testR_titanic/testR.json")
