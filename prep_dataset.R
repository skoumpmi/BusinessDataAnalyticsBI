
get_data <- function(){

  coldesc <- data.frame(names=c("trans_date","cust_id","age_band"
                               ,"res_area","prod_cat","prod_id"
                               ,"quantity","asset","price")
                       ,types=c("character","factor","factor"
                                ,"factor","factor","factor"
                                ,"numeric","numeric","numeric"),stringsAsFactors = F)
  
  df_nov <- read.csv("/home/billiout/mixalis/tafeng/dataset/D11.csv"
                                 ,sep=",",header=F,encoding="UTF-8"
                                 ,col.names=coldesc$names,colClasses=coldesc$types
                                 ,stringsAsFactors=F)
  
  df_dec <- read.csv("/home/billiout/mixalis/tafeng/dataset/D12.csv"
                                 ,sep=",",header=F,encoding="UTF-8"
                                 ,col.names=coldesc$names,colClasses=coldesc$types
                                 ,stringsAsFactors=F)
  
  df_jan <- read.csv("/home/billiout/mixalis/tafeng/dataset/D01.csv"
                                 ,sep=",",header=F,encoding="UTF-8"
                                 ,col.names=coldesc$names,colClasses=coldesc$types
                                 ,stringsAsFactors=F)
  
  df_feb <- read.csv("/home/billiout/mixalis/tafeng/dataset/D02.csv"
                                 ,sep=",",header=F,encoding="UTF-8"
                                 ,col.names=coldesc$names,colClasses=coldesc$types
                                 ,stringsAsFactors=F)
  
  data <- rbind(df_nov, df_dec, df_jan, df_feb)
  data$trans_date <- as.Date(data$trans_date)
  
  return(data)
}
