# RFM
source("/home/billiout/mixalis/tafeng/prep_dataset.R")
df <- get_data()

max_date <- max(df$trans_date) + 1

df$revenue <- df$price - df$asset
transactions <- aggregate(df$revenue~df$trans_date+df$cust_id, FUN=sum)

R_table <- aggregate(trans_date ~ cust_id, df, FUN=max) # Calculate R
R_table$R <- as.numeric(max_date - R_table$trans_date)

F_table <- aggregate(trans_date ~ cust_id, transactions, FUN=length) # Calculate F

M_table <- aggregate(price ~ cust_id, df, FUN=sum) # Calculate M

RFM_table <- merge(R_table,F_table,by.x="cust_id", by.y="cust_id") # Merge R with F
RFM_table <- merge(RFM_table,M_table, by.x="cust_id", by.y="cust_id") # Merge M into RF
RFM_table$trans_date.x <- NULL # Remove unnecessary column
names(RFM_table) <- c("cust_id", "R", "F", "M") # And change names


getIndependentScore <- function(df,r=5,f=5,m=5) {
  
  if (r<=0 || f<=0 || m<=0) return
  
  #order and the score
  df <- df[order(df$R,-df$F,-df$M),]
  R_Score <- scoring(df,"R",r)
  df <- cbind(df, R_Score)
  
  df <- df[order(-df$F,df$R,-df$M),]
  F_Score <- scoring(df,"F",f)
  df <- cbind(df, F_Score)
  
  df <- df[order(-df$M,df$R,-df$F),]
  M_Score <- scoring(df,"M",m)
  df <- cbind(df, M_Score)
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  # caculate the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  df$R_Score <- as.factor(df$R_Score)
  df$F_Score <- as.factor(df$F_Score)
  df$M_Score <- as.factor(df$M_Score)
  df$Total_Score <- as.factor(df$Total_Score)
  return (df)
  
} # end of function getIndependentScore

scoring <- function (df,column,r=5){
  
  #get the length of rows of df
  len <- dim(df)[1]
  
  score <- rep(0,times=len)
  
  # get the quantity of rows per 1/r e.g. 1/5
  nr <- round(len / r)
  if (nr > 0){
    
    # seperate the rows by r aliquots
    rStart <-0
    rEnd <- 0
    for (i in 1:r){
      
      #set the start row number and end row number
      rStart = rEnd+1
      
      #skip one "i" if the rStart is already in the i+1 or i+2 or ...scope.
      if (rStart> i*nr) next
      
      if (i == r){
        if(rStart<=len ) rEnd <- len else next
      }else{
        rEnd <- i*nr
      }
      
      # set the Recency score
      score[rStart:rEnd]<- r-i+1
      
      # make sure the customer who have the same recency have the same score
      s <- rEnd+1
      if(i<r & s <= len){
        for(u in s: len){
          if(df[rEnd,column]==df[u,column]){
            score[u]<- r-i+1
            rEnd <- u
          }else{
            break;
          }
        }
        
      }
      
    }
    
  }
  return(score)
  
} #end of function Scoring



drawHistograms <- function(df,r=5,f=5,m=5){
  
  #set the layout plot window
  par(mfrow = c(f,r))
  par(mar = c(1, 4, 1, 4))
  names <-rep("",times=m)
  for(i in 1:m) names[i]<-paste("M",i)
  
  
  for (i in 1:f){
    for (j in 1:r){
      c <- rep(0,times=m)
      for(k in 1:m){
        tmpdf <-df[df$R_Score==j & df$F_Score==i & df$M_Score==k,]
        c[k]<- dim(tmpdf)[1]
        
      }
      if (i==1 & j==1) 
        barplot(c,col="lightblue",names.arg=names)
      else
        barplot(c,col="lightblue")
      if (j==1) title(ylab=paste("F",i))	
      if (i==1) title(main=paste("R",j))	
      
    }
    
  }
  
  par(mfrow = c(1,1))
  
} # end of drawHistograms function

RFM_table <- getIndependentScore(RFM_table)

RFM_table$age_band <- df[match(RFM_table$cust_id, df$cust_id),"age_band"] 
RFM_table$res_area <- df[match(RFM_table$cust_id, df$cust_id),"res_area"] 


ggplot(RFM_table, aes(x=M)) + geom_density() + facet_grid(.~res_area, labeller = label_both)


library(gclus)
dta <- RFM_table[c(2,3,4)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="RFM Variables Ordered and Colored by Correlation" )

#RFM_table$Rsegment <- findInterval(RFM_table$R, quantile(RFM_table$R, c(0.0, 0.25, 0.50, 0.75, 1.0)))
#RFM_table$Fsegment <- findInterval(RFM_table$F, quantile(RFM_table$F, c(0.0, 0.25, 0.50, 0.75, 1.0)))
#RFM_table$Msegment <- findInterval(RFM_table$M, quantile(RFM_table$M, c(0.0, 0.25, 0.50, 0.75, 1.0)))
#RFM_table$RFM <- paste0(RFM_table$Rsegment, RFM_table$Fsegment, RFM_table$Msegment)

#RFM_table$Rscaled <- scale(RFM_table$R)
#RFM_table$Fscaled <- scale(RFM_table$F)
#RFM_table$Mscaled <- scale(RFM_table$M)

