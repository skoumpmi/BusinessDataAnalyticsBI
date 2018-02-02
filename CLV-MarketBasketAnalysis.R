# CLV ARPU

source("/home/billiout/mixalis/tafeng/prep_dataset.R")
df <- get_data()

df$revenue <- df$price - df$asset

transactions <- aggregate(df$revenue~df$trans_date+df$cust_id, FUN=sum)
names(transactions) <- c("trans_date", "cust_id", "revenue")
transactions_max_date <- aggregate(transactions$trans_date~transactions$cust_id, FUN=max)
names(transactions_max_date) <- c("cust_id", "max_date")
transactions_min_date <- aggregate(transactions$trans_date~transactions$cust_id, FUN=min)
names(transactions_min_date) <- c("cust_id", "min_date")

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  diff <- ceiling(12 * (ed$year - sd$year) + (ed$mon - sd$mon) + (abs(ed$mday - sd$mday))/30)
  diff[diff==0] <- 1
  diff
}

transactions_overall <- aggregate(transactions$revenue~transactions$cust_id, FUN=sum)
names(transactions_overall) <- c("cust_id", "total_revenue")
transactions_overall <- merge(transactions_overall, transactions_min_date, by="cust_id")
transactions_overall <- merge(transactions_overall, transactions_max_date, by="cust_id")
transactions_overall$amr <- transactions_overall$total_revenue/elapsed_months(transactions_overall$max_date, transactions_overall$min_date)

transactions_overall$age_band <- df[match(transactions_overall$cust_id, df$cust_id),"age_band"] 
transactions_overall$res_area <- df[match(transactions_overall$cust_id, df$cust_id),"res_area"] 

means_age <- aggregate(amr ~  age_band, transactions_overall, mean)
means_age$amr <- round(means_age$amr,digits = 2)
rules<-apriori(retail.csv,parameter=list(supp=0.03, conf=0.7, target="rules"))

ggplot(transactions_overall, aes(x=age_band, y=amr, fill=age_band)) + 
  geom_boxplot() + stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means_age, aes(label = amr, y = amr + 140),fontface=3) +
  labs(y="CLV (ARPU)",x="Age Band",title="CLV by Age Band:\nA <25,B 25-29,C 30-34,D 35-39,E 40-44,F 45-49,G 50-54,H 55-59,I 60-64,J >65")+
  theme(plot.title=element_text(size=10,face="bold")) + guides(fill=guide_legend(title="Age Band"))

means_res <- aggregate(amr ~  res_area, transactions_overall, mean)
means_res$amr <- round(means_res$amr,digits = 2)

ggplot(transactions_overall, aes(x=res_area, y=amr, fill=res_area)) + 
  geom_boxplot() + stat_summary(fun.y=mean, colour="black", geom="point", shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = means_res, aes(label = amr, y = amr + 140),fontface=3) +
  labs(y="CLV (ARPU)",x="Residential Area",title="CLV by Residential Area:\nA-F: zipcode area: 105,106,110,114,115,221,G: others, H: Unknown
Distance to store, from the closest: 115,221,114,105,106,110")+
  theme(plot.title=element_text(size=10,face="bold")) + guides(fill=guide_legend(title="Residential Area"))


