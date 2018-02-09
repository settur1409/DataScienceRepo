install.packages("sqldf")
install.packages("countrycode")
install.packages("tidyr")
install.packages("dplyr")
library(sqldf)
library(countrycode)
library(tidyr)
library(dplyr)

#Reading the data into dataframes

companies <- read.delim("companies.txt",header=TRUE, na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
rounds2 <- read.csv("rounds2.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)
mapping <- read.csv("mapping.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)




#Unique companies in rounds2 file
round2 = rounds2;
round2_company_permalink = round2$company_permalink
round2_company_df <- data.frame(round2_company_list = c(round2_company_permalink))
df2 = separate(round2_company_df, round2_company_list, c('link1','link2'), sep = 14)
unique_list1 = unique(tolower(df2$link2))
length(unique_list1)

#Unique companies in companies file
company_details = companies
company_permalink = company_details$permalink
company_df <- data.frame(company_list = c(company_permalink))
df3 = separate(company_df, company_list, c('link1','link2'), sep = 14)
unique_list2 = unique(tolower(df3$link2))
length(unique_list2)

#upper case to lower case convertion if any, to merge to rounds2 dataset
df2$link3 = paste(tolower(df2$link1), tolower(df2$link2))
df3$link3 = paste(tolower(df3$link1), tolower(df3$link2))

#creating new column with cleaned permalinks
round2$new_permalink = df2$link3
company_details$new_permalink = df3$link3

#Merging companies and rounds2 file into a single dataframe, all.x ensures that, no data missed from round2 dataset
master_frame <- merge(round2, company_details, by="new_permalink", all.x = TRUE)

#Average funding amounts


sqldf("select funding_round_type, avg(raised_amount_usd) from round2 
      where funding_round_type in ('venture','angel','seed','private_equity') 
      group by funding_round_type")

#Deciding which investment type is suitable for Sparks Fund

sqldf("select funding_round_type, avg(raised_amount_usd) from round2 
      where funding_round_type in ('venture','angel','seed','private_equity')
      group by funding_round_type having (avg(raised_amount_usd)>5000000 and avg(raised_amount_usd)<15000000)
      order by avg(raised_amount_usd) desc limit 1")

#Top three english speaking countries

country_list <- c("United States","India","Philippines","Nigeria","United Kingdom", "Germany","Canada","France","Australia","Italy")
country_list1 <- countrycode(country_list, 'country.name', 'iso3c')
eng_country <-data.frame(country_list1)
colnames(eng_country) <- "code"

top9 = sqldf("select country_code, sum(raised_amount_usd) from master_frame where funding_round_type='venture' group by country_code having country_code !='NA' order by sum(raised_amount_usd) desc limit 9")
top_eng = sqldf("select code from eng_country inner join top9 on top9.country_code=eng_country.code limit 3")

top_eng$code <- countrycode(top_eng$code, 'iso3c', 'country.name')
colnames(top_eng) <- 'Country'

#Sector wise analysis

sector_map <- gather(mapping, sector,val, Automotive...Sports:Social..Finance..Analytics..Advertising)
sector_map <- sector_map[!(sector_map$val==0),1:2]

sector_map[as.integer(nrow(sector_map)+1), ]<-"NA"
temp_category <- strsplit(as.character(master_frame$category_list), split='|', fixed=TRUE)
master_frame[,"Primary_sector"]<-do.call(rbind,temp_category)[,1]

temp2 = data.frame(sqldf("select sector_map.sector from master_frame left join sector_map 
                         on master_frame.Primary_sector=sector_map.category_list"))
master_frame[,"Main_sector"] = temp2$sector

temp_d1 <- sqldf("select * from master_frame where country_code='USA' and
                            funding_round_type='venture' and
                            (raised_amount_usd>5000000 and raised_amount_usd<15000000)")
temp_d2 <- sqldf("select * from master_frame where country_code='IND' 
                 and funding_round_type='venture' and 
                 (raised_amount_usd>5000000 and raised_amount_usd<15000000)")
temp_d3 <- sqldf("select * from master_frame where country_code='GBR' and
                 funding_round_type='venture' and 
                 (raised_amount_usd>5000000 and raised_amount_usd<15000000)")

dm1<-sqldf("select Main_sector, count(*) as total_count, sum(raised_amount_usd) as total_amount_invested from temp_d1 group by Main_sector")
dm2<-sqldf("select Main_sector, count(*) as total_count, sum(raised_amount_usd) as total_amount_invested from temp_d2 group by Main_sector")
dm3<-sqldf("select Main_sector, count(*) as total_count, sum(raised_amount_usd) as total_amount_invested from temp_d3 group by Main_sector")

D1 = merge(temp_d1, dm1, by='Main_sector', all.x = TRUE)
D2 = merge(temp_d2, dm2, by='Main_sector', all.x = TRUE)
D3 = merge(temp_d3, dm3, by='Main_sector', all.x = TRUE)

#Total number of investments
sqldf("select country_code, count(*) from master_frame where country_code in ('USA', 'IND', 'GBR') and funding_round_type='venture' group by country_code order by count(*) desc")

#Total amount of investment
sqldf("select country_code, sum(raised_amount_usd) from master_frame where country_code in ('USA', 'IND', 'GBR') and funding_round_type='venture' group by country_code")

#Top sectors by number ofinvestments
D1_top <- sqldf("select Main_sector, country_code, total_count from D1
                           group by Main_sector order by total_count desc limit 3")
D2_top <- sqldf("select Main_sector, country_code, total_count from D2
                           group by Main_sector order by total_count desc limit 3")
D3_top <- sqldf("select Main_sector, country_code, total_count from D3
                           group by Main_sector order by total_count desc limit 3")

#Companies which recieved highest investment in top 2 sectors by each country

sqldf("select name,sum(raised_amount_usd), country_code, Main_sector from D1 
        where total_count=2400
        group by permalink order by sum(raised_amount_usd) desc limit 1")

sqldf("select name,sum(raised_amount_usd), country_code, Main_sector from D1 
      where total_count=1950
      group by permalink order by sum(raised_amount_usd) desc limit 1")

sqldf("select name,sum(raised_amount_usd), country_code, Main_sector from D2
      where total_count=77
      group by permalink order by sum(raised_amount_usd) desc limit 1")

sqldf("select name,sum(raised_amount_usd), country_code, Main_sector from D2
      where total_count=41
      group by permalink order by sum(raised_amount_usd) desc limit 1")

sqldf("select name,sum(raised_amount_usd), country_code, Main_sector from D3
      where total_count=126
      group by permalink order by sum(raised_amount_usd) desc limit 1")


sqldf("select name,sum(raised_amount_usd), country_code, Main_sector from D3 
      where total_count=123
      group by permalink order by sum(raised_amount_usd) desc limit 1")