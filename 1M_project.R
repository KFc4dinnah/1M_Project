library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(plyr)
library(arules)
library(arulesViz)
#import the data
df <- read_excel("C:/Users/khiem.phung/Downloads/Test_Data_Skill.xlsx", 
                              sheet = "Data")

###SQL test
#first two services and the date 
df_first2 <- df %>%
  select(User_id,Serviceid,Date) %>%
  group_by(User_id) %>%
  arrange(Date) %>%
  group_by(User_id) %>%
  slice(1:2)
#last service and the date
df_last <- df %>%
  select(User_id,Serviceid,Date) %>%
  group_by(User_id) %>%
  arrange(desc(Date)) %>%
  group_by(User_id) %>%
  slice(1)

#distinct serviceid that users use
df_service <- df %>%
  distinct(User_id, Serviceid) %>%
  group_by(User_id) %>%
  count()

#put data in wide format
df_first_wide <- df_first2 %>%
  merge(df_first2[-1,], by = 'User_id') %>%
  group_by(User_id) %>%
  slice(1)

#merge all tables together 
df_sql <- df_first_wide %>%
  inner_join(df_last, by = 'User_id') %>%
  inner_join(df_service, by = 'User_id')

#rename the columns 
df_sql <- df_sql %>%
  rename(FirstServiceid = Serviceid.x,
         FirstServiceDate = Date.x,
         SecondServiceid = Serviceid.y,
         SecondServiceDate = Date.y,
         LastServiceid = Serviceid,
         LastServiceDate = Date,
         TotalService = n)

#reorder columns 
df_sql <- df_sql[c(1,2,4,3,5,6,7,8)]

###Analysis test 
#duplicate to a new df 
df_sql2 <- df_sql
#create customer's age column
df_sql2$Customer_age <- (df_sql$LastServiceDate
                        - df_sql$FirstServiceDate)
df_sql2$Customer_age <- time_length(df_sql2$Customer_age,
                                    unit = 'days')

#create table with user id and their age 
df_age <- df_sql2 %>%
  select(User_id, Customer_age)
#join that table with the original table to get user id, their age, and all
#service ids they use
df_age <- df_age %>%
  inner_join(df, by = 'User_id')

#select only age groups and all service id used 
df_age2 <- df_age %>%
  ungroup(User_id) %>%
  select(Customer_age, Serviceid) %>%
  distinct()

#initially visualize the clusters using scatterplot
ggplot(df_age2, aes(x = Customer_age,
                    y = Serviceid)) +
  geom_point()

#print out serviceid with customer age < 90
df_age2 %>%
  group_by(Customer_age) %>%
  filter(Customer_age < 90) %>%
  ungroup(Customer_age) %>%
  select(Serviceid)

#print out serviceid with customer age > 90 and < 365
df_age2 %>%
  group_by(Customer_age) %>%
  filter(Customer_age > 90, Customer_age < 365) %>%
  ungroup(Customer_age) %>%
  select(Serviceid)

#print out serviceid with customer age > 365
df_age2 %>%
  group_by(Customer_age) %>%
  filter(Customer_age > 365) %>%
  ungroup(Customer_age) %>%
  select(Serviceid)

#get transaction data by putting all service ids on one row, grouped by
#users and the date users used those services
df_transaction <- ddply(df,c('User_id','Date'),
                        function(df1)paste(df1$Serviceid,
                                           collapse = ','))
#select on the service column 
df_transaction <- df_transaction %>%
  select(V1)

#store the data to a csv file 
write.csv(df_transaction,
'C:/Users/khiem.phung/Downloads/basket_transaction.csv',
quote = FALSE, row.names = FALSE)

#load the data into transaction class
tr <- read.transactions('C:/Users/khiem.phung/Downloads/basket_transaction.csv',
                        format = 'basket', sep=',')

#see the service with most frequent appearance 
itemFrequencyPlot(tr,topN=10,type="absolute")

#mine the rules using the APRIORI algorithm
association.rules <- apriori(tr, 
                             parameter = list(supp=0.001, conf=0.8))
summary(association.rules)

#sort the rules by count 
association.rules <- sort(association.rules, by="count", decreasing=TRUE)
#print top 10 rules
inspect(association.rules[1:20])

##Bonus question
#count number of service ids used each date 
df_date <- df %>%
  distinct(Date, Serviceid) %>%
  group_by(Date) %>%
  count()

#visualize the findings
ggplot(df_date, aes(x = Date, y = n)) +
  geom_point()

#filter the date to 2018
df_date_2018 <- df_date %>%
  filter(Date > '2017-12-31')

#visualize the new series 
ggplot(df_date_2018, aes(x = Date, y = n)) +
  geom_line()
