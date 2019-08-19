library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('ggrepel') # visualisation
library('RColorBrewer') # visualisation
library('data.table') # data manipulation
library('dplyr') # data manipulation
library('readr') # data input
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('stringr') # string manipulation
library('purrr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library("forecast") # time series analysis
library(lookup)

# import the documents
data_sales <- read.csv(file="sales_data.csv", header=TRUE, sep=";",dec=",")
data_store <- read.csv(file="store_master.csv", header=TRUE, sep=";",dec=",")

View(data_sales)
View(data_store)

#Shows the number of unique variables 
data_sales %>% summarise_all(n_distinct)
data_store %>% summarise_all(n_distinct) #Two stores have same latitude and longitude values

class(names(data_sales))

names(data_sales)

#Check the data types; both datasets contain numeric variables
str(data_sales)
str(data_store)

#Check if any null value exists or not
sapply(data_sales, is.null)
sapply(data_store, is.null)

#Splitting date into three different columns; year, month and day
#To analyse trends on yearly, monthly and daily basis
data_sales = data_sales %>% 
  mutate(date = ymd(date)) %>% 
  mutate_at(vars(date), funs(year, month, day))

#New column is creater as <Quarter>
data_sales$Quarter <- cut(data_sales$month, breaks = c(-Inf, 3, 6, 9, Inf), 
                                            labels = c("Q1", "Q2", "Q3", "Q4"))


#It's now categoric; we will encode it later
data_sales$weekday <- weekdays(as.Date(data_sales$date)) 

#New feature is created called <Total_Price>
data_sales$Total_Price <- data_sales$unit_price * data_sales$qty


#Merging two datasets
data_sales <- merge(data_sales, data_store[, c("store", "latitude")], by="store")
data_sales <- merge(data_sales, data_store[, c("store", "longitude")], by="store")

library(tidyverse)
data_sales <- data_sales %>% 
                 rename(
                   Store = store, 
                   Date = date,
                   Item = item,
                   Quantity = qty,
                   Unit_price = unit_price,
                   Item_category = item_category,
                   Year = year,
                   Month = month,
                   Day = day,
                   Quarter = Quarter,
                   Weekday = weekday,
                   Total_price = Total_Price,
                   Latitude = latitude,
                   Longitude = longitude
                    )

str(data_sales)

#First 10 item in overall
df_item_withoutyear <- data.frame("Item" = data_sales$Item, "Quantity" = data_sales$Quantity)
df_item_withoutyear <- df_item_withoutyear %>% group_by(Item) %>% summarise(Quantity = sum(Quantity))
df_item_withoutyear <- df_item_withoutyear[order(-df_item_withoutyear$Quantity),]

df_item_withoutyear <- df_item_withoutyear[c(1:10),]
                                           

df_item_withyear <- data.frame("Item" = data_sales$Item, "Quantity" = data_sales$Quantity, "Year" = data_sales$Year)

#Each year's top 10 items are checked to be sure that the items are still actively sold
#Top items in 2017 are checked 
df_item_withyear_2017 <- df_item_withyear %>% filter(Year == 2017)
df_item_withyear_2017 <- df_item_withyear_2017 %>% group_by(Item) %>% summarise(Quantity = sum(Quantity))
df_item_withyear_2017 <- df_item_withyear_2017[order(-df_item_withyear_2017$Quantity),]
df_item_withyear_2017 <- df_item_withyear_2017[c(1:10),]

#Top items in 2018 are checked 
df_item_withyear_2018 <- df_item_withyear %>% filter(Year == 2018)
df_item_withyear_2018 <- df_item_withyear_2018 %>% group_by(Item) %>% summarise(Quantity = sum(Quantity))
df_item_withyear_2018 <- df_item_withyear_2018[order(-df_item_withyear_2018$Quantity),]
df_item_withyear_2018 <- df_item_withyear_2018[c(1:10),]

#Top items in 2019 are checked 
df_item_withyear_2019 <- df_item_withyear %>% filter(Year == 2019)
df_item_withyear_2019 <- df_item_withyear_2019 %>% group_by(Item) %>% summarise(Quantity = sum(Quantity))
df_item_withyear_2019 <- df_item_withyear_2019[order(-df_item_withyear_2019$Quantity),]
df_item_withyear_2019 <- df_item_withyear_2019[c(1:10),]

#Items 186, 416, 385, 179, 408, 346, 352, 348, 436, 465 are chosen for the further analysis
data_sales_final <- data_sales %>% filter(Item == 186 |
                                          Item == 416 |
                                          Item == 385 |
                                          Item == 179 |
                                          Item == 408 |
                                          Item == 346 |
                                          Item == 352 |
                                          Item == 348 |
                                          Item == 436 |
                                          Item == 465 )

#In order to encode <Weekday> I changed the its datatype
data_sales_final$Weekday <- as.factor(data_sales_final$Weekday)
str(data_sales_final)

#Encoding for factor datatypes
onehotencoder <- function(df_orig) {
  df<-cbind(df_orig)
  df_clmtyp<-data.frame(clmtyp=sapply(df,class))
  df_col_typ<-data.frame(clmnm=colnames(df),clmtyp=df_clmtyp$clmtyp)
  for (rownm in 1:nrow(df_col_typ)) {
    if (df_col_typ[rownm,"clmtyp"]=="factor") {
      clmn_obj<-df[toString(df_col_typ[rownm,"clmnm"])] 
      dummy_matx<-data.frame(model.matrix( ~.-1, data = clmn_obj))
      dummy_matx<-dummy_matx[,c(1,2:ncol(dummy_matx))]
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
      df<-cbind(df,dummy_matx)
      df[toString(df_col_typ[rownm,"clmnm"])]<-NULL
    }  }
  return(df)
}

data_sales_final <- onehotencoder(data_sales_final)

#We need to remove outliers with the function given below;
remove_all_outliers <- function(df){
  # We only want the numeric columns
  a<-df[,sapply(df, is.numeric)]
  b<-df[,sapply(df, negate(is.numeric))]
  a<-lapply(a,function(x) remove_outliers(x))
  d<-merge(a,b)
  d
}

remove_all_outliers(data_sales_final)

#To visualize outliers in <Item> column
boxplot(data_sales_final$Item)$out

#To visualize outliers in <Quantity> column
boxplot(data_sales_final$Quantity)$out

drop <- c("Date")
data_sales_final <- data_sales_final[ , !(names(data_sales_final) %in% drop)]

str(data_sales_final)

#then the multicollinearity is checked with correlation matrix
mydata.cor = cor(data_sales_final)
view(mydata.cor)


#The following code gives significant values to columns with respect to out target variable 
# with 95% CI
#Such features usually have a p-value less than 0.05 which indicates that confidence in their significance is more than 95%
fit_glm = glm(Quantity~., data= data_sales_final, family = "quasi")
summary(fit_glm)

#Basic statistical descriptions are checked
summary(data_sales_final)


# Import the random forest library and fit a model

library(randomForest)
fit_rf = randomForest(Quantity~., data=data_sales_final)
# Create an importance based on mean decreasing gini
importance(fit_rf)
