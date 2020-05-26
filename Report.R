# 1. Introduction

# Housing price in Beijing dataset
# https://www.kaggle.com/ruiqurm/lianjia/download

# url: the url which fetches the data
# id: the id of transaction
# Lng: and Lat coordinates, using the BD09 protocol.
# Cid: community id
# tradeTime: the time of transaction
# DOM: active days on market.Know more in https://en.wikipedia.org/wiki/Days_on_market
# followers: the number of people follow the transaction.
# totalPrice: the total price
# price: the average price by square
# square: the square of house
# livingRoom: the number of living room
# drawingRoom: the number of drawing room
# kitchen: the number of kitchen
# bathroom the number of bathroom
# floor: the height of the house. I will turn the Chinese characters to English in the next version.
# buildingType: including tower( 1 ) , bungalow( 2 )，combination of plate and tower( 3 ), plate( 4 ).
# constructionTime: the time of construction
# renovationCondition: including other( 1 ), rough( 2 ),Simplicity( 3 ), hardcover( 4 )
# buildingStructure: including unknow( 1 ), mixed( 2 ), brick and wood( 3 ), brick and concrete( 4 ),steel( 5 ) and steel-concrete composite ( 6 ).
# ladderRatio: the proportion between number of residents on the same floor and number of elevator of ladder. It describes how many ladders a resident have on average.
# elevator have ( 1 ) or not have elevator( 0 )
# fiveYearsProperty: if the owner have the property for less than 5 years

# Note: this process could take a couple of minutes
# Check all necessary libraries
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(tidyverse)) install.packages("caret")
if(!require(tidyverse)) install.packages("lubridate")
if(!require(tidyverse)) install.packages("data.table")
if(!require(tidyverse)) install.packages("DataExplorer")
if(!require(tidyverse)) install.packages("corrplot")
if(!require(tidyverse)) install.packages("gridExtra")
if(!require(tidyverse)) install.packages("PerformanceAnalytics")
if(!require(tidyverse)) install.packages("ggthemes")
if(!require(tidyverse)) install.packages("psych")
if(!require(tidyverse)) install.packages("kernlab")
if(!require(tidyverse)) install.packages("e1071")
library(tidyverse)
library(caret)
library(lubridate)
library(data.table)
library(DataExplorer)
library(corrplot)
library(gridExtra)
library(PerformanceAnalytics)
library(ggthemes)
library(psych)
library(glmnet)
library(e1071)

# 2. Data Preparation

house <- read_csv('beijing.csv')

# preprocessing of the data. remove non-important columns
house <- house %>% mutate(year = as.numeric(format(house$tradeTime, '%Y'))) 
house <- house %>% mutate(month = as.numeric(format(house$tradeTime, '%m'))) 
house <- house %>% select(-c(url, id, Cid, tradeTime))

# change constructionTime to numeric
house <- house %>% mutate(constructionTime = as.numeric(constructionTime))

# check the first several rows of dataset
head(house)
names(house)
str(house)

# A summary of the subset shows that there are missing values in it.
summary(house)

# parse the floor and change to numeric due to chinese characters
house <- house %>% mutate(floor = parse_number(iconv(enc2utf8(house$floor),sub="byte")))

# we check the na values in each columns
DataExplorer::plot_missing(house)

# Since we miss 49.55% of DOM (active days on market), we will replace them with median
house$DOM <- ifelse(is.na(house$DOM), median(house$DOM, na.rm = TRUE), house$DOM)

DataExplorer::plot_missing(house)

# remove all missing data
house <- na.omit(house)

DataExplorer::plot_missing(house)
cat('Everything looks cleaned up!')

str(house)

# 3. Data Exploration
# 3.1 Categorical Features
# 3.1.1 Map
house %>% ggplot(aes(Lat, Lng)) + 
  geom_point(aes(color = price)) + 
  scale_color_gradient(low="blue", high="red") 

# 3.1.2 Building Type
box1 <- house %>% mutate(buildingType = case_when(buildingType == 1 ~ 'Tower',
                                                       buildingType == 2 ~ 'Bungalow',
                                                       buildingType == 3 ~ 'Plate and Tower',
                                                       buildingType == 4 ~ 'Plate')) %>%
  mutate(buildingType = reorder(buildingType, price, FUN = median)) %>%
  ggplot(aes(price, buildingType)) + 
  geom_boxplot(alpha = 0.2) + 
  xlab('Average Price of Homes') +
  ylab('Building Type') + 
  theme_economist()

map1 <- house %>% mutate(buildingType = case_when(buildingType == 1 ~ 'Tower',
                                          buildingType == 2 ~ 'Bungalow',
                                          buildingType == 3 ~ 'Plate and Tower',
                                          buildingType == 4 ~ 'Plate')) %>%
  ggplot(aes(Lat, Lng)) + 
  geom_point(aes(color = buildingType), alpha = 0.2) +
  scale_color_discrete(name = 'Building Type') +
  facet_wrap(~buildingType)

grid.arrange(box1, map1, ncol = 2)

# 3.1.3 Renovation Condition
box2 <- house %>% mutate(renovationCondition = case_when(renovationCondition == 1 ~ 'Other',
                                                              renovationCondition == 2 ~ 'Rough',
                                                              renovationCondition == 3 ~ 'Simplicity',
                                                              renovationCondition == 4 ~ 'Hardcover')) %>%
  mutate(renovationCondition = reorder(renovationCondition, price, FUN = median)) %>%
  ggplot(aes(price, renovationCondition)) + 
  geom_boxplot(alpha = 0.2) + 
  xlab('Average Price of Homes') +
  ylab('Renovation Condition') +
  theme_economist()

map2 <- house %>% mutate(renovationCondition = case_when(renovationCondition == 1 ~ 'Other',
                                                         renovationCondition == 2 ~ 'Rough',
                                                         renovationCondition == 3 ~ 'Simplicity',
                                                         renovationCondition == 4 ~ 'Hardcover')) %>%
  ggplot(aes(Lat, Lng)) + 
  geom_point(aes(color = renovationCondition), alpha = 0.2) +
  facet_wrap(~renovationCondition) +
  scale_color_discrete(name = 'Renovation Condition') +
  theme_economist()

grid.arrange(box2, map2, ncol = 2)

# 3.1.4 Building Structure
box3 <- house %>% mutate(buildingStructure = case_when(buildingStructure == 1 ~ 'Unavailable',
                                                            buildingStructure == 2 ~ 'Mixed',
                                                            buildingStructure == 3 ~ 'Brick and Wood',
                                                            buildingStructure == 4 ~ 'Brick and Concrete',
                                                            buildingStructure == 5 ~ 'Steel',
                                                            buildingStructure == 6 ~ 'Steel-concrete Composite')) %>%
  mutate(buildingStructure = reorder(buildingStructure, price, FUN = median)) %>%
  ggplot(aes(price, buildingStructure)) + 
  geom_boxplot(alpha = 0.2) + 
  xlab('Average Price of Homes') +
  ylab('Building Structure') +
  theme_economist()

map3 <- house %>% mutate(buildingStructure = case_when(buildingStructure == 1 ~ 'Unavailable',
                                               buildingStructure == 2 ~ 'Mixed',
                                               buildingStructure == 3 ~ 'Brick and Wood',
                                               buildingStructure == 4 ~ 'Brick and Concrete',
                                               buildingStructure == 5 ~ 'Steel',
                                               buildingStructure == 6 ~ 'Steel-concrete Composite')) %>%
  ggplot(aes(Lat, Lng)) + 
  geom_point(aes(color = buildingStructure), alpha = 0.2) +
  facet_wrap(~buildingStructure) +
  scale_color_discrete(name = 'Building Structure') +
  theme_economist()

grid.arrange(box3, map3, ncol = 2)

# 3.1.5 Elevator
box4 <- house %>% mutate(elevator = case_when(elevator == 1 ~ 'Has Elevator',
                                                   elevator == 0 ~ 'No Elevator')) %>%
  mutate(elevator = reorder(elevator, price, FUN = median)) %>%
  ggplot(aes(price, elevator)) + 
  geom_boxplot(alpha = 0.2) + 
  xlab('Average Price of Homes') +
  ylab('Elevator') +
  theme_economist()

map4 <- house %>% mutate(elevator = case_when(elevator == 1 ~ 'Has Elevator',
                                      elevator == 0 ~ 'No Elevator')) %>%
  ggplot(aes(Lat, Lng)) + 
  geom_point(aes(color = elevator), alpha = 0.2) +
  facet_wrap(~elevator) +
  scale_color_discrete(name = 'Elevator') +
  theme_economist()

grid.arrange(box4, map4, ncol = 2)

# 3.1.6 Five Years Property
box5 <- house %>% mutate(fiveYearsProperty = case_when(fiveYearsProperty == 1 ~ 'Ownership > 5years',
                                                            fiveYearsProperty == 0 ~ 'Ownership < 5years')) %>%
  mutate(fiveYearsProperty = reorder(fiveYearsProperty, price, FUN = median)) %>%
  ggplot(aes(price, fiveYearsProperty)) + 
  geom_boxplot(alpha = 0.2) + 
  xlab('Average Price of Homes') +
  ylab('Five Years Property') +
  theme_economist()

map5 <- house %>% mutate(fiveYearsProperty = case_when(fiveYearsProperty == 1 ~ 'Ownership > 5years',
                                               fiveYearsProperty == 0 ~ 'Ownership < 5years')) %>%
  ggplot(aes(Lat, Lng)) + 
  geom_point(aes(color = fiveYearsProperty), alpha = 0.2) +
  facet_wrap(~fiveYearsProperty) +
  scale_color_discrete(name = 'Five Years Property') +
  theme_economist()

grid.arrange(box5, map5, ncol = 2)

# 3.1.7 Subway
box6 <- house %>% mutate(subway = case_when(subway == 1 ~ 'Has Subway',
                                               subway == 0 ~ 'No Subway')) %>%
  mutate(subway = reorder(subway, price, FUN = median)) %>%
  ggplot(aes(price, subway)) + 
  geom_boxplot(alpha = 0.2) + 
  xlab('Average Price of Homes') +
  ylab('Subway') +
  theme_economist()

map6 <- house %>% mutate(subway = case_when(subway == 1 ~ 'Has Subway',
                                    subway == 0 ~ 'No Subway')) %>%
  ggplot(aes(Lat, Lng)) + 
  geom_point(aes(color = subway), alpha = 0.2) +
  facet_wrap(~subway) +
  scale_color_discrete(name = 'Subway') +
  theme_economist()

grid.arrange(box6, map6, ncol = 2)

# 3.1.8 District
box7 <- house %>% mutate(district = case_when(district == 1 ~ 'DongCheng',
                                                   district == 2 ~ 'FengTai',
                                                   district == 3 ~ 'DaXing',
                                                   district == 4 ~ 'FaXing',
                                                   district == 5 ~ 'FangShang',
                                                   district == 6 ~ 'ChangPing',
                                                   district == 7 ~ 'ChaoYang',
                                                   district == 8 ~ 'HaiDian',
                                                   district == 9 ~ 'ShiJingShan',
                                                   district == 10 ~ 'XiCheng',
                                                   district == 11 ~ 'TongZhou',
                                                   district == 12 ~ 'ShunYi',
                                                   district == 13 ~ 'MenTouGou')) %>% 
  mutate(district = reorder(district, price, FUN = median)) %>%
  ggplot(aes(price, district)) + 
  geom_boxplot(alpha = 0.2) + 
  xlab('Average Price of Homes') +
  ylab('District') +
  theme_economist()

map7 <- house %>% mutate(district = case_when(district == 1 ~ 'DongCheng',
                                      district == 2 ~ 'FengTai',
                                      district == 3 ~ 'DaXing',
                                      district == 4 ~ 'FaXing',
                                      district == 5 ~ 'FangShang',
                                      district == 6 ~ 'ChangPing',
                                      district == 7 ~ 'ChaoYang',
                                      district == 8 ~ 'HaiDian',
                                      district == 9 ~ 'ShiJingShan',
                                      district == 10 ~ 'XiCheng',
                                      district == 11 ~ 'TongZhou',
                                      district == 12 ~ 'ShunYi',
                                      district == 13 ~ 'MenTouGou')) %>% 
  ggplot(aes(Lat, Lng)) + 
  geom_point(aes(color = district), alpha = 0.2) +
  facet_wrap(~district) +
  scale_color_discrete(name = 'District') +
  theme_economist()

grid.arrange(box7, map7, ncol = 2)

## 3.2 Numerical Features

# write a function to analyze distributions
plot_hist = function(df, numcol,Color = "green2" ,bins = 15,Title = "")
{
  skew = skewness(df[,numcol])  
  options(repr.plot.width=4, repr.plot.height=3) # Set the initial plot area dimensions
  bw = (max(df[,numcol]) - min(df[,numcol]))/(bins + 1)
  p <- ggplot(df, aes_string(numcol)) + 
    geom_histogram(alpha = 0.6, binwidth = bw,color = Color)+
    ggtitle(paste("Histogram of ",Title,numcol,sep = ''))+
    annotate(geom = "text",x = (max(df[,numcol]) + min(df[,numcol]))/2, y = 200000,label = paste("Skew:",round(skew,2)))+
    theme_minimal()
}
# check the distribution of totalprice, price
p1 <- plot_hist(house, 'totalPrice')

p2 <- plot_hist(house, 'price')

# check square distribution
p3 <- plot_hist(house, 'square')

# check living room distribution
p4 <- plot_hist(house, 'livingRoom')

# check bathroom distribution
p5 <- plot_hist(house, 'bathRoom')

grid.arrange(p1, p2, p3, p4, p5)

# obtaining correlation matrix of numeric variables
numeric_cols <- names(house[,sapply(house,function(x) {is.numeric(x)})] %>% select(-Lng, -Lat, -buildingType, -renovationCondition, -buildingStructure, -elevator, -fiveYearsProperty, -subway, -district))
cor_numerics <- cor(house[,numeric_cols])
corrplot(cor_numerics,
         method = 'ellipse',
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

# remove outliers
house <- house %>% filter(totalPrice < 2500,
                          square < 400,
                          livingRoom < 8,
                          bathRoom < 6)

# 4. Regression Model
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
## 4.1. Prepare Train/Test Samples
set.seed(2019)
test_index <- createDataPartition(y = house$totalPrice, times = 1, p = 0.2, list = FALSE)
house_train <- house[-test_index,]
house_test <- house[test_index,]

## 4.2. Simplest Model (Average)
mu <- mean(house_train$totalPrice)
naive_rmse <- RMSE(house_test$totalPrice, mu)
rmse_results <- data_frame(method = 'Just the average',
                           RMSE = naive_rmse)
rmse_results %>% knitr::kable()

## 4.3. Generalized Linear Model
temp_train <- as.data.frame(cbind(
  house_train %>% select_if(is.numeric) %>% select(-Lng, -Lat, -month),
  'bldgType'= dummy.code(house_train$buildingType),
  'bldgStruc'= dummy.code(house_train$buildingStructure),
  'renovation'= dummy.code(house_train$renovationCondition),
  'hasElevator'= dummy.code(house_train$elevator),
  'hasSubway'= dummy.code(house_train$subway),
  'IsFiveYears'= dummy.code(house_train$fiveYearsProperty),
  'districtCat'= dummy.code(house_train$district)))

temp_test <- as.data.frame(cbind(
  house_test %>% select_if(is.numeric) %>% select(-Lng, -Lat, -month),
  'bldgType'= dummy.code(house_test$buildingType),
  'bldgStruc'= dummy.code(house_test$buildingStructure),
  'renovation'= dummy.code(house_test$renovationCondition),
  'hasElevator'= dummy.code(house_test$elevator),
  'hasSubway'= dummy.code(house_test$subway),
  'IsFiveYears'= dummy.code(house_test$fiveYearsProperty),
  'districtCat'= dummy.code(house_test$district)))

# Train Model
my_control <-trainControl(method="cv", number=10)
glmGrid <- expand.grid(alpha = seq(0, 1, by = 0.1), lambda = seq(0.001,0.1,by = 0.0005)) 
glm_mod <- train(x= temp_train %>% select(-totalPrice), y=temp_train$totalPrice, method='glmnet', trControl= my_control, tuneGrid=glmGrid)

# Test Model
glm_preds <- predict(glm_mod,temp_test%>% select(-totalPrice))
glm_rmse <- RMSE(temp_test$totalPrice, glm_preds)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = 'Generalized Linear Model',
                                     RMSE = glm_rmse))
rmse_results %>% knitr::kable()


## 4.4. Support Vector Machine

svm_model <- svm(totalPrice ~ ., data = temp_train)
svm_preds <- predict(svm_model, temp_test%>% select(-totalPrice))
svm_rmse <- RMSE(temp_test$totalPrice, svm_preds)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = 'SVM Model',
                                     RMSE = svm_rmse))
rmse_results %>% knitr::kable()
