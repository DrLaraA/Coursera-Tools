library(tidyverse)
library(rvest)
Jupyter="https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/module_1/lab-jupyter-data-collection-webscraping.ipynb"



#DATA COLLECTION WEB SCRAPING
url="https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
root=read_html(url)
node=html_nodes(root, "table")
tab=html_table(node, fill=T)
for(i in tab)
  print(tab)
head(tab)
tab2=tab[[1]]
head(tab2)
tail(tab2)
summary(tab2)
glimpse(tab2)
write.csv(tab2,"raw_bike_sharing_systems.csv", row.names=FALSE)

#DATA COLLECTION OPENWEATHER API
your_api_key="4d6e421cf339ec3783726f1c059a89d9"

library(httr)
urlcw <- 'https://api.openweathermap.org/data/2.5/weather'
query <- list(q = "Seoul", appid =your_api_key, units="metric")
response <- GET(urlcw, query=query)
response
http_type(response)
json_result <- content(response, as="parsed")
class(json_result)
json_result

furl <- 'https://api.openweathermap.org/data/2.5/forecast'
fquery <- list(q = "Seoul", appid = your_api_key, units="metric")
response <- GET(furl, query=fquery)
json_list <- content(response, as="parsed")
results <- json_list$list
results

weather <- c(weather, json_result$weather[[1]]$main)
visibility <- c(visibility, json_result$visibility)
temp <- c(temp, json_result$main$temp)
temp_min <- c(temp_min, json_result$main$temp_min)
temp_max <- c(temp_max, json_result$main$temp_max)
pressure <- c(pressure, json_result$main$pressure)
humidity <- c(humidity, json_result$main$humidity)
wind_speed <- c(wind_speed, json_result$wind$speed)
wind_deg <- c(wind_deg, json_result$wind$deg)

weather_data_frame <- data.frame(weather=weather, 
                                 visibility=visibility, 
                                 temp=temp, 
                                 temp_min=temp_min, 
                                 temp_max=temp_max, 
                                 pressure=pressure, 
                                 humidity=humidity, 
                                 wind_speed=wind_speed, 
                                 wind_deg=wind_deg)

weather_data_frame

city <- c()
weather <- c()
visibility <- c()
temp <- c()
temp_min <- c()
temp_max <- c()
pressure <- c()
humidity <- c()
wind_speed <- c()
wind_deg <- c()
forecast_datetime <- c()
season <- c()
current_month <-  c()

get_weather_forecaset_by_cities <- function(city_names){
  df <- data.frame()
  for (city_name in city_names){
    furl <- 'https://api.openweathermap.org/data/2.5/forecast'
    fquery <- list(q = city_name, appid = your_api_key, units="metric")
    response <- GET(furl, query=fquery)
    json_list <- content(response, as="parsed")
    results <- json_list$list
    
    for(result in results) {
      city <- c(city, city_name)
      weather <- c(weather, result$weather[[1]]$main)
      visibility = c(visibility, result$visibility)
      temp <-c(temp, result$main$temp)
      temp_min <- c(temp_min, result$main$temp_min)
      temp_max <- c(temp_max, result$main$temp_max)
      pressure <- c(pressure, result$main$pressure)
      humidity <-c(humidity, result$main$humidity)
      wind_speed <- c(wind_speed, result$wind$speed)
      wind_deg  <- c(wind_deg , result$wind$deg)
      forecast_datetime <- c(forecast_datetime, result$dt_txt)
      forecast_datetime<-as.POSIXct(forecast_datetime,  format = "%Y-%m-%d %H:%M:%S")
      forecast_datetime<-format(forecast_datetime, format = "%Y-%m-%d %H:%M:%S")
      current_month <-  c(current_month, result$dt_txt) 
      current_month <-as.POSIXct(current_month, format = "%Y-%m-%d %H:%M:%S")
      current_month <-format(current_month, format = "%m")
      current_month<- as.integer(current_month)
      ifelse(current_month==12 |1 |2,season<-"Winter", season)
      ifelse(current_month>2 & current_month <=5,season<-"Spring", season)
      ifelse(current_month>5 & current_month <=8,season<-"Summer", season)
      ifelse(current_month>8 & current_month <=11,season<-"Autumn", season)
    }
    
    df <- data.frame(city=city,
                     weather=weather ,
                     visibility=visibility,
                     temp=temp,
                     temp_min=temp_min, 
                     temp_max=temp_max, 
                     pressure=pressure,
                     humidity=humidity,
                     wind_speed=wind_speed,
                     wind_deg=wind_deg,
                     forecast_datetime=forecast_datetime,
                     season=season
    )
    
    
    
  }
  
  return(df)
  
}

cities <- c("Seoul", "Washington, D.C.", "Paris", "Suzhou")
cities_weather_df <- get_weather_forecaset_by_cities(cities)

head(cities_weather_df)

write.csv(cities_weather_df, "raw_cities_weather_forecast.csv", row.names=FALSE)

url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
download.file(url, destfile = "raw_worldcities.csv")

url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
download.file(url, destfile = "raw_seoul_bike_sharing.csv")

#DATA WRANGLING
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_bike_sharing_systems.csv"
download.file(url, destfile = "raw_bike_sharing_systems.csv")
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_cities_weather_forecast.csv"
download.file(url, destfile = "raw_cities_weather_forecast.csv")
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
download.file(url, destfile = "raw_worldcities.csv")
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
download.file(url, destfile = "raw_seoul_bike_sharing.csv")
dataset_list <- c('raw_bike_sharing_systems.csv', 'raw_seoul_bike_sharing.csv', 'raw_cities_weather_forecast.csv', 'raw_worldcities.csv')
for (dataset_name in dataset_list){
  dataset <- read_csv(dataset_name)
  colnames(dataset)<-toupper(colnames(dataset))
  colnames(dataset)<- str_replace_all(colnames(dataset)," ", "_")
  write.csv(dataset, dataset_name, row.names=FALSE)
}

for (dataset_name in dataset_list){
  dataset <- read_csv(dataset_name)
  print(head(dataset, 2))
}

bike_sharing_df <- read_csv("raw_bike_sharing_systems.csv")
head(bike_sharing_df)
sub_bike_sharing_df <- bike_sharing_df %>% select(COUNTRY, CITY, SYSTEM, BICYCLES)
sub_bike_sharing_df%>%
  summarize_all(class)%>%
  gather(variable, class)
find_character <- function(strings) grepl("[^0-9]", strings)
sub_bike_sharing_df %>% 
  select(BICYCLES) %>% 
  filter(find_character(BICYCLES)) %>%
  slice(0:10)

ref_pattern <- "\\[[A-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)
sub_bike_sharing_df %>% 
  select(COUNTRY) %>% 
  filter(find_reference_pattern(COUNTRY)) %>%
  slice(0:10)
sub_bike_sharing_df %>% 
  select(CITY) %>% 
  filter(find_reference_pattern(CITY)) %>%
  slice(0:10)
sub_bike_sharing_df %>% 
  select(SYSTEM) %>% 
  filter(find_reference_pattern(SYSTEM)) %>%
  slice(0:10)

remove_ref <- function(strings) {
  ref_pattern <- "\\[[A-z0-9]+\\]"
  result<-str_replace_all(strings, ref_pattern, "")
  result<- trimws(result)
  return(result)
}
result<-sub_bike_sharing_df %>% mutate(CITY = remove_ref(CITY), SYSTEM = remove_ref(SYSTEM))
result %>% 
  select(CITY, SYSTEM, BICYCLES) %>% 
  filter(find_reference_pattern(CITY) | find_reference_pattern(SYSTEM) | find_reference_pattern(BICYCLES))

extract_num <- function(columns){
  digitals_pattern <- "[0-9]+"
  result<-str_extract(columns,digitals_pattern)
  result<-as.numeric(result)
  return(result)
}
result<-result %>% mutate(BICYCLES=extract_num(BICYCLES))
summary(result$BICYCLES)
write.csv(result, file = "bike_sharing_systems.csv", row.names=FALSE)

#WRANGLING DPLYR
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
download.file(url, destfile = "raw_seoul_bike_sharing.csv")
bike_sharing_df <- read_csv("raw_seoul_bike_sharing.csv")
summary(bike_sharing_df)
dim(bike_sharing_df)
bike_sharing_df<- bike_sharing_df %>% drop_na(RENTED_BIKE_COUNT)
dim(bike_sharing_df)
bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))
summer=filter(bike_sharing_df, SEASONS=="Summer")
summer_average_temperature<-mean(summer$TEMPERATURE, na.rm=T)
summer_average_temperature
bike_sharing_df<- bike_sharing_df %>% replace_na(list(TEMPERATURE = summer_average_temperature))
bike_sharing_df %>% 
  filter(is.na(TEMPERATURE))
write.csv(bike_sharing_df, file="seoul_bike_sharing.csv", row.names=FALSE)

bike_sharing_df<- bike_sharing_df %>% mutate(HOUR=as.character(Hour))
seoul_bike_sharing_converted<-bike_sharing_df %>%
  mutate(dummy = 1) %>%spread( key = SEASONS, value = dummy, fill = 0) %>%
  mutate(dummy = 1) %>% spread( key = HOLIDAY, value = dummy, fill = 0) %>%
  mutate(dummy = 1) %>%spread( key = HOUR, value = dummy, fill = 0) 
glimpse(seoul_bike_sharing_converted)
write_csv(seoul_bike_sharing_converted, "seoul_bike_sharing_converted.csv")

normalization <- function(min_max_normalization_name) {    
  min_max_normalization<- (seoul_bike_sharing_converted[min_max_normalization_name] - min(seoul_bike_sharing_converted[min_max_normalization_name])) /
    (max(seoul_bike_sharing_converted[min_max_normalization_name]) - min(seoul_bike_sharing_converted[min_max_normalization_name]))    
  return(min_max_normalization[[min_max_normalization_name]])
}
seoul_bike_sharing_converted_normalized<-seoul_bike_sharing_converted %>% mutate(
  RENTED_BIKE_COUNT=normalization("RENTED_BIKE_COUNT"), 
  TEMPERATURE=normalization("TEMPERATURE"), 
  HUMIDITY=normalization("HUMIDITY"),
  WIND_SPEED=normalization("WIND_SPEED"), 
  VISIBILITY=normalization("Visibility"),
  DEW_POINT_TEMPERATURE=normalization("DEW_POINT_TEMPERATURE"),
  SOLAR_RADIATION=normalization("SOLAR_RADIATION"),
  RAINFALL=normalization("RAINFALL"), 
  SNOWFALL=normalization("Snowfall"))
summary(seoul_bike_sharing_converted_normalized)
write_csv(seoul_bike_sharing_converted_normalized, "seoul_bike_sharing_converted_normalized.csv")

dataset_list <- c('seoul_bike_sharing.csv', 'seoul_bike_sharing_converted.csv', 'seoul_bike_sharing_converted_normalized.csv')
for (dataset_name in dataset_list){
  # Read dataset
  dataset <- read_csv(dataset_name)
  # Standardized its columns:
  # Convert all columns names to uppercase
  names(dataset) <- toupper(names(dataset))
  # Replace any white space separators by underscore, using str_replace_all function
  names(dataset) <- str_replace_all(names(dataset), " ", "_")
  # Save the dataset back
  write.csv(dataset, dataset_name, row.names=FALSE)
}

#EDA Data Visualization ggplot2
seoul_bike_sharing <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing.csv" 
seoul_bike_sharing<-read_csv(seoul_bike_sharing)
head(seoul_bike_sharing)
seoul_bike_sharing$DATE<-as.Date(seoul_bike_sharing$DATE,format="%d/%m/%Y", tryFormat="%d/%m/%Y")
seoul_bike_sharing$HOUR<-as.factor(seoul_bike_sharing$HOUR)
seoul_bike_sharing$SEASONS<-as.factor(seoul_bike_sharing$SEASONS)

str(seoul_bike_sharing)

sum(is.na(seoul_bike_sharing))

summary(seoul_bike_sharing)

Holidays<-as.factor(seoul_bike_sharing$HOLIDAY)
summary(Holidays)
408/24 #408 is the number of Holidays
(408/8465)*100 #8465 is the total

365*24 #days*hours

summary(seoul_bike_sharing$FUNCTIONING_DAY)

library(dplyr)
seoul_bike_sharing %>% 
  group_by (SEASONS) %>% 
  summarize(RAINFALL = sum(RAINFALL), SNOWFALL = sum(SNOWFALL))

library(ggplot2)
ggplot(seoul_bike_sharing, mapping=aes(x=RENTED_BIKE_COUNT, y=DATE))+
  geom_point(alpha=.1)

ggplot(seoul_bike_sharing, mapping=aes(x=RENTED_BIKE_COUNT, y=DATE))+
  geom_point(alpha=.3, aes(color=HOUR))

ggplot(seoul_bike_sharing, mapping=aes(x=RENTED_BIKE_COUNT, y=..density..))+
  geom_histogram(bins=25, color="white")+
  geom_density()+
  theme_linedraw()

ggplot(seoul_bike_sharing, mapping=aes(x=RENTED_BIKE_COUNT, y=TEMPERATURE, color=HOUR))+
  geom_point(alpha=0.5)+
  facet_wrap(~SEASONS)

ggplot(seoul_bike_sharing) +
  geom_point(aes(x=TEMPERATURE,y=RENTED_BIKE_COUNT,colour=HOUR),alpha=1/5)

ggplot(seoul_bike_sharing, mapping=aes(y=RENTED_BIKE_COUNT, x=HOUR, color=SEASONS)) +
  geom_boxplot()+
  facet_wrap(~SEASONS)

DRS<-seoul_bike_sharing %>% 
  group_by (DATE) %>% 
  summarize(DAILY_TOTAL_RAINFALL = sum(RAINFALL), DAILY_TOTAL_SNOWFALL = sum(SNOWFALL))
DRS

summary(DRS$DAILY_TOTAL_SNOWFALL!=0)


#PREDICTIVE ANALYSIS
#Baseline model
library(tidymodels)
library(tidyverse)
library(stringr)

dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
readr::spec(bike_sharing_df)

bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)
set.seed(1234)
bike_sharing_split<- initial_split(bike_sharing_df, prop=3/4)
train_data<-training(bike_sharing_split)
test_data<-testing(bike_sharing_split)

lm_model=linear_reg(mode="regression", engine="lm")
lm_model_weather=lm_model%>%
  fit(RENTED_BIKE_COUNT~TEMPERATURE+
          HUMIDITY+WIND_SPEED+VISIBILITY+
          DEW_POINT_TEMPERATURE+
          SOLAR_RADIATION+RAINFALL+
          SNOWFALL, data=train_data)
print(lm_model_weather$fit)

lm_model_all<- lm_model %>% 
  fit(RENTED_BIKE_COUNT ~ ., data=train_data)
summary(lm_model_all$fit)

test_results_weather <- 
  lm_model_weather %>% 
  predict(new_data=test_data) %>% 
  mutate(truth=test_data$RENTED_BIKE_COUNT)

test_results_all <- 
  lm_model_all %>% 
  predict(new_data=test_data) %>% 
  mutate(truth=test_data$RENTED_BIKE_COUNT)

head(test_results_weather)
head(test_results_all)

rsq_weather <- 
  rsq(test_results_weather, truth=truth, estimate=.pred)
rsq_all <- 
  rsq(test_results_all, truth=truth, estimate=.pred)

rmse_weather <- 
  rmse(test_results_weather, truth=truth, estimate=.pred)
rmse_all <- 
  rmse(test_results_all, truth=truth, estimate=.pred)

rsq_weather
rsq_all
rmse_weather
rmse_all

lm_model_all$fit$coefficients
sorted=sort(lm_model_all$fit$coefficients, decreasing=T)
sorted_df=data.frame(sorted)
data=factor(row.names(sorted_df))

ggplot(sorted_df, mapping=aes(reorder(data, sorted), sorted))+
  geom_col()+ coord_flip()

#Improved model
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
readr::spec(bike_sharing_df)

bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)
lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")

set.seed(1234)
data_split <- initial_split(bike_sharing_df, prop = 4/5)
train_data <- training(data_split)
test_data <- testing(data_split)

ggplot(data = train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() 

ggplot(data=train_data, aes(RENTED_BIKE_COUNT, TEMPERATURE)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x, color="red") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color="yellow") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color="green") + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color="blue")

lmpoly=RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6)+poly(HUMIDITY, 4)+ poly(WIND_SPEED, 7)+ poly(VISIBILITY, 8)+ 
  poly(DEW_POINT_TEMPERATURE,5)+ poly(SOLAR_RADIATION, 2) + poly(RAINFALL, 3)+ poly(SNOWFALL, 9) + 
  `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `2` + `20` + 
  `21` + `22` + `23` + `3` + `4` + `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY 
lm_poly=lm_spec%>%
  fit(lmpoly, data=train_data)

summary(lm_poly$fit)

test=lm_poly%>%
  predict(new_data=test_data)%>%
  mutate(truth=test_data$RENTED_BIKE_COUNT)
head(test)

test[test<0] <- 0

rsq_1=rsq(test, truth=truth, estimate=.pred)
rmse_1=rmse(test, truth=truth, estimate=.pred)
model_1_results<-c( rsq_1, rmse_1)
model_1_results

lmint=RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6)*poly(HUMIDITY, 4)+ poly(WIND_SPEED, 7)+ poly(VISIBILITY, 8)+ 
  poly(DEW_POINT_TEMPERATURE,5)*poly(SOLAR_RADIATION, 2) + poly(RAINFALL, 3)*poly(SNOWFALL, 9) + 
  `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `2` + `20` + 
  `21` + `22` + `23` + `3` + `4` + `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER*HOLIDAY + NO_HOLIDAY 
lm_int=lm_spec%>%
  fit(lmint, data=train_data)

summary(lm_int$fit)

intresult=lm_int%>%
  predict(new_data=test_data)%>%
  mutate(truth=test_data$RENTED_BIKE_COUNT)

rsq_2=rsq(intresult, truth=truth, estimate=.pred)
rmse_2=rmse(intresult, truth=truth, estimate=.pred) 
model_2_results<-c( rsq_2, rmse_2)
model_2_results

library('glmnet')
glmnet_spec <- linear_reg(penalty = 0.2, mixture = 0.5) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

glmnet_fit<-glmnet_spec %>% fit(RENTED_BIKE_COUNT ~ `18`*TEMPERATURE*DEW_POINT_TEMPERATURE + RAINFALL*HUMIDITY*`4` +SOLAR_RADIATION*SNOWFALL +
                                  WIND_SPEED*VISIBILITY +`18`*TEMPERATURE* +poly(TEMPERATURE, 6) + poly(HUMIDITY, 4)+ poly(WIND_SPEED, 4)+ poly(VISIBILITY, 3)+ 
                                  poly(DEW_POINT_TEMPERATURE,6)+ poly(SOLAR_RADIATION, 5) + poly(RAINFALL, 6)+ poly(SNOWFALL, 4) + 
                                  `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17`  + `19` + `2` + `20` + 
                                  `21` + `22` + `23` + `3`  + `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER + HOLIDAY + NO_HOLIDAY , 
                                data = train_data )

test_results_glment <- glmnet_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$RENTED_BIKE_COUNT)

head(test_results_glment)

test_results_glment[test_results_glment<0] <- 0
rsq_3<-rsq(test_results_glment, truth = truth, estimate = .pred)

rmse_3<-rmse(test_results_glment, truth = truth, estimate = .pred)

model_3_results<-c(rsq_3, rmse_3)
model_3_results

model_prediction<- function(model_fit, test_data )  {
  test_results<-model_fit %>%
    predict(new_data = test_data) %>%
    mutate(truth = test_data$RENTED_BIKE_COUNT)
  test_results[test_results<0] <- 0
  return(test_results)}

model_evaluation <- function(test_results){
  rsq_model<-rsq(test_results, truth = truth, estimate = .pred)
  rmse_model<-rmse(test_results, truth = truth, estimate = .pred)
  results<-c(rsq_model, rmse_model)
  return(results)
}

lm_spec <- linear_reg() %>%
  set_engine("lm") %>% 
  set_mode("regression")

model_4_fit <- lm_spec %>% fit(RENTED_BIKE_COUNT ~ `18`*TEMPERATURE+DEW_POINT_TEMPERATURE + RAINFALL*HUMIDITY*`4` +SOLAR_RADIATION*SNOWFALL+
                                 WIND_SPEED*VISIBILITY +TEMPERATURE+RAINFALL*HUMIDITY*`18` +poly(TEMPERATURE, 6) + poly(HUMIDITY, 6)+ poly(WIND_SPEED, 2)+ poly(VISIBILITY, 3)+ 
                                 poly(DEW_POINT_TEMPERATURE,6)+ poly(SOLAR_RADIATION, 5) + poly(RAINFALL, 6)+ poly(SNOWFALL, 2) + `18`+`4` +
                                 `0` + `1` + `10` + `11` + `12` + `13` + `14` + `15` + `16` + `17`  + `19` + `2` + `20` + 
                                 `21` + `22` + `23` + `3`  + `5` + `6` + `7` + `8`+ `9` + AUTUMN+ SPRING + SUMMER + WINTER*HOLIDAY + NO_HOLIDAY, data = train_data )

results_model_4<-model_prediction(model_4_fit, test_data)
model_4_results<-model_evaluation(results_model_4)
model_4_results

rsq_rsme_data<-data.frame(model_1_results)
rsq_rsme_data<-rbind(rsq_rsme_data, model_2_results, model_3_results, model_4_results)
rsq_rsme_data['model']<-c("model_1", "model_2", "model_3", "model_4")
colnames(rsq_rsme_data)[6]<-"RSME"
colnames(rsq_rsme_data)[3]<-"Rsquared"
rsq_rsme_data

library(ggplot2)

ggplot(rsq_rsme_data, aes(fill=RSME, x=model, y=Rsquared))+   
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = format(round(Rsquared, 3), nsmall = 2)), vjust = -0.2, size = 4)+
  ylab("R-squared")+
  ggtitle("RMSE and R-squared values of models ") 

ggplot(results_model_3)+
  stat_qq(aes(sample=truth), color='green') +
  stat_qq(aes(sample=.pred), color='red') 

ggplot(results_model_4)+
  stat_qq(aes(sample=truth), color='green') +
  stat_qq(aes(sample=.pred), color='red') 




#SHINY APP
library(tidyverse)
library(rio)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(tidymodels)
library(ggpubr)
library(glmnet)
library(shiny)
library(leaflet)
library(httr)
library(rsconnect) 

get_weather_forecaset_by_cities <- function(city_names){
  # Create some empty vectors to hold data temporarily
  city <- c()
  weather <- c()
  temperature <- c()
  visibility <- c()
  humidity <- c()
  wind_speed <- c()
  seasons <- c()
  hours <- c()
  forecast_date <-c()
  weather_labels<-c()
  weather_details_labels<-c()
  
  # Get 5-days forecast data for each city
  for (city_name in city_names){
    url_get='https://api.openweathermap.org/data/2.5/forecast'
    api_key <- "457ea191b13b162c0e6cf625c9248971"
    forecast_query <- list(q = city_name, appid = api_key, units="metric")
    response <- GET(url_get, query=forecast_query)
    json_list <-content(response, as="parsed")
    results <- json_list$list
    
    for(result in results) {
      # Get weather data and append them to vectors
      city <- c(city, city_name)
      weather <- c(weather, result$weather[[1]]$main)
      
      # Get predictor variables
      temperature <- c(temperature, result$main$temp)
      visibility <- c(visibility, result$visibility)
      humidity <- c(humidity, result$main$humidity)
      wind_speed <- c(wind_speed, result$wind$speed)
      
      forecast_datetime <- result$dt_txt
      hour <- as.numeric(strftime(forecast_datetime, format="%H"))
      month <- as.numeric(strftime(forecast_datetime, format="%m"))
      forecast_date <-c(forecast_date, forecast_datetime)
      season <- "Spring"
      # Simple rule to determine season
      if (month >= 3 && month <= 5)
        season <- "SPRING"
      else if(month >= 6  &&  month <= 8)
        season <- "SUMMER"
      else if (month >= 9  && month <= 11)
        season <- "AUTUMN"
      else
        season <- "WINTER"
      # Add a HTML label to be shown on Leaflet
      weather_label <- paste(sep = "",
                             "<b><a href=''>",
                             city_name, 
                             "</a></b>", "</br>", 
                             "<b>", result$weather[[1]]$main, "</b></br>")
      # Add a detailed HTML label to be shown on Leaflet
      weather_detail_label <- paste(sep = "",
                                    "<b><a href=''>",
                                    city_name, 
                                    "</a></b>", "</br>", 
                                    "<b>", result$weather[[1]]$main, "</b></br>",
                                    "Temperature: ", result$main$temp, " C </br>",
                                    "Visibility: ", result$visibility, " m </br>",
                                    "Humidity: ", result$main$humidity, " % </br>", 
                                    "Wind Speed: ", result$wind$speed, " m/s </br>", 
                                    "Datetime: ", forecast_datetime, " </br>")
      weather_labels <- c(weather_labels, weather_label)
      weather_details_labels <- c(weather_details_labels, weather_detail_label)
      
      seasons <- c(seasons, season)
      hours <- c(hours, hour)
    }
  }
  # Create and return a tibble
  weather_df <- tibble(CITY_ASCII=city, WEATHER=weather, 
                       TEMPERATURE=temperature,
                       VISIBILITY=visibility, 
                       HUMIDITY=humidity, 
                       WIND_SPEED=wind_speed, SEASONS=season, HOURS=hours, FORECASTDATETIME=forecast_date, 
                       LABEL=weather_labels, DETAILED_LABEL=weather_details_labels)
  
  return(weather_df)
  
}

# Load a saved regression model (variables and coefficients) from csv
load_saved_model <- function(model_name){
  model <- read_csv(model_name)
  model <- model %>% 
    mutate(Variable = gsub('"', '', Variable))
  coefs <- setNames(model$Coef, as.list(model$Variable))
  return(coefs)
}


# Predict bike-sharing demand using a saved regression model
predict_bike_demand <- function(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS){
  model <- load_saved_model("model.csv")
  weather_terms <- model['Intercept'] + TEMPERATURE*model['TEMPERATURE'] + HUMIDITY*model['HUMIDITY'] +
    WIND_SPEED*model['WIND_SPEED'] + VISIBILITY*model['VISIBILITY'] 
  season_terms <- c()
  hour_terms <- c()
  # Calculate season related regression terms
  for(season in SEASONS) {
    season_term <- switch(season, 'SPRING'=model['SPRING'],'SUMMER'=model['SUMMER'],
                          'AUTUMN'=model['AUTUMN'], 'WINTER'=model['WINTER'])
    season_terms <- c(season_terms, season_term)
  }
  # Calculate hour related regression terms
  for(hour in HOURS){
    hour_term<- switch(as.character(hour),'0'=model['0'],'1'=model['1'],'2'=model['2'],'3'=model['3'],
                       '4'=model['4'],'5'=model['5'],'6'=model['6'],'7'=model['7'],
                       '8'=model['8'],'9'=model['9'],'10'=model['10'],'11'=model['11'],
                       '12'=model['12'],'13'=model['13'],'14'=model['14'],'15'=model['15'],'16'=model['16'],
                       '17'=model['17'],'18'=model['18'],'19'=model['19'],'20'=model['20'],
                       '21'=model['21'],'22'=model['22'],'23'=model['23'])
    hour_terms <- c(hour_terms, hour_term)
    
  }
  
  return(as.integer(weather_terms + season_terms + hour_terms))     
  
}

# Define a bike-sharing demand level, used for leaflet visualization
calculate_bike_prediction_level<- function(predictions) {
  levels <- c()
  for(prediction in predictions){
    if(prediction <= 1000 && prediction > 0)
      levels <- c(levels, 'small')
    else if (prediction > 1000 && prediction < 3000)
      levels <- c(levels, 'medium')
    else
      levels <- c(levels, 'large')
  }
  return(levels)
}

generate_city_weather_bike_data <- function (){
  cities_df <- read_csv("selected_cities.csv")
  weather_df <- get_weather_forecaset_by_cities(cities_df$CITY_ASCII)
  results <- weather_df %>% 
    mutate(BIKE_PREDICTION=predict_bike_demand(TEMPERATURE, HUMIDITY, WIND_SPEED, VISIBILITY, SEASONS, HOURS)) %>%
    mutate(BIKE_PREDICTION_LEVEL=calculate_bike_prediction_level(BIKE_PREDICTION))
  
  cities_bike_pred <- cities_df %>% left_join(results) %>% 
    select(CITY_ASCII, LNG, LAT, TEMPERATURE, HUMIDITY, BIKE_PREDICTION, BIKE_PREDICTION_LEVEL, LABEL, DETAILED_LABEL, FORECASTDATETIME)
  return(cities_bike_pred)
}

test_weather_data_generation<-function(){
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

city_weather_bike_extracted=test_weather_data_generation()
city_weather_bike_df=city_weather_bike_extracted
city_weather_bike_df$FORECASTDATETIME=ymd_hms(city_weather_bike_df$FORECASTDATETIME)
city_weather_bike_df$DATE=date(city_weather_bike_df$FORECASTDATETIME)
cities_max_bike=city_weather_bike_df %>% group_by(CITY_ASCII,DATE) %>% arrange(desc(BIKE_PREDICTION)) %>% 
  slice(1) 
cities_today_status=cities_max_bike %>% filter(DATE==min(cities_max_bike$DATE))
cities_today_status=cities_today_status %>% mutate(CIRCLE=case_when(BIKE_PREDICTION_LEVEL=="large"~12,
                                                                    BIKE_PREDICTION_LEVEL=="medium"~10,
                                                                    TRUE~6))
cities_today_status=cities_today_status %>% mutate(COLOR=case_when(BIKE_PREDICTION_LEVEL=="large"~"red",
                                                                   BIKE_PREDICTION_LEVEL=="medium"~"yellow",
                                                                   TRUE~"green"))
#UI
ui=shinyUI(
  fluidPage(padding=5,
            titlePanel("Bike-sharing demand prediction app"), 
            sidebarLayout(
              mainPanel(
                leafletOutput("city_bike_map", height=1000)
              ),
              sidebarPanel(
                selectInput(inputId="city_dropdown","Cities:", choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris") ),
                plotOutput(outputId="temp_line", width = "100%", height = "350px"),
                plotOutput(outputId="bike_line", width = "100%", height = "350px", click = "plot_click"),
                verbatimTextOutput("bike_date_output", placeholder = FALSE),
                plotOutput(outputId="humidity_pred_chart", width = "100%", height = "350px")
                
                
              )
            )
  ))

#SERVER
#source("model_prediction.R")

test_weather_data_generation<-function(){
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

server=shinyServer(function(input, output){
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  cities_max_bike<-  city_weather_bike_df[c("CITY_ASCII", "LNG",  "LAT",   "BIKE_PREDICTION", "BIKE_PREDICTION_LEVEL", "LABEL", "DETAILED_LABEL"  )] %>% 
    group_by(CITY_ASCII) %>% top_n(1,BIKE_PREDICTION)
  
  print(cities_max_bike[cities_max_bike$CITY_ASCII=="Seoul",])
  
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
    
      output$city_bike_map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
          addCircleMarkers(data = cities_max_bike[cities_max_bike$CITY_ASCII==input$city_dropdown,], lng = ~LNG, lat = ~LAT, 
                           radius= ~ifelse(BIKE_PREDICTION_LEVEL=='small', 6, ifelse(BIKE_PREDICTION_LEVEL=='medium', 10,12 ) ),
                           color =  ~ifelse(BIKE_PREDICTION_LEVEL=='small', "green", ifelse(BIKE_PREDICTION_LEVEL=='medium', "yellow","red" ) ),
                           popup = ~DETAILED_LABEL,
                           popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)
          )
      })
      
      output$temp_line<- renderPlot({
        ggplot(data=city_weather_bike_df[city_weather_bike_df$CITY_ASCII==input$city_dropdown,], aes(x=1:length(TEMPERATURE), y=TEMPERATURE)) +
          geom_line(color="yellow") +
          geom_point(size=2)+
          geom_text(aes(label=paste(TEMPERATURE, " C")),hjust=0, vjust=-0.5)+
          xlab("TIME( 3 hours ahead)")+
          ylab("TEMPERATURE (C) ") +
          ggtitle("Temperature Chart") 
      })
      
      output$bike_line<- renderPlot({
        ggplot(data=city_weather_bike_df[city_weather_bike_df$CITY_ASCII==input$city_dropdown,], aes(x=as.POSIXct(FORECASTDATETIME, format= "%Y-%m-%d %H:%M:%S"), y=BIKE_PREDICTION)) +
          geom_line(color="green") +
          geom_point(size=2)+
          geom_text(aes(label=paste(BIKE_PREDICTION)),hjust=0, vjust=-0.5)+
          xlab("TIME( 3 hours ahead)")+
          ylab("Predicted Bike Count") 
      })
      
      output$bike_date_output <- renderText({
        paste0("Time=", as.POSIXct(as.integer(input$plot_click$x), origin = "1970-01-01"),
               "\nBikeCountPred=", as.integer(input$plot_click$y))
      })
      
      output$humidity_pred_chart<- renderPlot({
        ggplot(data=city_weather_bike_df[city_weather_bike_df$CITY_ASCII==input$city_dropdown,], aes(x=HUMIDITY, y=BIKE_PREDICTION)) +
          geom_point(size=2)+
          geom_smooth(method=lm, formula=y ~ poly(x, 5), se = FALSE)
      })
      
    }
    else {
      output$city_bike_map <- renderLeaflet({
        leaflet() %>% addTiles() %>% 
          addCircleMarkers(data = cities_max_bike, lng = ~LNG, lat = ~LAT, 
                           radius= ~ifelse(BIKE_PREDICTION_LEVEL=='small', 6, ifelse(BIKE_PREDICTION_LEVEL=='medium', 10,12 ) ),
                           color =  ~ifelse(BIKE_PREDICTION_LEVEL=='small', "green", ifelse(BIKE_PREDICTION_LEVEL=='medium', "yellow","red" ) ),
                           popup = ~LABEL,
                           popupOptions = popupOptions(autoClose = FALSE, closeOnClick = FALSE)
          )
      })
      output$temp_line<- renderPlot({
        ggplot(data=city_weather_bike_df, aes(x=1:length(TEMPERATURE), y=TEMPERATURE)) +
          geom_line(color="yellow") +
          geom_point(size=1)+
          geom_text(aes(label=paste(TEMPERATURE, " C")),hjust=0, vjust=-0.5)+
          xlab("TIME( 3 hours ahead)")+
          ylab("TEMPERATURE (C) ") +
          ggtitle("Temperature Chart") 
      })
      
      output$bike_line<- renderPlot({
        ggplot(data=city_weather_bike_df, aes(x=as.POSIXct(FORECASTDATETIME, format= "%Y-%m-%d %H:%M:%S"), y=BIKE_PREDICTION)) +
          geom_line(color="green") +
          geom_point(size=2)+
          geom_text(aes(label=paste(BIKE_PREDICTION)),hjust=0, vjust=-0.5)+
          xlab("TIME( 3 hours ahead)")+
          ylab("Predicted Bike Count") 
      })
      
      output$bike_date_output <- renderText({
        paste0("Time=", as.POSIXct(as.integer(input$plot_click$x), origin = "1970-01-01"),
               "\nBikeCountPred=", as.integer(input$plot_click$y))
      })
      
      output$humidity_pred_chart<- renderPlot({
        ggplot(data=city_weather_bike_df, aes(x=HUMIDITY, y=BIKE_PREDICTION)) +
          geom_point(size=2)+
          geom_smooth(method=lm, formula=y ~ poly(x, 5), se = FALSE)
      })}})}) 
shinyApp(ui, server)
