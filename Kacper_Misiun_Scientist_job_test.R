library('tidyverse')
#1 Get Data  ----
# get list of csv with ATM name. read them and union
ATM_df <- map(list.files(pattern = "ATM.*\\.csv"), data.table::fread) %>%
    bind_rows()

#check summary
summary(ATM_df)

#check duplicates
sum(duplicated(ATM_df))

#2 Short Analysis ----
ATM_df <- ATM_df %>%
    mutate(week_day = lubridate::wday(ProcessDate, week_start = 1), # 1 means monday as 1
           week_day_name = weekdays(ProcessDate),
           month_day = lubridate::day(ProcessDate),
           month = lubridate::month(ProcessDate),
           month_name = months(ProcessDate)
    )

# weekday
ATM_sum_week_day <- ATM_df %>% 
    group_by(week_day, week_day_name) %>% 
    summarise(Withdrawal_sum = sum(Withdrawal))

ggplot(ATM_sum_week_day, aes(x = week_day, y = Withdrawal_sum))+
    geom_col() +
    scale_y_continuous(labels = scales::comma) + # to prevent scientific notation on y axis
    theme_classic()

# Generally people withdraw money before weekend (Friday pick)

#month day
ATM_sum_month_day <- ATM_df %>% 
    group_by(month_day) %>% 
summarise(Withdrawal_sum = sum(Withdrawal))

ggplot(ATM_sum_month_day, aes(x = month_day, y = Withdrawal_sum))+
    geom_col()+
    scale_y_continuous(labels = scales::comma)+
    theme_classic()

# Payout on the 10th or at the beginning/end of the month

#month
ATM_sum_month <- ATM_df %>% 
    group_by(month, month_name) %>% 
summarise(Withdrawal_sum = sum(Withdrawal)) %>% 
    arrange(month)

ggplot(ATM_sum_month, aes(x = month, y = Withdrawal_sum))+
    geom_col()+
    scale_y_continuous(labels = scales::comma)+
    scale_x_continuous(breaks = scales::pretty_breaks()) + # by default breaks was decimal for example 7,5
    theme_classic()
# do we spend the most on holidays? interesting that there is no increase in December (Christmas), maybe it's ATMs in an Orthodox country because the increase in January :)

#3 Forecasting ----
library('tidymodels')
library('forecast')
library('prophet')
set.seed(1234)
summary(ATM_df)

#check time series
ATM_df %>% 
    transmute(ProcessDate = as.Date(ProcessDate),
              Atm = Atm,
              Withdrawal = Withdrawal
              ) %>% 
    group_by(Atm) %>% 
    timetk::plot_time_series(ProcessDate, Withdrawal, .facet_ncol = 2) 

#prepare data 
model_data <- ATM_df %>% 
    select(ProcessDate, Atm, Withdrawal) %>% 
    mutate(ProcessDate = as.Date(ProcessDate)) %>% 
    arrange(desc(ProcessDate)) %>% 
    nest(data = -Atm) %>% 
    mutate(train_data = map(data, ~ filter(.x, ProcessDate <= max(ProcessDate) - 30) %>% #all except last 30 days || last date is same for all atms
                                rename(ds = ProcessDate, y = Withdrawal)), 
           test_data =  map(data, ~ filter(.x, ProcessDate > max(ProcessDate) - 30) %>% # last 30 days
                               rename(ds = ProcessDate, y = Withdrawal))
    ) 

#show train/test
model_data %>% 
    select(Atm,train_data,test_data) %>% 
    pivot_longer(-Atm) %>% 
    unnest(value) %>% 
    ggplot(aes(x = ds , y =  y, color = name, group = Atm)) +
    geom_line()+
    facet_wrap(~Atm, scales = "fixed")

#add arima function
arima_forecast <- function(df) {
        model <- auto.arima(df$y , seasonal = FALSE)
        future <- forecast(model, h = 60)
    return(future)
}

# add prophet function
prophet_forecast <- function(df) {
    m1 <- prophet(df, seasonality.mode = "multiplicative", daily.seasonality=FALSE)
    future <- make_future_dataframe(m1, periods = 60, freq = "day")
    forecast <- predict(m1, future)
    return(forecast)
}

# Function to calculate prophet performance metrics // prophet::performance_metrics() return me null
forecast_metrics <- function(test_data, forecast_data) {
    forecast_data <- forecast_data %>% 
        as_tibble() %>% 
        mutate(ds = as.Date(ds))
    test_data <- test_data %>% 
        mutate(ds = as.Date(ds))
    RMSE <- sqrt(mean((test_data$y - forecast_data$yhat)^2))
    MAE <- mean(abs(test_data$y - forecast_data$yhat))
    MAPE <- mean(abs((test_data$y - forecast_data$yhat)/test_data$y))
    metrics <- c(RMSE = RMSE, MAE = MAE, MAPE = MAPE)
    return(metrics)
}

# add forecast arima/prophet to data model
model_forecast <- model_data %>% 
    mutate(arima = map(train_data, arima_forecast),
           prophet = map(train_data, prophet_forecast),
           prophet_metrics = map2(test_data, prophet, forecast_metrics) #MAPE 1,4% 
           ) 

# plot prophet forecasting (yhat, low/upper) compare to train/test data
model_forecast %>% 
    select(Atm, train_data, test_data, prophet) %>% 
    pivot_longer(cols = c(train_data, test_data, prophet), names_to = "dataset", values_to = "data") %>% 
    unnest(data) %>% 
    ggplot(aes(x = ds, y = y, color = dataset)) +
    geom_point(size = 1) +
    geom_smooth(aes(y = yhat), se = TRUE, method = "loess", color = "red", span = 0.1, alpha = 0.2) +
    geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper, fill = "forecast"), alpha = 0.1) +
    scale_fill_manual(values = c("forecast" = "blue")) +
    facet_wrap(~Atm, scales = "free") +
    labs(title = "Prophet forecast for ATM withdrawals") 


# ggsave("prophet_plot.png", dpi = 300, width = 50, height = 30,limitsize = FALSE)

# I used prophet because "atm traffic" is is susceptible to seasonality and additional components like trend, holiday effect and weekday effect could have big inpact for withdrawal
# I tested ARIMA, but ATM 1 return Arima(0,0,0)- it could be stationality - but when i check other Atm's just prophet results looks better (maybe because i used auto.arima) so i decided to focus on prophet

#4 SQL ----
#Create sample data
Atm_History <- data.frame(
    Atm = paste0("ATM", 1:10),
    ProcessDate = sample(seq(as.Date("2017-01-01"), as.Date("2017-12-31"), by = "day"), 100, replace = TRUE),
    Currency = sample(c("EUR", "USD", "PLN"), 100, replace = TRUE),
    Withdrawal = round(runif(100, min = 0, max = 1000000), 2),
    Deposit = round(runif(100, min = 0, max = 1000000), 2)
)

#use duckdb to test query
library(duckdb)
con = dbConnect(duckdb())
duckdb_register(con, name = "Atm_History", df = Atm_History)
res <- dbGetQuery(con, "SELECT h.*
FROM Atm_History as h
INNER JOIN (
  SELECT Atm, MAX(ProcessDate) AS MaxDate
  FROM Atm_History
  GROUP BY Atm
) AS atm_last_date ON
     atm_last_date.Atm = h.Atm AND
     atm_last_date.MaxDate = h.ProcessDate")
