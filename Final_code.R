library('dplyr')
library('caret')
library('gridExtra')
library('ggplot2')
library(GGally)
library('MASS')
library('lmtest')
library('car')
library('psych')

## LOADING DATASET

df <- read.csv("SeoulBikeData.csv", check.names = F, na.strings = c("", "NA", "N/A", "na"))
print(names(df))
df <- setNames(df, c("date","bike_count","hour","temp","humidity","wind_speed","visibility","dew_point_temp","solar_radiation","rainfall","snowfall","seasons","holiday","functioning_day"))

## DATA ANALYSIS

  # Check for NULL/MISSING Values
  
    colSums(is.na(df))
    
  # Check for duplicate entries
    
    df = distinct(df)
    dim(df)

## DATA CLEANING
    
  # REMOVING REDUNDANT COLUMNS
    
    df_clean <- df[, !names(df) %in% "date"]
    
    # Convert specified columns to factors
    cols_to_factor <- c("seasons", "holiday", "functioning_day", "hour")
    df_clean[cols_to_factor] <- lapply(df_clean[cols_to_factor], as.factor)
    
    print(ncol(df))
    print(nrow(df))
  
## EXPLORATORY DATA ANALYSIS
    
  # CORRELATION ANALYSIS
    
    ggcorr(df_clean, label = TRUE, label_size = 2.9, hjust = 1, layout.exp = 2)
  
  # VARIABLE ANALYSIS
    
    #Numerical Variable Analysis
    
    ggpairs(df_clean ,
            columns = c("temp", "humidity", "wind_speed","visibility", "dew_point_temp", "solar_radiation", "rainfall", "snowfall"),
            title = "Pairplot of Data")
    
    # Categorical Variable Analyis
    
    plot1 = ggplot(df_clean, aes(x = as.factor(seasons), y = bike_count)) + geom_boxplot(fill="skyblue", alpha=0.2)
    
    plot2 = ggplot(df_clean, aes(x = as.factor(holiday), y = bike_count)) + geom_boxplot(fill="skyblue", alpha=0.2)
    
    plot3 = ggplot(df_clean, aes(x = as.factor(functioning_day), y = bike_count)) + geom_boxplot(fill="skyblue", alpha=0.2)
    
    plot4 = ggplot(df_clean, aes(x = as.factor(hour), y = bike_count)) + geom_boxplot(fill="skyblue", alpha=0.2)
    
    
    
    grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
    

## MODELLING
    
  # Removing the 'Functioning_data' column
    
    df_clean = df_clean[df_clean$functioning_day != "No", ]
    df_clean = df_clean[, !names(df_clean) %in% "functioning_day"]
    summary(df_clean)
    
  # Splitting the data
    
    set.seed(123)
    samplesize <- round(0.7 * nrow(df_clean), 0)
    index <- sample(seq_len(nrow(df_clean)), size = samplesize)
    
    train_data <- df_clean[index, ]
    test_data <- df_clean[-index, ]
    
  # Regression 
    
    # Model_1
    
      set.seed(123)
      model_1 <- lm(bike_count ~ ., data = data_train)
      
      summary(model_1)
    
      stepReg = MASS::stepAIC(model_1 , direction = 'both')
      stepReg$anova

    # Model with Significant Predictors
      
      df_sig <- df_clean[, !(names(df_clean) %in% c('visibility', 'wind_speed'))]
      
      data_train_sig <- df_sig[index, ]
      data_test_sig <- df_sig[-index, ]
      
      set.seed(123)
      model_1_sig <- lm(bike_count ~ ., data = data_train_sig)
      
      summary(model_1_sig)
  
  # Checking for Assumptions
      
      model_check_df <- data.frame(residual = model_1_sig$residuals, fitted = model_1_sig$fitted.values)
      
      
    # Normality Test
      
      # Shapiro Wilk's Test
      
      shapiro.test(model_1_sig$residuals[0:5000])

    # Heterocedasticity
      
      # Breusch Pagan test
      
      bptest(model_1_sig)
      
      # Graph Test
      
      model_check_df %>% ggplot(aes(fitted, residual)) + geom_point() + theme_light() + geom_hline(aes(yintercept = 0))
      
    # Multicollinearity
      
      # Checking Variance Inflation Factor
      
      vif(model_1_sig)
  
  # Building a Average Daily Model
    
    # Making Dataset
      
      df_average <- df[!(df$functioning_day=="No"),]
        
      df_average$seasons <- factor(df_average$seasons, levels = c("Winter", "Spring", "Summer", "Autumn"))
      df_average$seasons <- as.numeric(df_average$seasons)    
      
      df_average$holiday <- factor(df_average$holiday, levels = c("No Holiday", "Holiday"))
      df_average$holiday <- as.numeric(df_average$holiday)
      
      df_average <- df_average %>% 
        group_by(date) %>% 
        summarise(sum_bike_count = sum(bike_count), 
                  mean_temp = mean(temp), 
                  mean_humidity = mean(humidity), 
                  mean_wind_speed = mean(wind_speed), 
                  mean_visibility = mean(visibility), 
                  mean_dew_point_temp = mean(dew_point_temp), 
                  mean_solar_radiation = mean(solar_radiation), 
                  mean_rainfall = mean(rainfall), 
                  mean_snowfall = mean(snowfall), 
                  seasons = mean(as.numeric(seasons)), 
                  holiday = mean(as.numeric(holiday)))
      
      df_average$holiday <- as.factor(df_average$holiday)
      df_average$seasons <- as.factor(df_average$seasons)
      levels(df_average$holiday) <- c('1'= 'No Holiday', '2'='Holiday')
      levels(df_average$seasons) <- c('1'= 'Winter' , '2'='Spring', '3'= 'Summer', '4'='Autumn')
      
      
      multi.hist(df_average[,sapply(df_average, is.numeric)], global = F)
    
    
    # Splitting Dataset
    
      sample_size = floor(0.8 * nrow(df_average))
      
      set.seed(123)
      train = sample(seq_len(nrow(df_average)), size = sample_size)
      
      df_test = df_average[ -train,]
      df_train = df_average[ train,]
    
    
    # Model Building
    
      model_2 <- lm(sum_bike_count ~ mean_temp + mean_humidity + mean_wind_speed + mean_visibility + mean_dew_point_temp + mean_solar_radiation + mean_rainfall + mean_snowfall + seasons + holiday, data=df_train)
      
      summary(model_2)
      
      model_2_step = step(model_2 , direction = 'back')
      
      summary(model_2_step)
      
      car::vif(model_2_step)
      
      model_2_final = lm_day_model_final = lm(sum_bike_count ~ mean_temp + mean_humidity + mean_wind_speed + mean_solar_radiation + mean_rainfall + seasons + holiday, data=df_train)
      
      summary(model_2_final)
      
      car::vif(model_2_final)
    
    # Model Testing
    
    model2_check_df <- data.frame(residual = model_2_final$residuals, fitted = model_2_final$fitted.values)
    
    
      # Normality Test
      
        # Shapiro Wilk's Test
        
        shapiro.test(model_2_final$residuals[0:5000])
      
      # Heterocedasticity
      
        # Breusch Pagan test
        
        bptest(model_2_final)
      
      
      # Multi collinearity
      
        # Checking Variance Inflation Factor
        
        vif(model_2_final)
      
    
    # Model Improvement
        
        
        # LOG TRANSFORMATION
        
        constant <- 100  # You can adjust this value based on your data
        df_average_log <- df_average
        df_average_log$sum_bike_count <- log(df_average$sum_bike_count + constant)
      
        smp_size_log <- floor(0.7 * nrow(df_average_log))
        set.seed(123)
        trainIndex_log <- sample(seq_len(nrow(df_average_log)), size = smp_size_log)
        df_test_log <- df_average_log[-trainIndex_log, ]
        df_train_log <- df_average_log[trainIndex_log, ]
        
        model_final_log <- lm(sum_bike_count ~ mean_temp + mean_wind_speed + 
                                mean_solar_radiation + mean_rainfall + seasons + holiday, 
                              data = df_train_log)
        
        summary(model_final_log)
        
        shapiro.test(model_final_log$residuals[0:5000])
        bptest(model_final_log)
        vif(model_final_log)
        
        
