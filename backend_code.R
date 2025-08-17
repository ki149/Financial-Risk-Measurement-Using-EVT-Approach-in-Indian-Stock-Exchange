install.packages("quantmod")
library(quantmod)

#IDFC data 
getSymbols("IDFCFIRSTB.NS", src = "yahoo", from = "2019-10-01", to = "2024-09-30")
idfc_daily=to.daily(IDFCFIRSTB.NS)
View(idfc_daily)

#ICICI data 
getSymbols("ICICIBANK.NS", src = "yahoo", from = "2019-10-01", to = "2024-09-30")
icici_daily=to.daily(ICICIBANK.NS)
View(icici_daily)

#SBI data
getSymbols("SBIN.NS", src = "yahoo", from = "2019-10-01", to = "2024-09-30")
sbi_daily=to.daily(SBIN.NS)
View(sbi_daily)

#HDFC bank
getSymbols("HDFCBANK.NS", src = "yahoo", from = "2019-10-01", to = "2024-09-30")
hdfc_daily <- to.daily(HDFCBANK.NS)
View(hdfc_daily)

#calculating daily_return for IDFC
idfc_daily$daily_return <- (idfc_daily$IDFCFIRSTB.NS.Close / lag(idfc_daily$IDFCFIRSTB.NS.Close, 1)) - 1

#calculating daily_return for ICICI
icici_daily$daily_return <- (icici_daily$ICICIBANK.NS.Adjusted / lag(icici_daily$ICICIBANK.NS.Adjusted, 1)) - 1

#calculating daily_return for SBI
sbi_daily$daily_return <- (sbi_daily$SBIN.NS.Adjusted / lag(sbi_daily$SBIN.NS.Adjusted, 1)) - 1

#calculating daily_return for HDFC bank
hdfc_daily$daily_return <- (hdfc_daily$HDFCBANK.NS.Close / lag(hdfc_daily$HDFCBANK.NS.Close, 1)) - 1

#Converting data from xts format to data frame 
idfc_daily_df <- data.frame(Date = index(idfc_daily), coredata(idfc_daily))
sbi_daily_df <- data.frame(Date = index(sbi_daily), coredata(sbi_daily))
icici_daily_df <- data.frame(Date = index(icici_daily), coredata(icici_daily))
hdfc_daily_df <- data.frame(Date = index(hdfc_daily), coredata(hdfc_daily))

#plotting graph of Daily returns over time to show the fluctuations of stock returns

library(ggplot2)

library(patchwork)

#For plotting return we first need to remove missing value 

#IDFC BANK 

na_count <- sum(is.na(idfc_daily_df$daily_return))
cat("Number of missing values in Daily_Return:", na_count, "\n")
idfc_daily_df_clean <- na.omit(idfc_daily_df)

#Plotting Daily returns over Time for IDFC BANK in line graph 
p1=ggplot(idfc_daily_df_clean, aes(x = Date, y = daily_return)) +
  geom_line(color = "blue", size = 0.5) +   # Add the blue line
  geom_point(color = "red", size = 1) +     # Add red markers for points
  theme_minimal() +                         # Minimal theme for a cleaner look
  labs(title = "IDFC",x = "Date",y = "Returns") 

#ICICI BANK 

na_count <- sum(is.na(icici_daily_df$daily_return))
cat("Number of missing values in Daily_Return:", na_count, "\n")
icici_daily_df_clean <- na.omit(icici_daily_df)

#Plotting Daily returns over Time for ICICI BANK in line graph 
p2=ggplot(icici_daily_df_clean, aes(x = Date, y = daily_return)) +
  geom_line(color = "blue", size = 0.5) +   # Add the blue line
  geom_point(color = "red", size = 1) +     # Add red markers for points
  theme_minimal() +                         # Minimal theme for a cleaner look
  labs(title = "ICICI",x = "Date",y = "Returns") 

#SBI BANK 

na_count <- sum(is.na(sbi_daily_df$daily_return))
cat("Number of missing values in Daily_Return:", na_count, "\n")
sbi_daily_df_clean <- na.omit(sbi_daily_df)

#Plotting Daily returns over Time for SBI BANK in line graph 
p3=ggplot(sbi_daily_df_clean, aes(x = Date, y = daily_return)) +
  geom_line(color = "blue", size = 0.5) +   # Add the blue line
  geom_point(color = "red", size = 1) +     # Add red markers for points
  theme_minimal() +                         # Minimal theme for a cleaner look
  labs(title = "SBI",x = "Date",y = "Returns") 

#HDFC BANK 

na_count <- sum(is.na(hdfc_daily_df$daily_return))
cat("Number of missing values in Daily_Return:", na_count, "\n")
hdfc_daily_df_clean <- na.omit(hdfc_daily_df)

#Plotting Daily returns over Time for HDFC BANK in line graph 
p4=ggplot(hdfc_daily_df_clean, aes(x = Date, y = daily_return)) +
  geom_line(color = "blue", size = 0.5) +   # Add the blue line
  geom_point(color = "red", size = 1) +     # Add red markers for points
  theme_minimal() +                         # Minimal theme for a cleaner look
  labs(title = "HDFC",x = "Date",y = "Returns") 


## Combined Plots for Comparing the data  
combined_plot <- (p1+p2) /(p3+p4) 
plot_annotation(title = "Daily Returns of Different Banks",
                theme = theme(plot.title = element_text(hjust = 0.5)))


# Descriptive stats of stock returns

idfc_mean<-mean(idfc_daily_df_clean$daily_return)
idfc_var<-var(idfc_daily_df_clean$daily_return)
idfc_min<-min(idfc_daily_df_clean$daily_return)
idfc_max<-max(idfc_daily_df_clean$daily_return)
idfc_sd<-sd(idfc_daily_df_clean$daily_return)

icici_mean<-mean(icici_daily_df_clean$daily_return)
icici_var<-var(icici_daily_df_clean$daily_return)
icici_min<-min(icici_daily_df_clean$daily_return)
icici_max<-max(icici_daily_df_clean$daily_return)
icici_sd<-sd(icici_daily_df_clean$daily_return)

sbi_mean<-mean(sbi_daily_df_clean$daily_return)
sbi_var<-var(sbi_daily_df_clean$daily_return)
sbi_min<-min(sbi_daily_df_clean$daily_return)
sbi_max<-max(sbi_daily_df_clean$daily_return)
sbi_sd<-sd(sbi_daily_df_clean$daily_return)

hdfc_mean<-mean(hdfc_daily_df_clean$daily_return)
hdfc_var<-var(hdfc_daily_df_clean$daily_return)
hdfc_min<-min(hdfc_daily_df_clean$daily_return)
hdfc_max<-max(hdfc_daily_df_clean$daily_return)
hdfc_sd<-sd(hdfc_daily_df_clean$daily_return)

# Create a boxplot for the 'return' column to identify extreme values & heavy tail

b1<-ggplot(idfc_daily_df_clean, aes(y = daily_return)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +  
  labs(title = "Boxplot of Stock Returns of IDFC", y = "Returns") +
  theme_minimal()

b2<-ggplot(icici_daily_df_clean, aes(y = daily_return)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +  
  labs(title = "Boxplot of Stock Returns of ICICI", y = "Returns") +
  theme_minimal()

b3<-ggplot(sbi_daily_df_clean, aes(y = daily_return)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +  
  labs(title = "Boxplot of Stock Returns of SBI", y = "Returns") +
  theme_minimal()

b4<-ggplot(hdfc_daily_df_clean, aes(y = daily_return)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +  
  labs(title = "Boxplot of Stock Returns of HDFC", y = "Returns") +
  theme_minimal()

# Combined Plots for Comparing the data  
combined_plot <- (b1 + b2) / (b3 + b4)
plot_annotation(title = "Daily Returns of Different Banks",
                theme = theme(plot.title = element_text(hjust = 0.5)))

# Create a Q-Q plot to show whether our data follows normality

q1=ggplot(idfc_daily_df_clean, aes(sample = daily_return)) +
  geom_qq() +                           # Q-Q plot for returns
  geom_qq_line(color = "red") +        # Reference line for normal distribution
  labs(title = "IDFC", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()                   # Simple theme

q2=ggplot(icici_daily_df_clean, aes(sample = daily_return)) +
  geom_qq() +                           # Q-Q plot for returns
  geom_qq_line(color = "red") +        # Reference line for normal distribution
  labs(title = "ICICI", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()                   # Simple theme

q3=ggplot(sbi_daily_df_clean, aes(sample = daily_return)) +
  geom_qq() +                           # Q-Q plot for returns
  geom_qq_line(color = "red") +        # Reference line for normal distribution
  labs(title = "SBI", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()                   # Simple theme

q4=ggplot(hdfc_daily_df_clean, aes(sample = daily_return)) +
  geom_qq() +                           # Q-Q plot for returns
  geom_qq_line(color = "red") +        # Reference line for normal distribution
  labs(title = "HDFC", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()                   # Simple theme

#combined
combined_plot <- (q1 + q2) / (q3 + q4)
plot_annotation(title = "Daily Returns of Different Banks",
                theme = theme(plot.title = element_text(hjust = 0.5)))

# Kolmogorov-smirnov test results data normality
#H0: stock return data follows normal distribution
#H1: stock return data does not follow normal distribution

# IDFC- Kolmogorov-smirnov test results data normality
idfc_return<-idfc_daily_df_clean$daily_return
idfc_ks<-ks.test(idfc_return,"pnorm",idfc_mean,idfc_sd)

# icici - Kolmogorov-smirnov test results data normality
icici_return<-icici_daily_df_clean$daily_return
icici_ks_result<-ks.test(icici_return,"pnorm",icici_mean,icici_sd)

# SBI- Kolmogorov-smirnov test results data normality
sbi_return<-sbi_daily_df_clean$daily_return
sbi__result<-ks.test(sbi_return,"pnorm",sbi_mean,sbi_sd)

# HDFC- Kolmogorov-smirnov test results data normality
hdfc_return<-hdfc_daily_df_clean$daily_return
hdfc_result<-ks.test(hdfc_return,"pnorm",hdfc_mean,hdfc_sd)

d_value1<-1.36/sqrt(1235)


#Calculating threshold value and estimating parameters 
install.packages("extRemes")
library(extRemes)
library(evd)

#IDFC
idfc_negative_returns <- idfc_return[idfc_return < 0]

threshold_percent <- 0.10
idfc_neg_threshold <- quantile(idfc_negative_returns,0.10)

idfc_negative_extremes <- idfc_negative_returns[idfc_negative_returns<=idfc_neg_threshold]

#fitting generalized pareto distribution and estimating parameters using MLE
idfc_gpd_fit_negative <- fevd(-idfc_negative_extremes, type = "GP", threshold = -idfc_neg_threshold, method = "MLE")

length(idfc_negative_returns)
length(idfc_negative_extremes)

# idfc VaR calculation
u1 <--0.03705888          # Threshold value
sigma_hat1 <- 0.01237413  # Scale parameter from GPD estimation
xi_hat1 <- 0.37958982     # Shape parameter from GPD estimation
n1 <- 604          # Total number of observations
k1 <- 61          # Number of observations above the threshold
alpha <- 0.05     # Confidence level (for 95% VaR)

idfc_VaR<- u1 + (sigma_hat1 / xi_hat1) * (((n1 / k1) * (1 - alpha))^(-xi_hat1) - 1)
print(paste("VaR for the IDFC is:", idfc_VaR))


#ICICI
icici_negative_returns <- icici_return[icici_return < 0]

threshold_percent <- 0.10
icici_neg_threshold <- quantile(icici_negative_returns,0.10)

icici_negative_extremes <- icici_negative_returns[icici_negative_returns <= icici_neg_threshold]

#fitting generalized pareto distribution and estimating parameters using MLE
icici_gpd_fit_negative <- fevd(-icici_negative_extremes, type = "GP", threshold = -icici_neg_threshold, method = "MLE")

length(icici_negative_returns)
length(icici_negative_extremes)

# icici VaR calculation
u2 <--0.02703365          # Threshold value
sigma_hat2 <- 0.01957255  # Scale parameter from GPD estimation
xi_hat2 <- 0.12830462     # Shape parameter from GPD estimation
n2 <- 575          # Total number of observations
k2 <- 58      # Number of observations above the threshold
alpha <- 0.05     # Confidence level (for 95% VaR)

icici_VaR<- u2 + (sigma_hat2 / xi_hat2) * (((n2 / k2) * (1 - alpha))^(-xi_hat2) - 1)
print(paste("VaR for the ICICI is:", icici_VaR))


#SBI
sbi_negative_returns <- sbi_return[sbi_return < 0]

threshold_percent <- 0.10
sbi_neg_threshold <- quantile(sbi_negative_returns,0.10)

sbi_negative_extremes <- sbi_negative_returns[sbi_negative_returns <= sbi_neg_threshold]

sbi_gpd_fit_negative <- fevd(-sbi_negative_extremes, type = "GP", threshold = -sbi_neg_threshold, method = "MLE")

length(sbi_negative_returns)
length(sbi_negative_extremes)

# sbi VaR calculation
u3 <--0.0340385            # Threshold value
sigma_hat3 <- 0.0133620  # Scale parameter from GPD estimation
xi_hat3 <- 0.2351731     # Shape parameter from GPD estimation
n3 <- 563          # Total number of observations
k3 <- 57         # Number of observations above the threshold
alpha <- 0.05     # Confidence level (for 95% VaR)

sbi_VaR<- u3 + (sigma_hat3 / xi_hat3) * (((n3 / k3) * (1 - alpha))^(-xi_hat3) - 1)
print(paste("VaR for the sbi is:", sbi_VaR))


#HDFC
hdfc_negative_returns <- hdfc_return[hdfc_return < 0]

threshold_percent <- 0.10
hdfc_neg_threshold <- quantile(hdfc_negative_returns,0.10)

hdfc_negative_extremes <- hdfc_negative_returns[hdfc_negative_returns <= hdfc_neg_threshold]

hdfc_gpd_fit_negative <- fevd(-hdfc_negative_extremes, type = "GP", threshold = -hdfc_neg_threshold, method = "MLE")

length(hdfc_negative_returns)
length(hdfc_negative_extremes)

# hdfc VaR calculation
u4 <--0.0245661           # Threshold value
sigma_hat4 <- 0.01190168  # Scale parameter from GPD estimation
xi_hat4 <- 0.27793726     # Shape parameter from GPD estimation
n4 <- 589          # Total number of observations
k4 <- 59         # Number of observations above the threshold
alpha <- 0.05     # Confidence level (for 95% VaR)

hdfc_VaR<- u4 + (sigma_hat4 / xi_hat4) * (((n4 / k4) * (1 - alpha))^(-xi_hat4) - 1)
print(paste("VaR for the hdfc is:", hdfc_VaR))
