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



# Load required libraries
install.packages("copula")
library(copula)
install.packages("extRemes")
library(extRemes)

# Step 1: Extract Daily Returns for Each Bank
sbi_returns <- sbi_daily_df_clean$daily_return
hdfc_returns <- hdfc_daily_df_clean$daily_return
icici_returns <- icici_daily_df_clean$daily_return
idfc_returns <- idfc_daily_df_clean$daily_return

# Step 2: Transform Returns to Uniform Margins using Rank Transformation
sbi_uniform <- rank(sbi_returns) / (length(sbi_returns) + 1)
hdfc_uniform <- rank(hdfc_returns) / (length(hdfc_returns) + 1)
icici_uniform <- rank(icici_returns) / (length(icici_returns) + 1)
idfc_uniform <- rank(idfc_returns) / (length(idfc_returns) + 1)

# Step 3: Initialize and Fit a 4-Dimensional t-Copula for All Banks
copula_model <- tCopula(dim = 4)  # 4-dimensional copula (for all 4 banks)

# Fit the copula model to the transformed data for all 4 banks
fit <- fitCopula(copula_model, cbind(sbi_uniform, hdfc_uniform, icici_uniform, idfc_uniform), method = "ml")

# Step 4: Pairwise Copula Evaluation (2D for each bank pair)

# Function to plot density for any pair of banks and calculate tail dependence
plot_pairwise_density_and_taildep <- function(bank1_uniform, bank2_uniform, bank1_name, bank2_name) {
  grid_size <- 50  # Grid size for plotting
  u_vals <- seq(0, 1, length.out = grid_size)
  v_vals <- seq(0, 1, length.out = grid_size)
  grid <- expand.grid(u = u_vals, v = v_vals)
  
  # Create a 2D t-Copula for the pair of banks
  copula_pair <- tCopula(dim = 2)
  
  # Fit the copula to the pair's data
  fit_pair <- fitCopula(copula_pair, cbind(bank1_uniform, bank2_uniform), method = "ml")
  
  # Calculate tail dependence for the pair
  tail_dep <- taildep(cbind(bank1_uniform, bank2_uniform), u = 0.95)
  
  cat(paste("Upper Tail Dependence Coefficient (", bank1_name, " vs ", bank2_name, "): ", tail_dep[1], "\n", sep = ""))
  cat(paste("Lower Tail Dependence Coefficient (", bank1_name, " vs ", bank2_name, "): ", tail_dep[2], "\n", sep = ""))
  
  # Evaluate the copula density for the grid points
  z_vals <- dCopula(cbind(grid$u, grid$v), fit_pair@copula)
  
  # Reshape the results to match the grid for contour plotting
  z_matrix <- matrix(z_vals, nrow = grid_size, ncol = grid_size)
  
  # Save the plot to a file to avoid display issues
  png(filename = paste0("copula_density_", bank1_name, "_", bank2_name, ".png"), width = 800, height = 800)
  persp(u_vals, v_vals, z_matrix, col = "lightblue", theta = 30, phi = 20,
        main = paste("Contour Plot of Fitted t-Copula Density (", bank1_name, " vs ", bank2_name, ")", sep = ""),
        xlab = paste(bank1_name, " Returns (Uniform)"),
        ylab = paste(bank2_name, " Returns (Uniform)"), zlab = "Density")
  dev.off()
}


# Plot pairwise densities and calculate tail dependence for each bank comb

plot_pairwise_density_and_taildep(sbi_uniform, icici_uniform, "SBI", "ICICI")
plot_pairwise_density_and_taildep(sbi_uniform, idfc_uniform, "SBI", "IDFC")
plot_pairwise_density_and_taildep(hdfc_uniform, icici_uniform, "HDFC", "ICICI")
plot_pairwise_density_and_taildep(hdfc_uniform, idfc_uniform, "HDFC", "IDFC")
plot_pairwise_density_and_taildep(icici_uniform, idfc_uniform, "ICICI", "IDFC")

# Step 5: Simulate Joint Returns Using Copula
copula_model <- tCopula(dim = 4)  # 4-dimensional copula (for all 4 banks)
fit <- fitCopula(copula_model, cbind(sbi_uniform, hdfc_uniform, icici_uniform, idfc_uniform), method = "ml")

# Define portfolio weights for the four banks (equally weighted portfolio in this case)
weights <- c(0.25, 0.25, 0.25, 0.25)  # Equal weights for the 4 banks (SBI, HDFC, ICICI, IDFC)

# Number of simulations
n_simulations <- 10000

# Step 5: Simulate Joint Returns Using Copula with Empirical Distributions
simulated_returns <- matrix(0, ncol = 4, nrow = n_simulations)

for (i in 1:n_simulations) {
  u_simulated <- rCopula(1, fit@copula)  # Generate uniform samples from the copula
  
  # Map to empirical quantiles of each bank's returns
  simulated_returns[i, 1] <- quantile(sbi_returns, u_simulated[1])
  simulated_returns[i, 2] <- quantile(hdfc_returns, u_simulated[2])
  simulated_returns[i, 3] <- quantile(icici_returns, u_simulated[3])
  simulated_returns[i, 4] <- quantile(idfc_returns, u_simulated[4])
}

# Step 6: Calculate Portfolio Returns
portfolio_returns <- rowSums(simulated_returns * weights)

# Step 7: Calculate Portfolio VaR at 95% Confidence Level
portfolio_VaR_95 <- quantile(portfolio_returns, 0.05)
cat("Portfolio 95% VaR:", portfolio_VaR_95, "\n")

#Code to Compare Individual and Portfolio VaRs:

# Assuming the VaR values for individual banks are already computed (e.g., from the Peak Over Threshold (POT) method)
idfc_VaR <- -0.0557357568264592
icici_VaR <- -0.0651771850525703
sbi_VaR <- -0.0572970243127165
hdfc_VaR <- -0.0444725322098508

# Portfolio VaR
portfolio_VaR <- -1.286901  # This is from your previous calculation

#Code to Calculate CVaR:

# Define a function to calculate CVaR from a given series of returns and the VaR level
calculate_CVaR <- function(returns, VaR_level) {
  # Subset returns that are below the VaR threshold (i.e., tail risk)
  tail_risk_returns <- returns[returns <= VaR_level]
  
  # Calculate the mean of the tail risk returns (Conditional VaR)
  CVaR <- mean(tail_risk_returns)
  
  return(CVaR)
}

# Assuming you have the daily returns for each bank (for example: sbi_returns)
# Calculate CVaR for each bank
sbi_CVaR <- calculate_CVaR(sbi_returns, sbi_VaR)
hdfc_CVaR <- calculate_CVaR(hdfc_returns, hdfc_VaR)
icici_CVaR <- calculate_CVaR(icici_returns, icici_VaR)
idfc_CVaR <- calculate_CVaR(idfc_returns, idfc_VaR)

#portfolio Cvar

# Assuming you have the following weights for each bank in your portfolio:
# For example, let's assume equal weight for each bank. Modify based on your actual weights.
weights <- c(SBI = 0.25, HDFC = 0.25, ICICI = 0.25, IDFC = 0.25)

# Combine the returns from each bank into a portfolio return
portfolio_returns <- weights["SBI"] * sbi_returns + 
  weights["HDFC"] * hdfc_returns + 
  weights["ICICI"] * icici_returns + 
  weights["IDFC"] * idfc_returns

# Calculate Portfolio VaR
portfolio_VaR <- quantile(portfolio_returns, 0.05)  # 5% quantile (VaR at 95% confidence level)

# Calculate Portfolio CVaR (Conditional VaR)
portfolio_CVaR <- mean(portfolio_returns[portfolio_returns <= portfolio_VaR])

# Print Portfolio VaR and CVaR
cat("Portfolio VaR:", portfolio_VaR, "\n")
cat("Portfolio CVaR:", portfolio_CVaR, "\n")
