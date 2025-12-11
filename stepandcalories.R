# Load the dataset from Excel
X25 <- read.csv("C:/Users/kasun/Desktop/25.csv")

# Convert to data frame and prepare numeric variables
dfCalStep <- as.data.frame(X25)
dfCalStep$Calories <- as.numeric(dfCalStep$calories_burned)
dfCalStep$Steps <- as.numeric(dfCalStep$step_count)

# Scatterplot (Main Plot) 
plot(X25$step_count, X25$calories_burned,
     main = "Does taking more steps lead to burning more calories?",  # Plot title
     xlab="Step Count",                                               # X-axis label
     ylab="Calories Burned",                                          # Y-axis label
     pch=19,                                                          # Point style
     col="blue",                                                      # Point colour
     xaxt = "n",                                                      # Custom x-axis
     las = 1)

# Major x-axis ticks (every 1000 steps)
axis(1,
     at     = seq(0, 8000, by = 1000),
     labels = seq(0, 8000, by = 1000),
     las    = 2)

# Minor x-axis ticks (every 500 steps)
axis(1,
     at     = seq(0, 8000, by = 500),
     labels = FALSE,
     tck    = -0.01)

# Add trend line 
abline(lm(calories_burned ~ step_count, data = X25),
       col = "red",
       lwd = 2)

# Statistical Test (Spearman Correlation) 
# Tests the hypothesis about relationship between steps and calories
cor.test(dfCalStep$Steps, dfCalStep$Calories, method="spearman" , exact = FALSE)

# Histogram 
hist(X25$calories_burned,
     breaks = 10,
     main = "Histogram of Daily Calories Burned",  # Histogram title
     xlab = "Calories Burned",                     # X-axis label
     col = "lightblue",
     border = "black",
     freq = FALSE,                                 # Density scale
     xaxt   = "n",
     las    = 1)

# Range of calorie values for axis creation
rng <- range(X25$calories_burned, na.rm = TRUE)

# Add x-axis to histogram
axis(1,
     at     = seq(floor(rng[1]/25)*25, 300, by = 25),
     labels = seq(floor(rng[1]/25)*25, 300, by = 25),
     las    = 2)

# Mean and SD for normal curve
mean_cal <- mean(X25$calories_burned, na.rm = TRUE)
sd_cal <- sd(X25$calories_burned, na.rm = TRUE)

# Overlay normal distribution curve
curve(dnorm(x, mean = mean_cal, sd = sd_cal),
      col = "red",
      lwd = 2,
      add = TRUE)
