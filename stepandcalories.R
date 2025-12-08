X25 <- read_xlsx("C:/Users/kasun/Desktop/25.xlsx")

dfCalStep <- as.data.frame(X25)
dfCalStep$Calories <- as.numeric(dfCalStep$calories_burned)
dfCalStep$Steps <- as.numeric(dfCalStep$step_count)

cor(dfCalStep$Calories, dfCalStep$Steps, use="pairwise.complete.obs", method="spearman")

plot(X25$step_count, X25$calories_burned,
     main = "Does taking more steps lead to burning more calories?",
     sub="Calories Burned vs Step Count",
     xlab="Step Count",
     ylab="Calories Burned",
     pch=19,
     col="blue",
 xaxt = "n",
 las = 1))

axis(1,
     at     = seq(0, 8000, by = 1000),
     labels = seq(0, 8000, by = 1000),
     las    = 2)

axis(1,
     at     = seq(0, 8000, by = 500),
     labels = FALSE,
     tck    = -0.01)

cor.test(dfCalStep$Steps, dfCalStep$Calories, method="spearman" , exact = FALSE)

hist(X25$calories_burned,
     breaks = 10,
     main = "Histogram of Daily Calories Burned",
     xlab = "Calories Burned",
     col = "lightblue",
     border = "black",
     freq = FALSE
     xaxt   = "n",   
     las    = 1)

rng <- range(X25$calories_burned, na.rm = TRUE)
axis(1,
     at     = seq(floor(rng[1]/25)*25, 300, by = 25),
     labels = seq(floor(rng[1]/25)*25, 300, by = 25),
     las    = 2)

mean_cal <- mean(X25$calories_burned, na.rm = TRUE)
sd_cal <- sd(X25$calories_burned, na.rm = TRUE)

curve(dnorm(x, mean = mean_cal, sd = sd_cal),
      col = "red",
      lwd = 2,

      add = TRUE)




