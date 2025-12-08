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
     col="blue")


cor.test(dfCalStep$Steps, dfCalStep$Calories, method="spearman" , exact = FALSE)

hist(X25$calories_burned,
     breaks = 10,
     main = "Histogram of Daily Calories Burned",
     xlab = "Calories Burned",
     col = "lightblue",
     border = "black",
     freq = FALSE)

mean_cal <- mean(X25$calories_burned, na.rm = TRUE)
sd_cal <- sd(X25$calories_burned, na.rm = TRUE)

curve(dnorm(x, mean = mean_cal, sd = sd_cal),
      col = "red",
      lwd = 2,

      add = TRUE)
