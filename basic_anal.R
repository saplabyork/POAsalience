setwd("~/Google Drive/My Drive/Studies/Debuccal/POAsalience/DATA/")
data_15 <- read.csv("Master_15.csv", header = TRUE)
data_5 <- read.csv("Master_5.csv", header = TRUE)
data_clean <- read.csv("Master_clean.csv", header = TRUE)

full_data <- rbind(data_clean,data_15,data_5)

##Clean
remove_rows <- function(data_clean) {
  allowed_values <- c("p-th", "p-t", "p-k", "p-h")
  filtered_data_clean <- subset(data_clean, con %in% allowed_values)
  return(filtered_data_clean)
}

filtered_data_clean <- remove_rows(filtered_data_clean)
filtered_data$snr <- as.factor(filtered_data$snr)

library(lme4)
filtered_data$snr <- relevel(filtered_data$snr, ref = "clean")

model <- glmer(corr ~ con * vowel + (1 | part), data = filtered_data_clean, family = binomial)
summary(model)

library(ggplot2)
p <- ggplot(filtered_data_clean, aes(x = con, y = corr, fill = con)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  facet_wrap(~ snr, scales = "free") +
  labs(title = "Mean of corr by con for each snr",
       x = "Contrast",
       y = "Mean proportion correct") +
  theme_minimal()
p

##SNR15

remove_rows <- function(data_15) {
  allowed_values <- c("p-th", "p-t", "p-k", "p-h")
  filtered_data_15 <- subset(data_15, con %in% allowed_values)
  return(filtered_data_15)
}

filtered_data_15 <- remove_rows(data_15)
filtered_data$snr <- as.factor(filtered_data$snr)

filtered_data_15$snr <- relevel(filtered_data_15$snr, ref = "clean")

model <- glmer(corr ~ con + (1 | part), data = filtered_data_15, family = binomial)
summary(model)

library(ggplot2)
p <- ggplot(filtered_data_15, aes(x = con, y = corr, fill = con)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  facet_wrap(~ snr, scales = "free") +
  labs(title = "Mean of corr by con for each snr",
       x = "Contrast",
       y = "Mean proportion correct") +
  theme_minimal()
p

##SNR5

remove_rows <- function(data_5) {
  allowed_values <- c("p-th", "p-t", "p-k", "p-h")
  filtered_data_5 <- subset(data_5, con %in% allowed_values)
  return(filtered_data_5)
}

filtered_data_5 <- remove_rows(data_5)
filtered_data$snr <- as.factor(filtered_data$snr)

filtered_data_5$snr <- relevel(filtered_data_5$snr, ref = "clean")

model <- glmer(corr ~ con + (1 | part), data = filtered_data_5, family = binomial)
summary(model)

library(ggplot2)
p <- ggplot(filtered_data_5, aes(x = con, y = corr, fill = con)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  facet_wrap(~ snr, scales = "free") +
  labs(title = "",
       x = "Contrast",
       y = "Mean proportion correct") +
  theme_minimal()
p
