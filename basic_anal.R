setwd("/Users/chandannarayan/Desktop/Master_data/")
data_15 <- read.csv("Master_15_local.csv", header = TRUE)
data_5 <- read.csv("Master_5_local.csv", header = TRUE)
data_clean <- read.csv("Master_clean_local.csv", header = TRUE)

full_data <- rbind(data_clean,data_15,data_5)

## Number of participants per snr

library(dplyr)

# Group by "snr" and count unique values of "part" for each group
result <- filtered_full_data %>%
  group_by(snr) %>%
  summarize(unique_participants = n_distinct(part))

# Print the result
print(result)

## Remove all contrasts except p- ones
# Full data clean
remove_rows <- function(full_data) {
  allowed_values <- c("p-th", "p-t", "p-k", "p-h")
  filtered_full_data <- subset(full_data, con %in% allowed_values)
  return(filtered_full_data)
}

filtered_full_data <- remove_rows(full_data)
filtered_full_data$snr <- as.factor(filtered_full_data$snr)

library(lme4)
filtered_full_data$snr <- relevel(filtered_full_data$snr, ref = "clean")

model <- glmer(corr ~ snr * con + (1 | part), data = filtered_full_data, family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
summary(model)

## Full plot of p-data by snr
ggplot(filtered_full_data, aes(x = con, y = corr, fill = con)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") + 
  #geom_errorbar() +
  ylim(0,1)+
  facet_wrap(~ snr) +
  labs(title = "",
       x = "Contrast",
       y = "Mean proportion correct") +
  theme_minimal()



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

model <- glmer(corr ~ con + (1 | part), data = filtered_data_clean, family = binomial, control = lmerControl(optimizer ="Nelder_Mead"))
summary(model)

library(ggplot2)
p_clean <- ggplot(filtered_data_clean, aes(x = con, y = corr, fill = con)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  facet_wrap(~ snr, scales = "free") +
  labs(title = "Mean of corr by con for each snr",
       x = "Contrast",
       y = "Mean proportion correct") +
  theme_minimal()
p_clean

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
p_5 <- ggplot(filtered_data_15, aes(x = con, y = corr, fill = con)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  facet_wrap(~ snr, scales = "free") +
  labs(title = "Mean of corr by con for each snr",
       x = "Contrast",
       y = "Mean proportion correct") +
  theme_minimal()
p_5

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
p_5 <- ggplot(filtered_data_5, aes(x = con, y = corr, fill = con)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") +
  facet_wrap(~ snr, scales = "free") +
  labs(title = "",
       x = "Contrast",
       y = "Mean proportion correct") +
  theme_minimal()
p_5


## Make a master bar chart of proportion correct w error bars

library(ggplot2)
library(dplyr)

# new names for snr facet labels
snr_names <- list(
  "clean"="No added noise",
  "15"="15dB SNR",
  "5"="5dB SNR")

# use the labeller function in facet
snr_labeller <- function(variable,value){
  return(snr_names[value])
}

# Calculate the mean and standard error (SEM) for each group
summary_filtered_full_data <- filtered_full_data %>%
  group_by(snr, con) %>%
  summarise(mean_corr = mean(corr),
            sd_corr = sd(corr),
            n = n()) %>%
  mutate(se_corr = sd_corr / sqrt(n))  # Calculate Standard Error

# Plot using ggplot2

ggplot(summary_filtered_full_data, aes(x = con, y = mean_corr, fill = con)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) + ylim(0,1)+
  geom_errorbar(aes(ymin = mean_corr - se_corr, ymax = mean_corr + se_corr),
                position = position_dodge(width = 0.9), width = 0.25) +
  facet_wrap(~snr,ncol = 3, labeller = snr_labeller) +
  labs(x = "con", y = "Proportion Correct", fill = "Contrast") +
  theme_minimal()
