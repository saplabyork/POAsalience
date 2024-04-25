setwd("/Users/chandannarayan/Google Drive/My Drive/Studies/Debuccal/POAsalience/Recordings/Noise")

#make spectrum of the multi-talker babble noise
noise_spec <- read.csv("bab_noise_1500_0_703-Table.csv", header=TRUE)

library(ggplot2)
library(zoo)

##smooth the data to rolling average of 100 samples
noise_spec$intensity_smooth <- 
  zoo::rollmean(x = noise_spec$pow.dB.Hz.,k=500,na.pad = TRUE)

##plot
noise_ltas_smooth <- ggplot(noise_spec) +
  aes(x = freq.Hz., y = intensity_smooth) +
  coord_cartesian(xlim = c(0,8000)) + ylim(c(-10,40))+
  geom_line() + xlab("Freq (Hz)") + ylab("SPL (dB)") +
  theme_bw()

noise_ltas_smooth
