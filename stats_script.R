# Professional Skills Statistical Assessment
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh
# Last updated: 14 October 2020

## LIBRARY ----
library(tidyverse)
library(ggpubr)

# loading the data
ingatraits <- read.csv("Inga_traits.csv")
str(ingatraits)  # checking the data loaded properly 

## EXERCISE 1: HISTOGRAMS & NORMALITY ----
# Creating a histogram of leaf area 
# checking distribution of data at a variety of binwidths 
(leafarea_hist <- ggplot(ingatraits, aes(x = Leaf_Area)) +
                    xlab("Leaf Area (cm^2)") +
                    ylab("Frequency") +
                    geom_histogram(binwidth = 5,
                                   color = "black") +
                    theme_classic() +
                    scale_y_continuous(expand = c(0,0)))

(leafarea_hist2 <- ggplot(ingatraits, aes(x = Leaf_Area)) +
                      xlab("Leaf Area (cm^2)") +
                      ylab("Frequency") +
                      geom_histogram(binwidth = 10,
                                     color = "black") +
                      theme_classic() +
                      scale_y_continuous(expand = c(0,0)))

(leafarea_hist3 <- ggplot(ingatraits, aes(x = Leaf_Area)) +
                      xlab("Leaf Area (cm^2)") +
                      ylab("Frequency") +
                      geom_histogram(binwidth = 30,
                                     color = "black") +
                      theme_classic() +
                      scale_y_continuous(expand = c(0,0)))

# all show the same distribution: a skew to the left, does not appear to be normally distributed

ggsave(leafarea_hist, file = "leafarea_histogram.png", width = 4, height = 4, units = c("in"))


# Log-transforming leaf area 
ingatraits_trans <- ingatraits %>% 
                        mutate(logLeaf_Area = log(Leaf_Area))

(log_hist <- ggplot(ingatraits_trans, aes(x = logLeaf_Area)) +
                xlab("log(Leaf Area) (cm^2)") +
                ylab("Frequency") +
                geom_histogram(binwidth = 0.15,
                               color = "black") +
                theme_classic() +
                scale_y_continuous(expand = c(0,0)))

# looks more normality distributed with the log transformation 

ggsave(log_hist, file = "log_leafarea_hist.png", width = 4, height = 4, units = c("in"))

