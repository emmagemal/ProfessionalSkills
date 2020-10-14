# Professional Skills Statistical Assessment
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh
# Last updated: 14 October 2020


## WORKFLOW ----
# 1. histogram of leaf area and normality check of data 
# do species in different habitats have different leaf chemical compositions?
# 2. leaf phosphorous concentration vs. habitat type 
#    - make a box plot and conduct an ANOVA
# 3. multiple explanatory variables 
#    - make a scatter plot of leaf [P] vs. leaf [C] with habitat type as 'shape'
#    - split all the species into 2 habitat groups: ADD WHICH
#    - make a model with habitat group and leaf [C] as predictors of leaf [P]
#    - conduct as ANOVA using the model 
# are there tradeoffs between the investment in chemical defences vs in physical ones?
# 4. leaf expansion rate, leaf trichome density and presence of mevalonic acid
#    - make a GLM of leaf expansion rate vs presence of mevalonic acid
#    - make a GLM of leaf trichome density vs presence of mevalonic acid 
#    - make a GLM incorporating both expansion rate and trichome density on mevalonic acid presence
#    - make a FIGURE (PUT WHICH) of how the predictor variable(s) influence the response variable


## LIBRARY ----
library(tidyverse)
library(ggpubr)
library(ggsci)

# loading the data
ingatraits <- read.csv("Inga_traits.csv")
str(ingatraits)  # checking the data loaded properly 

## EXERCISE 1: HISTOGRAMS & NORMALITY ----
# creating a histogram of leaf area 
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

ggsave(leafarea_hist, file = "leafarea_histogram.png", width = 4, height = 4, units = c("in"),
       path = "Figures/")


# log-transforming leaf area (adding it as a new column)
ingatraits <- ingatraits %>% 
                  mutate(logLeaf_Area = log(Leaf_Area))

(log_hist <- ggplot(ingatraits, aes(x = logLeaf_Area)) +
                xlab("log(Leaf Area) (cm^2)") +
                ylab("Frequency") +
                geom_histogram(binwidth = 0.15,
                               color = "black") +
                theme_classic() +
                scale_y_continuous(expand = c(0,0)))

# looks more normality distributed with the log transformation 

ggsave(log_hist, file = "log_leafarea_hist.png", width = 4, height = 4, units = c("in"),
       path = "Figures/")


## EXERCISE 2: BOX PLOTS & ANOVA ----
# removing NA values for chemical concentration variables (N, C, P)
chem_na <- ingatraits %>% 
              drop_na(c(N_Leaf, C_Leaf, P_Leaf))

# creating a box plot of leaf phosphorous concentration in different habitats 
(p_habitat_box <- ggplot(chem_na, aes(x = Habitat, y = P_Leaf, fill = Habitat)) +
                    ylab("Leaf [P]") +
                    geom_boxplot() +
                    theme_classic() +
                    theme(axis.title.x = element_text(margin = margin(t = 10)),
                                  axis.title.y = element_text(margin = margin(r = 10))) +
                    scale_x_discrete(breaks = c("floodplain", "generalist", "upland"),
                                     labels = c("Floodplain", "Generalist", "Upland")) +
                    scale_fill_npg(breaks = c("floodplain", "generalist", "upland"),
                                        labels = c("Floodplain", "Generalist", "Upland")))

ggsave(p_habitat_box, file = "leafP_habitat_boxplot.png", width = 4, height = 3, units = c("in"),
       path = "Figures/")


# conducting an ANOVA to test for statistical significant differnces 
# between leaf phosphorous concentrations of species in different habitats
p_hab_anova <- anova(lm(P_Leaf ~ Habitat, data = chem_na))
p_hab_anova
