# Professional Skills Statistical Assessment
# Emma Gemal, s1758915@sms.ed.ac.uk
# University of Edinburgh
# Last updated: 25 October 2020


## LIBRARY ----
library(tidyverse)
library(ggpubr)
library(ggsci)
library(lmtest)
library(car)
library(mvShapiroTest)
library(ggiraphExtra)

# loading the data
ingatraits <- read.csv("Inga_traits.csv")
str(ingatraits)  # checking the data loaded properly 

## EXERCISE 1: HISTOGRAMS & NORMALITY ----
# creating a histogram of leaf area 
# checking distribution of data at a variety of binwidths 
(leafarea_hist <- ggplot(ingatraits, aes(x = Leaf_Area)) +
                    xlab(expression(paste("Leaf Area", ' ', (cm^-2), sep = ''))) +
                    ylab("Frequency") +
                    geom_histogram(binwidth = 5,
                                   color = "black") +
                    theme_classic() +
                    theme(axis.title.x = element_text(margin = margin(t = 10)),
                          axis.title.y = element_text(margin = margin(r = 10))) +
                    scale_y_continuous(expand = c(0,0)))

(leafarea_hist2 <- ggplot(ingatraits, aes(x = Leaf_Area)) +
                      xlab(expression(paste("Leaf Area", ' ', (cm^-2), sep = ''))) +
                      ylab("Frequency") +
                      geom_histogram(binwidth = 10,
                                     color = "black") +
                      theme_classic() +
                      scale_y_continuous(expand = c(0,0)))

(leafarea_hist3 <- ggplot(ingatraits, aes(x = Leaf_Area)) +
                      xlab(expression(paste("Leaf Area", ' ', (cm^-2), sep = ''))) +
                      ylab("Frequency") +
                      geom_histogram(binwidth = 30,
                                     color = "black") +
                      theme_classic() +
                      scale_y_continuous(expand = c(0,0)))

# all show the same distribution: a skew to the left, does not appear to be normally distributed

ggsave(leafarea_hist, file = "leafarea_histogram.png", width = 4, height = 4, units = c("in"),
       path = "Figures/")

# checking for normality of leaf size using a Shapiro-Wilks test
shapiro.test(ingatraits$Leaf_Area)   # is significant (p < 0.05), it is NOT normally distributed


# log-transforming leaf area (adding it as a new column)
ingatraits <- ingatraits %>% 
                  mutate(logLeaf_Area = log(Leaf_Area))

(log_hist <- ggplot(ingatraits, aes(x = logLeaf_Area)) +
                xlab(expression(paste("log(Leaf Area)", ' ', (cm^-2), sep = ''))) +
                ylab("Frequency") +
                geom_histogram(binwidth = 0.15,
                               color = "black") +
                theme_classic() +
                theme(axis.title.x = element_text(margin = margin(t = 10)),
                      axis.title.y = element_text(margin = margin(r = 10))) +
                scale_y_continuous(expand = c(0,0)))

# looks more normally distributed with the log transformation 

ggsave(log_hist, file = "log_leafarea_hist.png", width = 4, height = 4, units = c("in"),
       path = "Figures/")


## EXERCISE 2: BOX PLOTS & ANOVA ----
# removing NA values for chemical concentration variables (N, C, P)
chem_na <- ingatraits %>% 
              drop_na(c(C_Leaf, P_Leaf))

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
p_hab <- lm(P_Leaf ~ Habitat, data = chem_na)
p_hab_anova <- anova(p_hab)
p_hab_anova

# conducting a Tukey Test to further investigate the differences between habitats
p_hab_tukey <- TukeyHSD(aov(lm(P_Leaf ~ Habitat, data = chem_na)))
p_hab_tukey

# checking for normality of residuals and heteroskedasticity 
hist(residuals(p_hab))   # could potentially be normally distributed
shapiro.test(residuals(p_hab))    # p-value > 0.05, the residuals ARE normally distribued

plot(p_hab)   # looks like there is heteroskedasticity too (based on residuals vs fitted)
              # looks like there could be some outliers in the data 
bptest(p_hab)   # p-value < 0.05, there IS heteroskedasticity in the model

# taking into account unequal variances
oneway.test(P_Leaf ~ Habitat, data = chem_na, var.equal=FALSE)

# another way to try and produce homoskedasticity (log transform)
chem_na <- chem_na %>% 
              mutate(logP_Leaf = log(P_Leaf))

logp_hab <- lm(logP_Leaf ~ Habitat, data = chem_na) 
plot(logp_hab)   # definitely looks better, maybe still heteroskedasticity
bptest(logp_hab)   # p-value > 0.05, there is no longer heteroskedasticity

logp_hab_anova <- anova(logp_hab)
logp_hab_anova


## EXERCISE 3: MULTIPLE EXPLANATORY VARIABLES ----
# plotting leaf [P] vs leaf [C], with habitat type indicated
(chem_plot <- ggplot(chem_na, aes(x = C_Leaf, y = P_Leaf, shape = Habitat, color = Habitat)) +
                geom_point(size = 2.5) +
                stat_smooth(method = lm, se = FALSE) +
                xlab("Leaf [C]") +
                ylab("Leaf [P]") +             
                theme_classic() +
                theme(axis.title.x = element_text(margin = margin(t = 10)),
                      axis.title.y = element_text(margin = margin(r = 10))) +
                scale_color_npg(labels = c("Floodplain", "Generalist", "Upland")) +
                scale_shape_discrete(labels = c("Floodplain", "Generalist", "Upland")))

ggsave(chem_plot, file = "leafC_P_plot.png", width = 6, height = 4, units = c("in"),
       path = "Figures/")

# combining upland and generalist species into a single habitat category = mixed
chem_na <- chem_na %>% 
              mutate(Habitat_new = str_replace_all(Habitat, c("floodplain" = "floodplain",
                                                              "upland" = "mixed",
                                                              "generalist" = "mixed")))

# making a model new habitat groups and leaf [C] as predictors of leaf [P]
p_hab_c <- lm(P_Leaf ~ Habitat_new + C_Leaf, data = chem_na)
p_hab_c_int <- lm(P_Leaf ~ Habitat_new*C_Leaf, data = chem_na)

AIC(p_hab_c, p_hab_c_int)  # p_hab_c_int is just >2 AIC units lower than p_hab_c

# conducting an ANCOVA (since the predictors are 1 categorical and 1 continuous variable)
Anova(p_hab_c_int, type = "III")   # using type 3 error to try and avoid incorrect results
anova(p_hab_c_int)   # checking results if we used type 1 error (the default)


# checking assumptions 
hist(residuals(p_hab_c_int), breaks = 10)   # residuals not normally distributed
shapiro.test(resid(p_hab_c_int))  # p-value is << 0.05, so it is NOT normally distributed

plot(p_hab_c_int)  # seems like the relationship may be non-linear, or heteroskedasticity is present
                   # there are a few outliers that may be causing non-normality 

plot(resid(p_hab_c_int) ~ C_Leaf, data = chem_na)   # looks random, no patterns, maybe an outlier
ggplot(chem_na, aes(x = Habitat_new, y = resid(p_hab_c_int))) +
    geom_boxplot() +
    theme_classic()     # there's an outlier 

bptest(p_hab_c_int)  # p > 0.05, there is no heteroskedasticity

# removing some outliers from the dataset
chem_na_less <- chem_na[-c(28, 10), ]
p_hab_c_int2 <- lm(P_Leaf ~ Habitat_new*C_Leaf, data = chem_na_less)

hist(residuals(p_hab_c_int2), breaks = 10)  # doesn't exactly look normally distributed
shapiro.test(resid(p_hab_c_int2))  # it IS normally distributed 
plot(p_hab_c_int2)
bptest(p_hab_c_int2)  # still no heteroskedasticity

# log transforming P to remove non-linearity and leftover non-normality of residuals
p_hab_c_int3 <- lm(logP_Leaf ~ Habitat_new*C_Leaf, data = chem_na_less)

hist(residuals(p_hab_c_int3), breaks = 10)  # looks normally distributed now
shapiro.test(resid(p_hab_c_int3))  # it IS normally distributed 
plot(p_hab_c_int3)
bptest(p_hab_c_int3)   # still no heteroskedasticity


# running a new ANCOVA test, using non-log transformed model
Anova(p_hab_c_int2, type = "III") 
anova(p_hab_c_int2)  # default of type 1 error 



## EXERCISE 4: GENERALIZED LINEAR MODELS ----
# removing NA values associated with trichome density and mevalonic acid (for GLM 1)
density_na <- ingatraits %>% 
                  drop_na(c(Trichome_Density, Mevalonic_Acid))

# removing NA values associated with expansion rate and mevalonic acid (for GLM 2)
expansion_na <- ingatraits %>% 
                  drop_na(c(Expansion, Mevalonic_Acid))

# making a GLM for trichome density and mevalonic acid
glm_den <- glm(Mevalonic_Acid ~ Trichome_Density, data = density_na, family = binomial)
glm_den_null <- glm(Mevalonic_Acid ~ 1, data = density_na, family = binomial)

AIC(glm_den, glm_den_null)  # trichome density explains more than the null model
summary(glm_den)   # trichome density does not significantly affect mevalonic acid presence
30.041/26  # >1 = overdispersion in the model

ggplot(density_na, aes(x = Trichome_Density, y = Mevalonic_Acid)) +
    geom_point() +
    theme_classic()
plot(glm_den)   

# making a GLM for expansion rate and mevalonic acid 
glm_exp <- glm(Mevalonic_Acid ~ Expansion, data = expansion_na, family = binomial)
glm_exp_null <- glm(Mevalonic_Acid ~ 1, data = expansion_na, family = binomial)

AIC(glm_exp, glm_exp_null)   # expansion rate explains more than the null model
summary(glm_exp)   # expansion rate DOES significantly affect mevalonic acid presence

ggplot(expansion_na, aes(x = Expansion, y = Mevalonic_Acid)) +
    geom_point() +
    theme_classic()
plot(glm_exp)


# combining expansion rate and trichome density into one model 
combo_na <- ingatraits %>% 
                drop_na(c(Trichome_Density, Expansion, Mevalonic_Acid))

glm_combo <- glm(Mevalonic_Acid ~ Expansion + Trichome_Density, data = combo_na, family = binomial)
glm_combo_int <- glm(Mevalonic_Acid ~ Expansion*Trichome_Density, data = combo_na, family = binomial)
glm_combo_null <- glm(Mevalonic_Acid ~ 1, data = combo_na, family = binomial)

AIC(glm_combo, glm_combo_int, glm_combo_null)  # both better than null model, no interaction is best
summary(glm_combo)

# checking for overdispersion (residual deviance/residual DF)
22.865/23    # is just <1 = no overdispersion 

# creating new univariate models using the combo data to see if the results are similar
glm_exp2 <- glm(Mevalonic_Acid ~ Expansion, data = combo_na, family = binomial)
summary(glm_exp)
summary(glm_exp2)  # they produce the exact same results = good!

glm_den2 <- glm(Mevalonic_Acid ~ Trichome_Density, data = combo_na, family = binomial)
summary(glm_den)
summary(glm_den2)  # don't produce the exact same results, but still non-significant and overdispersed

# comparing models (univariate vs. multivariate)
AIC(glm_exp2, glm_den2, glm_combo)  # the combination of variables is the best option


# visualizing the GLM
glmvis_both <- ggPredict(glm_combo, point = TRUE, jitter = TRUE)
(glm_plot <- glmvis_both +
                theme_classic())    

ggsave(glm_plot, file = "mevalonic_expansion.png", width = 4, height = 4, units = c("in"),
       path = "Figures/")
