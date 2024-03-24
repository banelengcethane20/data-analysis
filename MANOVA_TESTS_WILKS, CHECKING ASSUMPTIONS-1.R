library(tidyverse)
library(ggpubr)
library(dplyr)
library(rstatix)
library(car)
library(broom)
library(datarium)
library(GGally)


#(1) data visualization
data("iris")
iris

#(2) data preparation rearrange against the group( long Format)
iris2 <- iris %>%
  select(Sepal.Length, Petal.Length,
         Species) %>%
  add_column(id = 1:nrow(iris), .before =
               1)
#
iris3 <- iris %>%
  select(Sepal.Width, Petal.Width,
         Species) %>%
  add_column(id = 1:nrow(iris), .before =
               1)

# visualization
ggboxplot(
  iris2, x = "Species", y =
    c("Sepal.Length", "Petal.Length"),
  merge = TRUE, palette = "jco"
)

# summary statistics
iris2 %>%
  group_by(Species) %>%
  get_summary_stats(Sepal.Length, Petal.Length, type =
                      "mean_sd")

#Check sample size assumption
iris2 %>%
  group_by(Species) %>%
  summarise(N = n())

#Identify univariate outliers
iris2 %>%
  group_by(Species) %>%
  identify_outliers(Sepal.Length)
##Identify Multivariate outliers
iris2 %>%
  group_by(Species) %>%
  identify_outliers(Petal.Length)
#there are not univariate extreme outliers in the sepal lenth and petal lenth as assess by the plot

# why do we have extreme outliers
#_______________________________
#Data entry errors
#measurement errors
#unusual values

#Detect multivariate outliers
iris2 %>%
  group_by(Species) %>%
  mahalanobis_distance(-id) %>%
  filter(is.outlier == TRUE) %>%
  as.data.frame()

#Check univariate normality assumption
iris2 %>%
  group_by(Species) %>%
  shapiro_test(Sepal.Length, Petal.Length) %>%
  arrange(variable)

#You can also create QQ plot for each group. QQ plot draws the correlation between a given data and the
#normal distribution.

ggqqplot(iris2, "Sepal.Length", facet.by = "Species",
         ylab = "Sepal Length", ggtheme = theme_bw())
#QQ plot of Petal.Length

ggqqplot(iris2, "Petal.Length", facet.by = "Species",
         ylab = "Petal Length", ggtheme = theme_bw())

#Multivariate normality
iris2 %>%
  select(Sepal.Length, Petal.Length) %>%
  mshapiro_test()

#Identify multicollinearity
iris2 %>% cor_test(Sepal.Length, Petal.Length)
# cor is less than 0.9

#Check linearity assumption
library(GGally)
results <- iris2 %>%
  select(Sepal.Length, Petal.Length, Species) %>%
  group_by(Species) %>%
  doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots

#Check the homogeneity of covariances assumption
# same sample size its a balanced design, no worry of homogeneity
# dont use wilk use pillar multivariate statistic
box_m(iris2[, c("Sepal.Length", "Petal.Length")], iris2$Species)

#Computation
model <- lm(cbind(Sepal.Length, Petal.Length) ~ Species, iris2)
Manova(model, test.statistic = "Wilks")

pwc=iris2 %>%
gather(key="variables", value="value",Sepal.Length,Petal.Length)%>%
group_by(variables) %>%
games_howell_test(value~Species)%>%
select(-estimate,-conf.low,-conf.high)
pwc
#they are different bcz the homogeneity was not violated

#do welch one way anova
grouped.data %>% welch_anova_test(value~Species)
# or do Kriskal wilks test
grouped.data %>% kruskal_test(value~Species)
# or use aov()
grouped.data %>%anova_test(value~Species)


# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "Species")
test.label <- create_test_label(
  description = "MANOVA", statistic.text = quote(italic("F")),
  statistic = 71.83, p= "<0.0001", parameter = "4,294",
  type = "expression", detailed = TRUE
)
ggboxplot(
  iris2, x = "Species", y = c("Sepal.Length", "Petal.Length"),
  merge = TRUE, palette = "jco"
) +
  stat_pvalue_manual(
    pwc, hide.ns = TRUE, tip.length = 0,
    step.increase = 0.1, step.group.by = "variables",
    color = "variables"
  ) +
  labs(
    subtitle = test.label,
    caption = get_pwc_label(pwc, type = "expression")
  )

