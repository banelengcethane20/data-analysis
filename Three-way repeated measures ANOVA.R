library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(rmarkdown)

##### data loading
data("weightloss")

# data manipulation
#ide format
set.seed(123)
data("selfesteem2",package="datarium")
selfesteem2 %>% sample_n_by(treatment, size=1)

set.seed(123)
data("weightloss",package="datarium")
weightloss%>% sample_n_by(diet, exercises, size=1)

# long format
#gather the columns t1, t2, t3 into long format
#convert id and time into factor variables
selfesteem2=selfesteem2%>%
  gather(key="time", value="score",t1,t2,t3) %>%
  convert_as_factor(id, time)

#inspect some random rows of the data by groups
set.seed(123)
weightloss %>% sample_n_by(diet, exercises, size=1)

weightloss=weightloss%>%
  gather(key="time", value="score",t1,t2,t3) %>%
  convert_as_factor(id, time)

#inspect some random rows of the data by groups
set.seed(123)
weightloss %>% sample_n_by(diet, exercises, size=1)

#summary
weightloss %>%
  group_by(diet,exercises,time) %>%
  get_summary_stats(score, type="mean_sd")

#visualization
bxp=ggboxplot(
  weightloss, x="exercises", y="score",
  facet.by = "diet", short.panel.labs = FALSE
)
bxp

#Checking assumptions
#outliers
weightloss %>%
  group_by(diet,exercises, time) %>%
  identify_outliers(score)

# normality (SHAPITO_WILK)
weightloss %>%
  group_by(diet, exercises, time) %>%
  shapiro_test(score)

#crteate a QQ plot for each design
ggqqplot(weightloss,"score",ggtheme = theme_bw())+
  facet_grid(diet + exercises ~ time, labeller = "label_both")

#computation
#three-way repeated measure anova
res.aov=anova_test(
  data=weightloss, dv=score, wid= id,
  within = c(diet,exercises, time)
  )
get_anova_table(res.aov)


########## POST-HOC TEST #############
#If there is a significant three-way interaction effect, you can decompose it into:
#•Simple two-way interaction: run two-way interaction at each level of third variable,
#•Simple simple main effect: run one-way model at each level of second variable, and
#•simple simple pairwise comparisons: run pairwise or other post-hoc comparisons if necessary.

# COMPUTE SIMPLE INTERACTION
#PAIRWISE COMPARISON
pwc = weightloss %>%
  group_by(diet, exercises) %>%
  pairwise_t_test(score~time, paired=TRUE, p.adjust.method = "bonferroni") %>%
  select(-df,-statistic) #remove details
#show comapriosn results for "diet:no,exercises:yes" group
pwc %>% filter(diet=="no", exercises == "yes") %>%
  select(-p) #remove p columns

# pairwise paired t-test comaprisons
#visualization: box plots with p -values
pwc = pwc %>% add_xy_position(x="exercises")
pwc
pwc.filtered = pwc %>%
  filter(diet =="no", exercises=="yes")
bxp +
  stat_pvalue_manual(pwc.filtered,tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov,detailed=TRUE),
    caption = get_pwc_label(pwc)
  )


















