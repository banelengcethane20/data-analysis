library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(kableExtra)
library(ggstatsplot)
library(ggplot2)
library(statsExpressions)

#2 imprt data
excercise1=treatment
head(excercise1,4)
#gather column t1,t2,t3 into long format
#convert Paatient ID and teatment in factor variables
excercise1=excercise1%>%
  gather(key="treattime",value="treatscore",T1,T2,T3)%>%
           convert_as_factor(PatientID, treattime)
head(excercise1,3)
kbl(excercise1)

#4 Summary statistics
excercise1%>%
  group_by(treattime)%>%
get_summary_stats(treatscore,type="full",
                  show=c("n","mean","sd","max","min","se","q1","median","q3"))
#OR
table=excercise1%>%
  group_by(treattime)%>%
  get_summary_stats(treatscore,type="full",
                    show=c("n","mean","sd","max","min","se","q1","median","q3"))
kbl(table)

#5visualization
ggboxplot(excercise1,x="treattime",y="treatscore",add="point")

#6 checking assumptions
# Check sample size assumption
enzymes2 %>%
  group_by(enzyme) %>%
  summarise(N = n())
# univariate outliers
enzymes2 %>%
  group_by(enzyme) %>%
  identify_outliers(enzyme1)

#(a) outliers
table=excercise1%>%
  group_by(treattime)%>%
  identify_outliers(treatscore)

kbl(table)

#(b) normality
excercise1%>%
  group_by(treattime)%>%
  shapiro_test(treatscore)
#(c) qqplot
ggqqplot(excercise1,"treatscore", facet.by="treattime")

#7 Anova assumptions
excercise1.anova=anova_test(data=excercise1,dv="treatscore",wid="PatientID",within="treattime")
get_anova_table(excercise1.anova)

#8 
ggwithinstats(excercise1,treatscore,treattime)

#9 posrt-hoc test
pwc=excercise1%>%
  pairwise_t_test(
    treatscore~treattime,paired=TRUE,
    p.adjust.method = "bonferroni")
pwc

# 10 visualization with box plot and p values
PWC = pwc%>%add_xy_position(x="treattime")
bxp+
  stat_pvalue_manual(pwc)+
  labs(
    substitute=get_test_label(excercise1.anova, detailed = TRUE),
    caption = get_pwc_label(PWC))

# ns means not significant
# ges = generalize effect size