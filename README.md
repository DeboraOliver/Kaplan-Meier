# Kaplan-Meier

Kaplan-Meier for Congestive Heart Failure Analysis
================
Debora Oliveira

Overview
--------

A Kaplan-Meier analysis allows estimation of survival over time, even when patients drop out or are studied for different lengths of time. Thus, the goal is to estimate a population survival curve from a sample. The curve may be estimated simply by computing the fraction surviving at each time.

## 1. Requirements


To run this project you need the following libraries:

``` r
library(redcapAPI)
library(dplyr)
library(plyr)
library(survival)
library(ggplot2)
library(survminer)
library(knitr)
```

### i) redcapAPI

In this project, we use data collected using redcap to further use redcap API to establish a connection between the two softwares. Despite recapConnection(), this package also has other very useful tools, such as exportRecords() which allows to import to RStudio our patient's data.

``` r
source("token.txt")

rcon <- redcapConnection(url=url, token=token)

rm(token)

##########
#Calling our variables 
##########

vector_id <- c("cadast_same")

trial_time <-  c("t0_arm_1") 


data_base_all <- exportRecords(rcon,  factors = FALSE,
                        fields = c("record_id", paste(vector_id)),
                        events = trial_time, dates = TRUE)
```

Both token and url must be provided by your institution.

### ii) plyr and dplyr

These two libraries can be used mainly to clean and group the data from our source. From package plyr, ddply() is used o group patients by record\_id and same. Although, it seems too repetitive, a patient can have only one same and many entries along the trial. The following code gives an example of how ddply() is implemented:

``` r
group_by_record <- ddply(data_base_all, ~record_id , summarise,
                          cadast_same = max(cadast_same))
```

Before ddply() number of rows and columns::

    ## [1] 636

    ## [1] 18

After ddply() number of rows and columns:

    ## [1] 231

    ## [1] 8

Furthermore, the dplyr package has two important functions mutate\_all() that allows to mutate all the data frame to numeric, including dates, and select() which helps cleaning the extra data that exportRecords() brings with the records, such as DAG.

### iii) survival

The survival package allows us to use both functions Suv() and survfit(). Surv() function contains failure time and censoring information. It also provides the basic survival analysis data structure.

    ## Call: survfit(formula = Surv(group_same_clean$dias_vivo, group_same_clean$censored) ~ 
    ##     1, data = group_same_clean)
    ## 
    ##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
    ##     3    117       1    0.991 0.00851        0.975        1.000
    ##     7    114       1    0.983 0.01209        0.959        1.000
    ##     8    111       2    0.965 0.01717        0.932        0.999
    ##     9    107       1    0.956 0.01923        0.919        0.994
    ##    13     99       2    0.937 0.02319        0.892        0.983
    ##    14     97       1    0.927 0.02488        0.880        0.977
    ##    16     95       1    0.917 0.02647        0.867        0.971
    ##    19     93       1    0.907 0.02796        0.854        0.964
    ##    20     92       1    0.898 0.02934        0.842        0.957
    ##    24     88       1    0.887 0.03073        0.829        0.950
    ##    26     86       2    0.867 0.03330        0.804        0.935
    ##    27     83       1    0.856 0.03450        0.791        0.927
    ##    35     78       1    0.845 0.03576        0.778        0.918
    ##    38     72       2    0.822 0.03843        0.750        0.901
    ##    42     66       2    0.797 0.04110        0.720        0.882
    ##    50     55       1    0.782 0.04283        0.703        0.871
    ##    52     53       1    0.768 0.04450        0.685        0.860
    ##    63     46       1    0.751 0.04655        0.665        0.848
    ##    66     45       1    0.734 0.04842        0.645        0.836
    ##    70     43       1    0.717 0.05021        0.625        0.823
    ##    91     39       1    0.699 0.05218        0.604        0.809
    ##   120     34       1    0.678 0.05455        0.579        0.794
    ##   138     33       1    0.658 0.05663        0.556        0.779
    ##   163     31       1    0.637 0.05865        0.531        0.762
    ##   175     29       1    0.615 0.06059        0.507        0.746
    ##   176     28       1    0.593 0.06228        0.482        0.728
    ##   180     27       1    0.571 0.06372        0.458        0.710
    ##   184     25       1    0.548 0.06513        0.434        0.692

### iv) survminer

The survminer package contain the function ggsurvplot().

``` r
group_same_clean$age <- cut(group_same_clean$cadast_idade_anos, c(20,60,80,100))

icc.fit<- survfit(Surv(group_same_clean$dias_vivo, group_same_clean$censored)~group_same_clean$age, 
                  data= group_same_clean)

ggsurvplot(icc.fit, fun = NULL, color = "strata", palette =  "hue", linetype = 1,
           break.x.by = 30, surv.scale = c("percent"),
           conf.int = FALSE, conf.int.fill = "strata", censor = TRUE, pval = TRUE,
           pval.size = TRUE, pval.coord = c(NULL, NULL), main = "Congestive Heart Failure Survival" , 
           xlab = "Days", ylab = "Survival probability", font.main = c(16, "plain", "black"),
           font.x = c(14, "plain", "black"), font.y = c(14, "plain", "black"),
           font.tickslab = c(12, "plain", "black"), xlim = c(0,248), ylim = c(0.5,1.0),
           legend = c("top"), legend.title = " ",
           legend.labs = NULL, font.legend = c(10, "plain", "black"), risk.table = TRUE,
           risk.table.title = "Number at risk by time", risk.table.col = "strata",
           risk.table.fontsize = 4.5, risk.table.y.text = TRUE, risk.table.y.text.col = TRUE,
           risk.table.height = 0.25, surv.plot.height = 0.75, ggtheme = theme_minimal())
```

![](READ_ME_files/figure-markdown_github/curvacod-1.png)

## 2. Organizing data to fit in Surv()


To plot a Kaplan-Meier curve few information is required. It is the number of days in the trial, a variable indicating whether the patient reached the event of interest (censored) and maybe a variable to stratify the sample (age, gender, comorbidities, etc).

    ##   censored dias_vivo      age
    ## 2        0   38 days (80,100]
    ## 3        0  106 days (80,100]
    ## 4        1   52 days (80,100]
    ## 5        0    5 days (80,100]
    ## 6        0   25 days (80,100]
    ## 7        1   26 days (80,100]
