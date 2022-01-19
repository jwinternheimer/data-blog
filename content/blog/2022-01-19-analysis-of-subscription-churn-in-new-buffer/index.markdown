---
title: Analysis of Subscription Churn in New Buffer
author: Julian Winternheimer
date: '2022-01-19'
slug: []
categories: []
tags:
  - churn
  - new buffer
---

In this analysis we will analyze the subscription churn rates of New Buffer subscriptions. We'll use a well known technique called [survival analysis](https://en.wikipedia.org/wiki/Survival_analysis) that's useful when dealing with _censored_ data. 

Censoring occurs when we have some information about the time it takes for the key event to occur, but we do not know the survival time exactly. In our case, it occurs when subscriptions have not yet canceled -- we know that the time to churn is _at least_ X days. 

We'll use survival analysis to estimate the probability of a New Buffer subscription surviving a given number of days and compare it to the probability of a Publish subscription surviving that long.


## Summary of Findings
So far, New Buffer subscriptions appear to be churning at lower rates than Publish subscriptions created since the beginning of 2020. This difference is statistically significant. 

Because New Buffer subscriptions haven't yet dropped to 50% survival -- there have only been approximately 263 churn events from 3103 New Buffer subscriptions -- the differences in median survival time can't yet be calculated. We'll have to circle back to that in a couple months.


## Data Collection
The data we'll use in this analysis consists of around 58 thousand subscriptions started since the beginning of 2020. Of these around 3 thousand are New Buffer subscriptions.

There are a couple important things to note. We're only including Stripe subscriptions in this analysis, so the subscription churn rates of mobile subscriptions are not included. 

I've also come across several subscriptions created by members of the Buffer team, presumably when testing. These subscriptions appear to have churned very quickly, so it's possible that they may influence churn rates.

I also want to mention that the _current_ plan ID is used to determine whether a subscription is a New Buffer subscription. If an old subscription that started on an Awesome plan was upgraded to a New Buffer plan at a certain point, the subscription is considered to be New Buffer and the length of the subscription, including the time on an Awesome plan, is used in the analysis.






## Data Tidying
There are a few things we need to do to the data before it's ready for analysis. We'll want to exclude subscriptions that are trialing or in a past-due state. We'll also set a new variable, `surv_status`, that indicates whether a subscription has been canceled. 

We'll also calculate the number of days that it took the subscription to cancel. If the data is censored, i.e. the subscription is still active, we calculate the number of days that have elapsed since the subscription was started.


```r
# gather data
subs <- readRDS("ob_sub_retention.rds")

# determine if sub is canceled
subs <- subs %>% 
  mutate(canceled = !is.na(end_date),
         surv_status = ifelse(canceled, 1, 0)) %>% 
  mutate(time = ifelse(canceled, as.numeric(end_date - start_date),
                       Sys.Date() - start_date)) %>% 
  filter(time >= 0)
```

Below is a glimpse of the survival data. The numbers indicates the number of days to either a cancellation event or censoring. The data that is censored has a "+" after the number, indicating that the time to churn is _at least_ X days.


```r
# build survival object
km <- Surv(subs$time, subs$canceled)

# preview data
head(km, 10)
```

```
##  [1] 235+ 282   99   65  318  228+ 100  596+ 236+  52
```

Next we want to determine if the subscriptions are New Buffer subscriptions. We'll look at the plan IDs to do this.


```r
# new buffer plans
nb_plans <- c("buffer_essentials_m_5_202104", "buffer_essentials-team_m_10_202104",
             "buffer_essentials_y_48_202104", "buffer_essentials-team_y_96_202104",
             "buffer_essentials_y_60_202106", "buffer_essentials_m_6_202106",
             "buffer_essentials-team_y_120_202106", "ob_team_monthly_2020_12",
             "buffer_essentials-team_m_12_202106", "ob_individual_monthly_2021_03",
             "ob_individual_yearly_2021_03", "ob_team_yearly_2020_12")

# monthly ob plans
monthly_ob_plans <- c("buffer_essentials_m_5_202104",
                      "buffer_essentials-team_m_10_202104",
                      "buffer_essentials_m_6_202106",
                      "buffer_essentials-team_m_12_202106")

# indicator of one buffer
subs <- subs %>% 
  mutate(is_nb = plan_id %in% nb_plans)

# filter nb subs
nb_subs <- filter(subs, is_nb)
```

## Survival Analysis
Now we're ready for the survival analysis. We'll estimate the survival curve for the entire dataset with the Kaplan-Meier method. This curve shows the estimated probability of surviving X days for all of the subscriptions in the dataset.


```r
# create kaplan-meier survival curve
fit.surv <- survfit(Surv(time, surv_status) ~ 1, data = subs)

# plot survival curve
autoplot(fit.surv, censor = TRUE, censor.size = 1) +
  scale_x_continuous(limits = c(0, 365)) +
  scale_y_continuous(limits = c(0.4, 1), labels = percent) +
  labs(x = "Days", y = "Estimated Probability of Survival",
       title = "Survival Curve of Buffer Stripe Subscriptions",
       subtitle = "All Publish and New Buffer Subscriptiosn Created Since 2020")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

Next we'll stratify the survival curve using the `is_ob` label to compare the survival probabilities of Publish and New Buffer subscriptions.


```r
# create survival curves stratified by whether plans are New Buffer
fit.nb <- survfit(Surv(time, surv_status) ~ is_nb, data = subs)

# plot stratified curves
autoplot(fit.nb, censor = TRUE, censor.size = 1) +
  scale_x_continuous(limits = c(0, 120)) +
  scale_y_continuous(limits = c(0.65, 1), labels = percent) +
  labs(x = "Days After Subscribing",
       y = "Estimated Percent of Subscriptions Remaining",
       title = "Retention Curves of Stripe Subscriptions",
       subtitle = "All Paid Subscriptions Created Since 2020") +
  scale_color_discrete(name = "New Buffer") +
  scale_fill_discrete(name = "New Buffer")
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

We can see that the survival curve of New Buffer subscriptions is higher than that of Publish subscriptions, which suggests that the churn rates are _lower_.

We can print the risk tables to see the number of subscriptions that cancelled at each time.


```r
# print risk table
summary(fit.nb, times = c(0, 7, 14, seq(30, 120, 30)))
```

```
## Call: survfit(formula = Surv(time, surv_status) ~ is_nb, data = subs)
## 
##                 is_nb=FALSE 
##  time n.risk n.event survival  std.err lower 95% CI upper 95% CI
##     0  56942    1475    0.974 0.000666        0.973        0.975
##     7  54009    1597    0.946 0.000947        0.944        0.948
##    14  52931    1062    0.927 0.001088        0.925        0.930
##    30  50337    2588    0.882 0.001354        0.879        0.884
##    60  44827    5232    0.790 0.001711        0.786        0.793
##    90  41043    3723    0.724 0.001877        0.720        0.727
##   120  37875    3029    0.670 0.001975        0.666        0.674
## 
##                 is_nb=TRUE 
##  time n.risk n.event survival std.err lower 95% CI upper 95% CI
##     0  15096     656    0.957 0.00166        0.953        0.960
##     7  13451     546    0.920 0.00222        0.915        0.924
##    14  12580     291    0.899 0.00247        0.894        0.904
##    30  10422    1139    0.812 0.00332        0.806        0.819
##    60   7455     949    0.729 0.00393        0.721        0.737
##    90   4940     484    0.674 0.00438        0.665        0.682
##   120   2770     231    0.633 0.00487        0.624        0.643
```


## Stratification by Subscription Quantity
We can stratify the New Buffer survival curves by the number of channels connected.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

There is a formal way to test if whether the survival distributions are truly different, the [Log-Rank Test](https://en.wikipedia.org/wiki/Logrank_test).


## Log-Rank Test
The log-rank test is a hypothesis test to compare the survival distributions of two samples. It's a nonparametric test and appropriate to use when the data are right skewed and censored. It is widely used in clinical trials to establish the efficacy of a new treatment in comparison with a control treatment when the measurement is the time to event (such as the time from initial treatment to a heart attack).

For us it's a useful way to tell if the survival curve of New Buffer subscriptions is significantly different to that of Publish subscriptions.


```r
# perform log-rank test
logrank.test <- survdiff(Surv(time, surv_status) ~ is_nb, data = subs)
logrank.test
```

```
## Call:
## survdiff(formula = Surv(time, surv_status) ~ is_nb, data = subs)
## 
##                 N Observed Expected (O-E)^2/E (O-E)^2/V
## is_nb=FALSE 56942    36085    36692      10.1       114
## is_nb=TRUE  15096     4482     3875      95.2       114
## 
##  Chisq= 114  on 1 degrees of freedom, p= <2e-16
```

The low p-value of this test (0.0002) tells us that there is a statistically significant difference in the two survival distributions.


## Cox Proportional Hazard Models
Next we'll fit a Cox model to estimate the effect that New Buffer has on subscription churn _given_ other predictors. In this case, we'll look at the effect of New Buffer given the plan interval, i.e. whether it was billed monthly or annually. 

We've seen in another analysis that New Buffer customers are more likely to choose annual plans, which tend to have lower churn rates. This model will help us compare New Buffer monthly plans to monthly Publish subscriptions.


```r
# fit cox model
fit.cox <- coxph(Surv(time, surv_status) ~ is_nb + interval, data = subs)
fit.cox
```

```
## Call:
## coxph(formula = Surv(time, surv_status) ~ is_nb + interval, data = subs)
## 
##                  coef exp(coef) se(coef)      z      p
## is_nbTRUE     0.23653   1.26685  0.01651  14.32 <2e-16
## intervalyear -1.11486   0.32796  0.01307 -85.30 <2e-16
## 
## Likelihood ratio test=9065  on 2 df, p=< 2.2e-16
## n= 71796, number of events= 40325 
##    (242 observations deleted due to missingness)
```

The negative coefficients of the model indicate a positive effect on the survival of the subscriptions. The results indicate that the risk of churn for annual subscriptions is around three times (i.e. e^1.11 = 3) less than that of monthly subscriptions. The risk of churn for New Buffer subscriptions is around 27% (e^0.236) higher than that of Publish subscriptions. 

In other words, after adjusting for the plan interval, New Buffer subscribers are significantly more likely to cancel than Publish subscribers.


## Median Survival Time
We can also use the median survival time to estimate the difference in survival distributions. Median survival time for New Buffer subs is around 354 days, whereas median survival time for Publish subscriptions is around 277 days.


```r
# summarise survival 
survfit(Surv(time, surv_status) ~ is_nb, data = subs)
```

```
## Call: survfit(formula = Surv(time, surv_status) ~ is_nb, data = subs)
## 
##                 n events median 0.95LCL 0.95UCL
## is_nb=FALSE 56942  36085    277     273     284
## is_nb=TRUE  15096   4482    354     299     518
```
