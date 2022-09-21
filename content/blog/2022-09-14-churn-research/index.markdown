---
title: Churn Research
author: Julian
date: '2022-09-14'
slug: []
categories: []
tags: []
---

In this analysis we'll take a deep dive into churn at Buffer. Because approximately 97% of paying customers subscribed through Stripe, we'll only look at Stripe subscriptions in this analysis. 

It's probably worth doing some analysis on mobile subscriptions as well, but given the relatively low volume I think that focusing only on Stripe is reasonable.


## Summary

 - The median number of days it takes for subscriptions to churn 322
 - The average number of days it takes for subscriptions to churn is 412 
 - More people that sign up for New Buffer subscription tend to churn in the first 30 days, particularly in the first couple of days after subscribing. 
 - After the first couple of months, New Buffer subscriptions tend to churn more slowly than legacy subscriptions.
 - The annual Team plan has a lot of churn in the first couple of days, suggesting that there is some confusion around billing.
 - This is the default plan for trialists, so it could be worth looking into what exactly happens when trials expire.
 - The median number of days it takes New Buffer subscriptions to churn is 386 days, compared to 296 for legacy subscriptions. 
 - There is a clear correlation between the number of channels, or slots, available and the churn rate for New Buffer subscriptions.
 - Subscriptions with only one slot have a median survival time of only 264 days, whereas those with 4 slots have a median survival time of 747 days.
 - There isn't a clear correlation between the age of the account at the time the subscription was started and churn. People that subscribe to paid plans in their first month with Buffer are retained fairly well.
 - The most common time to churn is within the first few days of starting a subscription. After that, the most common time to churn is around day 30. 
 - On average, customers that churned in their first 60 days on the subscription were more likely to experience a channel connection failure, more likely to have failed posts, _much_ more likely to have a payment failure, and more likely to create a support request.
 - Around 30% of subscriptions that have ended in the past few months have an unpaid last invoice and multiple failed payment attempts. These could be considered involuntary.


## Recommendations 

 - Do further research on why people churn on the first day. Determine if people were mistakenly subscribed or weren't aware of how much they'd be charged. 
 - Use Stripe checkout and require a postal code field for US customers to mitigate involuntary churn.
 - Intervene proactively with customers with only a single channel slot or those with a failed payment. 
 - Consider defaulting trials to monthly plans, so that customers might be less confused by the amounts they're billed. 
 
 
## Data Collection
The data we'll use in this analysis includes 90 thousand Stripe subscriptions that were started since January 2020. Analyze subscriptions are excluded from this analysis.

The SQL query below is used to gather the data from our data warehouse.








## Survival Ananlysis
Because this data is censored, meaning some subscribers haven't yet churned at the time of data collection, survival analysis is an appropriate technique to analyze the time it takes for customers to churn.

First, we'll create a new variable, `surv_status` that indicates whether a subscription has churned or is censored:

 - 0 = censored, i.e. the customer has not churned yet
 - 1 = churned
 
We'll also calculate the observed survival time in days. For customers that churned, it will be the number of days between the subscription start and cancellation. For those that haven't, it will be the number of days since the subscription started.
 

```r
# create surv_status variable
subs <- subs %>% 
  mutate(surv_status = ifelse(status == "canceled", 1, 0),
         time = ifelse(status == "canceled", as.numeric(end_date - start_date),
                       as.numeric(Sys.Date() - start_date)))
```


## Creating Survival Curves
The Kaplan-Meier method is the most common way to estimate survival times and probabilities. It is a non-parametric approach that results in a step function, where there is a step down each time a churn event occurs.

The `Surv()` function from the `survival` package creates a survival object for use as the response in a model formula. There will be one entry for each subscription that is the survival time (in days), which is followed by a `+` if the subscription hasn't yet been cancelled. 
Here's what the first 10 observations look like:


```r
Surv(subs$time, subs$surv_status)[1:10]
```

```
##  [1] 103+  21  212  897+ 156+  63+ 519+  65  489+ 396+
```

The `survfit()` function creates survival curves using the Kaplan-Meier method. We'll first generate the overall survival curve for the entire cohort of subscriptions. Later on we'll stratify it to see curves for different segments within this sample.


```r
# create survival curve
s1 <- survfit(Surv(time, surv_status) ~ 1, data = subs)

# plot curve
survfit2(Surv(time, surv_status) ~ 1, data = subs) %>% 
  ggsurvfit() +
  scale_x_continuous(limits = c(0, 365), breaks = seq(0, 365, 30)) +
  labs(x = "Days", y = "Survival Probability") +
  add_confidence_interval() +
  add_risktable()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />


In the plot above, the x-axis represents the number of days since a subscription started and the y-axis shows the proportion of subscriptions still active.

The risk table shows 1) number of subscriptions that haven't churned and are considered at-risk at each 30-day interval, and 2) the number of subscriptions that have churned by each day.



## Estimating Average and Median Survival Time
Another quantity that is useful to calculate is the average, or median, survival time. Survival times are not normally distributed, so the median is usually a more representative summary statistic.

The table below shows us that the median survival time for Stripe subscriptions is 322 days and the average is 412 days.


```
##   records     n.max   n.start    events     rmean se(rmean)    median   0.95LCL 
##   90142.0   90142.0   90142.0   53184.0     412.5       1.4     322.0     315.0 
##   0.95UCL 
##     325.0
```

**_Note: I understand that this may be slightly different from what's reported in [ChartMogul's customer churn report](https://app.chartmogul.com/#/reports/cohorts/customer-churn?filters=data_source~ANY~ds_c14855a4-62ea-11e9-aa54-bbe88fa6cc8c;13324B;primary). After investigating, it looks like ChartMogul excludes subscriptions created by customers that reactivate, i.e. those that have previously had subscriptions. I've reached out to the ChartMogul team and confirmed that this is the case ._**


## New Buffer vs Legacy
Next we'll compare survival times for New Buffer and Legacy customers. To do that, we'll need to determine whether a subscription was a New Buffer subscription. If the `plan_id` includes "essentials", we'll consider it to be New Buffer. Otherwise, it's legacy.

It's worth mentioning that this means that customers that were on legacy plans that switched to New Buffer plans at some point will be considered New Buffer customers, and the subscription duration will include the time that they were on a legacy plan.

The plot below shows the survival curves for New Buffer (blue) and Legacy (red) subscriptions. It suggests that churn rates are slightly higher in the first month for New Buffer subscriptions. However, after around 3 months, churn rates appear to be lower for New Buffer subscriptions.

In the next section we'll look at the effect that annual subscriptions have on these overall survival curves.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />

The median survival time for New Buffer subscriptions is 386 days, compared to 296 for legacy subscriptions. 


```r
# create survival curves
s2 <- survfit(Surv(time, surv_status) ~ new_buffer, data = subs)

# show median survival times
summary(s2)$table
```

```
##                  records n.max n.start events rmean se(rmean) median 0.95LCL
## new_buffer=FALSE   57077 57077   57077  41556   401       1.6    296     294
## new_buffer=TRUE    33065 33065   33065  11628   483       6.5    386     386
##                  0.95UCL
## new_buffer=FALSE     301
## new_buffer=TRUE      386
```


## Effect of Annual Plans
Annual plans have significantly lower churn rates (see green and purple survival curves). The reason for the different slopes in the survival curves for New Buffer and Legacy customers appears to be due to the difference in churn rates for annual plans.

It's worth noting that there's not a ton of volume for annual New Buffer plans, so it's possible that the survival curve might change over time as more annual plans hit the one-year mark.

For both monthly and annual plans, New Buffer seems to have more churn up front. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />


## Channel Quantity
Next we'll show the survival curves of New Buffer subscriptions grouped by subscription quantity, i.e. the number of channels connected.

The plot below shows that there is a clear correlation between the number of channels, or slots, available and the churn rate. 

Subscriptions with only a single slot churned at the highest rates, followed by those with 2 slots, 3, and so on.

The group that has the highest survival probability has 4 channel slots.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

The table below shows the median survival times for New Buffer subscriptions of different quantities. Subscriptions with only one slot have a median survival time of only 264 days, whereas those with 4 slots have a median survival time of 747 days.


```r
# create survival curves
s3 <- survfit(Surv(time, surv_status) ~ quantity_bucket, data = filter(subs, new_buffer))

# show median survival times
summary(s3)$table
```

```
##                          records n.max n.start events rmean se(rmean) median
## quantity_bucket=(0,1]       8166  8166    8166   3480   382        16    264
## quantity_bucket=(1,2]       7212  7212    7212   2641   479        13    386
## quantity_bucket=(2,3]       8128  8128    8128   2697   489        15    386
## quantity_bucket=(3,4]       4876  4876    4876   1370   595        14    747
## quantity_bucket=(4,5]       1807  1807    1807    532   520        25    570
## quantity_bucket=(5,10]      2303  2303    2303    701   530        20    464
## quantity_bucket=(10,Inf]     573   573     573    207   534        33    406
##                          0.95LCL 0.95UCL
## quantity_bucket=(0,1]        246     294
## quantity_bucket=(1,2]        366     386
## quantity_bucket=(2,3]        386     401
## quantity_bucket=(3,4]        447      NA
## quantity_bucket=(4,5]        409     726
## quantity_bucket=(5,10]       387     733
## quantity_bucket=(10,Inf]     386      NA
```


## Age of Account
Next we'll group survival curves by the age of the account at the time the subscription was created.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-13-1.png" width="672" />

The table below shows that there isn't a hugely strong correlation between the age of account and survival time. One interesting thing to note is that people that subscribe during their first month tend to have _longer_ survival times than average.


```r
# create survival curves
s4 <- survfit(Surv(time, surv_status) ~ age_bucket, data = filter(subs, new_buffer))

# show median survival times
summary(s4)$table
```

```
##                      records n.max n.start events rmean se(rmean) median
## age_bucket=(-Inf,0]    18678 18678   18678   6402   488       9.6    386
## age_bucket=(0,1]        2336  2336    2336    798   518      25.0    393
## age_bucket=(1,6]        3158  3158    3158   1103   493      21.3    369
## age_bucket=(6,12]       1873  1873    1873    663   498      25.1    386
## age_bucket=(12,24]      2048  2048    2048    774   447      21.3    352
## age_bucket=(24, Inf]    4972  4972    4972   1888   466      14.4    370
##                      0.95LCL 0.95UCL
## age_bucket=(-Inf,0]      386     386
## age_bucket=(0,1]         355      NA
## age_bucket=(1,6]         355     386
## age_bucket=(6,12]        347     637
## age_bucket=(12,24]       312     396
## age_bucket=(24, Inf]     358     387
```

## Team vs Essentials
The plot below shows us something curious -- churn rates for monthly Essentials and Team plans are very similar, but churn rates for the annual Team plan are significantly higher than those of the annual Essentials plan.

The biggest divergence appears to occur in the first few days of the plan. One hypothesis is that this is another effect of billing confusion. Buffer defaults web signups onto trials, and all trials are Team trials (mostly annual trials). It's possible that some confusion around the amount charged led to the churn happening in the first couple of days.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

The plot below confirms that a large portion of churn of Team subscriptions occurs in the first few days.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-16-1.png" width="672" />

If we group this data by whether or not the subscription was refunded, we can see that many of those that cancelled on day 0 or 1 were refunded.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

Even if we excluded refunded subscriptions, we still see a large amount of churn for the annual team plan occurs in the first few days.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-18-1.png" width="672" />

## When Cancellations Occur
For this part of the analysis, we'll focus on New Buffer subscriptions. The plot below shows the distribution of the number of days to churn for New Buffer subscribers that churned. 

We can see that the most common time to churn is within the first few days of starting a subscription. After that, the most common time to churn is before the 60-day mark.

For those that churned in the first couple of days, my suspicion is that people might not have been aware that they would be charged the amount that they were charged for. Experiments could be run to make sure that pricing and billing are as clear as possible at the point of purchase to mitigate churn that occurs this early.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-19-1.png" width="672" />

If we group the data by whether or not the customer was refunded, we can see that a significant number of customers that cancelled in the first couple of days on a paid plan were refunded. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-20-1.png" width="672" />

The cumulative distribution function below shows us that around 8% of churn cases occur on the day that the subscription was created. 

 - **Around 9% of churn occurs on the first day**.
 - **Over 50% of churn occurs by day 60**.
 
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-21-1.png" width="672" />

## Actions Associated with Churn
We'll check the following events that occurred in the first 30 days on the subscription:

 - Posts created
 - Posts failed
 - Invoice payment failures
 - Channel connection failures
 - Support tickets created



The graph below shows us that, on average, customers that churned in their first 60 days on the subscription were more likely to experience a channel connection failure, more likely to have failed posts, _much_ more likely to have a payment failure, and more likely to create a support request.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-1.png" width="672" />

Now we can look at failed posts by channel. On average, customers that churned in their first 60 days tended to have more failed Instagram, Facebook, and Twitter posts. However, retained users tended to have more failed LinkedIn posts.





<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-26-1.png" width="672" />

Customers that churned had slightly more failed connections for Instagram and Facebook. Interestingly, customers that didn't churn in their first 60 days had more Twitter and LinkedIn connection failures, on average





<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-29-1.png" width="672" />

## Involuntary Churn
In this case we'll consider churn to be involuntary if the last invoice was not paid and there were multiple payment attempts.

The plot below shows that over 30% of subscriptions that have ended in the past few months fit this criteria.





<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-32-1.png" width="672" />

