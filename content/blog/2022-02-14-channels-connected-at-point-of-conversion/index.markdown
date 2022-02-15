---
title: Channels Connected at Point of Conversion
author: Julian Winternheimer
date: '2022-02-14'
slug: []
categories: []
tags: []
---



The data team recently received several questions that we'll try to answer in this analysis:

 - What is the average number of channels connected at the point of conversion vs. 3 months of subscription? How does it differ for people that signed up with and without a trial?
 
 - What effect does an additional channel have on revenue? For example, if the current average is 1.2 channels and we increased that to 1.3, what impact does that have on revenue?
 
To answer the first question, we'll first collect data from all users that have signed up since January 2020 and subscribed to a paid plan. We'll count the unique `service_id` values that they connected before paying their first non-zero invoice and subtract the `service_id` values that had been disconnected by then. 

Because there have been multiple anomalous events due to spam activity, we'll look at medians in addition to averages.

## Data Collection
We'll use the SQL query below to collect the data.


```r
# define sql query
sql <- "
  with first_sub as (
    select
      u.user_id
      , u.signup_at_date as signup_date
      , u.stripe_customer_id as customer_id
      , u.did_signup_on_trial as has_trial
      , min(s.first_paid_invoice_created_at) as converted_at
    from dbt_buffer.buffer_users u
    inner join dbt_buffer.stripe_paid_subscriptions s
      on u.stripe_customer_id = s.customer_id
    where u.signup_at >= '2020-01-01'
    group by 1,2,3,4
  )
  select
    f.user_id
    , f.signup_date
    , f.customer_id
    , f.has_trial
    , f.converted_at
    , s.id as subscription_id
    , s.plan_id
    , date(s.created_at) as sub_created_at
    , date(s.canceled_at) as sub_canceled_at
    , count(distinct case 
                      when c.timestamp < f.converted_at
                      then c.channel_service_id end) as channels_at_conversion
    , count(distinct case 
                      when d.timestamp < f.converted_at
                      then d.channel_service_id end) as channels_disconnected_at_conversion
    , count(distinct case
                      when c.timestamp < timestamp_add(f.converted_at, interval 90 day)
                      then c.channel_service_id end) as channels_at_3_months
    , count(distinct case
                      when d.timestamp < timestamp_add(f.converted_at, interval 90 day)
                      then d.channel_service_id end) as channels_disconnected_at_3_months
  from first_sub f
  inner join dbt_buffer.stripe_paid_subscriptions s
    on f.customer_id = s.customer_id 
    and s.first_paid_invoice_created_at = f.converted_at
  left join dbt_buffer.segment_channels_connected as c
      on f.user_id = c.user_id
  left join dbt_buffer.segment_channels_disconnected as d
      on f.user_id = d.user_id
  group by 1,2,3,4,5,6,7,8,9
"

# collect data from bigquery
users <- bq_query(sql = sql)

# save data
saveRDS(users, "sub_channels.rds")
```




## Channels Connected at Conversion
First we'll visualize the distribution of number of channels connected at conversion for all users. Then we'll break it down by whether or not the users signed up with a trial.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

This shows us that most customers had 4 or fewer channels connected when they first converted to a paid plan. A shocking number of customers hadn't connected any channels by then. 

Visualizing the cumulative distrubtion function (CDF) is another useful way to visualize the distribution.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

This CDF tells us that around 72% of customers had 3 or fewer channels connected at the time of conversion. Around 91% of customers had 5 or fewer channels connected and 98% had 10 or fewer channels connected.

## Segmenting by Trial
Next we'll plot the same distributions, but segment the data by whether or not the users signed up with a trial. It's worth noting that there was a significant period of time in which _all_ new signups had to start a trial when creating a Buffer account. 

Signups that came through one of the mobile apps generally don't have trials.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

These plots suggest that people that signed up with trials generally had more channels connected when they converted to a paid plan.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

The CDFs support the previous statement. Around 70% of people that signed up with a trial had 3 channels or fewer (30% had more) at the point of conversion. Around 90% of people that signed up without a trial had 3 channels or fewer (only 10% had more).

**The average number of channels connected at conversion is 2.3 for people that signed up without a trial and 2.9 for those that signed up with a trial**.

**The median number of channels connected at conversion is 2 for people that signed up without a trial and 3 for those that signed up with a trial**.

**The overall average is 2.8 and the median number of channels connected is 3**.


```r
# calculate avg and median
users %>% 
  mutate(channels = channels_at_conversion - channels_disconnected_at_conversion) %>% 
  group_by(has_trial) %>% 
  summarise(avg_channels = mean(channels),
            med_channels = median(channels))
```

```
## # A tibble: 2 × 3
##   has_trial avg_channels med_channels
##   <lgl>            <dbl>        <dbl>
## 1 FALSE             2.30            2
## 2 TRUE              2.95            3
```

## Channels Connect at Day 90 on Subscription
We'll take the same approach to answer this question, but first we'll want to filter out customers that weren't subscribed for 90 days.


```r
# filter out subs that weren't active for 90 days
subs90 <- users %>% 
  filter((sub_created_at < "2021-11-16" & is.na(sub_canceled_at)) |
           as.numeric(sub_canceled_at - sub_created_at) >= 90)
```

Now let's look at the distributions for those customers that started a trial when they signed up and those that didn't.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

These plots show us that most customers still had 4 or fewer channels connected after 90 days on a subscription.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

The CDFs show us again that users that signed up with trials tended to have more channels connected after 90 days on a subscription.

 - Approximately 59% of users that signed up with trials had 3 or fewer channels connected after 90 days (41% had more).
 
 - Approximately 71% of users that signed up without trials had 3 or fewer channels connected after 90 days (29% had more).
 

**The average number of channels connected after 90 days on a paid subscription is 2.9 for people that signed up without a trial and 3.5 for those that signed up with a trial**.

**The median number of channels connected at day 90 is 3 for people that signed up with or without a trial**.

**The overall average number of channels connected after 90 days is 3.4 and the median is 3**.


```r
# calculate avg and median
users %>% 
  mutate(channels = channels_at_3_months - 
           channels_disconnected_at_3_months) %>% 
  group_by(has_trial) %>% 
  summarise(avg_channels = mean(channels),
            med_channels = median(channels))
```

```
## # A tibble: 2 × 3
##   has_trial avg_channels med_channels
##   <lgl>            <dbl>        <dbl>
## 1 FALSE             2.91            3
## 2 TRUE              3.52            3
```


## Revenue Impact
This is a trickier question to answer, since only a portion of Buffer's subscribers have plans that increase in value directly and proportionally with the number of channels connected.

Let's start with the simplest case, assuming that an increase in channels causes an increase in revenue. We'll gather all active _Stripe_ subscriptions and calculate the average and median number of channels connected per customer.


```r
# define sql query
sql <- "
  select
    u.user_id
    , u.stripe_customer_id
    , s.id as subscription_id
    , s.plan_id
    , s.quantity
    , sp.amount as plan_amount
    , sp.interval as plan_interval
    , count(distinct p.id) as channels_connected
  from dbt_buffer.buffer_users u
  left join dbt_buffer.stripe_paid_subscriptions s
    on u.stripe_customer_id = s.customer_id
    and s.status in ('active', 'past_due')
  left join dbt_buffer.stripe_plans sp
    on sp.id = s.plan_id
  left join dbt_buffer.publish_profiles p
    on u.user_id = p.account_id
    and p.is_deleted is not true
    and p.is_disabled is not true
  where u.is_currently_paying_buffer_user
  group by 1,2,3,4,5,6,7
"

# collect data from bigquery
customers <- bq_query(sql = sql)

# save data
saveRDS(customers, "customer_channels.rds")
```




```r
# calculate average and median
customers %>% 
  summarise(avg_channels = mean(channels_connected),
            med_channels = median(channels_connected))
```

```
## # A tibble: 1 × 2
##   avg_channels med_channels
##          <dbl>        <int>
## 1         5.29            4
```

The average number of channels connected for all Buffer customers is 5.3. As of February 14, 2022, MRR is \$1,631,121. The total number of channels connected by all customers is 306,009. The average MRR value per channel connected is therefore \$5.33. 

In this over-simplified world, increasing the average number of channels connected by 1 for all Buffer customers would result in \$5.33 x 57,836 customers = \$308k. 

That isn't the real world though. Increasing the number of channels connected does not necessarily equate to more revenue for customers on legacy plans. Increasing the average by 1 would require a huge number of customers to add (and pay for) more channels than they otherwise would have.

Another way to look at this would be to consider only MRR from New Buffer subscriptions, which increase in value with each new channel connected.


```r
# new buffer plans
nb_plans <- c("buffer_essentials_m_5_202104", "buffer_essentials-team_m_10_202104",
             "buffer_essentials_y_48_202104", "buffer_essentials-team_y_96_202104",
             "buffer_essentials_y_60_202106", "buffer_essentials_m_6_202106",
             "buffer_essentials-team_y_120_202106", "ob_team_monthly_2020_12",
             "buffer_essentials-team_m_12_202106", "ob_individual_monthly_2021_03",
             "ob_individual_yearly_2021_03", "ob_team_yearly_2020_12")


# indicator of new buffer
customers <- customers %>% 
  mutate(is_nb = plan_id %in% nb_plans,
         plan_amount = plan_amount / 100,
         mrr_amount = case_when(
           plan_interval == "year" ~ plan_amount * quantity / 12,
           plan_interval == "month" ~ plan_amount * quantity,
           TRUE ~ plan_amount * quantity
         ))
```

Below we calculate the average number of channels New Buffer customers have connected.


```r
# calculate average and median
customers %>% 
  filter(is_nb) %>%
  summarise(avg_channels = mean(quantity),
            med_channels = median(quantity),
            mrr = sum(mrr_amount),
            total_channels = sum(quantity),
            customers = n_distinct(stripe_customer_id))
```

```
## # A tibble: 1 × 5
##   avg_channels med_channels    mrr total_channels customers
##          <dbl>        <int>  <dbl>          <int>     <int>
## 1         3.16            3 308706          41019     12983
```

The average number of channels connected for New Buffer customers is 3.2 and the rough MRR value per channel is 41,019. The MRR value per channel is around \$7.53. 

Increasing the average number of channels connected by 0.1 for New Buffer customers would result in approximately \$7.53 * 0.1 * 12,983 customers = \$978 in MRR _with the current number of New Buffer customers_. 




