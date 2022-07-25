---
title: Buffer's Free Users
author: Julian
date: '2022-06-20'
slug: []
categories: []
tags: []
---

The four main goals of this analysis are:

1. Gaining a better understanding of Buffer's active free users
2. Identifying opportunities for converting free users to paid
3. Mapping our free user onboarding actions
4. Definning cohorts within our free user base to inform user qualification

There are a lot of sections below, so settle in with a cup of tea and enjoy. Thank you in advance for reading this and taking the time to learn about Buffer's free users!


## Summary
Most of Buffer's active users (around 76%) are on free plans. However, there are a couple distinct populations within the free user base: one is a group of new users that signed up very recently and took a key action, and the other is a group of users that have been active in Buffer for a long time. Around 50% of Buffer's active free users signed up over one year ago. 

Unsurprisingly, the US contributes the most to Buffer's free user base. **_Over 75% of conversions from free users come from people based in the US, UK, Canada, or Australia_**. 

Only around 7% of free signups go on to start a trial after signing up and 1.38% end up upgrading to a paid plan. However, of those 1.38%, around 53% actually started a trial in the 30 days prior to starting a subscription! Buffer has upgrade paths that lead to people starting trials, so this makes sense.

The Add Channel, Top Nav Bar, and Queue Limit upgrade paths contribute the most paid conversions from the free user base. The Add Channel button contributes over 36% of all attributed free user upgrades! The top nav bar contributes around 25%, and the queue limit upgrade path contributes around 13%.

Usage patterns vary. Active free users take around 5-7 key actions per week and paying MAU take about twice as many. This is logical, as paying customers are likely to have more channels connected and more key actions to take.

Paying customers are more likely to create posts in general, but free users are more likely to post to Twitter and less likely to post to Instagram. As many free active users post to LinkedIn as they do to Instagram.

Relatively few free users hit the channel and queue limits. In the past 30 days, around 2.2% of active free users viewed the channel limit upgrade path and 0.3% viewed the queue limit upgrade path. It seems that many free users are operating within Buffer's free plan limits. 

The main channels that Buffer's free users connect are Facebook, Twitter, Instagram, and LinkedIn. StartPage, Pinterest, TikTok, Shopify, and GoogleMyBusiness all lag behind the top four channels.

Free users do convert to paid plans -- between 0.7% and 0.9% of active upgrade to paid plans _within 30 days of being active_. That number only takes Stripe subscriptions into account, so the actual percentage may be slightly higher.

It's most common for people to become inactive 30 days after they first become active, which suggests that they were only active for a single day. The distribution, unsurprisingly, is markedly different for users that had signed up recently and for those that hadn't. There is a very long tail of free users that were active for a while before becoming inactive.

Gmail is the most common email domain by far, but there are others that have higher conversion rates. People that sign up with protonmail.com, me.com, live.com, and icloud.com email addresses all have significantly higher probabilities of converting than people that sign up with gmail domains. Still, we should explore single sign on with Google since so many users sign up with a gmail address.

## Recommendations

 - Because Buffer's free users get so much value out of LinkedIn and Twitter, I'd suggest giving those networks due attention and trying to achieve as much parity as possible with the native posting experience. Twitter threads and LinkedIn tagging are two projects that I think are worthwhile investments for our free users.
 
 - Very few free users hit the queue limit. That may a good thing, but we might consider testing another option: a scheduled post limit. Something like 100 scheduled posts per month could be worth trying. 
 
 - Optimize the "Add Channel" upgrade path. Replace the "Manage Channels" button in the dashboard with an "Add Channel" button that directs people directly to the channel connection page. Add a (+) icon in the composer, calendar, and below the channels listed in the queue.
 
 - Give free users basic analytics, like weekly aggregations. Give them a taste of what they could get with Buffer's paid offerings. 
 
 - Build predictive model that identifies free signups that are most likely to convert and design communications to get them there more easily. Consider offering discounts if they do not convert after a certain time period.




## Breakdown of Buffer's Monthly Active Users
The first thing we want to do is see how many of Buffer's monthly active users are on free plans.

As of June 2022, there are around 105k monthly active users on a free plan and 32k on a paid subscription. Around 76% of monthly active users are on a free plan.


```r
# define sql query
sql <- "select * from dbt_buffer.buffer_metrics_mau_movements"

# collect data from bigquery
mau <- bq_query(sql = sql)

# save data
saveRDS(mau, "buffer_mau.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />


## Account Age of Active Free Users
Next we'll look at the distribution of account age for Buffer's free active users. To do this, we'll need to calculate the number of months that have elapsed between their signup dates and the current date.


```r
# define sql query
sql <- "
  select distinct
    u.user_id
    , u.signup_at_date as signup_date
  from dbt_buffer.buffer_users as u
  inner join dbt_buffer.buffer_key_actions as a
    on u.user_id = a.user_id
    and a.timestamp >= timestamp_sub(current_timestamp(), interval 30 day)
  where u.is_currently_paying_buffer_user is not true 
"

# collect data from bigquery
free_mau <- bq_query(sql = sql)

# save data
saveRDS(free_mau, "buffer_free_mau.rds")
```




```r
# calculate months elapsed
free_mau <- free_mau %>% 
  mutate(months_passed = (interval(ymd(signup_date), 
             ymd(Sys.Date()))) %/% months(1))
```

Now we can plot the distribution of account age for Buffer's free monthly active users. The plot shows that a large portion of Buffer's free MAUs signed up less than one month ago.  

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-9-1.png" width="672" />

The plot below shows the months grouped into buckets. As a reminder, the "(" parenthesis preceding a number is _exclusive_ -- the number to the right is not included. The "]" bracket is _inclusive_, meaning that the number is included.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-10-1.png" width="672" />

We can see that Buffer's monthly active users are largely made up of new users and people that have been using Buffer for years! 

If we consider people to be "new to Buffer" if they signed up less than 6 months ago, approximately 36% (37.7k) are new and 64% (67.5k) are not.


```r
# new to buffer proportion
free_mau %>% 
  group_by(new = signup_date >= "2021-07-22") %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users))
```

```
## # A tibble: 2 × 3
##   new   users  prop
##   <lgl> <int> <dbl>
## 1 FALSE 54173 0.514
## 2 TRUE  51232 0.486
```

## Geographic Region
Next we'll look at the geographic regions of our MAUs. Over 50% of free MAUs are from the US, UK, Canada, or France. Around 31% are from the US. 


```r
# define sql query
sql <- "
  select distinct
    u.user_id
    , p.country_code
  from dbt_buffer.buffer_users as u
  left join fivetran_mixpanel.people as p
    on p.distinct_id = u.user_id
  inner join dbt_buffer.buffer_key_actions as a
    on u.user_id = a.user_id
    and a.timestamp >= timestamp_sub(current_timestamp(), interval 30 day)
  where u.is_currently_paying_buffer_user is not true 
"

# collect data from bigquery
countries <- bq_query(sql = sql)

# save data
saveRDS(countries, "mau_countries.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />

Next we'll look at the geographic breakdown of free _signups_ that ended up paying for a paid subscription.


```r
# define sql query
sql <- "
  select
    u.user_id
    , p.country_code
    , count(distinct t.id) as trials
  from dbt_buffer.buffer_users as u
  left join fivetran_mixpanel.people as p
    on p.distinct_id = u.user_id
  inner join dbt_buffer.stripe_paid_subscriptions as s
    on s.account_id = u.user_id
    and s.status = 'active'
  left join dbt_buffer.stripe_trials as t
    on t.customer_id = s.customer_id
    and t.trial_started_at between 
      timestamp_sub(s.first_paid_invoice_created_at, interval 30 day) and 
      s.first_paid_invoice_created_at
  where u.did_signup_on_trial is not true
  group by 1,2
"

# collect data from bigquery
paid <- bq_query(sql = sql)

# save data
saveRDS(paid, "free_paid.rds")
```



Over 50% of converted free users -- defined as not having signed up with a trial -- are from the US. A further 14% or so are from the UK, and around 6% are from Canada. **Over 75% of conversions from free users come from people based in the US, UK, Canada, or Australia**. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

Interestingly, around 19% of conversions from free users, i.e. those that do not sign up with a trial, did have a trial in the 30 days prior to subscribing to the paid plan.


```r
paid %>% 
  group_by(had_trial = trials >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = scales::percent(users / sum(users)))
```

```
## # A tibble: 2 × 3
##   had_trial users prop 
##   <lgl>     <int> <chr>
## 1 FALSE     26805 81%  
## 2 TRUE       6308 19%
```


## Free Usage Patterns
Next we'll look at how usage patterns differ for different user Segments. We'll look at new Free MAU, old Free MAU, and Paying MAU.


```r
# read data from Mixpanel: https://mixpanel.com/s/4zBctB
freq <- read_csv("~/Desktop/mau_frequency.csv")

# update column names
names(freq) <- c("date", "event", "cohort", "paid", "med_key_actions")

# remove column
freq$event <- NULL

# filter
freq <- freq %>% filter(cohort != "All User Profiles")

# save data
saveRDS(freq, "mau_key_action_frequency.rds")
```




The plot below shows us that, in general, free MAU take around 5-7 key actions per week and paying MAU take about twice as many. This makes sense given that paying customers are more likely to have more channels connected.

New MAU also take slightly fewer key actions in a given week than old MAU (those that signed up over 90 days ago). 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-21-1.png" width="672" />

Next we'll look at how likely free MAU are to post to different social networks. Unsurprisingly, paying customers are more likely to create posts in general. Free MAU are more likely to post to Twitter and less likely to post to Instagram.


```r
# read data from Mixpanel: https://mixpanel.com/s/2OwcWZ
channels <- read_csv("~/Desktop/mau_channels.csv")

# update column names
names(channels) <- c("week", "event", "paid", "channel", "users")

# remove column
channels$event <- NULL

# save data
saveRDS(channels, "mau_channels.csv")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-24-1.png" width="672" />


## Signup Referrers
Next we'll compare the signup referrers for both free and paid MAU. We'll only do this for MAU that signed up since April 2022, since that's when we were able to create our new attribution model.


```r
# define sql query
sql <- "
  select distinct
    u.user_id
    , u.is_currently_paying_buffer_user as paid
    , s.referrer_domain_grouped as referrer
  from dbt_buffer.buffer_users as u
  inner join dbt_buffer.buffer_key_actions as a
    on u.user_id = a.user_id
    and a.timestamp >= timestamp_sub(current_timestamp(), interval 30 day)
  inner join dbt_buffer.segment_sessions as s
    on s.dbt_visitor_id = u.user_id
    and s.signup_session
  where u.signup_at >= '2022-04-01'
"

# collect data from bigquery
mau_referrers <- bq_query(sql = sql)

# save data
saveRDS(mau_referrers, "mau_referrers.rds")
```




<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-27-1.png" width="672" />

Free MAU are much more likely to have signed up on a mobile app or been invited to a team. They're also more likely to have signed up through social. Paying MAU are more likely to have signed up to Buffer directly, through organic search, or through a paid advertisement.

Let's see what this would look like if we removed team members.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-28-1.png" width="672" />

Free MAU are much more likely to have signed up or through social, but other than that the distributions look fairly similar.


## How Many Free MAU Receive Buffer's Emails?
Approximately 84% of free monthly active users has received one of Buffer's emails since June 1, and approximately 64% have opened an email.


```r
# define sql query
sql <- "
  select distinct
    u.user_id
    , count(distinct ed.id) as emails_delivered
    , count(distinct eo.id) as emails_opened
  from dbt_buffer.buffer_users as u
  inner join dbt_buffer.buffer_metrics_mau_movements_per_user as a
    on u.user_id = a.user_id
    and a.date = '2022-07-14'
    and a.is_active_last_30_days
  left join segment_customerio_webhook.email_delivered as ed
    on ed.user_id = u.user_id
    and ed.timestamp >= '2022-06-01'
  left join segment_customerio_webhook.email_opened as eo
    on eo.user_id = u.user_id
    and eo.timestamp >= '2022-06-01'
  where u.is_currently_paying_buffer_user is not true
  group by 1
"

# collect data from bigquery
free_mau_emails <- bq_query(sql = sql)

# save data
saveRDS(free_mau_emails, "free_mau_emails.rds")
```




```r
# calculate percent
free_mau_emails %>% 
  group_by(has_emails = emails_delivered >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users))
```

```
## # A tibble: 2 × 3
##   has_emails users  prop
##   <lgl>      <int> <dbl>
## 1 FALSE      16670 0.159
## 2 TRUE       88160 0.841
```


```r
# calculate percent
free_mau_emails %>% 
  group_by(opened_email = emails_opened >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users))
```

```
## # A tibble: 2 × 3
##   opened_email users  prop
##   <lgl>        <int> <dbl>
## 1 FALSE        38052 0.363
## 2 TRUE         66778 0.637
```

## How Many Active Free Users Previously Had a Subscription
Here we'll only look at Stripe subscriptions, but it's possible that free users previously had a mobile subscription.

Around 6% of active free users previously had a subscription. However, if we exclude team members, only around 1% of active free users previously had a paid subscription.


```r
# define sql query
sql <- "
  select distinct
    u.user_id
    , u.is_team_member as team_member
    , count(distinct s.id) as previous_subscriptions
  from dbt_buffer.buffer_users as u
  inner join dbt_buffer.buffer_metrics_mau_movements_per_user as a
    on u.user_id = a.user_id
    and a.date = '2022-07-14'
    and a.is_active_last_30_days
  left join dbt_buffer.stripe_paid_subscriptions as s
    on s.customer_id = u.stripe_customer_id
  where u.is_currently_paying_buffer_user is not true
  group by 1,2
"

# collect data from bigquery
free_mau_subs <- bq_query(sql = sql)

# save data
saveRDS(free_mau_subs, "free_mau_subs.rds")
```




```r
# calculate percent with sub
free_mau_subs %>%  
  group_by(has_sub = previous_subscriptions >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users))
```

```
## # A tibble: 2 × 3
##   has_sub users   prop
##   <lgl>   <int>  <dbl>
## 1 FALSE   98393 0.939 
## 2 TRUE     6437 0.0614
```

## Channel Distribution
The plot below shows the distribution of channels connected for active free users that are not on a trial. We can see that most tend to have three channels connected.


```r
# define sql query
sql <- "
  select
    u.user_id
    , ca.organization_id
    , u.is_currently_on_one_buffer as new_buffer
    , u.is_team_member as team_member
    , u.is_currently_trialing as trialing
    , count(distinct c.id) as channels_connected
  from dbt_buffer.buffer_users as u
  inner join dbt_buffer.core_accounts as ca
    on u.user_id = ca.id
  inner join dbt_buffer.buffer_metrics_mau_movements_per_user as a
    on u.user_id = a.user_id
    and a.date = '2022-07-14'
    and a.is_active_last_30_days
  left join dbt_buffer.core_channels as c
      on ca.organization_id = c.organization_id
      and c.is_deleted is not true
      and c.is_locked is not true
  where u.is_currently_paying_buffer_user is not true
  and u.stripe_customer_id is not null
  group by 1,2,3,4,5
"

# collect data from bigquery
free_channels <- bq_query(sql = sql)

# save data
saveRDS(free_channels, "free_channels_dist.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-38-1.png" width="672" />

## How Many Hit Queue and Channel Limits?
Very few (around 0.3%) of free MAU hit the queue limit. Around 2.2% hit the channel limit.


```r
# read data from Mixpanel:https://mixpanel.com/s/1KOXgS
paths <- read_csv("~/Desktop/upgrade_paths.csv")

# update column names
names(paths) <- c("event", "paid", "path", "users")

# remove column
paths$event <- NULL

# save data
saveRDS(paths, "mau_upgrade_paths.rds")
```




```r
# calculate percentages
paths %>% 
  filter(!paid) %>% 
  mutate(prop = percent(users / 107333)) %>% 
  arrange(desc(prop))
```

```
## # A tibble: 10 × 4
##    paid  path                                              users prop  
##    <lgl> <chr>                                             <dbl> <chr> 
##  1 FALSE publish-navBar-trialExpiredModal                   9307 8.671%
##  2 FALSE analyze-paywallModal-paywallModal                  7384 6.880%
##  3 FALSE engage-paywallModal-paywallModal                   4882 4.548%
##  4 FALSE profileSidebar-addChannelButton-upgrade-1          2392 2.229%
##  5 FALSE account-navBar-trialExpiredModal                   1879 1.751%
##  6 FALSE composer-profileQueueLimit-showPaidPlans-1          314 0.293%
##  7 FALSE start_page-navBar-trialExpiredModal                 273 0.254%
##  8 FALSE campaigns-emptyState-upgrade-1                      152 0.142%
##  9 FALSE composer-hashtagManager-upgrade-1                   115 0.107%
## 10 FALSE publish-profileSidebar-addChannelButton-upgrade-1     1 0.001%
```



## Which Channels Are Connected and Used Most Often by Free Users?
The most commonly connected channels connected for Buffer's free users are Facebook, Twitter, Instagram, and LinkedIn. StartPage, Pinterest, TikTok, Shopify, and GoogleMyBusiness all lag behind the top four channels.


```r
# define sql query
sql <- "
  select
    u.user_id
    , ca.organization_id
    , u.is_currently_on_one_buffer as new_buffer
    , u.is_team_member as team_member
    , u.is_currently_trialing as trialing
    , c.service_type as channel
    , count(distinct c.id) as channels_connected
  from dbt_buffer.buffer_users as u
  inner join dbt_buffer.core_accounts as ca
    on u.user_id = ca.id
  inner join dbt_buffer.buffer_metrics_mau_movements_per_user as a
    on u.user_id = a.user_id
    and a.date = '2022-07-18'
    and a.is_active_last_30_days
  left join dbt_buffer.core_channels as c
      on ca.organization_id = c.organization_id
      and c.is_deleted is not true
      and c.is_locked is not true
  where u.is_currently_paying_buffer_user is not true
  and u.stripe_customer_id is not null
  group by 1,2,3,4,5,6
"

# collect data from bigquery
channel_counts <- bq_query(sql = sql)

# save data
saveRDS(channel_counts, "free_channel_counts.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-44-1.png" width="672" />


Interestingly, more active free users tend to post to Facebook, LinkedIn, and Twitter than to Instagram. 


```r
# define sql query
sql <- "
  select
    p.user_id
    , p.channel
    , count(distinct p.id) as posts
  from dbt_buffer.segment_posts_created as p
  inner join dbt_buffer.buffer_users as u
    on u.user_id = p.user_id
    and u.is_currently_paying_buffer_user is not true
  where p.timestamp >= '2022-06-17'
  group by 1,2
"

# collect data from bigquery
post_channels <- bq_query(sql = sql)

# save data
saveRDS(post_channels, "free_post_channels.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-47-1.png" width="672" />



## Free Conversions Rates
Now we'll look at the rate at which free active users convert to paid customers. To do this, we'll calculate the proportion of active free users on a given date that start a paid _Stripe_ subscription within 30 days of that date.

The data shows us that there are some cyclical patterns with clear peaks and troughs, but the conversion rate is generally between 0.7% and 0.9%. 


```r
# define sql query
sql <- "
  select
    m.date
    , count(distinct m.user_id) as active_users
    , count(distinct s.customer_id) as conversions
  from dbt_buffer.buffer_metrics_mau_movements_per_user as m
  left join dbt_buffer.stripe_paid_subscriptions as s
    on m.user_id = s.account_id
    and date(s.first_paid_invoice_created_at) between m.date and date_add(m.date, interval 30 day)
  where m.date >= '2021-01-01'
    and m.is_paid is not true
    and m.is_active_last_30_days
    group by 1
"

# collect data from bigquery
conversions <- bq_query(sql = sql)

# save data
saveRDS(conversions, "free_conversions.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-50-1.png" width="672" />

## When Do Free Users Become Inactive


```r
# define sql query
sql <- "
  with users as (
    select distinct
      m.user_id
      , u.signup_at_date as signup_date
      , min(m.date) as inactive_date
    from dbt_buffer.buffer_metrics_mau_movements_per_user as m
    inner join dbt_buffer.buffer_users as u
      on m.user_id = u.user_id
    where m.is_paid is not true
    and m.is_churn_mau
    and m.date >= '2021-01-01'
    group by 1,2
  )
  select distinct
    user_id
    , signup_date
    , inactive_date
    , date_diff(inactive_date, signup_date, day) as days_to_inactive
  from users
"

# collect data from bigquery
inactive <- bq_query(sql = sql)

# save data
saveRDS(inactive, "inactive_free_users.rds")
```




First it's important to note that users can become inactive multiple times. This distribution only shows the number of days until the _first_ time that a free user becomes inactive.

We can see that it's most common for people to become inactive 30 days after they first become active, indicating that they were only active for a single day. The distribution, unsurprisingly, is markedly different for users that had signed up recently and for those that hadn't. 

Users that had signed up earlier than 30 days prior to the inactivity date tended to be active longer. This bimodal distribution makes it clear that there are two distinct populations. 

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-53-1.png" width="672" />

## Time to Activation
Next we'll visualize the distribution of the amount of time it takes for free users to take their first key action.

The distributions suggest that most people that activate do so very early on. In fact, [this Mixpanel report](https://mixpanel.com/s/2NQw3T) shows us that most people that activate do so within the first 30 minutes after signing up.

The distribution is similar for those that did and didn't end up converting to a paid plan. It's worth noting that a larger proportion of people that did end up converting took longer to a full day or more to activate.


```r
# define sql query
sql <- "
  select
    u.user_id
    , u.signup_at
    , u.first_key_action_at as activated_at
    , u.did_signup_from_mobile as mobile_signup
    , u.did_signup_on_trial as trial_signup
    , timestamp_diff(u.first_key_action_at, u.signup_at, hour) as hours_to_activate
    , min(s.timestamp) as converted_at
  from dbt_buffer.buffer_users as u
  left join dbt_buffer.segment_subscription_starts as s
    on u.user_id = s.user_id
  where u.signup_at >= '2022-01-01'
  group by 1,2,3,4,5,6
"

# collect data from bigquery
activation_time <- bq_query(sql = sql)

# save data
saveRDS(activation_time, "free_activation_time.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-56-1.png" width="672" />

## Email Domains
The plots below show the most common email domains for Buffer users and the conversion rates for each. I've listed the 15 most common domains and grouped the rest into the "Other" category.

Gmail is the most common domain by far, however there are several other domains that have higher conversion rates (e.g. me.com, outlook.com, protonmail.com).


```r
# define sql query
sql <- "
  select
    u.user_id
    , net.host(u.email) as domain
    , u.signup_at
    , u.first_key_action_at as activated_at
    , u.did_signup_from_mobile as mobile_signup
    , u.did_signup_on_trial as trial_signup
    , min(s.timestamp) as converted_at
  from dbt_buffer.buffer_users as u
  left join dbt_buffer.segment_subscription_starts as s
    on u.user_id = s.user_id
  where u.signup_at >= '2022-01-01'
  group by 1,2,3,4,5,6
"

# collect data from bigquery
domains <- bq_query(sql = sql)

# save data
saveRDS(domains, "free_domains.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-59-1.png" width="672" />

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-60-1.png" width="672" />


## How Many Free Users Start Trials?
The dataset used to answer this question includes all users that signed up since January 2021. It includes whether or not they signed up on a trial, the number of subscriptions they started (both with and without trials in the preceding 30 days), and the number of trials started. 



```r
# define sql query
sql <- "
  select
    u.user_id
    , u.signup_at_date as signup_date
    , u.did_signup_on_trial as trial_signup
    , count(distinct s.id) as subs
    , count(distinct t.id) as trials
    , count(distinct case when s.has_trial then s.id end) as subs_with_trial
  from dbt_buffer.buffer_users as u
  left join dbt_buffer.buffer_subscription_starts as s
        on u.user_id = s.user_id
  left join dbt_buffer.segment_trial_starts as t
    on u.user_id = t.user_id
  where u.signup_at >= '2021-01-01'
  and not u.did_signup_on_trial
  group by 1,2,3
"

# collect data from bigquery
user_trials <- bq_query(sql = sql)

# save data
saveRDS(user_trials, "free_user_trials.rds")
```



The data tells us that only around 7% of free signups go on to start a trial after they sign up.


```r
# calculate percent that end up starting trial
user_trials %>% 
  filter(!trial_signup) %>% 
  group_by(started_trial = trials >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users), 0.01))
```

```
## # A tibble: 2 × 3
##   started_trial  users prop  
##   <lgl>          <int> <chr> 
## 1 FALSE         735837 93.31%
## 2 TRUE           52776 6.69%
```

Only around 1.38% of free signups go on to purchase a paid subscription.


```r
# calculate percent that end up starting subscription
user_trials %>% 
  filter(!trial_signup) %>% 
  group_by(started_sub = subs >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users), 0.01))
```

```
## # A tibble: 2 × 3
##   started_sub  users prop  
##   <lgl>        <int> <chr> 
## 1 FALSE       777744 98.62%
## 2 TRUE         10869 1.38%
```

However, of those 1.38% of free signups that went on to start a subscription, approximately 53% of them had started a trial in the 30 days prior to starting the subscription! 


```r
# calculate percent of subscribers that have trial
user_trials %>% 
  filter(!trial_signup) %>% 
  mutate(converted = subs >= 1,
         converted_with_trial = subs_with_trial >= 1) %>% 
  filter(converted) %>% 
  group_by(converted_with_trial) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users), 0.01))
```

```
## # A tibble: 2 × 3
##   converted_with_trial users prop  
##   <lgl>                <int> <chr> 
## 1 FALSE                 5171 47.58%
## 2 TRUE                  5698 52.42%
```


## Which Upgrade Paths Drive Conversions
We'll only look at free signups that created an account on or after January 2021. The "Add Channel" button drives the most upgrades, followed by the top nav bar button and queue limit upgrade prompt


```r
# define sql query
sql <- "
  select
    u.user_id
    , u.signup_at_date as signup_date
    , u.did_signup_on_trial as trial_signup
    , s.last_upgrade_path_viewed as upgrade_path
    , count(distinct s.id) as subs
  from dbt_buffer.buffer_users as u
  left join dbt_buffer.buffer_subscription_starts as s
        on u.user_id = s.user_id
  where u.signup_at >= '2021-01-01'
  and not u.did_signup_on_trial
  group by 1,2,3,4
"

# collect data from bigquery
upgrade_paths <- bq_query(sql = sql)

# save data
saveRDS(upgrade_paths, "free_upgrade_paths.rds")
```



<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-68-1.png" width="672" />

