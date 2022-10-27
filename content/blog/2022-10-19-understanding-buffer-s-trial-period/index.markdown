---
title: Understanding Buffer's Trial Period
author: Julian Winternheimer
date: '2022-10-19'
slug: []
categories: []
tags: []
---



Before getting started, I have to provide an important disclaimer. Since March of 2022, Buffer has automatically started trials for people that sign up on buffer.com. Additionally, there are periods of time in which users could have chosen to sign up on a Free plan. 

This inevitably affects our data, as separate populations (those that would have chosen to start a trial on their own and those that would not have) are lumped together. 

**That means that the findings in this analysis may not fully reflect reality in a system in which people are able to select whether they want to start a trial or not.**

The learning objectives of this analysis are as follows: 

 - To discover and describe what the average trial experience is.
 - To discover and describe areas in which we can improve the experience.

## Summary
 - A significant number of new signups were put onto trials automatically, so the data presented in this analysis is somewhat biased.
 - Because many people were automatically put on trials, the trial conversion rate, defined as the proportion of trialists that start a subscription within 28 days of starting a trial, has decreased from around 10% to around 4%.
 - People that sign up for Buffer on a non-mobile web browser convert at the highest rates. People that sign up on one of the mobile apps convert at the lowest rates.
 - In general people that convert do so on the day the trial starts or on the day the trial ends. Almost as many trialists convert on the first day as on the last day.
 - Trialists that end up converting signal their intent early on in the trial. Of those that add a payment method, the vast majority do so on day one.
 - The median number of days active during the trial is 2 for those that don't convert and 5 for those that did.
 - The number of unique sessions during the trial is a good indicator of whether or not a trialist will end up converting.
 - In general, Buffer's trial does not seem to be very effective in getting people to try premium features.
   - The median number of channels connected by trialists during their trial is very low. It is 0 for those that didn’t convert and 1 for those that did.
   - Around 18% of trialists that didn’t convert connected an Instagram channel during the trial, compared to around 42% of trialists that did convert.
   - Around 22% of trialists that didn’t convert connected a Facebook channel during the trial, compared to around 40% of trialists that did convert.
   - Around 28% of trialists viewed at least one analytics page, though this doesn’t necessarily mean they got value from the feature. Only 4% of trialists published a Start Page and 1% replied to a comment in the Engagement feature.
   - Of trialists that converted, around 52% viewed an Analyze page, 6% replied to a comment, and only 5% published a Start Page.
   - Only around 2% of trialists added team members during their trial.
   - Of users that converted, around 18% added at least one team member during their trial.
   - Less than 1% of trialists approved a draft during their trial.

## Recommendations
 - We should remove the in-app popup asking people to start a trial on the onboarding product solution screen. They just chose whether or not to start a trial on the previous page. Also the offer is incorrect. The trial plan is the Essentials Team.

![](images/trial_prompt.png)

 - When someone is on a trial and hasn't added a payment method, the banner below is displayed. We ask the user to add a payment method in the copy, but the CTA says "Start Subscription" and opens the plan selector.
   - Instead, we should have a CTA that says "Add Credit Card" that opens the payment method entry form.
   - Additionally, there seems to be too much text. We could possibly get away with something simpler, like "You have 12 days left on your free trial. Add a credit card for the best experience."
 
![](images/countdown.jpg)


 - **When someone has a credit card on file, nothing is displayed in the product to indicate that the trial is still active. Additionally, we seem to charge people automatically when the trial ends. This could be a cause of major billing confusion.**
 
   - When someone is on a trial, we should always have something that indicates that. We should also display the number of days left in the trial.
  
   - If we're going to automatically charge trialists, we should let them know. It feels a little shady to charge automatically for our highest value plan without displaying this information to the user.
  
   - Additionally, we should allow a trialist to choose which plan they want if they already have a card on file and we're about to automatically charge them. They should be able to choose the plan and billing interval.

 - When a trialists clicks "Start Subscription" and opens the plan selector, there is a section that says that they're currently paying a certain amount based on the number of channels connected. In reality they are on a trial and not paying anything yet. We should correct that.

![](images/currently_paying.jpg)
 
 - When someone has "Annual" selected in the plan selector, the price to be paid does not show up. This should also be corrected.

![](images/annual_price.jpg)
 
 - We should update the "Manage Channels" button at the bottom left hand corner of the screen with a more prominent CTA to "Add Channel" or "Connect Channel" or "Connect a Social Account".
    - Otherwise, we can have specific CTAs to "Connect Instagram Page", "Connect Facebook Page" under the channels they already have connected in the Queue view.
    
 - We should prompt trialists to add team members, perhaps by including a prominent CTA on the dashboard. Team member and collaboration features are underutilized.
 
 - If an Instagram channel is connected, we should prompt users to create a Start Page in the publishing feature. 
 
 - We should notify users when new data is available to view in Analyze.
 
 - We should notify users when there are new unreplied comments in Engage.
 
## Baseline Volume and Conversion Rates
These will vary during the times in which people could choose to create an account on a free plan. We will define a conversion as someone starting a subscription within 14 days of the trial ending.

The number of trial starts can be viewed in Mixpanel [here](https://mixpanel.com/s/1EcVmr).





Our dataset includes 516K trials started by 499K users since July 2021. The plot below shows that the number of trials started weekly has increased from around 5K to 10K in 2022.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="672" />

The 14-day conversion rate has decreased from around 10% to around 4% in this time. This is no doubt influenced by us putting new signups onto trials automatically.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" width="672" />

## Conversion Rates by Signup Type
Web signups that start on trial have the highest conversion rates, followed by web signups that start on a free plan. People that sign up on the mobile apps have the lowest conversion rates.





<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" width="672" />


## How Long it Takes Trialists to Convert
I used [this Mixpanel report](https://mixpanel.com/s/lt7cR) for this section. It only includes conversions that happened within 90 days of the trial starting.






```r
# calculate conversion time in days
convert_time <- convert_time %>% 
  mutate(start_day = start_time / 60/ 60 / 24,
         end_day = end_time / 60 / 60 / 24,
         percent = as.numeric(sub("%", "", conversion_percent)) / 100)
```

Most conversions occur on the day the trial is started or on the day the trial ends.This is unsurprising, but should we do anything to make it easier to convert on day 1?

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-12-1.png" width="672" />


## How Many Free Signup Start a Trial and Then Convert
Approximately 14% of people that signed up on a free plan went on to start a trial later.






```r
# calculate percent that started a trial
free %>% 
  group_by(started_trial = !is.na(trial_date)) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users)))
```

```
## # A tibble: 2 × 3
##   started_trial  users prop 
##   <lgl>          <int> <chr>
## 1 FALSE         194593 86%  
## 2 TRUE           31680 14%
```

Only around 1.4% of free signups converted to a paid plan.


```r
# percent converted
free %>% 
  group_by(converted = !is.na(conversion_date)) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users))
```

```
## # A tibble: 2 × 3
##   converted  users   prop
##   <lgl>      <int>  <dbl>
## 1 FALSE     223119 0.986 
## 2 TRUE        3154 0.0139
```

Only around 0.8% of free signups started a trial and then converted to a paid plan.


```r
# percent started trial and converted
free %>% 
  group_by(trial_then_converted = !is.na(trial_date) & !is.na(conversion_date) &
             trial_date <= conversion_date) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users))
```

```
## # A tibble: 2 × 3
##   trial_then_converted  users    prop
##   <lgl>                 <int>   <dbl>
## 1 FALSE                224411 0.992  
## 2 TRUE                   1862 0.00823
```

Over 75% of users that signed up on a free plan and started a trial did so on their first day.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-18-1.png" width="672" />


## Behaviors of Trialists
We'll start by exploring the number of sessions that trialists had during their trials.


```r
# define sql query
sql <- "
  select distinct
    t.user_id
    , a.client_name
    , a.created_with_trial
    , date(t.timestamp) as trial_date
    , date(a.timestamp) as signup_date
    , count(distinct s.id) as subs
    , count(distinct ss.session_id) as sessions
    , count(distinct date(ss.started_at)) as session_days
    , avg(ss.n_events) as avg_session_events
    , avg(ss.session_duration_minutes) as avg_session_duration
  from dbt_buffer.segment_trial_starts as t
  left join dbt_buffer.segment_accounts_created as a
    on a.user_id = t.user_id
  left join dbt_buffer.segment_sessions as ss
    on ss.dbt_visitor_id = t.user_id
    and ss.started_at between t.timestamp and timestamp_add(t.timestamp, interval 14 day) 
  left join dbt_buffer.segment_subscription_starts as s
    on t.user_id = s.user_id
    and s.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 28 day)
  where t.timestamp >= '2022-03-07'
  and t.timestamp <= timestamp_sub(current_timestamp(), interval 14 day)
  group by 1,2,3,4,5
"

# query bigquery
sessions <- bq_query(sql = sql)

# save data
saveRDS(sessions, "trial_sessions.rds")
```



The median number of sessions during trial is around 2 for people that don't convert and 8 for those that do. The averages are 5.3 and 11.7 respectively.

Interestingly, the 25th quantile is 1 (meaning at least 25% only had 1 session) for trialists that didn't end up converting. 


```r
# define quantiles of interest
q = c(.25, .5, .75)

# calculate quantiles by group
sessions %>%
  group_by(converted = subs >= 1) %>%
  summarize(quant25 = quantile(sessions, probs = q[1]), 
            quant50 = quantile(sessions, probs = q[2]),
            avg = mean(sessions, na.rm = T),
            quant75 = quantile(sessions, probs = q[3]))
```

```
## # A tibble: 2 × 5
##   converted quant25 quant50   avg quant75
##   <lgl>       <dbl>   <dbl> <dbl>   <dbl>
## 1 FALSE           0       0  2.65       2
## 2 TRUE            3       7 10.5       14
```

The median number of days active during the trial is two for users that didn't convert and 5 for those that did. The averages are 3.1 and 5.6, respectively. 


```r
# calculate session day stats 
sessions %>%
  group_by(converted = subs >= 1) %>%
  summarize(quant25 = quantile(session_days, probs = q[1]), 
            quant50 = quantile(session_days, probs = q[2]),
            avg = mean(session_days, na.rm = T),
            quant75 = quantile(session_days, probs = q[3]))
```

```
## # A tibble: 2 × 5
##   converted quant25 quant50   avg quant75
##   <lgl>       <dbl>   <dbl> <dbl>   <dbl>
## 1 FALSE           0       0  1.54       2
## 2 TRUE            2       4  5.08       8
```

In general, most trialists either have a single session or between three and five sessions during their trials.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-1.png" width="672" />

Let's see what the distribution looks like for those that converted. We can see that a greater proportion of trialists that converted (yellow) had between 3 and 20 sessions during their trials.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-24-1.png" width="672" />

## Channels Connected
Let's calculate some summary statistics on the number of channels connected by trialists.


```r
# define sql query
sql <- "
  select distinct
    t.user_id
    , date(t.timestamp) as trial_date
    , date(a.timestamp) as signup_date
    , count(distinct s.id) as subs
    , count(distinct ch.channel_service_id) as channels_connected
    , count(distinct case when channel = 'facebook'
                          then ch.channel_service_id end) as fb_channels_connected
    , count(distinct case when channel = 'instagram'
                          then ch.channel_service_id end) as ig_channels_connected
    , count(distinct case when channel = 'twitter'
                          then ch.channel_service_id end) as tw_channels_connected
    , count(distinct case when channel = 'linkedin'
                          then ch.channel_service_id end) as li_channels_connected
  from dbt_buffer.segment_trial_starts as t
  left join dbt_buffer.segment_accounts_created as a
    on a.user_id = t.user_id
  left join dbt_buffer.segment_subscription_starts as s
    on t.user_id = s.user_id
    and s.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 28 day)
  left join dbt_buffer.segment_channels_connected as ch
    on ch.user_id = t.user_id
    and ch.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 14 day)
  where t.timestamp >= '2022-03-07'
  and t.timestamp <= timestamp_sub(current_timestamp(), interval 14 day)
  group by 1,2,3
"

# query bigquery
channels <- bq_query(sql = sql)

# save data
saveRDS(channels, "trial_channels.rds")
```



The median number of channels connected by trialists during their trial is very low. It is 0 for those that didn't convert and 1 for those that did. It is possible to connect channels before starting a trial, but it seems clear that relatively few people are connecting more channels once their trial begins.

The average number of channels connected during the trial is 1 for those that did not convert and 2 for those that did. 


```r
# calculate channels connected summary stats
channels %>%
  group_by(converted = subs >= 1) %>%
  summarize(quant25 = quantile(channels_connected, probs = q[1]), 
            quant50 = quantile(channels_connected, probs = q[2]),
            avg = mean(channels_connected, na.rm = T),
            quant75 = quantile(channels_connected, probs = q[3]))
```

```
## # A tibble: 2 × 5
##   converted quant25 quant50   avg quant75
##   <lgl>       <dbl>   <dbl> <dbl>   <dbl>
## 1 FALSE           0       0  1.04       1
## 2 TRUE            0       1  2.09       3
```

Around 18% of trialists that didn't convert connected an Instagram channel during the trial, compared to around 42% of trialists that did convert.


```r
# calculate percentage that connected instagram
channels %>% 
  group_by(converted = subs >= 1,
           connected_instagram = ig_channels_connected >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users)) %>% 
  filter(connected_instagram)
```

```
## # A tibble: 2 × 4
## # Groups:   converted [2]
##   converted connected_instagram users  prop
##   <lgl>     <lgl>               <int> <dbl>
## 1 FALSE     TRUE                46696 0.185
## 2 TRUE      TRUE                 5929 0.422
```


Around 22% of trialists that didn't convert connected a Facebook channel during the trial, compared to around 40% of trialists that did convert.


```r
# calculate percentage that connected facebook
channels %>% 
  group_by(converted = subs >= 1,
           connected_fb = fb_channels_connected >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users)) %>% 
  filter(connected_fb)
```

```
## # A tibble: 2 × 4
## # Groups:   converted [2]
##   converted connected_fb users  prop
##   <lgl>     <lgl>        <int> <dbl>
## 1 FALSE     TRUE         56358 0.223
## 2 TRUE      TRUE          5665 0.403
```

Around 15% of trialists that didn't convert connected a Twitter channel during the trial, compared to around 26% of trialists that did convert.


```r
# calculate percentage that connected facebook
channels %>% 
  group_by(converted = subs >= 1,
           connected_tw = tw_channels_connected >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users)) %>% 
  filter(connected_tw)
```

```
## # A tibble: 2 × 4
## # Groups:   converted [2]
##   converted connected_tw users  prop
##   <lgl>     <lgl>        <int> <dbl>
## 1 FALSE     TRUE         37939 0.150
## 2 TRUE      TRUE          3638 0.259
```

Around 16% of trialists that didn't convert connected a LinkedIn channel during the trial, compared to around 27% of trialists that did convert.


```r
# calculate percentage that connected facebook
channels %>% 
  group_by(converted = subs >= 1,
           connected_li = li_channels_connected >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = users / sum(users)) %>% 
  filter(connected_li)
```

```
## # A tibble: 2 × 4
## # Groups:   converted [2]
##   converted connected_li users  prop
##   <lgl>     <lgl>        <int> <dbl>
## 1 FALSE     TRUE         39414 0.156
## 2 TRUE      TRUE          3813 0.272
```


## Premium Features: Analyze, Engage, and Start Page
How many people used each of these features during their trial?


```r
# define sql query
sql <- "
  select distinct
    t.user_id
    , date(t.timestamp) as trial_date
    , count(distinct s.id) as subs
    , count(distinct cr.id) as comments_replied
    , count(distinct ap.id) as analyze_views
    , count(distinct sp.id) as start_pages_published
  from dbt_buffer.segment_trial_starts as t
  left join dbt_buffer.segment_subscription_starts as s
    on t.user_id = s.user_id
    and s.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 28 day)
  left join segment_engage.comment_replied as cr
    on cr.user_id = t.user_id
    and cr.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 14 day)
  left join dbt_buffer.segment_start_page_published as sp
    on sp.user_id = t.user_id
    and sp.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 14 day)
  left join segment_analyze.pages as ap
    on ap.user_id = t.user_id
    and ap.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 14 day)
  where t.timestamp >= '2022-03-07'
  and t.timestamp <= timestamp_sub(current_timestamp(), interval 14 day)
  group by 1,2
"

# query bigquery
analyze <- bq_query(sql = sql)

# save data
saveRDS(analyze, "trial_analyze.rds")
```



Around 28% of trialists viewed at least one analytics page, though this doesn't necessarily mean they got value from the feature. Only 4% of trialists published a Start Page and 1% replied to a comment in the Engagement feature.


```r
# calculate percentage that used each
analyze %>% 
  mutate(analyze = analyze_views >= 1,
         engage = comments_replied >= 1,
         start_page = start_pages_published >= 1) %>% 
  select(user_id, analyze:start_page) %>% 
  pivot_longer(-user_id, names_to = "feature", values_to = "used") %>% 
  group_by(feature, used) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users))) %>% 
  filter(used)
```

```
## `summarise()` has grouped output by 'feature'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 3 × 4
## # Groups:   feature [3]
##   feature    used  users prop 
##   <chr>      <lgl> <int> <chr>
## 1 analyze    TRUE  75749 28%  
## 2 engage     TRUE   3298 1%   
## 3 start_page TRUE  10996 4%
```

We can also break this down by whether or not people converted. Of trialists that converted, around 52% viewed an Analyze page, 6% replied to a comment, and only 5% published a Start Page.


```r
# calculate percentage that used each
analyze %>% 
  mutate(converted = subs >= 1,
         analyze = analyze_views >= 1,
         engage = comments_replied >= 1,
         start_page = start_pages_published >= 1) %>% 
  select(user_id, converted:start_page) %>% 
  pivot_longer(-c(user_id, converted), names_to = "feature", values_to = "used") %>% 
  group_by(converted, feature, used) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users))) %>% 
  filter(used)
```

```
## `summarise()` has grouped output by 'converted', 'feature'. You can override
## using the `.groups` argument.
```

```
## # A tibble: 6 × 5
## # Groups:   converted, feature [6]
##   converted feature    used  users prop 
##   <lgl>     <chr>      <lgl> <int> <chr>
## 1 FALSE     analyze    TRUE  68507 27%  
## 2 FALSE     engage     TRUE   2434 1%   
## 3 FALSE     start_page TRUE  10313 4%   
## 4 TRUE      analyze    TRUE   7319 52%  
## 5 TRUE      engage     TRUE    865 6%   
## 6 TRUE      start_page TRUE    686 5%
```


## Team and Collaboration Features


```r
# define sql query
sql <- "
  select distinct
    t.user_id
    , date(t.timestamp) as trial_date
    , count(distinct s.id) as subs
    , count(distinct ta.id) as team_adds
    , count(distinct da.id) as drafts_approved
  from dbt_buffer.segment_trial_starts as t
  left join dbt_buffer.segment_subscription_starts as s
    on t.user_id = s.user_id
    and s.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 28 day)
  left join segment_publish_server.team_member_added as ta
    on ta.user_id = t.user_id
    and ta.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 14 day)
    left join segment_publish.draft_approved as da
    on da.user_id = t.user_id
    and da.timestamp between t.timestamp and timestamp_add(t.timestamp, interval 14 day)
  where t.timestamp >= '2022-03-07'
  and t.timestamp <= timestamp_sub(current_timestamp(), interval 14 day)
  group by 1,2
"

# query bigquery
team <- bq_query(sql = sql)

# save data
saveRDS(team, "trial_team_members.rds")
```



Only around 2% of trialists added team members during their trial.


```r
# calculate how many added team member
team %>% 
  group_by(added = team_adds >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users)))
```

```
## # A tibble: 2 × 3
##   added  users prop 
##   <lgl>  <int> <chr>
## 1 FALSE 260970 98%  
## 2 TRUE    6613 2%
```

Of users that converted, around 18% added at least one team member during their trial.


```r
# calculate how many added team member
team %>% 
  group_by(converted = subs >= 1,
           added = team_adds >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users)))
```

```
## `summarise()` has grouped output by 'converted'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 4 × 4
## # Groups:   converted [2]
##   converted added  users prop 
##   <lgl>     <lgl>  <int> <chr>
## 1 FALSE     FALSE 249692 98%  
## 2 FALSE     TRUE    4030 2%   
## 3 TRUE      FALSE  11520 82%  
## 4 TRUE      TRUE    2590 18%
```

Less than 1% of trialists approved a draft during their trial.


```r
# calculate how many added team member
team %>% 
  group_by(approved_draft = drafts_approved >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users)))
```

```
## # A tibble: 2 × 3
##   approved_draft  users prop 
##   <lgl>           <int> <chr>
## 1 FALSE          266985 100% 
## 2 TRUE              421 0%
```

Of those that converted, only 1% approved a draft. 


```r
# calculate how many added team member
team %>% 
  group_by(converted = subs >= 1,
           approved_draft = drafts_approved >= 1) %>% 
  summarise(users = n_distinct(user_id)) %>% 
  mutate(prop = percent(users / sum(users)))
```

```
## `summarise()` has grouped output by 'converted'. You can override using the
## `.groups` argument.
```

```
## # A tibble: 4 × 4
## # Groups:   converted [2]
##   converted approved_draft  users prop 
##   <lgl>     <lgl>           <int> <chr>
## 1 FALSE     FALSE          253295 100% 
## 2 FALSE     TRUE              321 0%   
## 3 TRUE      FALSE           13990 99%  
## 4 TRUE      TRUE              100 1%
```

## When Payment Methods are Added
Interestingly, most trialists that add a payment method do so on the first day of starting a trial. After that, most do so on the day that the trial ends. 






```r
# calculate conversion time in days
payment <- payment %>% 
  mutate(start_day = start / 60/ 60 / 24,
         end_day = end / 60 / 60 / 24,
         percent = as.numeric(sub("%", "", conversion_rate)) / 100)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-45-1.png" width="672" />












