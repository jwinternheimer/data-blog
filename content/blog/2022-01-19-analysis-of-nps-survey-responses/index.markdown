---
title: Analysis of NPS Survey Responses
author: Julian Winternheimer
date: '2022-01-19'
slug: []
categories: []
tags:
  - nps
---

In this post we'll analyze 7166 NPS survey responses gathered from Pendo. We'll find the most frequently occurring words for Promoters, Passives, and Detractors, and calculate which of them are most unique to each segment. We'll also visualize networks of terms that commonly occur together.







## Tidy Text Data Format
In order to analyze the text efficiently, we'll want to make use of some "tidy" data principles. To consider this data set tidy we need to have one _token_ (or one observation in most other analyses) per row. A token in this text analysis is one word or a group of words.


```r
# unnnest the tokens
text_df <- nps %>%
  unnest_tokens(word, text)

# glimpse data
glimpse(text_df)
```

```
## Rows: 79,795
## Columns: 5
## $ date        <date> 2021-12-10, 2021-12-10, 2021-12-10, 2021-12-10, 2021-12-1…
## $ user_id     <chr> "5ca1d7921d99bc38dbf8ef52", "5ca1d7921d99bc38dbf8ef52", "5…
## $ rating      <int> 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10…
## $ nps_segment <chr> "promoter", "promoter", "promoter", "promoter", "promoter"…
## $ word        <chr> "canva", "is", "my", "favorite", "tool", "for", "use", "in…
```

Next we'll remove stop words like "a", "the", etc. that aren't useful to us.


```r
# get stop words
data(stop_words)

# remove stop words from dataset
text_df <- text_df %>%
  anti_join(stop_words, by = "word") %>% 
  filter(word != "buffer")
```


## Data Exploration
Now that the data has been restructured into a tidy format, we can do some exploration. Let's start by looking at the most common terms present in the NPS survey responses.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" width="672" />

It's great to see that "easy" is the most common word in our NPS survey responses. It's easy to see themes of simplicity and saving time. Now let's look at the top words for each NPS segment.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" width="672" />

We can see that there are words that appear frequently for all three segments (e.g. "buffer", "post"). To address this we'll use a different technique to look at words that occur with more frequency for promoters than for detractors or passives.

To find these words, we can calculate the relative frequency of words that appear in promoters' responses and compare that to the relative frequency of the words that appear in detractors'.

The idea of term frequency–inverse document frequency (tf-idf) is to find the important words for the content of each document by decreasing the weight for commonly used words and increasing the weight for words that are not used very much in a collection or corpus of documents, in this case, all survey responses for each segment. Calculating tf-idf attempts to find the words that are common in a text document, but not _too_ common. 


```r
# get all words in the survey
survey_words <- nps %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>% 
  count(nps_segment, word, sort = TRUE)

# get total of all words
total_words <- survey_words %>% 
  group_by(nps_segment) %>% 
  summarize(total = sum(n))

# join to get proportions
survey_words <- left_join(survey_words, total_words)
head(survey_words) 
```

```
## # A tibble: 6 × 4
##   nps_segment word       n total
##   <chr>       <chr>  <int> <int>
## 1 promoter    easy    1290 15060
## 2 promoter    buffer   548 15060
## 3 promoter    social   349 15060
## 4 promoter    love     319 15060
## 5 promoter    media    292 15060
## 6 detractor   post     261  9803
```

The `bind_tf_idf()` function takes a tidy text data set as input with one row per token, per document. One column (`word`) contains the tokens, one column contains the documents (`nps_segment`), and the last necessary column contains the counts, how many times each document contains each term (`n`).


```r
# calculate tf-idf
segment_tf_idf <- survey_words %>%
  bind_tf_idf(word, nps_segment, n)
```

The `idf` term is very low for words that appear frequently for each segment. The inverse document frequency (and thus tf-idf) is very low (near zero) for words that occur in many of the documents in a collection; this is how this approach decreases the weight for common words. The inverse document frequency will be a higher number for words that occur in fewer of the documents in the collection.

Let's look at words with high `tf-idf` in the NPS surveys.


```r
# high tf-idf words
segment_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  head()
```

```
## # A tibble: 6 × 6
##   nps_segment word           n      tf   idf  tf_idf
##   <chr>       <chr>      <int>   <dbl> <dbl>   <dbl>
## 1 promoter    excellent     97 0.00644 0.405 0.00261
## 2 promoter    excelent      17 0.00113 1.10  0.00124
## 3 promoter    wonderful     17 0.00113 1.10  0.00124
## 4 detractor   unreliable    10 0.00102 1.10  0.00112
## 5 detractor   errors        25 0.00255 0.405 0.00103
## 6 detractor   glitchy       25 0.00255 0.405 0.00103
```

This checks out. The terms "excellent", "amazing", and "fantastic" appear relatively frequently in Promoters' responses, whereas the terms "bad", "clunky", and "layout" appear relatively frequently for detractors.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-11-1.png" width="672" />

We can start to pick up themes that appear for detractors and promoters. Detractors mention poor performance and reliability, whereas promoters do not. Passives seem to resemble detractors more than promoters.

## Bigrams
We can also consider groups of words as tokens. Bigrams are groups of two words, trigrams are groups of three, and so on.


```r
# get bigrams
bigrams <- nps %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# separate the words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# filter out stop words
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!is.na(word1) & !is.na(word2))

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

# view top bigrams
head(bigram_counts, 10)
```

```
## # A tibble: 10 × 3
##    word1    word2         n
##    <chr>    <chr>     <int>
##  1 social   media       410
##  2 user     friendly    169
##  3 free     version      86
##  4 love     buffer       51
##  5 schedule posts        50
##  6 super    easy         49
##  7 customer service      43
##  8 media    posts        37
##  9 media    accounts     36
## 10 multiple platforms    33
```


```r
# get word pairs
nps <- nps %>% 
  mutate(id = row_number())

# unnest tokens
nps_text <- nps %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  filter(word != "buffer")

# get word pairs
text_word_pairs <- nps_text %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-14-1.png" width="672" />


```r
# get word pairs
detractor_word_pairs <- nps_text %>% 
  filter(nps_segment == "detractor") %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

# plot network for detractors
detractor_word_pairs %>%
  filter(n >= 7) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 3) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" width="672" />

It's great to see bigrams like "super easy" and "user friendly". Next we'll visualize the network of bigrams by looking at words that have strong correlations with other words. I'll spare you a long explanation of the methodology for creating this plot.


```r
# reunite bigrams
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n >= 6) %>%
  graph_from_data_frame()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-17-1.png" width="672" />

Here we can see the relationships between terms. Things like people -> tag and technical -> issues make sense. "Posts" and "media" appear to be central nodes, as well as "loses/losing connection".

Let's recreate this plot only for detractors and passives.

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-18-1.png" width="672" />

These are the related terms for detractors and passives. Core functionality, technical issues, losing connection, people tagging, and time consuming slots are all interesting things to potentially look into. It's nice to see that "love buffer" is still present.
