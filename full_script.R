
## Load libraries
if (!require("academictwitteR")) install.packages("academictwitteR")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")
if (!require("lubridate")) install.packages("lubridate")
if (!require("syuzhet")) install.packages("syuzhet")
if (!require("rstatix")) install.packages("rstatix")
if (!require("plotly")) install.packages("plotly")
if (!require("wesanderson")) install.packages("wesanderson")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("scales")) install.packages("scales")
if (!require("ggrepel")) install.packages("ggrepel")

## Identify release date of tags
#Pull tweets that include the terms Twitch and tags.

tag_release = get_all_tweets(
  query = "twitch tags",
  start_tweets = "2021-05-21T00:00:00Z",
  end_tweets = "2021-06-01T00:00:00Z",
  data_path = "tag_release/",
  lang = "en",
  is_retweet = FALSE,
  remove_promoted = TRUE,
  bind_tweets = FALSE,
  n = Inf
  )

#Bind the pulled tweets

tag_release = bind_tweets(data_path = "data/tag_release", output_format = "tidy")

#Write the tweets to a csv file.

fwrite (tag_release, "data/tag_release.csv")

#Explore the collected tweets for release

released = tag_release %>%
  filter(str_detect(text, 'released')) %>%
  select(text, created_at, tweet_id)
View(released)

live = tag_release %>%
  mutate(date = date(created_at)) %>% 
  filter(str_detect(text, 'live')) %>%
  select(text, created_at, tweet_id)
View(live)

new = tag_release %>%
  mutate(date = date(created_at)) %>% 
  filter(str_detect(text, 'new')) %>%
  filter(str_detect(created_at, '2021-05-26T16:')) %>%
  select(text, created_at, tweet_id)
View(new)

tag_release %>%
  filter(user_username == 'TwitchSupport')

## Pull tweets pre-release

pre_release = get_all_tweets(
  query = c("twitch raid", "twitch raids"),
  start_tweets = "2021-04-28T16:00:00Z",
  end_tweets = "2021-05-26T16:00:00Z",
  data_path = "pre_release/",
  lang = "en",
  is_retweet = FALSE,
  remove_promoted = TRUE,
  bind_tweets = FALSE,
  n = Inf
  )

pre_release = bind_tweets(data_path = "data/pre_release", output_format = "tidy")

pre_release = pre_release %>%
  distinct(tweet_id, .keep_all = TRUE)

fwrite (pre_release, "data/pre_release.csv")

## Pull tweets post-release

post_release = get_all_tweets(
  query = c("twitch raid", "twitch raids"),
  start_tweets = "2021-05-26T16:00:00Z",
  end_tweets = "2021-06-23T16:00:00Z",
  data_path = "post_release/",
  lang = "en",
  is_retweet = FALSE,
  remove_promoted = TRUE,
  bind_tweets = FALSE,
  n = Inf
  )

post_release = bind_tweets(data_path = "data/post_release", output_format = "tidy")

post_release = post_release %>%
  distinct(tweet_id, .keep_all = TRUE)

fwrite (post_release, "data/post_release.csv")

pre_release2 = pre_release %>%
  mutate (set = "Pre-Release")
post_release2 = post_release %>%
  mutate (set = "Post-Release")

tweets = rbind(pre_release2, post_release2)

## Public Metrics

pre_release_pm = bind_tweets(data_path = "data/pre_release") %>% 
  as_tibble

pre_release_pm = pre_release_pm %>%
  unnest(public_metrics) %>%
  select(id, retweet_count, reply_count, like_count, quote_count) %>%
  distinct(id, .keep_all = TRUE)

post_release_pm = bind_tweets(data_path = "data/post_release") %>% 
  as_tibble

post_release_pm = post_release_pm %>%
  unnest(public_metrics) %>%
  select(id, retweet_count, reply_count, like_count, quote_count) %>%
  distinct(id, .keep_all = TRUE)

pre_release_pm
post_release_pm

tweets_pm = rbind(pre_release_pm, post_release_pm) %>%
  rename(tweet_id = id) %>%
  distinct(tweet_id, .keep_all = TRUE)

tweets %>%
  select(-c(retweet_count, like_count, quote_count))

tweets_all = merge((tweets %>%
                      select(-c(retweet_count, like_count, quote_count))),
                   tweets_pm, by.x = "tweet_id", by.y = "tweet_id", all = FALSE) %>%
  distinct(tweet_id, .keep_all = TRUE)

# filter for game terms

filtered_tweets = tweets_all %>%
  filter(!str_detect(text, "(?i)warcraft|(?i)destiny|(?i)shadow|(?i)legends|(?i)ffxiv|(?i)pokemon|(?i)vault")) %>%
  filter(conversation_id == tweet_id) %>%
  distinct(tweet_id, .keep_all = TRUE)

fig2 = filtered_tweets %>%
  mutate(date = date(created_at)) %>% 
  ggplot(aes(date, fill=set)) +
  geom_histogram(color="#333333",
                 binwidth = 1) +
  labs(x = "Date", y = "Number of Tweets") +
  scale_x_date(breaks=date_breaks("week"),
               labels=date_format("%b %d")) + 
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3),
                    name=NULL)

fig2 = fig2 +
  theme_minimal() +
  theme(legend.position = "top",
        text = element_text(size = 24),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
  )
fig2

ggplotly(fig2)

fig3 = tweets_nrc %>%
  mutate(date = date(created_at)) %>% 
  group_by(date, set) %>%
  summarise(mean = mean(emo_score)) %>% 
  ggplot(aes(set,
           mean,
           fill = set)
       ) +
  geom_boxplot(colour = "#111111") +
  geom_jitter(aes(fill = set),
              alpha = 0.75,
             width = 0.25,
             ) +
  labs(x = "Tweet Set", y = "Average Sentiment Score") +
  theme_minimal()+
  theme(text = element_text(size = 24),
        legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3)) +
  coord_flip()
fig3

ggplotly(fig3)

fig4 = tweets_nrc %>%
  mutate(date = date(created_at)) %>% 
  group_by(date, set) %>%
  summarise(mean = mean(eng_score)) %>% 
  ggplot(aes(set,
           mean,
           fill = set)
       ) +
  geom_boxplot(colour = "#111111") +
  geom_jitter(aes(fill = set),
              alpha = 0.75,
             width = 0.25,
             ) +
  labs(x = "Tweet Set", y = "Average Engagement Score") +
  theme_minimal()+
  theme(text = element_text(size = 24),
        legend.position = "none") +
  scale_fill_manual(values = wes_palette("Darjeeling1", n = 3)) +
  coord_flip()
fig4

ggplotly(fig4)

## Clean data

#Filter out unnecessary noise in tweets and limit to text column

#vr_work_y0_tweets = vr_work_y0_tweets %>%
#  mutate(period = "y0")
#vr_work_y1_tweets = vr_work_y1_tweets %>%
#  mutate(period = "y1")

url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

tweets_text = filtered_tweets %>%
  mutate(text = str_remove_all(text, url_regex)) %>%
  select(text)

tweets_text = tweets_text
tweets_text %>%
  select(text)
tweets_text$text = gsub('(?i)raid','!.!.!.!', tweets_text$text)
tweets_text %>%
  select(text)

tweets_sv = as.vector(get_sentiment(tweets_text$text, method = "nrc"))

tweets_sv_df = as.data.frame(tweets_sv)

tweets_sen = cbind(filtered_tweets, tweets_sv_df) %>%
  rename (sent_value = tweets_sv)

## Score for Sentiment

emotions_tweets = get_nrc_sentiment(tweets_sen$text)
emo_bar_tweets = colSums(emotions_tweets)
emo_sum_tweets = data.frame(count=emo_bar_tweets, 
                             emotion=names(emo_bar_tweets))
emo_sum_tweets$emotion = factor(emo_sum_tweets$emotion, 
                                 levels=emo_sum_tweets$emotion[order(emo_sum_tweets$count, decreasing = TRUE)])

tweets_nrc = cbind(tweets_sen, emotions_tweets) %>%
  filter(conversation_id == tweet_id)

## Calculate Emotional Score

tweets_nrc = tweets_nrc %>%
  mutate (emo_score = positive - negative)
tweets_nrc
  

tweets_nrc %>%
  group_by (set) %>%
  summarise(mean = mean(emo_score))

## Analyse for emotional score

tweets_nrc %>%
  t_test(emo_score ~ set, detailed = TRUE) %>%
  add_significance()

tweets_nrc %>%
  cohens_d(emo_score ~ set)

# Analyze for Engagement Score

tweets_nrc = tweets_nrc %>%
  mutate (eng_score = reply_count + like_count + retweet_count + quote_count)
tweets_nrc

tweets_nrc %>%
  t_test(eng_score ~ set, detailed = TRUE) %>%
  add_significance()

tweets_nrc %>%
  cohens_d(eng_score ~ set)

## Pre-Release and Post-Release Hate Raid Tweets

hr_tweets = filtered_tweets %>%
  group_by(set) %>%
  filter(str_detect(text, "(?i)hate raids|(?i)hate raid")) %>%
  ungroup()

hr_tweets

## Wrangle Hate Raid Tweet Data

hr_tweets_count = hr_tweets %>%
  group_by(set, created_at) %>%
  mutate(date =  ymd_hms(created_at),
         date = round_date(date, "day")) %>%
  count(date) %>%
  ungroup()

hr_tweets_count

hr_tweet_count = hr_tweets_count %>%
  complete(date = seq.Date(as.Date("2021/04/28"), as.Date("2021/06/23"), by="day")) %>%
  mutate(set) %>%
  select(-created_at)

hr_tweet_count_pre = hr_tweet_count %>%
  filter (date > as.Date("2021-04-28") & date < as.Date("2021-05-26")) %>%
  mutate (set = "Pre-Release",
          n = ifelse(is.na(n), 0, n)) %>%
  add_row(date = as.Date("2021-04-28"), set = "Pre-Release", n = 0)

hr_tweet_count_post = hr_tweet_count %>%
  filter (date > as.Date("2021-05-26") & date < as.Date("2021-06-23")) %>%
  mutate (set = "Post-Release",
          n = ifelse(is.na(n), 0, n)) %>%
  add_row(date = as.Date("2021-04-28"), set = "Post-Release", n = 0)

hr_tweet_count = rbind(hr_tweet_count_pre, hr_tweet_count_post)

hr_tweet_count

## T-test for Pre and Post-Release Hate Raid Tweets

hr_tweet_count %>%
  t_test(n ~ set, detailed = TRUE) %>%
  add_significance()

hr_tweet_count %>%
  cohens_d(n ~ set)

## Collect Hate Raid Tweets for Six Months Follow Tag Release

hate_raids_6mos = get_all_tweets(
      query = "Twitch hate raids",
      start_tweets = "2021-05-26T00:00:00Z",
      end_tweets = "2021-11-26T00:00:00Z",
      data_path = "hate_raids_6mos/",
      lang = "en",
      is_retweet = FALSE,
      remove_promoted = TRUE,
      bind_tweets = FALSE,
      n = Inf
      )

    

hate_raids_6mos = bind_tweets(data_path = "data/hate_raids_6mos", output_format = "tidy")

hate_raids_6mos = hate_raids_6mos %>%
  distinct(id, .keep_all = TRUE)

fwrite (hate_raids_6mos, "data/hate_raids_6mos.csv")

hate_raids_6mos = fread("data/hate_raids_6mos.csv")

## Wrangle Six Month Hate Raid Tweet Data

hr_6mos_time = hate_raids_6mos %>%
  mutate(date =  ymd_hms(created_at),
         date = round_date(date, "hour"))
hr_6mos_time

hr_6mos_count = hr_6mos_time %>%
  group_by(date) %>%
  count(date) %>%
  arrange(desc(n)) %>%
  mutate(date = as.Date(date),
         flag = n > 97.84)

hr_6mos_count

(mean(hr_6mos_count$n)) + (sd(hr_6mos_count$n))

## Visualization Hate Raid Frequency

# Most basic bubble plot
fig5 = hr_6mos_count %>%
  ggplot(aes(x=date, 
             y=n,
             color = "#F98400")) +
  geom_line(size=1) + 
  labs(x = "Dates Between May 26 and November 25, 2021",
       y = "Number of Tweets") +
  geom_label_repel(data = hr_6mos_count %>% 
                     filter(flag) %>% 
                     unique(),
                   aes(label = n,
                       fill = "#FF2500"),
                   color = "white") +
  scale_x_date(date_breaks = "2 weeks", 
               date_labels = "%b %d") + 
  theme_minimal() + 
  guides(color = FALSE, 
         fill = FALSE) + 
  geom_hline(yintercept=41.42,
             color = "#01A08A") +
  annotate(geom="text", 
           x = as.Date("2021-07-01", "%Y-%m-%d"),
           y = 55, 
           label = "One SD Above Mean (41.42)",
           color= "#01A08A") +
  theme(text = element_text(size = 20),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = "none") 

fig5

ggplotly(fig5)
