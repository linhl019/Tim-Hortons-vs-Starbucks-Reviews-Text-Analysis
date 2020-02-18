################################################################################
############### TEXT ANALYTICS BUSINESS INSIGHT REPORT | LINH LE ###############
################################################################################
####################### Topic: Tim Hortons vs. Starbucks ######################
################################################################################

# Loading all necessary libraries

library(tm)
library(dplyr)
library(tidytext)
library(tidyverse)
library(tidyr)
library(textreadr)
library(ggplot2)
library(wordcloud)
library(plotly)
library(readr)

# Loading txt files into R:

setwd("/Users/linhle/Desktop/Text Analytics/Assignments") 

tims_txt <- read_document(file='tims_reviews.txt')
starbs_txt <- read_document(file='starbucks_reviews.txt')

# Creating dataframe for each txt object:

tims_df <- data_frame(line=1:478, text=tims_txt)
starbs_df <- data_frame(line=1:415, text=starbs_txt)

#########################################################
####################### Tokenizing ######################
#########################################################

# Tokenizing each data frame separately

# Creating a list of custom stop words that don't give busines insight:

custom_stopwords <- tribble(
  ~word,           ~lexicon,
  "tim",           "CUSTOM",
  "hortons",       "CUSTOM",
  "starbucks",     "CUSTOM",
  "1",             "CUSTOM",
  "2",             "CUSTOM",
  "told",          "CUSTOM",
  "tim's",         "CUSTOM",
  "tims",          "CUSTOM",
  "horton",        "CUSTOM",
  "horton's",      "CUSTOM",
  "starbuck",      "CUSTOM"
)
  
#TIM HORTONS
tims_nostop <- tims_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%         # Removing stop words from dictionary
  anti_join(custom_stopwords) %>%   # Removing stop words from custom list
  count(word, sort=TRUE)

#STARBUCKS
starbs_nostop <- starbs_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%     
  anti_join(custom_stopwords) %>%
  count(word, sort=TRUE)

### Plotting frequencies ###

#Plotting Tim Hortons token frequency:

tims_freq_hist <- tims_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stopwords)%>%
  count(word, sort=TRUE) %>%
  top_n(15)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col(fill="firebrick")+
  xlab(NULL)+
  coord_flip()

print(tims_freq_hist + ggtitle("Tim Hortons"))


# Plotting Starbuck's token frequency:

starbs_freq_hist <- starbs_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stopwords)%>%
  count(word, sort=TRUE) %>%
  top_n(15)%>%
  mutate(word=reorder(word, n))%>%
  ggplot(aes(word, n))+
  geom_col(fill="forestgreen")+
  xlab(NULL)+
  coord_flip()

print(starbs_freq_hist + ggtitle("Starbucks"))


###########################################################
####################### Correlograms ######################
###########################################################

### Comparing Tim Hortons and Starbucks ###

frequency <- bind_rows(mutate(tims_nostop, company="Tim Hortons"),  
                       mutate(starbs_nostop, company= "Starbucks"),
)%>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(company, word) %>%
  group_by(company) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(company, proportion) %>%
  gather(company, proportion, `Starbucks`)

# Plotting correlograms:

library(scales)

ggplot(frequency, aes(x=proportion, y=`Tim Hortons`, 
                      color = abs(`Tim Hortons`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=1, height=1)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~company, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Tim Hortons", x=NULL)

#########################################################
######################### TF-IDF ########################
#########################################################

# Putting both companies into one dataframe

both_df <- bind_rows(
  mutate(tims_nostop, company="Tim Hortons"),
  mutate(starbs_nostop, company='Starbucks'),
)

# Piping tf_idf to new dataframe with both companies to create TF_IDF table

both_df <- both_df %>%       # all_questions is our tokenized dataframe
  bind_tf_idf(word, company, n)       #word, location information (company), and frequency (n)

# Arranging to get the highest tf_idf first

both_df %>%
  arrange(desc(tf_idf))

# Plotting the tf_idf:

both_df %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(company) %>%
  top_n(9) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=company))+
  geom_col(show.legend=FALSE)+
  scale_fill_manual(values = c("Starbucks" = "forestgreen", "Tim Hortons" = "firebrick"))+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~company, ncol=2, scales="free")+
  coord_flip()

### Term frequency ###

total_words <- both_df %>% 
  group_by(company) %>% 
  summarize(total = sum(n))

company_words2 <- left_join(both_df, total_words)

ggplot(company_words2, aes(n/total, fill = company)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~company, ncol = 2, scales = "free_y")

#########################################################
################### Sentiment Analysis ##################
#########################################################


##### TIM HORTONS #####

### BING ###

tims_bing <- tims_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tims_bing_plot <- tims_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

print(tims_bing_plot + ggtitle("Tim Hortons"))

### AFINN ###

tims_afinn <- tims_nostop %>%                      
  inner_join(get_sentiments("afinn"))%>%    # inner joining afinn sentiment
  summarise(sentiment=sum(value)) %>%       # inner joining by the sum of value
  mutate(method="AFINN")

### NRC ###

library(reshape2)

tims_nostop %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.6,0.6),
                   fixed.asp=TRUE,
                   title.size = 1) #to customize text size


##### STARBUCKS #####

### BING ###

starbs_bing <- starbs_df %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

starbs_bing_plot <- starbs_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

print(starbs_bing_plot + ggtitle("Starbucks"))

### AFINN ###

starbs_afinn <- starbs_nostop %>%                      
  inner_join(get_sentiments("afinn"))%>%    # inner joining afinn sentiment
  summarise(sentiment=sum(value)) %>%       # inner joining by the sum of value
  mutate(method="AFINN")

### NRC ###

library(reshape2)

starbs_nostop %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.6,0.6),
                   fixed.asp=TRUE,
                   title.size = 1) #to customize text size


##############################################################
######## Most common positive and negative words #############
##############################################################

tims_bing_counts <- tims_nostop %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tims_bing_counts


######################################################
####################### Bigrams ######################
######################################################

##### Tim Hortons #####

tims_bigrams <- tims_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram) %>%
  na.omit()

tims_bigrams #We want to see the bigrams (words that appear together, "pairs")

# Removing stop words:

tims_bigrams_separated <- tims_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")      # Splits bigrams into two separate objects

tims_bigrams_filtered <- tims_bigrams_separated %>%      
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stopwords$word) %>%
  filter(!word2 %in% custom_stopwords$word) %>%
  arrange(desc(n))

##### Starbucks #####

starbs_bigrams <- starbs_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)%>%
  count(bigram) %>%
  na.omit()

starbs_bigrams #We want to see the bigrams (words that appear together, "pairs")

# Removing stop words:

starbs_bigrams_separated <- starbs_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")      # Splits bigrams into two separate objects

starbs_bigrams_filtered <- starbs_bigrams_separated %>%      
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% custom_stopwords$word) %>%
  filter(!word2 %in% custom_stopwords$word) %>%
  arrange(desc(n))

### Plotting bigrams frequency ###

# Uniting the previously separated bigrams:

tims_bigrams_united <- tims_bigrams_filtered %>%         # Uniting the two columns, word1 and word2
  unite(bigram, word1, word2, sep=" ")       #we need to unite what we split in the previous section

starbs_bigrams_united <- starbs_bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")

# Plotting Tim's bigrams:
tims_bigrams_freqplot <- tims_bigrams_united %>%
  top_n(10)%>%
  ggplot(aes(reorder(bigram,n), n)) +
  geom_bar(stat = "identity", fill='firebrick') + coord_flip() +
  xlab("Bigrams") + ylab("n") +
  ggtitle("Tim Hortons")


#Plotting Starbucks' bigrams:
starbs_bigrams_freqplot <- starbs_bigrams_united %>%
  top_n(10)%>%
  ggplot(aes(reorder(bigram,n), n)) +
  geom_bar(stat = "identity", fill='forestgreen') + coord_flip() +
  xlab("Bigrams") + ylab("n") +
  ggtitle("Starbucks")


############################################################
####################### Bigram TF-IDF ######################
############################################################

all_bigrams_united <- bind_rows(
  mutate(tims_bigrams_united, company="Tim Hortons"),
  mutate(starbs_bigrams_united, company='Starbucks'),
)

all_bigrams_tf_idf <- all_bigrams_united %>%
  count(company, bigram) %>%             # Count the bigram per company
  bind_tf_idf(bigram, company, n) %>%    # Bind tf_idf
  arrange(desc(tf_idf))

all_bigrams_tf_idf


# Plotting the tf_idf:

all_bigrams_tf_idf %>%
  arrange(desc(all_bigrams_tf_idf)) %>%
  mutate(word=factor(bigram, levels=rev(unique(bigram)))) %>%
  group_by(company) %>%
  top_n(9) %>%
  ungroup %>%
  ggplot(aes(word, all_bigrams_tf_idf, fill=company))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~company, ncol=2, scales="free")+
  coord_flip()
