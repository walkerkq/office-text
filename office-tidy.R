#' ---
#' output: github_document
#' ---
library(dplyr)
library(tidytext)
library(tidyr)
library(corrplot) 

office <- read.csv("office-lines.csv", stringsAsFactors=F) 

# top 20 speakers
main_characters <- data.frame(table(office$speaker)) %>% top_n(20, Freq)

# tidy the text
office_tidy <- office %>%
  # remove action phrases like [clears throat]
  mutate(line_text = gsub("\\[.*?\\]", "", line_text)) %>%
  unnest_tokens(word, line_text) %>%
  anti_join(stop_words) %>%
  count(word, speaker) %>%
  group_by(speaker) %>%
  mutate(Prop = n / sum(n)) %>%
  ungroup()

# tf_idf
office_tf <- office_tidy %>%
  bind_tf_idf(word, speaker, n) %>%
  group_by(word) %>%
  mutate(word.total = sum(n),
         word.pct = n/word.total) %>%
  ungroup() %>%
  subset(tf_idf > 0 & word.total > 10 
         & word.pct < 1 & n > 5 
         & speaker %in% main_characters$Var1)

# stage for finding correlation between speakers
office_corr <- office_tidy %>%
  subset(speaker %in% main_characters$Var1) %>%
  group_by(speaker) %>%
  select(-n) %>%
  spread(speaker, Prop) 

# replace NAs with 0
office_corr[is.na(office_corr)] <- 0

# correlation plot
mycol <- colorRampPalette(c("darkgrey", "grey", "white", "darkseagreen1", "darkseagreen"))  
corr <- cor(office_corr[,-1], use = "pairwise.complete.obs") %>%  
  corrplot(method="color", order="hclust", diag=FALSE, 
           tl.col = "black", tl.srt = 45, tl.cex=0.6,
           col=mycol(100), 
           type="lower",
           title="Text Correlation Between Speakers", 
           family="Avenir",
           mar=c(0,0,1,0))

# calculate euclidean distance and cluster
office_corr_t <- t(office_corr[,-1])
office_dist <- dist(office_corr_t, method="euclidean")
fit <- hclust(office_dist, method="ward.D")
plot(fit,main="Cluster Dendrogram of Speakers", family="Avenir")
rect.hclust(fit, k=3, border="darkseagreen") 

# use knn to classify new lines of text

