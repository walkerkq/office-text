library(dplyr)
library(tidytext)
library(tidyr)
library(corrplot) 
library(class)

office <- read.csv("office-lines.csv", stringsAsFactors=F) 

# top speakers
top20 <- data.frame(table(office$speaker)) %>% top_n(20, Freq)
top5 <- data.frame(table(office$speaker)) %>% top_n(5, Freq)

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
         & speaker %in% top20$Var1)

# stage for finding correlation between speakers
office_corr <- office_tidy %>%
  subset(speaker %in% top20$Var1) %>%
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
office_class <- office %>%
  mutate(speaker = ifelse(speaker %in% top5$Var1, speaker, "Other"),
         id2 = paste(season, episode, scene, sep=".")) %>%
  subset(!speaker %in% "Other") %>%
  unnest_tokens(word, line_text) %>%
  anti_join(stop_words) %>%
  count(word, speaker, id2) %>%
  group_by(id2) %>%
  mutate(Prop = n / sum(n))  %>%
  subset(Prop >= .005) %>%
  select(-n) %>%
  spread(word, Prop)
office_class[is.na(office_class)] <- 0

set.seed(337)
subset_ids <- sample(1:length(office_class$speaker), length(office_class$speaker)*.50, replace=F)  
office_class_subset <- office_class[subset_ids, ]

train_ids <- sample(length(office_class_subset$speaker), length(office_class_subset$speaker)*.80, replace=F)
train_bk <- office_class_subset[train_ids, -c(1,2)] # training
test_bk <- office_class_subset[-train_ids, -c(1,2)] # testing
train_resp <- office_class_subset[train_ids, c(1,2)] # groups for training
test_resp <- office_class_subset[-train_ids, c(1,2)] # groups for testing

# run
system.time(kresult <- knn(train = train_bk, test = test_bk, cl = train_resp$speaker, k = 1, use.all=F))
#30 min... wowza

# check results
knn_results <- data.frame(round(prop.table(table(kresult, test_resp$speaker),2),2)) %>%
  set_colnames(c("Classification", "Actual", "Pct"))

ggplot(knn_results, aes(Actual, Pct, fill=Classification)) + 
  geom_bar(stat="identity", position="dodge", alpha=0.75) +
  labs(title="Accuracy of KNN Classification into HClusters\n", y = "Percent") + 
  theme_classic(base_size=10, base_family="Avenir") + 
  theme(axis.text.x=element_text(angle=30, hjust=1)) + ylim(c(0,1))
# very poor
