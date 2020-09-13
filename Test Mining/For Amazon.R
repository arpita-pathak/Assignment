library(rvest)
library(ggplot2)
library(dplyr)
library(magrittr)
library(stringr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(syuzhet)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)

# load in the data
reviews <- read.csv("electronics_downsample.csv")
colnames(reviews) <- tolower(colnames(reviews))
# aggregate the sdcard data
sdcard <- reviews %>% 
  mutate(sd.detected = str_detect(reviewtext, "\\b(sd|SD)\\b")) %>% 
  filter(sd.detected == TRUE) %>% 
  group_by(asin) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total))

head(sdcard, 3)
top3 <- reviews %>% 
  filter(asin %in% c("B007WTAJTO","B002WE6D44","B000VX6XL6"))

mean(top3$overall) #4.499221
top3.mean <- reviews %>% 
  filter(asin %in% c("B007WTAJTO","B002WE6D44","B000VX6XL6")) %>% 
  group_by(asin) %>% 
  summarize(mean(overall))

head(top3.mean,3)
# average sentiment score for all top 3 products
top3sentiments <- as.vector(top3$reviewtext)
mean(get_sentiment(top3sentiments)) #1.260529

# average sentiment score for "B007WTAJTO"
B007sentiments <- as.vector(top3$reviewtext[top3$asin == "B007WTAJTO"])
mean(get_sentiment(B007sentiments)) #1.341498

# average sentiment score for "B002WE6D44"
B002sentiments <- as.vector(top3$reviewtext[top3$asin == "B002WE6D44"])
mean(get_sentiment(B002sentiments)) #1.103794

# average sentiment score for "B000VX6XL6"
B000sentiments <- as.vector(top3$reviewtext[top3$asin == "B000VX6XL6"])
mean(get_sentiment(B000sentiments)) #1.267422
corp.original = VCorpus(VectorSource(top3$reviewtext)) 
corp = tm_map(corp.original, removePunctuation) 
corp = tm_map(corp, removeNumbers) 
corp = tm_map(corp, content_transformer(tolower) ,lazy=TRUE) 
corp = tm_map(corp, content_transformer(stemDocument) ,lazy=TRUE) 
corp = tm_map(corp, stripWhitespace)
corp = tm_map(corp, content_transformer(removeWords), c("TIL"), lazy=TRUE) 
corp = tm_map(corp, content_transformer(removeWords), 
              c(stopwords("english")), lazy=TRUE)
dtm = DocumentTermMatrix(corp)
dim(dtm)
dtms = removeSparseTerms(dtm, .983) #300 words is between 0.983 and 0.984
dim(dtms)
dtms_matrix = as.matrix(dtms)
dim(dtms_matrix)

#starscore <- as.vector(top3$overall)
#dtms_matrix2 <- cbind(dtms_matrix, starscore)
dtms_matrix2 <- cbind(dtms_matrix, top3$overall)
# Create a new dataframe for correlation. 
corr = cor(dtms_matrix2, top3$overall)
words <- rownames(corr)
corr2 <- as.data.frame(cbind(words,corr))

# Noticed that there is an empty row whose correlation is equal to '1'. Remove it. 
which(corr2$V2 == '1') #289 
corr2 <- corr2[-c(289),] 

# top 30 +ve correlated words
corr30positive = order(corr2$V2, decreasing = T)[1:30]
corr30positivewords = rownames(corr2)[corr30positive]
corr30positivewords

# top 30 +ve correlated words
corr30negative = order(corr2$V2, decreasing = F)[1:30]
corr30negativewords = rownames(corr2)[corr30negative]
corr30negativewords
set.seed(111)
corrpositive <- corr2 %>% 
  mutate(numeric = as.numeric(paste(V2))) %>% 
  filter(numeric > 0)
wordcloud(words = corrpositive$words, freq = corrpositive$numeric)
set.seed(111)
corrnegative <- corr2 %>% 
  mutate(numeric = as.numeric(paste(V2))) %>% 
  filter(numeric < 0) %>% 
  mutate(numeric.abs = abs(numeric))
wordcloud(words = corrnegative$words, freq = corrnegative$numeric.abs, scale=c(2.2,0.01))
reviews$helpful.extracted <- str_extract(reviews$helpful, "\\d{1,}")
reviews$helpful.extracted2 <- as.numeric(reviews$helpful.extracted)
reviews$helpful.binary <- ifelse(reviews$helpful.extracted2 >0,1,0)
#reviews$reviews.sentiments <- as.vector(reviews$reviewtext)
#reviews$mean.sentiments <- (get_sentiment(reviews$reviews.sentiments))

# feature 1: sentiment score
reviews <- reviews %>%
  mutate(text.vector = as.vector(reviewtext)) %>% 
  mutate(sentiments= get_sentiment(text.vector))

# feature 2: number of words in a review
# install.packages("ngram")
library(ngram)
reviews <- reviews %>% 
  mutate(reviewtextcounts = sapply(strsplit(text.vector, " "), length))

# feature 3: number of words in the summary tab
reviews <- reviews %>% 
  mutate(summarycounts = sapply(strsplit(as.vector(summary), " "), length))

# feature 4: year of the review (discovered later when building a model this was not significant)
reviews$year <- str_sub(reviews$reviewtime,-4,-1)
# Divide into training and testing. 
set.seed(1234)
train <- sample(nrow(reviews), 0.8*nrow(reviews), replace = FALSE)
trainset <- reviews[train,]
testset <- reviews[-train,]

# Build logistic regression 

model = glm(helpful.binary ~ reviewtextcounts + sentiments, data = reviews, family = 'binomial') # add in the summarycounts later
summary(model)
# only using zero
testset$onlyzero <- 0
mean(testset$onlyzero == testset$helpful.binary) #0.6084882

# compare to my model
z = predict(model, newdata = testset)
testset$outcomes <- predict(model, newdata = testset, type = "response")

# classify as accept if the predicted probability is more than 0.6
x <- 0.36
testset$glm.binary <- 0
testset$glm.binary[testset$outcomes >= x] <- 1
mean(testset$glm.binary == testset$helpful.binary) #0.6853062
# build revised model by adding one new feature
model.revised = glm(helpful.binary ~ year + reviewtextcounts + sentiments, data = reviews, family = 'binomial') 
summary(model.revised)
testset$outcomes.revised <- predict(model.revised, newdata = testset, type = "response")

# classify as accept if the predicted probability is more than 0.6
x <- 0.36
testset$glm.binary.revised <- 0
testset$glm.binary.revised[testset$outcomes.revised >= x] <- 1
mean(testset$glm.binary.revised == testset$helpful.binary) #0.7109102