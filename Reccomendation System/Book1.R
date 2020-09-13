library(tidyverse)
library(Matrix)
library(recommenderlab)
library(kableExtra)
library(gridExtra)
book_ratings <- read.csv("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/ratings.csv", sep = ",", header = T, stringsAsFactors = F)
book_titles <- read.csv("https://raw.githubusercontent.com/zygmuntz/goodbooks-10k/master/books.csv", sep = ",", header = T, stringsAsFactors = F) %>% select(book_id, title)
book_titles$book_id <- as.factor(book_titles$book_id)
# table dimensions
dim(book_ratings)
# first few ratings for books
head(book_ratings, 10)
object.size(book_ratings)
book_ratings$user_id <- as.factor(book_ratings$user_id)
book_ratings$book_id <- as.factor(book_ratings$book_id)
bmatrix <- as(book_ratings, "realRatingMatrix")
dim(bmatrix@data)
sim <- similarity(bmatrix[1:10, ], method = "cosine", which = "users")
image(as.matrix(sim), main = "User Similarity")
sim2 <- similarity(bmatrix[ ,1:10], method = "cosine", which = "items")
image(as.matrix(sim2), main = "Item Similarity")
# users who rated at least 100 books and books rated at least 100 times
bmatrix <- bmatrix[rowCounts(bmatrix) > 150, colCounts(bmatrix) > 300]
bmatrix
tbl_ratings <- as.data.frame(table(as.vector(bmatrix@data)))
tbl_ratings
tbl_ratings <- tbl_ratings[-1,] #0 means missing values so remove missing values
ggplot(tbl_ratings, aes(x = Var1, y = Freq, fill = Var1)) + geom_bar(stat = "identity") + ggtitle("Distribution of Book Ratings")
rated_count <- colCounts(bmatrix)
read_book <- data.frame(
  book_id = names(rated_count),
  read = rated_count
)
top_books <- 
  inner_join(read_book, book_titles, by = "book_id") %>% 
  arrange(desc(read)) %>% 
  select(-book_id) %>% 
  head(10) %>% 
  ggplot(aes(x = title, y = read)) + geom_bar(stat = "identity", fill = "lightblue") + geom_text(aes(label=read), vjust=-0.3, size=3.5) + ggtitle("Top 10 Rated Books") +  coord_flip()
top_books
avg_book_ratings <- data.frame("avg_rating" = colMeans(bmatrix)) %>% 
  ggplot(aes(x = avg_rating)) + 
  geom_histogram(color = "black", fill = "lightgreen") + 
  ggtitle("Distribution of Average Ratings for Books")
avg_book_ratings
image(bmatrix[1:100, 1:100], main = "First 100 users and books")
min_readers <- quantile(rowCounts(bmatrix), 0.99)
min_books <- quantile(colCounts(bmatrix), 0.99)
a <- image(bmatrix[rowCounts(bmatrix) > min_readers, colCounts(bmatrix) > min_books], main = "Non-Normalized")
# to eliminate bias therefore average rating would be 0
book_norm <- normalize(bmatrix)
b <- image(book_norm[rowCounts(book_norm) > min_readers, colCounts(book_norm) > min_books], main = "Normalized")
grid.arrange(a, b, ncol = 2)
train <- sample(x = c(T, F), size = nrow(bmatrix), replace = T, prob = c(0.8, 0.2)) 
books_train <- bmatrix[train, ] 
books_test <- bmatrix[-train, ]
Imodel <- Recommender(data = books_train, method = "IBCF")
Imodel
Ipredict <- predict(Imodel, newdata = books_test, n = 5) %>% list()
Ipredict
# function created to display recommended similar books to users
item_recc_books <- function(i){
  p <- Ipredict[[1]]@items[[i]]
  p <- data.frame("guess" = as.factor(p))
  p <- inner_join(p, book_titles, by = c("guess" = "book_id")) %>% select(title)
  r <- data.frame("name" = as.factor(i))
  r <- inner_join(r, book_titles, by = c("name" = "book_id")) %>% select(title)
  print(paste("Books similar to --", r))
  return(as.list(p))
}
item_recc_books(5); item_recc_books(200); item_recc_books(18)
ibcf <- table(unlist(Ipredict[[1]]@items)) %>% barplot(main = "Distribution of the number of items for IBCF")
Umodel <- Recommender(data = books_train, method = "UBCF")
Umodel
Upredict <- predict(Umodel, newdata = books_test, n = 5) %>% list()
Upredict
# function created to display recommended similar books to users
user_recc_books <- function(u){
  p <- Upredict[[1]]@items[[u]]
  p <- data.frame("guess" = as.factor(p))
  p <- inner_join(p, book_titles, by = c("guess" = "book_id")) %>% select(title)
  r <- data.frame("name" = as.factor(u))
  r <- inner_join(r, book_titles, by = c("name" = "book_id")) %>% select(title)
  print(paste("Books similar to --", r, "-- based on similar users"))
  return(as.list(p))
}
user_recc_books(5); user_recc_books(200); user_recc_books(18)
ubcf <- table(unlist(Upredict[[1]]@items)) %>% barplot(main = "Distribution of the number of items for UBCF")
