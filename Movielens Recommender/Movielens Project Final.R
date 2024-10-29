################################################################################
# MovieLens Capstone Project - Aditi Gupta
# HarvardX Data Science Professional Certificate PH125.9x
# End Date: 10/29/24
################################################################################

##########################################################
# Step 1: Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rlang)) install.packages("rlang", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(gplots)) install.packages("gplots", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(tibble)
library(caret)
library(rlang)
library(ggplot2)
library(dplyr)
library(corrplot)
library(gplots)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
#Warning message:
#In set.seed(1, sample.kind = "Rounding") :
#  non-uniform 'Rounding' sampler used
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

######################################
# End of Provided Code
######################################

#######################################################################################################################
# Step 2: Looking at data sets, looking for missing values, columns, rows, summary of datasets "edx" and "final_holdout_test" 
#######################################################################################################################

# Checking for any missing value
anyNA(edx)
anyNA(final_holdout_test)


# Counting the number of missing values in each column
sapply(edx, function(x) sum(is.na(x)))
sapply(final_holdout_test, function(x) sum(is.na(x)))

# Checking column names with data types
sapply(edx, class)
sapply(final_holdout_test, class)

# Listing the amount of observations and variables
dim(edx)
dim(final_holdout_test)

#Looking at the first few rows and stats like mean, median, min, max, 1 and 3rd Qu.of numeric columns.
head(edx)
summary(edx)

head(final_holdout_test)
summary(final_holdout_test)


#######################################################################################################################
# Step 3: Pre-processing data by converting columns to appropriate  data type, replacing values those makes no sense like age -1 and -2 etc. 
#in datasets "edx" and "final_holdout_test" 
#######################################################################################################################


# Converting time stamp to year rated and adding it to edx
edx <- edx %>% mutate(year_rated = year(as_datetime(timestamp)))

# Double checking any invalid value after conversion of time stamp
unique(edx$year_rated)

# Extracting the year released from title and adding it to edx
edx <- edx %>% mutate(year_released = as.numeric(str_sub(title,-5,-2)))

# Double checking any invalid value of year released
unique(edx$year_released)


# Calculating the movie age when movie was rated and adding it to edx
edx <- edx %>% 
  mutate(ages = pmax(as.numeric(year_rated) - as.numeric(year_released), 0))

# Double checking any invalid value of ages
unique(edx$ages)

# Replacing the values -1 and -2 with NA
edx$ages[edx$ages == -1 | edx$ages == -2] <- NA
# Filter out and display rows with non-finite values in the 'ages' column
non_finite_ages <- edx %>%
  filter(!is.finite(ages))


# Verifying if the replacement worked
unique(edx$ages)



# Did same changes of the data pre-processing for final_holdout_test set
final_holdout_test <- final_holdout_test %>% mutate(year_rated = year(as_datetime(timestamp)))
unique(final_holdout_test$year_rated)

final_holdout_test <- final_holdout_test %>% mutate(year_released = as.numeric(str_sub(title,-5,-2)))
unique(final_holdout_test$year_released)

final_holdout_test <- final_holdout_test %>% mutate(ages = as.numeric(year_rated)-as.numeric(year_released))
# Replacing the values -1 and -2 with 0
final_holdout_test$ages[final_holdout_test$ages == -1 | final_holdout_test$ages == -2] <- 0

# Verifying if the replacement worked
unique(final_holdout_test$ages)

#After changes, looking at the datasets.
head(edx)
head(final_holdout_test)


# Counting unique userId and movieId in the edx dataset
unique_users_edx <- length(unique(edx$userId))
unique_movies_edx <- length(unique(edx$movieId))

# Counting unique userId and movieId in the final_holdout_test dataset
unique_users_holdout <- length(unique(final_holdout_test$userId))
unique_movies_holdout <- length(unique(final_holdout_test$movieId))

# Printing the counts
cat("Unique userId in edx:", unique_users_edx, "\n")
cat("Unique movieId in edx:", unique_movies_edx, "\n")
cat("Unique userId in final_holdout_test:", unique_users_holdout, "\n")
cat("Unique movieId in final_holdout_test:", unique_movies_holdout, "\n")
###################################################################################################################################
#Summary of edx Dataset:
  
# The dataset contains 69878 unique users and 10677 movies.
# The ratings range from 0.5 to 5.0, with a median of 4.0 and an average of 3.512.
# Movies were rated between 1995 and 2009, with most ratings occurring around the year 2002.
# Movies in the data sets were released between 1915 and 2008, with the median release year being 1994.
# The ages column ranges from 0 to 93, with an average of 12 years.

#Summary of final_holdout_test Dataset:
  
# The dataset range of users (68534) and movies (9809).
# The distribution of ratings is the same as in edx, with a median of 4.0 and an average of 3.512.
# Ratings span the years 1995 to 2009, with a median rating year of 2002, while movies were released between 1915 and 2008.
# The ages column has values from 0 to 93, with an average of 12 years.
###################################################################################################################################


######################################
### Exploratory Data Analysis
######################################

# Summary Statistics
summary(edx)
summary(final_holdout_test)

# Table of 5 movies rated most (sorted by number of ratings in descending order)
top_movies <- edx %>%
  group_by(movieId, title) %>%
  summarize(n_rating = n(),
            avg_rating = mean(rating),
            max_rating = max(rating),
            min_rating = min(rating),
            .groups = 'drop') %>%
  arrange(desc(n_rating)) %>%
  distinct(movieId, .keep_all = TRUE) %>%
  slice(1:5)

# Display the table
top_movies %>%
  knitr::kable()

# Table of 5 users who rated the most movies (sorted by number of ratings in descending order)
top_users <- edx %>%
  group_by(userId) %>%
  summarize(n_ratings = n(),
            avg_rating = mean(rating),
            max_rating = max(rating),
            min_rating = min(rating),
            .groups = 'drop') %>%
  arrange(desc(n_ratings)) %>%
  slice(1:5)

# Display the table
top_users %>%
  knitr::kable()

# Table of 20 ages of movie who rated the most movies (sorted by number of ratings in descending order)
top_users <- edx %>%
  group_by(ages) %>%
  summarize(n_ratings = n(),
            avg_rating = mean(rating),
            max_rating = max(rating),
            min_rating = min(rating),
            .groups = 'drop') %>%
  arrange(desc(n_ratings)) %>%
  slice(1:20)

# Display the table
top_users %>%
  knitr::kable()

# Generate correlation matrix excluding 'userId' and 'movieId' from numeric columns list
corr_matrix <- cor(edx %>% 
                     select(rating, year_rated, year_released, ages), 
                   use = "complete.obs")

# Heatmap with correlation values using heatmap
heatmap(corr_matrix, 
          cellnote = round(corr_matrix, 2),
          notecol = "black",
          dendrogram = "none",
          trace = "none",
          margins = c(8, 8),
          cexRow = 0.8,
          cexCol = 0.8,
          key = TRUE,
          density.info = "none",
          lhei = c(2, 8),
          lwid = c(2, 8))


# Plot Ratings distribution
#The histogram shows that the most common rating given is 4, followed by 3 and 5. 
#Ratings of 1 and 2 are much less frequent, 
#indicating that users tend to rate movies more favorably overall, 
#with the distribution skewed toward higher ratings.
edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, fill = "blue4", color = "black") +
  ggtitle("Rating Distribution") +
  xlab("Rating") +
  ylab("Frequency")

# Plot age distribution

#The histogram shows that the majority of ratings are for relatively newer movies (0-10 years old),
#with the frequency of ratings sharply declining as the movie age increases.
#Older films (50+ years) receive significantly fewer ratings,
#indicating a preference for more recent releases among users.

edx %>%
  ggplot(aes(ages)) +
  geom_histogram(bins = 30, color = "blue4", fill = "skyblue") +
  xlab("Age of Movies") +
  ylab("Frequency") +
  ggtitle("Distribution of Movie Ages")

# Line chart of average rating per year released

#As we can see that there was a sharp decline of average rating from 3.85 -> 3.45 
#somewhere in 1976-1979.and most of the old movies are rated high on a average.
#This is mostly due to the data set, I assume only classic or well known 
#old movies are part of this data set when all or most of the new movies are included 
#which is causing this huge difference in average rating of movies.

edx %>%
  group_by(year_released) %>%
  summarise(avg_rating = mean(rating, na.rm = TRUE)) %>%  # Calculate average rating per year
  ggplot(aes(x = year_released, y = avg_rating)) +
  geom_line(color = "blue4") +
  geom_point(color = "red") +
  labs(title = "Average Rating per Year Released", 
       x = "Year Released", 
       y = "Average Rating") +
  theme_minimal()


# Plot number of ratings per movie
edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue4") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")



# Plot number of ratings given by users
edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "blue4") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings given by users")

# Plot mean movie ratings given by users
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "blue4") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean movie ratings given by users") +
  scale_x_continuous(breaks = seq(0.5, 5, 0.5)) +
  theme_light()


#User Behavior Analysis: Ratings vs. Number of Ratings

# Calculate the number of ratings and average rating per user
user_behavior <- edx %>%
  group_by(userId) %>%
  summarise(num_ratings = n(), avg_rating = mean(rating))

# Create a scatter plot
ggplot(user_behavior, aes(x = num_ratings, y = avg_rating)) +
  geom_point(color = "blue4", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  xlab("Number of Ratings Given by User") +
  ylab("Average Rating Given by User") +
  ggtitle("User Behavior Analysis: Ratings vs. Number of Ratings") +
  scale_x_log10() +
  theme_minimal()


######################################
# Modeling
######################################

#Note: This section is heavily inspired by the chapter 33 of text book https://rafalab.dfci.harvard.edu/dsbook/large-datasets.html#recommendation-systems
#and I had to upgrade the R version and I have used chatgpt for debugging while fitting correct version of libraries.

### 1. Naive Model (Using a Training/Test Split)

#splitting the edx dataset into training and test sets for model development. Will reserve final_holdout_test for the final evaluation step only.

# Split the edx data into training and test sets (80/20 split)
set.seed(1)  # For reproducibility
train_index <- createDataPartition(edx$rating, times = 1, p = 0.8, list = FALSE)
train_set <- edx[train_index, ]
test_set <- edx[-train_index, ]

# Ensurinf that the test set only contains users and movies from the training set
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Computing the mean rating (mu) from the edx training set
mu <- mean(train_set$rating)

# Testing results based on simple prediction using the training/test edx split
naive_rmse <- RMSE(test_set$rating, mu)

# Saving prediction into data frame
rmse_results <- data_frame(method = "Naive Model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()


### Movie Effect Model (Using Training/Test Split)

# Naive model taking into account the movie effect b_m
# and calculating the rating minus the mean for each rating the movie received

# Movie effect model: Calculate b_m from the training set
movie_effects <- train_set %>%
  group_by(movieId) %>%
  summarize(b_m = mean(rating - mu))

# Plot the distribution of the movie effect (b_m)
movie_effects %>%
  ggplot(aes(x = b_m)) +
  geom_histogram(bins = 10, color = "blue4", fill = "skyblue") +
  labs(title = "Distribution of Movie Effects (b_m)",
       x = "Movie Effect (b_m)",
       y = "Number of Movies")

# Test and save RMSE results on the test set
predicted_ratings <- test_set %>%
  left_join(movie_effects, by='movieId') %>%
  mutate(prediction = mu + b_m) %>%
  pull(prediction)

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie Effect Model",  
                                     RMSE = model_1_rmse))
rmse_results %>% knitr::kable()



### Movie + User Effect Model


# User effect (b_u): Calculate the user effect from the training set
user_effects <- train_set %>%
  left_join(movie_effects, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_m))

# Plot the distribution of the user effect (b_u)
user_effects %>%
  ggplot(aes(x = b_u)) +
  geom_histogram(bins = 30, color = "blue4", fill = "lightblue") +
  labs(title = "Distribution of User Effects (b_u)",
       x = "User Effect (b_u)",
       y = "Number of Users")

# Test and save RMSE results on the test set
predicted_ratings <- test_set %>%
  left_join(movie_effects, by='movieId') %>%
  left_join(user_effects, by='userId') %>%
  mutate(prediction = mu + b_m + b_u) %>%
  pull(prediction)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Movie + User Effect Model",  
                                     RMSE = model_2_rmse))
rmse_results %>% knitr::kable()



### Regularized Movie + User Effect Model

# Lambda is a tuning parameter, I tried various combinations and picked the step size 0.25 
##- A step size of 0.25 provides a reasonably fine granularity of testing without being 
##computationally excessive.
# Regularization model with cross-validation to select lambda
# Regularized Movie + User Effect Model (Using Training/Test Split)

# Lambda is a tuning parameter
lambdas <- seq(0, 10, 0.25)

# Compute RMSE for each lambda
rmses <- sapply(lambdas, function(l){
  # Mean rating from the training set
  mu_reg <- mean(train_set$rating)
  
  # Regularized movie effect (b_m_reg)
  b_m_reg <- train_set %>%
    group_by(movieId) %>%
    summarize(b_m_reg = sum(rating - mu_reg) / (n() + !!l))
  
  # Regularized user effect (b_u_reg)
  b_u_reg <- train_set %>%
    left_join(b_m_reg, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u_reg = sum(rating - b_m_reg - mu_reg) / (n() + !!l))
  
  # Predict ratings for the test set
  predicted_ratings <- test_set %>%
    left_join(b_m_reg, by = "movieId") %>%
    left_join(b_u_reg, by = "userId") %>%
    mutate(prediction = mu_reg + b_m_reg + b_u_reg) %>%
    pull(prediction)
  
  # Compute RMSE
  return(RMSE(test_set$rating, predicted_ratings))
})

# Plot RMSE against lambdas
qplot(lambdas, rmses) + 
  labs(title = "RMSE vs. Lambda",
       x = "Lambda",
       y = "RMSE")

# Find the optimal lambda (minimizing RMSE)
lambda_optimal <- lambdas[which.min(rmses)]
lambda_optimal

# Calculate RMSE for the optimal lambda
model_m_u_reg_rmse <- min(rmses)
model_m_u_reg_rmse

# Save RMSE results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Regularized Movie + User Effect Model",
                                     RMSE = model_m_u_reg_rmse))
rmse_results %>% knitr::kable()


######################################
# Final Model Fitting
######################################

# Compute the mean rating on the full edx dataset
mu_final <- mean(edx$rating)

# Regularized movie effects on the full edx dataset
b_m_final <- edx %>%
  group_by(movieId) %>%
  summarize(b_m_final = sum(rating - mu_final) / (n() + lambda_optimal))

# Regularized user effects on the full edx dataset
b_u_final <- edx %>%
  left_join(b_m_final, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u_final = sum(rating - b_m_final - mu_final) / (n() + lambda_optimal))

# Final predictions on the final_holdout_test dataset
final_predicted_ratings <- final_holdout_test %>%
  left_join(b_m_final, by = "movieId") %>%
  left_join(b_u_final, by = "userId") %>%
  mutate(final_prediction = mu_final + b_m_final + b_u_final) %>%
  pull(final_prediction)

# Calculate final RMSE using final_holdout_test
final_rmse <- RMSE(final_holdout_test$rating, final_predicted_ratings)

# Add final RMSE to results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "Final Model (Regularization)",
                                     RMSE = final_rmse))

# Display final RMSE results
rmse_results %>% knitr::kable()
