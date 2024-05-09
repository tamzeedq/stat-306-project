#============================================
# SETUP
#============================================

# Visualization libraries
install.packages("corrplot")
install.packages("cowplot")

#import libraries
library(corrplot)
library(ggplot2)
library(cowplot)
library("leaps")



# Load in data 
data_full <- read.csv(file="C:/Users/tamze/Downloads/spotify_data2.csv") # Change file path as necessary

# Reduce columns to the ones we chose for analysis
data <- subset(data_full, select = c("streams", "artist_count", "bpm", "danceability_.", "energy_.", "acousticness_.", "liveness_.", "speechiness_."))

#============================================
# VISUALIZATIONS
#============================================

# Variable Histograms
stream_hist <- data |>
  ggplot(aes(x = streams)) +
  geom_histogram() +
  ggtitle("Streams Distribution")

art_cnt_hist <- data |>
  ggplot(aes(x = artist_count)) +
  geom_histogram() +
  ggtitle("Artist Count Distribution")

bpm_hist <- data |>
  ggplot(aes(x = bpm)) +
  geom_histogram() + 
  ggtitle("BPM Distribution")

dance_hist <- data |>
  ggplot(aes(x = danceability_.)) +
  geom_histogram() + 
  ggtitle("Danceability Distribution")

energy_hist <- data |>
  ggplot(aes(x = energy_.)) +
  geom_histogram() + 
  ggtitle("Energy Distribution")

acc_hist <- data |>
  ggplot(aes(x = acousticness_.)) +
  geom_histogram() + 
  ggtitle("Acoustic Distribution")

live_hist <- data |>
  ggplot(aes(x = liveness_.)) +
  geom_histogram() + 
  ggtitle("Liveliness Distribution")


speech_hist <- data |>
  ggplot(aes(x = speechiness_.)) +
  geom_histogram() + 
  ggtitle("Speechiness Distribution")


plot_grid(stream_hist, art_cnt_hist, bpm_hist, dance_hist, energy_hist, acc_hist, live_hist, speech_hist, ncol = 3, nrow=3)


# Plot Correlation Matrix
corrplot(cor(data), method = "color", addCoef.col = "black", number.cex = 0.7)


#============================================
# MODEL SELECTION
#============================================


# Log scale streams
full_model <- lm(log(streams) ~ artist_count + bpm + danceability_. + energy_. + acousticness_. + liveness_. + speechiness_., data)
summary(full_model)

# Perform backward elimination
be_model <- step(full_model)
summary(be_model)

# plot residuals of backward elimination model
plot(x=be_model$fitted.values, y=be_model$residuals, xlab = "Fitted Log Streams Values", ylab = "Residual", main = "Residual Plot BE Model")
abline(h = 0, col = "red")



# Perform exhaustive search for best model from full model
model_selection <- regsubsets(log(streams) ~ artist_count + bpm + danceability_. + energy_. + acousticness_. + liveness_. + speechiness_., data = data)

# Summary of the method
summary(model_selection)$which

# CP values of models
cp <- summary(model_selection)$cp

# CP plot for the models selected by regsubsets
plot(cp, xlab="P", ylab="CP", main = "CP Plot")


# Final model based off backward elimination and exhaustive search
final_model <- lm(log(streams) ~ artist_count  + danceability_. + acousticness_. + liveness_. + speechiness_., data)
summary(final_model)

# Residual plot of final model
plot(x=final_model$fitted.values, y=final_model$residuals, xlab = "Fitted Log Streams Values", ylab = "Residual", main = "Residual Plot of Final Model")
abline(h = 0, col = "red")



# RMSE check
rmse <- function(u,v) sqrt(mean((u-v)^2))


training_indices <- sample.int(nrow(data), 120)
training_set <- data[training_indices, ]
summary(training_set)


# Fit the final model to the training set
final_model_training <- lm(log(streams) ~ artist_count  + danceability_. + acousticness_. + liveness_. + speechiness_., data = training_set)

# Predict the values on the test set
predicted_values_full <- predict(final_model_training, newdata = data[-training_indices,])

# Compute RMSE between fitted values and observed responses on the test set
rmse_full <- rmse(predicted_values_full, log(data[-training_indices, "streams"]))

rmse_full


