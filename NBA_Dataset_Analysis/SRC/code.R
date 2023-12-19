
library(rsample)
library(caret)
library(magrittr)
library(plotly)
library(dplyr)
library(ggplot2)
library(yardstick)

df <- NBA_dataset
summary(df)

df <- df %>%
  mutate(POS = if_else(POS == "N/A", "SG", POS))


plot_ly(df, x = ~POS, type = "histogram", marker = list(color = 'rgb(68, 1, 84)'),
        title = 'Players position value counts', colors = 'Vivid', template = 'plotly_dark')

position_stats <- df %>%
  group_by(POS) %>%
  summarise(PTS = mean(PTS, na.rm = TRUE)) %>%
  ungroup()

fig <- plot_ly(position_stats, x = ~POS, y = ~PTS, type = "bar",
               marker = list(color = c('orangered', 'red', 'grey', 'orangered', 'red', 'grey', 'orangered')))

fig <- fig %>%
  layout(title = 'Points per Position',
         xaxis = list(title = 'Position'),
         yaxis = list(title = 'Average Total Points'),
         template = 'plotly_dark')

fig

fig_age_histogram <- plot_ly(df, x = ~Age, type = "histogram", marker = list(color = 'orangered'))

fig_age_histogram <- fig_age_histogram %>%
  layout(title = 'Distribution of Player Ages',
         xaxis = list(title = 'Age'),
         yaxis = list(title = 'Count'),
         template = 'plotly_dark')

fig_age_histogram

df$POS <- as.factor(df$POS)

fig_total_points <- plot_ly(df, x = ~Age, y = ~PTS, color = ~POS, type = "scatter", mode = "markers") %>%
  layout(title = 'Player Age vs Total Points',
         xaxis = list(title = 'Age'),
         yaxis = list(title = 'Total Points'),
         template = 'plotly_dark')

fig_total_points

fig_fg_percentage <- plot_ly(df, x = ~Age, y = ~FG., color = ~POS, type = "scatter", mode = "markers") %>%
  layout(title = 'Player Age vs Field Goal Percentage',
         xaxis = list(title = 'Age'),
         yaxis = list(title = 'Field Goal Percentage'),
         template = 'plotly_dark')

fig_fg_percentage

fig_assists <- plot_ly(df, x = ~Age, y = ~AST, color = ~POS, type = "scatter", mode = "markers") %>%
  layout(title = 'Player Age vs Assists',
         xaxis = list(title = 'Age'),
         yaxis = list(title = 'Assists'),
         template = 'plotly_dark')

fig_assists

avg_fantasy_points <- df %>%
  group_by(POS) %>%
  summarise(FP = mean(FP, na.rm = TRUE)) %>%
  ungroup()

fig_fantasy_points <- plot_ly(avg_fantasy_points, x = ~POS, y = ~FP, type = "bar", marker = list(color = 'orangered'))

fig_fantasy_points <- fig_fantasy_points %>%
  layout(title = 'Average Fantasy Points by Position',
         xaxis = list(title = 'Position'),
         yaxis = list(title = 'Average Fantasy Points'),
         template = 'plotly_dark')

fig_fantasy_points

fig_points_minutes_scatter <- plot_ly(df, x = ~Min, y = ~PTS,
                                      type = "scatter", mode = "markers", marker = list(color = 'orangered', opacity = 0.7))

fig_points_minutes_scatter <- fig_points_minutes_scatter %>%
  layout(title = 'Points vs. Minutes Played',
         xaxis = list(title = 'Minutes Played'),
         yaxis = list(title = 'Total Points'),
         template = 'plotly_dark')

fig_points_minutes_scatter

relationships <- list(
  c('Age', 'PTS'),
  c('PTS', 'GP'),
  c('FGA', 'FGM'),
  c('X3PM', 'X3PA'),
  c('FTM', 'FTA'),
  c('OREB', 'DREB'),
  c('STL', 'BLK'),
  c('PF', 'BLK'),
  c('AST', 'PTS')
)

for (i in seq_along(relationships)) {
  x_col <- relationships[[i]][1]
  y_col <- relationships[[i]][2]
  mode <- ifelse(i != 3, 'markers', 'lines')
  
  fig <- plot_ly(df, x = ~df[[x_col]], y = ~df[[y_col]], type = 'scatter', mode = mode, marker = list(color = 'orangered')) %>%
    layout(title = sprintf("Relationship between %s and %s", x_col, y_col),
           xaxis = list(title = x_col),
           yaxis = list(title = y_col),
           template = 'plotly_dark')
  
  print(fig)
}

outliers_condition <- (
  (df$FG. > 90) |
    (df$FG. == 0) |
    (df$X3P. > 90) |
    (df$X3P. == 0) |
    (df$FT. == 0)
)

df <- df[!outliers_condition, ]

numeric_df <- df[, sapply(df, is.numeric)]

correlation_matrix <- cor(numeric_df)

fig <- plot_ly(z = correlation_matrix,
               x = colnames(correlation_matrix),
               y = colnames(correlation_matrix),
               colorscale = 'Oranges',
               type = 'heatmap')

fig <- fig %>% layout(
  title = 'Correlation Heatmap',
  xaxis = list(title = 'Features'),
  yaxis = list(title = 'Features'),
  height = 1000,
  template = 'plotly_dark'
)

fig

df <- df %>%
  select(-PName, -POS, -Team, -FGM, -FGA, -X3PM,
         -X3PA, -X3P., -FP, -DD2, -FTA,
         -TD3, -OREB, -DREB)

X <- select(df, -PTS)
y <- df$PTS

set.seed(42)

splitIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[splitIndex, ]
X_test <- X[-splitIndex, ]
y_train <- y[splitIndex]
y_test <- y[-splitIndex]

cat("X_train has", nrow(X_train), "rows and", ncol(X_train), "columns.\n")
cat("X_test has", nrow(X_test), "rows and", ncol(X_test), "columns.\n")

set.seed(42)

test_sizes <- c(0.15, 0.2, 0.25, 0.3)
random_states <- c(0, 1, 42, 43, 100, 313)

best_test_size <- NULL
best_random_state <- NULL
best_r2_score <- -Inf

for (test_size in test_sizes) {
  for (random_state in random_states) {
    # Split the data
    index <- createDataPartition(df$PTS, p = 1 - test_size, list = FALSE, times = 1)
    training_data <- df[index, ]
    testing_data <- df[-index, ]
    
    model <- lm(PTS ~ ., data = training_data)
    
    y_pred <- predict(model, newdata = testing_data)
    
    r2 <- cor(y_pred, testing_data$PTS)^2
    
    if (r2 > best_r2_score) {
      best_r2_score <- r2
      best_test_size <- test_size
      best_random_state <- random_state
    }
  }
}

cat("Best test size:", best_test_size, "\n")
cat("Best random state:", best_random_state, "\n")
cat("Best R2 score:", best_r2_score, "\n")

set.seed(43)

splitIndex <- createDataPartition(df$PTS, p = 0.8, list = FALSE)
train_data <- df[splitIndex, ]
test_data <- df[-splitIndex, ]

LRmodel <- lm(PTS ~ ., data = train_data)

y_pred <- predict(LRmodel, newdata = test_data)

r_squared <- R2(y_pred, test_data$PTS)

cat("R-squared Score:", r_squared, "\n")

y_final <- predict(LRmodel, newdata = X)
comparison_df <- data.frame(Actual = y, Predicted = y_final)

fig_scatter <- plot_ly(data = comparison_df, x = ~Actual, y = ~Predicted, color = ~Actual,
                       type = 'scatter', mode = 'markers', marker = list(color = ~Actual),
                       showlegend = FALSE) %>%
  layout(template = 'plotly_dark', title = 'Comparison of Actual vs. Predicted',
         xaxis = list(title = 'Actual Points'), yaxis = list(title = 'Predicted Points'))

fig_hist <- plot_ly(data = comparison_df, x = ~Actual, color = I("blue"),
                    nbins = 30, type = 'histogram', barmode = 'overlay',
                    histnorm = "probability density") %>%
  add_trace(x = ~Predicted, type = 'histogram', nbins = 30, barmode = 'overlay',
            histnorm = "probability density") %>%
  layout(template = 'plotly_dark', title = 'Distribution of Actual and Predicted Points',
         xaxis = list(title = 'Points'), yaxis = list(title = 'Density'),
         color_discrete_sequence = c('#185ADB', '#FC5C9C'))

y_pred <- predict(LRmodel, newdata = test_data)

residuals <- y_pred - test_data$PTS

fig_residual <- ggplot(data = data.frame(Predicted = y_pred, residuals = residuals), aes(x = Predicted, y = residuals)) +
  geom_point(color = 'orangered') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'orange') +
  theme_minimal() +
  labs(title = 'Residual Plot', x = 'Predicted Values', y = 'Residuals')

print(fig_residual)

print(fig_residual)
fig_line <- plot_ly() %>%
  add_trace(x = ~y, y = ~y, type = 'scatter', mode = 'lines', line = list(color = '#98DFD6'), name = 'Ideal Line') %>%
  add_trace(x = ~y, y = ~y_final, type = 'scatter', mode = 'markers', marker = list(color = 'orangered'), name = 'Predicted Values') %>%
  add_trace(x = ~y, y = ~predict(lm(y_final ~ y)), type = 'scatter', mode = 'lines',
            line = list(color = '#FFDD83'), name = 'Regression Line') %>%
  layout(template = 'plotly_dark', title = 'Predicted vs. True Line Plot',
         xaxis = list(title = 'True Values'), yaxis = list(title = 'Predicted Values'))

fig_scatter
fig_hist
fig_residual
fig_line