###############################################################################
# importing and installing libraries
library(caret)
library(readr)
library(ggplot2)
dev.off()
###############################################################################
# reading, preprocessing data and statistics

# reading the data
data <- read.csv("OnlyNumeric_Data.csv")
data <- subset(data, select = -c(Id))
View(data)

# Checking if there are any nan values
missing_values <- colSums(is.na(data))
print(missing_values)

# Plot the correlation matrix using ggplot2
data <- data.frame(sapply(data, as.numeric))
corr <- cor(data, method = "pearson", use = "pairwise.complete.obs")
cor_matrix <- as.matrix(corr[, "SalePrice"])
cor_df <- reshape2::melt(cor_matrix)
ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "", y = "") +
  theme_minimal()
print(cor_matrix)

# Select subset of data with columns above the correlation threshold
selected_cols <- colnames(corr)[abs(corr["SalePrice", ]) > 0.5]
data <- data[, c(selected_cols)]
View(data)


################################################################################
#-------------------------------------------------------------------------------
# Training , Validation and Plotting 


# split data into training set and validation set
set.seed(123)
train_index <- createDataPartition(data$SalePrice, p = 0.8, list = FALSE)
train <- data[train_index, ]
valid <- data[-train_index, ]

# fit linear model on training set
model <- lm(SalePrice ~ ., data = train)

#print model summary
summary(model)

# make prediction on validation set
predictions <- predict(model,valid)

# evaluate model performance
cv <- trainControl(method="cv",number=10)
cv_results <- train(SalePrice~.,data=data,method = "lm",trControl=cv)
print(cv_results)
