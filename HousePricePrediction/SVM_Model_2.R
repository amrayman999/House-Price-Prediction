library(caret)
library(readr)

# reading, preprocessing data and statistics
df <- read.csv("E:/ZEZO/fourth year/Second Term/Distributed Computing/project/data_science/PreprocessedData.csv")
#df <- subset(df, select = -c(Id))
#df <- df[, c("GarageArea", "GarageCars", "TotRmsAbvGrd","FullBath","GrLivArea","X1stFlrSF","TotalBsmtSF","YearRemodAdd","YearBuilt","OverallQual","SalePrice")]
View(df)

df <- data.frame(sapply(df, as.numeric))
corr <- cor(df, method = "pearson", use = "pairwise.complete.obs")
corr_output <- as.matrix(corr[, "SalePrice"])
print(corr_output)

# Apply threshold to select columns with correlation > 0.5
selected_cols <- colnames(corr)[abs(corr["SalePrice", ]) > 0.2]
# Select subset of data with columns below the correlation threshold
data <- df[, c(selected_cols)]
View(data)
print(selected_cols)

missing_values <- colSums(is.na(data))
print(missing_values)

# split data into training set and validation set
set.seed(123)
train_index <- createDataPartition(data$SalePrice, p = 0.8, list = FALSE)
train <- data[train_index, ]
valid <- data[-train_index, ]
View(train)
View(valid)

# fit SVM model on training set
model <- train(SalePrice ~ ., data = train , method = "svmRadial", trControl = trainControl(method = "cv", number = 10), preProcess = c("center", "scale"))
warnings()

#print model summary
summary(model)

# make prediction on validation set
predictions <- predict(model,valid)

# evaluate model performance
cv_results <- train(SalePrice~., data = data, method = "svmRadial", trControl = trainControl(method="cv", number=10), preProcess = c("center", "scale"))
print(cv_results)