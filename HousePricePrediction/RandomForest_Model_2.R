############### Data##########################
data <- read.csv("PreprocessedData.csv")
column_names <- colnames(data)
print(column_names) 
############ X and Y data##########

y_column <-"SalePrice"
y <- data["SalePrice"]
print(nrow(y))

######################################
y_N <- data$SalePric
#X <- data[, -which(names(data) == "SalePrice")]
X <- data[, c("GarageArea", "GarageCars", "TotRmsAbvGrd","FullBath","GrLivArea","X1stFlrSF","TotalBsmtSF","YearRemodAdd","YearBuilt","OverallQual")]
column_names <- colnames(X)
column_names
#X <- X[, 1:10]
num_cols <- ncol(X)
print(num_cols)
print(X)
#corr <- cor(X, method = "pearson", use = "pairwise.complete.obs")
#corr_output <- as.matrix(corr[, "SalePrice"])
#print(corr_output)
#############################################################
# select columns to standardize
cols_to_scale <- c("GarageArea", "GarageCars", "TotRmsAbvGrd","FullBath","GrLivArea","X1stFlrSF","TotalBsmtSF","OverallQual")

# standardize selected columns
scaled_data <- X
scaled_data[, cols_to_scale] <- scale(X[, cols_to_scale])
print(scaled_data)
###############################################################

install.packages("caret")
install.packages('randomForest')
library(caret) 
library(randomForest)

############################################


train_index <- createDataPartition(data[["SalePrice"]], p = 0.7, list = FALSE)
print(nrow(data))
train_X <- scaled_data[train_index, ]
train_y <-y[train_index]
test_X <- scaled_data[-train_index, ]
test_y <-y[-train_index]
print(nrow((train_X)))
print(nrow((train_y)))
print(nrow((test_X)))
print(nrow((test_y)))

print(train_X)

################################################################

# fit random forest model
rf_model <- randomForest(train_X, train_y, ntree = 51)

# print the model summary
print(rf_model)




pred_y <- predict(rf_model, test_X)
mse <- mean((pred_y - test_y)^2)

# print the result
mse
rmse <- sqrt(mse)

# print the result
rmse

