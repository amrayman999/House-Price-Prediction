# importing libraries
install.packages("corrplot")
library(corrplot)
install.packages("dplyr")
library(dplyr)

# select only numeric variables
numeric_df <- data %>% select_if(is.numeric)
# getting dataset
data <- read.csv("train.csv")
df_filled <- apply(numeric_df, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
write.csv(df_filled, "OnlyNumeric_Data.csv", row.names = FALSE)
View(df_filled)
#statistics -------------------------------------------------------------------------------------------------------
print(ncol(data)) #---> 81 columns before preprocessing , 75 after preprocessing 
View(data)    # ---> view data
print(colnames(data)) #---> colummn names

missing_values <- colSums(is.na(data))
print(missing_values)       #-----> numbers of nan values in each column

# get data types of each column in dataframe
types <- sapply(data, class)
# print the data types
print(types)


# print correlations 
data <- data.frame(sapply(data, as.numeric))
corr <- cor(data, method = "pearson", use = "pairwise.complete.obs")
corr_output <- as.matrix(corr[, "SalePrice"])
dim(corr_output)
corrplot(corr_output, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
#------------------------------------------------------------------------------------------------------------------

# data preprocessing ----------------------------------------------------------------------------------------------

# handling nan values in columns

# Replace missing values in LotFrontage with the mean of the column
lotfrontage_mean <- mean(data$LotFrontage, na.rm = TRUE)
data$LotFrontage[is.na(data$LotFrontage)] <- lotfrontage_mean


# dropping MiscFeature , Fence , Alley , FireplaceQu , PoolQC columns (all have hude number of nan values)
data <- subset(data, select = -c(MiscFeature))
data <- subset(data, select = -c(Fence,Alley,FireplaceQu,PoolQC))
data <- subset(data, select = -c(Id))

# filling Nan values of categorical columns (MasVnrType , MasVnrArea , BsmtQual , BsmtCond , BsmtExposure , BsmtFinType1 , BsmtFinType2 , Electrical , GarageType , GarageYrBlt, GarageFinish , GarageQual , GarageCond) with mode value
cat_cols <- c('MasVnrType' , 'MasVnrArea' , 'BsmtQual' , 'BsmtCond' , 'BsmtExposure' , 'BsmtFinType1' , 'BsmtFinType2' , 'Electrical' , 'GarageType' , 'GarageYrBlt', 'GarageFinish' , 'GarageQual' , 'GarageCond')
for(col in cat_cols) {
# get the mode of the column
col_mode <- names(sort(table(data[, col]), decreasing = TRUE))[1]
# replace Nan values with mode
data[is.na(data[, col]), col] <- col_mode
}

# apply one hot encoding for all categorical columns ()
cat_cols <- c("MSZoning","Street","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood",
              "Condition1","Condition2","BldgType","HouseStyle","RoofStyle","RoofMatl","Exterior1st","Exterior2nd","MasVnrType",
              "ExterQual","ExterCond","Foundation","BsmtQual","BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
              "Heating","HeatingQC","CentralAir","Electrical","KitchenQual","Functional","GarageType","GarageFinish",
              "GarageQual","GarageCond","PavedDrive","SaleType","SaleCondition")

for(col in cat_cols) {
  # create one-hot encoding using the model.matrix function
  one_hot <- model.matrix(~0 + data[, col])
  
  # rename columns in one-hot encoding matrix
  col_names <- gsub(paste0(col, "\\."), "", colnames(one_hot))
  colnames(one_hot) <- col_names
  
  # add one-hot encoding matrix to original dataframe
  data <- cbind(data, one_hot)
  
  # remove original categorical column from the dataframe
  data[, col] <- NULL
}

write.csv(data, "PreprocessedData.csv", row.names = FALSE)
#------------------------------------------------------------------------------------------------------------------