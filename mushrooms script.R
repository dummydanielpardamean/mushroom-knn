# Declare Func and import library
library(class)

convertToNominal <- function(x){
	dataNominal <- x
	numberOfColumns <- ncol(x)
	for(i in 1:numberOfColumns){
	  columnName <- colnames(dataNominal)[i]
	  dataNominal[[columnName]] <- as.numeric(as.factor(dataNominal[[columnName]]))
	}
	return (dataNominal)
}

normalize_func <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# Set dir
setwd('C:/Users/My Chick/Documents/mushrooms')
# Import .csv file
data <- read.csv("mushrooms.csv")
# Hapus "veil.type" kolom pada data
data <- subset(data, select = -veil.type)
# Convert data ke nominal
dataNominal <- convertToNominal(data)

# Normalisasi data
normalized <- as.data.frame(lapply(dataNominal, normalize_func))

dataHasNoMissingValue <- any(is.na(data))
dataNominalHasNoMissingValue <- any(is.na(dataNominal))
normalizedHasNoMissingValue <- any(is.na(normalized))
# Data train dimulai dari row 1-6000 dan exclude kolom 1
data_train <- normalized[1:6000, -1]
# Data train dimulai dari row 6001:8124 dan exclude kolom 1
data_test <- normalized[6001:8124, -1]
# Ambil class label untuk data train
data_train_label <- normalized[1:6000, 1]
# Ambil class label untuk data test
data_test_label <- normalized[6001:8124, 1]

data_trainHasNoMissingValue <- any(is.na(data_train))
data_testHasNoMissingValue <- any(is.na(data_test))

data_test_pred <- knn(data_train, data_test, data_train_label, k=10)

table(data_test_label)
table(data_test_pred)
table(data_test_pred, data_test_label)