if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}


# STEP 1 - Required Packages
## mlbench ----
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## factoextra ----
if (require("factoextra")) {
  require("factoextra")
} else {
  install.packages("factoextra", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## FactoMineR ----
if (require("FactoMineR")) {
  require("FactoMineR")
} else {
  install.packages("FactoMineR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## dplyr ----
if (!is.element("dplyr", installed.packages()[, 1])) {
  install.packages("dplyr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
require("dplyr")

#STEP 2 - Load the dataset

StudentPerformanceDataset <- read.csv("data/BI1StudentPerformanceDataset.csv")
View(StudentPerformanceDataset)
dim(StudentPerformanceDataset)
sapply(StudentPerformanceDataset, class)

variable_to_find <- c("YOB", 
                      "Coursework TOTAL: x/40 (40%)", 
                      "EXAM: x/60 (60%)",
                      "TOTAL = Coursework TOTAL + EXAM (100%)")

column_indices <- which(colnames(StudentPerformanceDataset) %in% variable_to_find)

# STEP 3 - Scale Data Transform

summary(StudentPerformanceDataset)

birth <- as.numeric(unlist(StudentPerformanceDataset[, 3]))
course <- as.numeric(unlist(StudentPerformanceDataset[, 97]))
exam <- as.numeric(unlist(StudentPerformanceDataset[, 98]))
total <- as.numeric(unlist(StudentPerformanceDataset[, 99]))

hist(birth, main = names(StudentPerformanceDataset)[3])
hist(course, main = names(StudentPerformanceDataset)[97])
hist(exam, main = names(StudentPerformanceDataset)[98])
hist(total, main = names(StudentPerformanceDataset)[99])

model_of_the_transform <- preProcess(StudentPerformanceDataset, method = c("scale"))
print(model_of_the_transform)
stdperf_data_scale_transform <- predict(model_of_the_transform, StudentPerformanceDataset)

summary(stdperf_data_scale_transform)

StudentPerformanceDataset_birth <- as.numeric(unlist(stdperf_data_scale_transform[, 3]))
hist(StudentPerformanceDataset_birth, main = names(stdperf_data_scale_transform)[3])

# STEP 4 - Apply Centre Basic Transform

summary(StudentPerformanceDataset)
model_of_the_transform <- preProcess(StudentPerformanceDataset, method = c("center"))
print(model_of_the_transform)
stdperf_data_center_transform <- predict(model_of_the_transform, StudentPerformanceDataset)
summary(stdperf_data_center_transform)

# STEP 5 - Apply a Standardize Data Transform

summary(StudentPerformanceDataset)
sapply(StudentPerformanceDataset[, 97], sd)
model_of_the_transform <- preProcess(StudentPerformanceDataset,
                                     method = c("scale", "center"))
print(model_of_the_transform)
stdperf_data_standardize_transform <- predict(model_of_the_transform, StudentPerformanceDataset) # nolint

summary(stdperf_data_standardize_transform)
sapply(stdperf_data_standardize_transform[, 97], sd)

# STEP 6 - Apply a Normalize Data Transform

summary(StudentPerformanceDataset)
model_of_the_transform <- preProcess(StudentPerformanceDataset, method = c("range"))
print(model_of_the_transform)
stdperf_data_normalize_transform <- predict(model_of_the_transform, StudentPerformanceDataset)
summary(stdperf_data_normalize_transform)

# STEP 7 - Apply a Box-Cox Power Transform

summary(stdperf_data_standardize_transform)

# Calculate the skewness before the Box-Cox transform
sapply(stdperf_data_standardize_transform[, 99],  skewness, type = 2)
sapply(stdperf_data_standardize_transform[, 99], sd)

model_of_the_transform <- preProcess(stdperf_data_standardize_transform,
                                     method = c("BoxCox"))
print(model_of_the_transform)
stdperf_data_box_cox_transform <- predict(model_of_the_transform,
                                       stdperf_data_standardize_transform)

summary(stdperf_data_box_cox_transform)

sapply(stdperf_data_box_cox_transform[, 99],  skewness, type = 2)
sapply(stdperf_data_box_cox_transform[, 99], sd)

# Calculate the skewness after the Box-Cox transform
sapply(stdperf_data_box_cox_transform[, 99],  skewness, type = 2)
sapply(stdperf_data_box_cox_transform[, 99], sd)

# STEP 8. Apply a Yeo-Johnson Power Transform

summary(stdperf_data_standardize_transform)

# Calculate the skewness before the Yeo-Johnson transform
sapply(stdperf_data_standardize_transform[, 3],  skewness, type = 2)
sapply(stdperf_data_standardize_transform[, 3], sd)

model_of_the_transform <- preProcess(stdperf_data_standardize_transform,
                                     method = c("YeoJohnson"))
print(model_of_the_transform)
stdperf_data_yeo_johnson_transform <- predict(model_of_the_transform, # nolint
                                           stdperf_data_standardize_transform)

# AFTER
summary(stdperf_data_yeo_johnson_transform)

# Calculate the skewness after the Yeo-Johnson transform
sapply(stdperf_data_yeo_johnson_transform[, 3],  skewness, type = 2)
sapply(stdperf_data_yeo_johnson_transform[, 3], sd)

