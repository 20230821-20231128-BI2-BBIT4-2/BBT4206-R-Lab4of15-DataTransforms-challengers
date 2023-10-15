if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

<<<<<<< HEAD
=======
library(readr)
StudentPerformanceDataset <- read.csv("data/StudentPerformanceDataset.csv")
View(StudentPerformanceDataset)
>>>>>>> main

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
<<<<<<< HEAD

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
sapply(StudentPerformanceDataset[, 3], sd)
model_of_the_transform <- preProcess(StudentPerformanceDataset,
                                     method = c("scale", "center"))
print(model_of_the_transform)
stdperf_data_standardize_transform <- predict(model_of_the_transform, StudentPerformanceDataset) # nolint

summary(stdperf_data_standardize_transform)
sapply(stdperf_data_standardize_transform[, 3], sd)

# STEP 6 - Apply a Normalize Data Transform

summary(StudentPerformanceDataset)
model_of_the_transform <- preProcess(StudentPerformanceDataset, method = c("range"))
print(model_of_the_transform)
stdperf_data_normalize_transform <- predict(model_of_the_transform, StudentPerformanceDataset)
summary(stdperf_data_normalize_transform)

# STEP 7 - Apply a Box-Cox Power Transform

summary(stdperf_data_standardize_transform)

# Calculate the skewness before the Box-Cox transform
sapply(stdperf_data_standardize_transform[, 3],  skewness, type = 2)
sapply(stdperf_data_standardize_transform[, 3], sd)

model_of_the_transform <- preProcess(stdperf_data_standardize_transform,
                                     method = c("BoxCox"))
print(model_of_the_transform)
stdperf_data_box_cox_transform <- predict(model_of_the_transform,
                                       stdperf_data_standardize_transform)

summary(stdperf_data_box_cox_transform)

sapply(stdperf_data_box_cox_transform[, 3],  skewness, type = 2)
sapply(stdperf_data_box_cox_transform[, 3], sd)

# Calculate the skewness after the Box-Cox transform
sapply(stdperf_data_box_cox_transform[, 3],  skewness, type = 2)
sapply(stdperf_data_box_cox_transform[, 3], sd)

# STEP 8 - Apply a Yeo-Johnson Power Transform

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

# STEP 9.a - PCA Linear Algebra Transform for Dimensionality Reduction

summary(StudentPerformanceDataset)

model_of_the_transform <- preProcess(StudentPerformanceDataset, method =
                                       c("scale", "center", "pca"))

print(model_of_the_transform)
stdperf_pca_dr <- predict(model_of_the_transform, StudentPerformanceDataset)

summary(stdperf_pca_dr)
dim(stdperf_pca_dr)

# STEP 9.b - PCA Linear Algebra Transform for Feature Extraction

stdperf_pca_fe <- princomp(cor(StudentPerformanceDataset[, 3]))
summary(stdperf_pca_fe)

factoextra::fviz_eig(stdperf_pca_fe, addlabels = TRUE)

stdperf_pca_fe$loadings[, 1:2]

factoextra::fviz_cos2(stdperf_pca_fe, choice = "var", axes = 1:2)
                      
factoextra::fviz_pca_var(stdperf_pca_fe, col.var = "cos2",
                         gradient.cols = c("red", "orange", "green"),
                         repel = TRUE)

# STEP 10 - ICA Linear Algebra Transform for Dimensionality Reduction

if (!is.element("fastICA", installed.packages()[, 1])) {
  install.packages("fastICA", dependencies = TRUE)
}
require("fastICA")

summary(StudentPerformanceDataset)

model_of_the_transform <- preProcess(StudentPerformanceDataset,
                                     method = c("scale", "center", "ica"),
                                     n.comp = 8)
print(model_of_the_transform)
stdperf_ica_dr <- predict(model_of_the_transform, StudentPerformanceDataset)

summary(stdperf_ica_dr)

=======
##data(StudentPerformanceDataset)
names()

stdperf <- StudentPerformanceDataset %>%
  select(class_group, YOB, `Quizzes and Bonus Marks`, `LabWork`, `CAT 1`, `CAT 2`, Coursework, EXAM, TOTAL)

summary(stdperf)
>>>>>>> main
