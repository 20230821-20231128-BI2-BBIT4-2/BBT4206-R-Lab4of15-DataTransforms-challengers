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

#crop_dataset <- read_csv("data/BI1StudentPerformanceDataset.csv",
#                         col_types = cols(
#                           class_group = col_factor(levels = c("A", "B", "C")),
#                           YOB = col_factor(levels = c("1998","1999", "2000", "2001", "2002", "2003")),
#                           GRADE = col_factor(levels = c("A", "B", "C", "D", "E", "F"))
#                         )
#)

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
