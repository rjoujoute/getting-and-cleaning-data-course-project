rm(list=ls())
library(base)
library(utils)
library(data.table)

download.data <- function () {
  zip.url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  zip.file <- 'dataset.zip'
  
  download.file(zip.url, destfile = zip.file, method = 'curl')
  unzip(zip.file)
}

load.dataset <- function (set, features, labels) {

  prefix <- paste(set, '/', sep = '')
  file.data <- paste(prefix, 'X_', set, '.txt', sep = '')
  file.label <- paste(prefix, 'y_', set, '.txt', sep = '')
  file.subject <- paste(prefix, 'subject_', set, '.txt', sep = '')

  data <- read.table(file.data)[, features$index]
  names(data) <- features$name
  
  label.set <- read.table(file.label)[, 1]
  data$label <- factor(label.set, levels=labels$level, labels=labels$label)
  
  subject.set <- read.table(file.subject)[, 1]
  data$subject <- factor(subject.set)
  
  data.table(data)
}

run.analysis <- function () {
  setwd('/Users/rodneyjoujoute/UCI HAR Dataset-2')
  
  feature.set <- read.table('features.txt', col.names = c('index', 'name'))
  features <- subset(feature.set, grepl('-(mean|std)[(]', feature.set$name))
  
  label.set <- read.table('activity_labels.txt', col.names = c('level', 'label'))
  
  train.set <- load.dataset('train', features, label.set)
  test.set <- load.dataset('test', features, label.set)
  
  dataset <- rbind(train.set, test.set)
  
  tidy.dataset <- dataset[, lapply(.SD, mean), by=list(label, subject)]

  names <- names(tidy.dataset)
  names <- gsub('-mean', 'Mean', names) 
  names <- gsub('-std', 'Std', names) 
  names <- gsub('[()-]', '', names) 
  names <- gsub('BodyBody', 'Body', names) 
  setnames(tidy.dataset, names)
  
  setwd('..')
  write.table(dataset, file = 'rawdata.csv', row.names = FALSE)
  write.table(tidy.dataset, file = 'tidydata.txt',
            row.names = FALSE, quote = FALSE)
  
  tidy.dataset
}
