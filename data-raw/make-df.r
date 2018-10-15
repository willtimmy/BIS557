setwd('/Users/willtimmy/Desktop/Documents/My Document/Schools/Yale University/Courses/Biostatistics & Statistics/BIS 557/HW/bis557/data-raw')
lm_patho <- read.csv("df.csv")
dir.create("../data")
save(lm_patho, file = "../data/lm_patho.rda")

ridge_train <- read.csv("ridge_train.csv")
save(ridge_train, file = "../data/ridge_train.rda")

ridge_test <- read.csv("ridge_test.csv")
save(ridge_test, file = "../data/ridge_test.rda")
