[![Build Status](https://travis-ci.org/willtimmy/bis557.svg?branch=master)](https://travis-ci.org/willtimmy/bis557)

BIS557
===

This is a repository for storing all code, documentation, and digital 
artifacts for BIS557.

1. linear regression
```{R}
library(bis557)
fit <- linear_model(Sepal.Length ~., iris)
summary(fit)
```

2. ridge regression
```{r}
data(ridge_train)
fit <- ridge_reg(y~., 1, ridge_train)
```

3. kernel density

4. sparse matrix