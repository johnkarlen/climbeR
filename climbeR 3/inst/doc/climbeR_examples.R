## ---- fig.show='hold'----------------------------------------------------
require(ranger)

## Classification forest with default settings
ranger(Species ~ ., data = iris)

## Prediction
train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
iris.train <- iris[train.idx, ]
iris.test <- iris[-train.idx, ]
rg.iris <- ranger(Species ~ ., data = iris.train, write.forest = TRUE, importance = "impurity")
pred.iris <- predict(rg.iris, dat = iris.test)
table(iris.test$Species, pred.iris$predictions)

## ---- include=FALSE------------------------------------------------------
library(climbeR)

# call to climber function
result <- getAndPlotFirstVsSecondOrderMetric(rg.iris)
# ^ evaluated data ^
eval_data <- result$subtree_metrics
# second order vs first order plot
so_vs_fo <- result$plot

## ---- eval=FALSE---------------------------------------------------------
#  library(climbeR)
#  # call to climber function
#  result <- getAndPlotFirstVsSecondOrderMetric(rg.iris)
#  # ^ evaluated data ^
#  eval_data <- result$subtree_metrics
#  # second order vs first order plot
#  so_vs_fo <- result$plot

## ---- fig.width=6, fig.height=6------------------------------------------
# let's take a look
plot(so_vs_fo)

## ------------------------------------------------------------------------
knitr::kable(rg.iris$variable.importance)

## ---- results='asis'-----------------------------------------------------
# call to climber function
result <- getAndPlotFirstVsSecondOrderMetric(rg.iris)
# evaluated data 
eval_data <- result$subtree_metrics
# another look at the result 
knitr::kable(eval_data)

## ---- fig.show='hold'----------------------------------------------------
library(ranger)
require(survival)

# execute RSF (ranger), on the veteran data set
rg.veteran <- ranger(Surv(time, status) ~ .,
                     data = veteran,
                     write.forest = TRUE)

# retrieve result of variable importance (VIMP) for comparison later
vimp <- data.frame(rg.veteran$variable.importance)

## ---- include=FALSE------------------------------------------------------
library(climbeR)
# call to climber function
result <- getAndPlotFirstVsSecondOrderMetric(rg.veteran)
# ^ evaluated data ^
eval_data <- result$subtree_metrics
# second ord. vs first ord. plot
so_vs_fo <- result$plot

## ---- eval=FALSE---------------------------------------------------------
#  library(climbeR)
#  # call to climber function
#  result <- getAndPlotFirstVsSecondOrderMetric(rg.veteran)
#  # evaluated data
#  eval_data <- result$subtree_metrics
#  # second ord. vs first ord. plot
#  so_vs_fo <- result$plot

## ---- fig.width=6, fig.height=6------------------------------------------
# let's take a look
plot(so_vs_fo)

## ---- results='asis'-----------------------------------------------------
# another look at the result 
knitr::kable(eval_data)

## ---- include = FALSE----------------------------------------------------
# We'll use some dplyr goodness here to mutate the df
library(dplyr)

## ------------------------------------------------------------------------
# this function generates noise features, which contain no information
addNoise <- function(input_df, n, discrete = FALSE){
    df.len <- length(input_df[[1]])
    for(i in 1:n){
        #add a categorical noise variable w/ cardinality: 4
        noise_vect <- rnorm(df.len)
        if(discrete){
            #create RV w/ cardinality 4 (same as celltype)
            noise_vect <- sample(c(0, 1, 2, 3), df.len, replace = TRUE)
        }
        name <- paste0("n_", toString(i))
        input_df <- input_df %>% mutate(noise_vect)
        names(input_df)[length(input_df)] <- name
    }
    return(input_df)
}

## ---- include=FALSE------------------------------------------------------
# make some noise!
veteran <- addNoise(veteran, 5)

# rerun RSF (ranger), on the veteran data set
rg.veteran <- ranger(Surv(time, status) ~ .,
                     data = veteran,
                     write.forest = TRUE)

# call to climber function
result <- getAndPlotFirstVsSecondOrderMetric(rg.veteran)
# ^ evaluated data ^
eval_data <- result$subtree_metrics
# second ord. vs first ord. plot
so_vs_fo <- result$plot

## ---- eval=FALSE---------------------------------------------------------
#  # make some noise!
#  veteran <- addNoise(veteran, 5)
#  
#  # rerun RSF (ranger), on the veteran data set
#  rg.veteran <- ranger(Surv(time, status) ~ .,
#                       data = veteran,
#                       write.forest = TRUE)
#  
#  # call to climber function
#  result <- getAndPlotFirstVsSecondOrderMetric(rg.veteran)
#  # ^ evaluated data ^
#  eval_data <- result$subtree_metrics
#  # second ord. vs first ord. plot
#  so_vs_fo <- result$plot

## ---- fig.width=6, fig.height=6------------------------------------------
# let's take a look
plot(so_vs_fo)

## ---- include=FALSE------------------------------------------------------
# just removing the noise features by reloading the dataset
data(veteran)
# make some noise!
veteran <- addNoise(veteran, 5, discrete = TRUE)

# rerun RSF (ranger), on the veteran data set
rg.veteran <- ranger(Surv(time, status) ~ .,
                     data = veteran,
                     write.forest = TRUE)

# call to climber function
result <- getAndPlotFirstVsSecondOrderMetric(rg.veteran)
# ^ evaluated data ^
eval_data <- result$subtree_metrics
# second ord. vs first ord. plot
so_vs_fo <- result$plot

## ---- eval=FALSE---------------------------------------------------------
#  # just removing the noise features by reloading the dataset
#  data(veteran)
#  # make some noise!
#  veteran <- addNoise(veteran, 5, discrete = TRUE)
#  
#  # rerun RSF (ranger), on the veteran data set
#  rg.veteran <- ranger(Surv(time, status) ~ .,
#                       data = veteran,
#                       write.forest = TRUE)
#  
#  # call to climber function
#  result <- getAndPlotFirstVsSecondOrderMetric(rg.veteran)
#  # ^ evaluated data ^
#  eval_data <- result$subtree_metrics
#  # second ord. vs first ord. plot
#  so_vs_fo <- result$plot

## ---- fig.width=6, fig.height=6------------------------------------------
# let's take a look
plot(so_vs_fo)

