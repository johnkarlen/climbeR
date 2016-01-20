library(ranger)
library(dplyr)
library(ggplot2)
library(xtable)
require(survival)
source("plotting_util.R")
source("tree_traversal_util.R")

# add n continuous noise vectors to a data set
addContinuousNoise <- function(input_df, n){
    df.len <- length(input_df[[1]])
    for(i in 1:n){
        noise_vect <- rnorm(df.len)
        name <- paste0("nc_", toString(i))
        input_df <- input_df %>% mutate(noise_vect)
        names(input_df)[length(input_df)] <- name
    }
    return(input_df)
}

addDiscreteNoise <- function(input_df, n){
    df.len <- length(input_df[[1]])
    for(i in 1:n){
        #add a categorical noise variable w/ cardinality: 4
        noise_vect <- round(rnorm(df.len)*2)
        name <- paste0("nd_", toString(i))
        input_df <- input_df %>% mutate(noise_vect)
        names(input_df)[length(input_df)] <- name
    }
    return(input_df)
}

########################## MAIN ###############################

#turn these lines on to add discrete/continuous noise vars to the veteran data set
veteran <- addDiscreteNoise(veteran, 5)
veteran <- addContinuousNoise(veteran, 5)

#execute RSF (ranger)
rg.veteran <- ranger(Surv(time, status) ~ .,
                     data = veteran,
                     write.forest = TRUE,
                     min.node.size = 4,
                     num.trees = 1000,
                     importance = "permutation",
                     splitrule = "logrank")

#retrieve result of variable importance (VIMP) for comparison
vimp <- data.frame(rg.veteran$variable.importance)

#plotting
result <- getAndPlotSOvsFO(rg.veteran)
eval_data <- result$subtree_metrics
so_vs_fo <- result$plot
result <- cbind(eval_data, "vimp" = vimp[match(rownames(eval_data), rownames(vimp)),])

#save results
if(!file.exists("./out")){system("mkdir out/")}
ggsave(filename = "out/so_vs_fo_tests.png", plot = so_vs_fo, width = 7, height = 7)
results_table <- xtable(eval_data)
print(results_table, type = "html", file = "out/table.html")
