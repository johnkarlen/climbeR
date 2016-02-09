#' Plot Second Order vs First Order, minimal depth of a maximal subtree,
#' averaged over the forest (shortened to "metric" for brevity)
#'
#' Given eval data from forestAvgMaxSubtree, plot the result
#'
#' @import ggplot2
#' @param eval_data, the output of forestAvgMaxSubtree
#' @return so_vs_fo, a ggplot2 object, which plots first order vs second
#' order minimal maximal subtree depth
#' @export
plotFirstAndSecondOrderMetric <- function(eval_data) {
    so_vs_fo <- ggplot(eval_data, aes(x = first_order,
                                      y = second_order,
                                      size = normalized_counts))

    so_vs_fo <- so_vs_fo +
        geom_point() +
        geom_text(aes(label = rownames(eval_data)),
                  hjust = -0.5, vjust = 0, size = 3) +
        xlab("First Order Depth") + ylab("Second Order Depth") +
        ggtitle("Minimal Maximal Subtree Depth") +
        labs(size = "normalized\nnumber of splits\nin forrest") +
        xlim(min(eval_data$first_order), max(eval_data$first_order) + 1)
    return(so_vs_fo)
}

#' Plot Second Order vs First Order minimal maximal subtree depth (base R)
#'
#' base r version of plotFirstAndSecondOrderMetric (just an alternative)
#'
#' @param eval_data, the output of forestAvgMaxSubtree
#' @export
baseRPlotting <- function(eval_data) {
    plot(eval_data$second_order ~ eval_data$first_order,
         xlab = "First Order Depth",
         ylab = "Second Order Depth",
         title = "Minimal Maximal Subtree Depth",
         data = eval_data[, 1:2])
    with(eval_data[, 1:2],
         text(eval_data$second_order ~ eval_data$first_order,
              labels = eval_data[[3]], pos = 4))
}

#' Calculate the metric, and make Second Ord. vs First Ord. plot
#'
#' comprehensive function for calculating minimal depth of a maximal 
#' subtree, averaged over the forest, and then plotting the result
#'
#' @param ranger_result ranger result with a saved $forest object.
#' (set write.forest to TRUE)
#' @return a list: element 1 is a data.frame containing subtree depth
#' data, element 2 is the plot
#' @examples
#' require(survival)
#' library(ranger)
#' rg.veteran <- ranger(Surv(time, status) ~ ., data = veteran, write.forest =
#' TRUE)
#' result <- getAndPlotFirstAndSecondOrderMetric(rg.veteran)
#' @export
getAndPlotFirstAndSecondOrderMetric <- function(ranger_result) {
    eval_data <- forestAvgMaxSubtree(ranger_result)
    so_vs_fo <- plotFirstAndSecondOrderMetric(eval_data)
    return(list(subtree_metrics = eval_data, plot = so_vs_fo))
}
