#' Plot Second Order vs First Order, minimal depth of a maximal subtree,
#' averaged over the forest (shortened to "metric" for brevity)
#'
#' Given eval data from calculateAMDMS, plot the result.
#'
#' @import ggplot2
#' @param eval_data The output of calculateAMDMS.
#' @param plot_missing_so An optional parameter to show features that only
#' have a first order metric value. Variables can have high feature strength, 
#' but may be unlikely to have a second maximal subtree because of low
#' cardinality.
#' @return so_vs_fo A ggplot2 object, which shows second order vs first
#' order average minimal depth of a maximal subtree depth.
#' @export
plotFirstAndSecondOrderMetric <- function(eval_data, 
                                          plot_missing_so = FALSE) {
    # exclude features that only have a first order value for the metric
    plot_data <- eval_data[eval_data$second_order != -1, ]
    
    so_vs_fo <- ggplot(plot_data, aes(x = first_order,
                                      y = second_order,
                                      size = counts),
                       environment = environment())
    
    so_vs_fo <- so_vs_fo +
        geom_point() +
        geom_text(aes(label = rownames(plot_data)), size = 3) +
        xlab("First Order Depth") + ylab("Second Order Depth") +
        ggtitle("Minimal Depth of a Maximal Subtree\nAveraged Over Forest") +
        labs(size = "number of splits\nin forrest") +
        xlim(min(plot_data$first_order), max(plot_data$first_order) + 1)
    
    # separate features that only have a first order value for the metric
    if( plot_missing_so || !any(eval_data$second_order == -1) ){
        only_first_order_data <- eval_data[eval_data$second_order == -1, ]
        so_vs_fo <- so_vs_fo +
            geom_vline(data = only_first_order_data,
                       xintercept = only_first_order_data$first_order,
                       linetype = 2, colour="blue") +
            geom_text(data = only_first_order_data, 
                      aes(first_order,
                          rep(max(plot_data$second_order),
                              nrow(only_first_order_data)),
                          label = rownames(only_first_order_data)),
                      size=3, colour="blue")
    }
    return(so_vs_fo)
}

#' Plot Second Order vs First Order minimal depth of a maximal subtree depth
#' for base R
#'
#' Base r version of plotFirstAndSecondOrderMetric (just an alternative).
#'
#' @param eval_data The output of calculateAMDMS.
#' @export
baseRPlotting <- function(eval_data) {
    plot(eval_data$second_order ~ eval_data$first_order,
         xlab = "First Order Depth",
         ylab = "Second Order Depth",
         title = "Minimal Depth of a Maximal Subtree\nAveraged Over Forest",
         data = eval_data[, 1:2])
    with(eval_data[, 1:2],
         text(eval_data$second_order ~ eval_data$first_order,
              labels = eval_data[[3]], pos = 4))
}

#' Calculate the metric, and make Second Order vs First Order plot
#'
#' Comprehensive function for calculating minimal depth of a maximal 
#' subtree averaged over the forest and then plotting the result.
#'
#' @param ranger_obj A ranger object from the ranger package, which was created
#' setting param write.forest to TRUE. In other words, it must have a 
#' 'forest' property.
#' #' @param plot_missing_so An optional parameter to show features that only
#' have a first order metric value. Variables can have high feature strength, 
#' but may be unlikely to have a second maximal subtree because of low
#' cardinality.
#' @return a list; element 1 is a data.frame containing subtree depth
#' data, element 2 is the plot.
#' @examples
#' require(survival)
#' library(ranger)
#' rg.veteran <- ranger(Surv(time, status) ~ ., data = veteran, write.forest =
#' TRUE)
#' result <- getAndPlotFirstAndSecondOrderMetric(rg.veteran)
#' @export
getAndPlotFirstAndSecondOrderMetric <- function(ranger_result, 
                                                plot_missing_so = FALSE){
    eval_data <- calculateAMDMS(ranger_result)
    so_vs_fo <- plotFirstAndSecondOrderMetric(eval_data, 
                                              plot_missing_so)
    return(list(subtree_metrics = eval_data, plot = so_vs_fo))
}
