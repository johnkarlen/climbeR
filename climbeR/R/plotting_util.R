#' Plot Second Order vs First Order, minimal depth of a maximal subtree,
#' averaged over the forest (shortened to "metric" for brevity)
#'
#' Given evaluated data from calculateAMDMS, plot the result.
#'
#' @import ggplot2
#' @param eval_data The output of calculateAMDMS.
#' @param plot_missing_so An optional parameter to show features that only
#' have a first order metric value. Variables can have high feature strength,
#' but may be unlikely to have a second maximal subtree because of low
#' cardinality.
#' @param add_text_labels An optional parameter to turn on text labels next
#' to the feature's dot on the plot.
#' @return so_vs_fo A ggplot2 object, which shows second order vs first
#' order average minimal depth of a maximal subtree depth.
#' @export
plotFirstAndSecondOrderMetric <- function(eval_data,
                                          plot_missing_so = FALSE,
                                          add_text_labels = FALSE) {

    # exclude features that only have a first order value for the metric
    plot_data <- eval_data |>
        dplyr::filter(second_order != -1) |>
        dplyr::arrange(second_order)

    # palette
    colors <- colorRampPalette(c("blue", "yellow", "red"))(nrow(plot_data))

    so_vs_fo <- ggplot(plot_data, aes(x = first_order,
                                      y = second_order,
                                      size = counts,
                                      color = factor(second_order)),
                       environment = environment())

    so_vs_fo <- so_vs_fo +
        geom_point() +
        xlab("First Order Depth") + ylab("Second Order Depth") +
        ggtitle("Minimal Depth of a Maximal Subtree\nAveraged Over Forest") +
        labs(size = "number of splits\nin forest", color = "feature") +
        xlim(min(plot_data$first_order) - 1, max(plot_data$first_order) + 1) +
        scale_colour_manual(breaks = rev(plot_data$second_order),
                            labels = rev(rownames(plot_data)),
                            values = rev(colors))

    if(add_text_labels){
        # offset the y coord of the text labels
        y_lab_offset = -2
        so_vs_fo <- so_vs_fo +
            geom_text(aes(label = rownames(plot_data)), size = 1, vjust = y_lab_offset)
    }

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

#' Plot metric vs number of splits per variable.
#'
#' Plots the metric against the number of splits in the forest. Features with
#' many splits will be weighed unfairly by the metric.
#'
#' @param eval_data The output of calculateAMDMS.
#' @param add_text_labels An optional parameter to turn on text labels next
#' to the feature's dot on the plot.
#' @return plot object
#' @export
plotAMDMSvsNumSplits <- function(eval_data,
                                 add_text_labels = FALSE) {

    # palette
    colors <- colorRampPalette(c("blue", "yellow", "red"))(nrow(eval_data))

    ns_vs_fo <- ggplot(eval_data, aes(x = first_order,
                                      y = counts,
                                      color = factor(first_order)),
                       environment = environment())

    ns_vs_fo <- ns_vs_fo +
        geom_point() +
        xlab("Average Minimal Depth\nof a Maximal Subtree") +
        ylab("Number of\nSplits") +
        ggtitle("Minimal Depth of a Maximal Subtree\nAveraged Over Forest") +
        labs(color = "feature") +
        xlim(min(eval_data$first_order), max(eval_data$first_order) + 1) +
        scale_colour_manual(breaks = rev(eval_data$first_order),
                            labels = rev(rownames(eval_data)),
                            values = rev(colors))

    if(add_text_labels){
        ns_vs_fo <- ns_vs_fo +
            geom_text(aes(label = rownames(eval_data)), size = 1)
    }

    return(ns_vs_fo)
}



#' Plot Second Order vs First Order minimal depth of a maximal subtree depth
#' for base R
#'
#' Base r version of plotFirstAndSecondOrderMetric (just an alternative).
#'
#' @param eval_data The output of calculateAMDMS.
#' @return plot object
#' @export
baseRPlotting <- function(eval_data) {
    so_vs_fo <- plot(eval_data$second_order ~ eval_data$first_order,
         xlab = "First Order Depth",
         ylab = "Second Order Depth",
         title = "Minimal Depth of a Maximal Subtree\nAveraged Over Forest",
         data = eval_data[, 1:2])
    with(eval_data[, 1:2],
         text(eval_data$second_order ~ eval_data$first_order,
              labels = eval_data[[3]], pos = 4))
    return(so_vs_fo)
}

#' Calculate the metric and make Second Order vs First Order plot
#'
#' Comprehensive function for calculating minimal depth of a maximal
#' subtree averaged over the forest and then plotting the result.
#'
#' @param ranger_result A ranger object from the ranger package, which was
#' created setting param write.forest to TRUE. In other words, it must have a
#' 'forest' property.
#' @param plot_missing_so An optional parameter to show features that only
#' have a first order metric value. Variables can have high feature strength,
#' but may be unlikely to have a second maximal subtree because of low
#' cardinality.
#' @return a list; element 1 is a data.frame containing subtree depth
#' data, element 2 is the plot of number of splits vs first order metric,
#' element 3 is a plot of second order vs first order.
#' @examples
#' require(survival)
#' library(ranger)
#' rg.veteran <- ranger(Surv(time, status) ~ ., data = veteran, write.forest =
#' TRUE)
#' result <- getAndPlotMetric(rg.veteran)
#' @export
getAndPlotMetric <- function(ranger_result,
                             plot_missing_so = FALSE){
    # get metric
    eval_data <- calculateAMDMS(ranger_result)
    # plot first perspective
    ns_vs_fo <- plotAMDMSvsNumSplits(eval_data)
    # plot second perspective
    so_vs_fo <- plotFirstAndSecondOrderMetric(eval_data,
                                              plot_missing_so)
    return(list(subtree_metrics = eval_data, ns_vs_fo_plot = ns_vs_fo,
                so_vs_fo_plot = so_vs_fo))
}
