plotFirstAndSecondOrderMetric <- function (eval_data,
                                           plot_missing_so = FALSE,
                                           add_text_labels = FALSE) {
  first_order <- second_order <- counts <- NULL
  plot_data <- eval_data[eval_data$second_order != -1, ]
  plot_data <- plot_data[order(plot_data$second_order), ]
  plot_data$position <- 1:nrow(plot_data)
  colors <- (grDevices::colorRampPalette(c("red", "yellow", 
                                           "blue")))(nrow(plot_data))
  so_vs_fo <- ggplot(plot_data, aes(x = first_order,
                                    y = second_order, 
                                    size = counts,
                                    color = factor(position)),
                     environment = environment())
  so_vs_fo <- so_vs_fo + geom_point() + xlab("First Order Depth") + 
    ylab("Second Order Depth") +
    ggtitle("") + # Minimal Depth of a Maximal Subtree\nAveraged Over Forest
    labs(size = "Number of splits\nin forest", color = "Feature") + 
    xlim(min(plot_data$first_order) - 1, max(plot_data$first_order) + 
           1) + scale_colour_manual(breaks = rev(plot_data$position),
                                    labels = rev(rownames(plot_data)),
                                    values = rev(colors)) 
  if (add_text_labels) {
    y_lab_offset = -2
    so_vs_fo <- so_vs_fo + geom_text(aes(label = rownames(plot_data)), 
                                     size = 1, vjust = y_lab_offset)
  }
  if (plot_missing_so || !any(eval_data$second_order == -1)) {
    only_first_order_data <- eval_data[eval_data$second_order == 
                                         -1, ]
    so_vs_fo <- so_vs_fo + geom_vline(data = only_first_order_data, 
                                      xintercept = only_first_order_data$first_order,
                                      linetype = 2, 
                                      colour = "red") +
      geom_text(data = only_first_order_data, 
      aes(first_order, rep(max(plot_data$second_order), 
      nrow(only_first_order_data)), label = rownames(only_first_order_data)), 
                                            size = 3, colour = "red")
  }
  return(so_vs_fo)
}