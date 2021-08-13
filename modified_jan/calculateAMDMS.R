calculateAMDMS <- function(ranger_obj) 
{
  variable_id <- NULL
  if (!("forest" %in% names(ranger_obj))) {
    stop("no forest attribute present in ranger result.")
  }
  forest <- ranger_obj$forest
  for (i in seq_along(forest$split.varIDs)) {
    x <- ranger::treeInfo(ranger_obj, i)
    forest$split.varIDs[[i]] <- x$splitvarID + 1 
  }
  binned_forest <- binForestByDepth(forest)
  var_ids <- binned_forest$variable_ids_used
  avg_fom_depths <- c()
  avg_som_depths <- c()
  for (var_depth_bins in binned_forest[[1]]) {
    var_fom_depths <- c()
    var_som_depths <- c()
    for (tree_depths in var_depth_bins) {
      var_fom_depths <- c(var_fom_depths, tree_depths[1])
      if (length(tree_depths) > 1) {
        var_som_depths <- c(var_som_depths, tree_depths[2])
      }
    }
    fom_depth <- mean(var_fom_depths, na.rm = T)
    if (length(var_som_depths) > 0) {
      som_depth <- mean(var_som_depths, na.rm = T)
    }
    else {
      som_depth <- -1
    }
    avg_fom_depths <- c(avg_fom_depths, fom_depth)
    avg_som_depths <- c(avg_som_depths, som_depth)
  }
  result <- data.frame(avg_fom_depths, avg_som_depths, var_ids)
  names(result) <- c("first_order", "second_order", "variable_id")
  splits_per_var <- countSplitsPerVar(forest)
  splits_per_var <- splits_per_var[match(result[["variable_id"]], 
                                         splits_per_var[["var_ids"]]), ]
  result <- cbind(result, splits_per_var)
  result <- result[order(result$first_order), ]
  result <- subset(result, select = -c(var_ids, variable_id))
  result
}
