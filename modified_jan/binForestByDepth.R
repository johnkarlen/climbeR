binForestByDepth <- function(forest) {
  trees <- forest$split.varIDs
  for (i in seq_along(trees)) {
    trees[[i]] <- ifelse(is.na(trees[[i]]), 0, trees[[i]])
  }
  num_trees <- forest$num.trees
  depth_bins <- list()
  forest_depths <- c()
  var_id_dump <- unlist(trees)
  var_id_set <- unique(var_id_dump[var_id_dump != 0])
  num_vars <- length(var_id_set)
  for (var in 1:num_vars) {
    depth_bins[[var]] <- vector(mode = "list", length = num_trees)
  }
  for (tree_idx in 1:num_trees) {
    tree_split_varIDs <- trees[[tree_idx]]
    tree_depth_bins <- startRecursiveDepthBinning(tree_split_varIDs)
    forest_depths <- c(forest_depths, length(tree_depth_bins))
    for (depth in seq_along(tree_depth_bins)) {
      for (var_id in tree_depth_bins[[depth]]) {
        var_idx <- match(var_id, var_id_set)
        depth_bins[[var_idx]][[tree_idx]] <- c(depth_bins[[var_idx]][[tree_idx]], 
                                               depth)
      }
    }
  }
  list(depth_bins = depth_bins, forest_depths = forest_depths, 
              variable_ids_used = var_id_set)
}


