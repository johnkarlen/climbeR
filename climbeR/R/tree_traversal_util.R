#' Recursive Depth Binning
#'
#' Function to recursively traverse depths of a tree.
#'
#' @param node_list Must be in the format of elements in the Ranger package's
#' forest$split.varIDs, which is a list representation of one tree in the forest.
#' Recursion is done by counting the number of terminal nodes at the current
#' depth to anticipate the correct number of nodes at the next depth.
#' @param depth Each recursive call must know the current depth in the tree.
#' @param expected_num_children The number of nodes in the current depth must
#' be anticipated, given the number of terminal nodes at the previous depth.
#' @param binned_depths A list passed between recursive calls, to store
#' results.
#' @return A list of vectors, where elements correspond to depths,
#' and vectors contain variable ID's of variables used to split at that depth.
recursiveDepthBinning <- function(node_list,
                                  depth,
                                  expected_num_children,
                                  binned_depths) {
  # take expected number of children at current depth
  children <- node_list[1:expected_num_children]
  # nodelist is subset to all nodes at depths lower than current depth
  node_list <- node_list[(expected_num_children + 1):length(node_list)]
  # store and count the # of non-leaves at the current depth
  nodes <- children[!is.na(children)]
  # calculate expected number of children in the next recursion
  expected_num_children <- 2 * length(nodes)
  ### BASE CASE ### base case (no nodes at current depth)
  if (expected_num_children == 0) {return(binned_depths)}
  # update binned depths
  binned_depths[[depth + 1]] <- nodes
  ### RECURSION ### recursive call on next depth
  binned_depths <- recursiveDepthBinning(node_list,
                                         depth + 1,
                                         expected_num_children,
                                         binned_depths)
  return(binned_depths)
}

#' Start Recursive Depth Binning
#'
#' The starter function for the recursion in recursiveDepthBinning.
#'
#' @param tree_split_varIDs Given one element of a 'split.varIDs' list, this
#' function will pass it to the recursiveDepthBinning function to bin the tree
#' by depth, starting at the root.
#' @return A list with an element per depth encountered. Each
#' element is a vector of variable IDs
startRecursiveDepthBinning <- function(tree_split_varIDs) {
  binned_depths <- list()
  binned_depths <- recursiveDepthBinning(tree_split_varIDs,
                                         0, 1, binned_depths)
  return(binned_depths)
}


#' Bin Forest by Depth
#'
#' Given a forest object from the ranger package, this function will bin the
#' forest into depths. This is a helper function for the 'calculateAMDMS'
#' function.
#'
#' @param ranger_obj A ranger object from the ranger package, which was created
#' with param write.forest set to TRUE. In other words, it must have a
#' 'forest' property.
#' @return A list with 3 elements. The first is a list of vectors -
#' one for each independent variable ocurring in the forest (this may not
#' be the complete set of independent variables, but we will account for any
#' variables that do not occur in the forest later). Each vector contains all
#' minimal depths of maximal subtrees in the forest, for the corresponding
#' independent variable. The second element is a vector of tree heights
#' 'forest_depths'. The third element is a set of variable id's for matching to
#' independent variable names.
binForestByDepth <- function(ranger_obj) {
  trees <- ranger_obj$split.varIDs

  # return these data structures, once populated
  depth_bins <- list()
  forest_depths <- c()

  # get all non-zero var ID's, (0 represents a leaf node)
  var_id_set <- ranger_obj$split.varIDs |>
    unlist() |>
    unique() |>
    stats::na.omit()

  # number of vars that occur in the forest
  num_vars <- length(var_id_set)

  # preallocate an array for each var's list of subtree depths
  for (var in 1:num_vars) {
    depth_bins[[var]] <- vector(mode = "list",
                                length = ranger_obj$num.trees)
  }

  # iterate over the forest
  for (tree_idx in 1:ranger_obj$num.trees) {
    # bin each tree
    tree_depth_bins <- startRecursiveDepthBinning(trees[[tree_idx]])
    # store the depth of each tree
    forest_depths <- c(forest_depths, length(tree_depth_bins))
    # add results to depth_bins structure
    for (depth in 1:length(tree_depth_bins)) {
      for (var_id in tree_depth_bins[[depth]]) {
        # find variable index
        var_idx <- match(var_id, var_id_set)
        depth_bins[[var_idx]][[tree_idx]] <-
          c(depth_bins[[var_idx]][[tree_idx]], depth)
      }
    }
  }
  return(list(depth_bins = depth_bins,
              forest_depths = forest_depths,
              variable_ids_used = var_id_set))
}


#' Count Splits Per Variable
#'
#' This function counts the number of times each variable was used to split a
#' tree.
#'
#' @param ranger_obj A ranger object from the ranger package, which was created
#' with param write.forest set to TRUE. In other words, it must have a
#' 'forest' property.
#' @return A dataframe with one column of counts, and one column of
#' normalized counts. Rows are labeled by variable names.
countSplitsPerVar <- function(ranger_obj) {
  trees <- ranger_obj$split.varIDs
  counts <- c()
  # check this, to see if we need to offset var id's by 1
  status_var_exists <- ("status.varID" %in% attributes(ranger_obj)$names)
  # we need a count for every independent var (some may be 0)
  num_ind_vars <- length(ranger_obj$independent.variable.names)

  # concatenate all tree info
  splitsPerVar <- lapply(1:ranger_obj$num.trees,
                         function(t){ranger::treeInfo(object = ranger_obj,
                                                      tree = t)
                         }) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(splitvarID)) |>
    dplyr::group_by(splitvarName, splitvarID) |>
    dplyr::summarise(counts = dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(normalized_counts = counts/sum(counts)) |>
    dplyr::arrange(splitvarID)

  # find any vars not used in forest
  missing_vars <- setdiff(ranger_obj$independent.variable.names,
                          splitsPerVar$splitvarName)

  if(length(missing_vars) > 0) splitsPerVar <- splitsPerVar |>
    rbind(data.frame(splitvarName = missing_vars,
                     splitvarID = NA,
                     counts = 0,
                     normalized_counts = 0))

  rownames(splitsPerVar) <- ranger_obj$independent.variable.names
  return(splitsPerVar)
}

#' Forest Averaged Minimal Depth of a Maximal Subtree (AMDMS)
#'
#' Given a result from the Ranger package (write.forest must
#' be set to TRUE), this function will traverse the trees and calculate the
#' first and second order average minimal depth of a maximal subtree.
#'
#' @param ranger_obj A ranger object from the ranger package, which was created
#' with param write.forest set to TRUE. In other words, it must have a
#' 'forest' property.
#' @return A data.frame with two columns: averaged first and second order
#' minimal depth of a maximal subtree.
#' @export
calculateAMDMS <- function(ranger_obj) {
  if(!("forest" %in% names(ranger_obj))){
    stop("no forest attribute present in ranger result.
             Please run Ranger with write_forest set to TRUE")
  }

  forest <- ranger_obj$forest

  # Jan Brederecke's modification:
  # Adjusts for the new ranger forest format, which moved the information we
  # used to get from forest$split.varIDs to a DF retrievable with
  # ranger::treeInfo()
  for (i in seq_along(forest$split.varIDs)) {
    x <- ranger::treeInfo(ranger_obj, i)
    forest$split.varIDs[[i]] <- x$splitvarID + 1
  }

  binned_forest <- binForestByDepth(forest)
  # retrieve variable ID's for matching
  var_ids <- binned_forest$variable_ids_used
  # forest averaged First and Second Order Minimal Depth
  avg_fom_depths <- c(); avg_som_depths <- c()
  # iterate over depth_bins to calculate first and second order minimal
  # depth of maximal subtrees
  for (var_depth_bins in binned_forest[[1]]) {
    var_fom_depths <- c()
    var_som_depths <- c()
    for (tree_depths in var_depth_bins) {
      var_fom_depths <- c(var_fom_depths, tree_depths[1])
      if (length(tree_depths) > 1) {
        var_som_depths <- c(var_som_depths, tree_depths[2])
      }
    }
    # assign first order max depth mean
    fom_depth <- mean(var_fom_depths)
    # assign second order max depth mean
    if (length(var_som_depths) > 0) {
      som_depth <- mean(var_som_depths)
    } else {
      # in the case where there is no second order depth, populate
      # with a -1
      som_depth <- -1
    }
    avg_fom_depths <- c(avg_fom_depths, fom_depth)
    avg_som_depths <- c(avg_som_depths, som_depth)
  }
  # combine the results
  result <- data.frame("first_order" = avg_fom_depths,
                       "second_order" = avg_som_depths,
                       "variable_id" = var_ids - 1) |>
    dplyr::inner_join(countSplitsPerVar(ranger_obj),
                      by = c('variable_id' = 'splitvarID')) |>
    dplyr::arrange(first_order) |>
    dplyr::mutate(splitvarName = factor(splitvarName, levels = splitvarName))

  return(result)
}
