#' Recursive Depth Binning
#'
#' Function to recursively traverse depths of a tree.
#'
#' @param node_list must be in the format of the ranger package's
#' forest$split.varIDs objects.
#' recursion is done by counting the number of terminal nodes at the current
#' depth, to anticipate the correct number of nodes in the next depth.
#' It represents one tree of the forest
#' @param depth each recursive call must know the current depth in the tree.
#' @param expected_num_children the number of nodes in the current depth must
#' be anticipated, given the number of terminal nodes at the previous depth
#' @param binned_depths a list passed between recursive calls, to store
#' results.
#' @return Output is a list of vectors, where elements correspond to depths,
#' and vectors contain variable ID's of variables used to split at that depth.
recursiveDepthBinning <- function(node_list,
                                  depth,
                                  expected_num_children,
                                  binned_depths) {
    # take expected number of children at current depth
    children <- node_list[1:expected_num_children]
    # nodelist becomes all nodes at depths lower than current depth
    node_list <- node_list[(expected_num_children + 1):length(node_list)]
    # store and count the # of non-leaves at the current depth
    nodes <- children[children != 0]
    # calculate expected number of children in the next recursion
    expected_num_children <- 2 * length(nodes)
    ### BASE CASE ### base case (no nodes at current depth)
    if (expected_num_children == 0) {
        return(binned_depths)
    }
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
#' @param tree_split_varIDs given one element of a 'split.varIDs' list, this
#' function will pass it to the recursiveDepthBinning function, to bin the tree
#' by depth, starting at the root.
#' @return returns a list with an element per depth encountered. Each element
#' is a vector of variable IDs
startRecursiveDepthBinning <- function(tree_split_varIDs) {
    binned_depths <- list()
    binned_depths <- recursiveDepthBinning(tree_split_varIDs,
                                           0, 1, binned_depths)
    return(binned_depths)
}


#' Bin Forest by Depth
#'
#' Given a forest object from the ranger package, this function will bin the
#' forest into depths. This is a helper function for the 'forestAvgMaxSubtree'
#' function.
#'
#' @param forest a ranger object created with the ranger package, which was 
#' created with param write.forest set to TRUE. In other words, it must have a 
#' 'forest' property.
#' @return returns a list of two elements. the first is a list which contains a
#' vector for each independent variable, where each element is that variable's
#' maximal depth of a minimal subtree, for one tree. The second
#' element returned, is a vector of tree heights 'forest_depths'.
binForestByDepth <- function(forest) {
    trees <- forest$split.varIDs
    num_trees <- forest$num.trees
    num_vars <- length(forest$independent.variable.names)
    depth_bins <- list()
    forest_depths <- c()
    # we need to know where to start indexing the vars. Its possible for a
    # variable never to be used in the forest which confuses matching between
    # variable names and var ID's
    var_id_dump <- unlist(forest$split.varIDs)
    # get all non-zero var ID's, (0 represents a leaf node)
    var_id_set <- unique(var_id_dump[var_id_dump != 0])
    min_var_id <- min(var_id_set)
    # if every var occurs in the forest
    if (length(var_id_set) == num_vars) {
        # then we can safely index like this:
        var_idx_offset <- min_var_id - 1
        # e.g. if min_var_id is 1, the offset is 0
    }

    # preallocate an array for each var's list of subtree depths
    for (var in 1:num_vars) {
        depth_bins[[var]] <- vector(mode = "list", length = num_trees)
    }

    # iterate over the forest
    for (tree_idx in 1:num_trees) {
        # bin each tree
        tree_depth_bins <- startRecursiveDepthBinning(trees[[tree_idx]])
        # remember the depth of each tree
        forest_depths <- c(forest_depths, length(tree_depth_bins))
        # add results to depth_bins structure
        for (depth in 1:length(tree_depth_bins)) {
            for (var_id in tree_depth_bins[[depth]]) {
                idx <- var_id - var_idx_offset
                depth_bins[[idx]][[tree_idx]] <-
                    c(depth_bins[[idx]][[tree_idx]], depth)
            }
        }
    }
    return(list(depth_bins = depth_bins, forest_depths = forest_depths))
}


#' Count Splits Per Variable
#'
#' This function counts the number of times each variable was used to split a
#' tree
#'
#' @param forest a ranger object from the ranger package, which was created
#' with param write.forest set to TRUE. In other words, it must have a
#' 'forest' property.
#' @return result a dataframe with one column of counts, and one column of
#' normalized counts. Rows are labeled by variable names
countSplitsPerVar <- function(forest) {
    trees <- forest$split.varIDs
    counts <- c()
    # check this, to see if we need to offset var id's by 1
    status_var_exists <- ("status.varID" %in% attributes(forest)$names)
    # we need a count for every independent var (some may be 0)
    num_ind_vars <- length(forest$independent.variable.names)

    # dump all the split ID's into one container
    dump_split_IDs <- unlist(trees)
    dump_split_IDs <- dump_split_IDs[dump_split_IDs != 0]

    # get the list of var id's that occurred in the forest
    sorted_var_id <- sort(unique(dump_split_IDs))

    # create a vector of the range of these ID's.
    vars_used <- min(sorted_var_id):max(sorted_var_id)
    # (this may find some of the var ID's that were not used to split in the
    # forest, but it may still be incomplete, which will be fixed below)

    # exclude the status var ID, if it's in the list of var ID's
    if (status_var_exists) {
        if (forest$status.varID %in% vars_used) {
            vars_used <- vars_used[c(-forest$status.varID)]
        }
    }
    # tally counts if var was used, otherwise, it gets a count of 0
    for (i in vars_used) {
        if (!(i %in% sorted_var_id)) {
            counts <- c(counts, 0)
        } else {
            counts <- c(counts, length(dump_split_IDs[dump_split_IDs == i]))
        }
    }
    # normalize the counts
    normalized_counts <- counts / length(dump_split_IDs)
    # call helper to look for missed variables
    counts <- lookForVarsAbsentInForest(counts, vars_used,
                                        num_ind_vars, forest)
    # ready to return
    result <- data.frame(normalized_counts, counts)
    rownames(result) <- forest$independent.variable.names
    return(result)
}


#' Look for Variable ID's that didn't occur in the Forest.
#'
#' Find any remaining vars, if missing. Vars can be absent in the forest, if
#' they were never used to split. This function does some bookkeeping, to find
#' where the 0's should be, in the counts vector. If there weren't enough vars
#' observed, their indeces must be, either at the end of vars_used, or the
#' beginning
#'
#' @param counts a vector of split counts in the forest. This may need to be
#' updated with 0's for variables that didn't occur in the forest
#' @param vars_used the current list of varID's that have been found in the
#' forest
#' @param num_ind_vars the number of independent vars. There needs to this
#' many elements in counts.
#' @param forest pass this to access the 'status.varID', if necessary
#' @return updated counts vector
lookForVarsAbsentInForest <- function(counts, vars_used,
                                      num_ind_vars, forest) {
    # get the number missing
    num_missing <- num_ind_vars - length(counts)
    if (num_missing > 0) {
        # find where to start
        ifelse(min(vars_used) == 1, missing_at_start <- c(),
               missing_at_start <- 1:(min(vars_used) - 1))
        # check for missing vars at the end
        ifelse(length(missing_at_start) < num_missing,
               missing_at_end <- (max(vars_used) + 1):num_ind_vars,
               missing_at_end <- c())
        # combine missing indexes
        missing_idxs <- c(missing_at_start, missing_at_end)
        # check to exclude status variable (not a possible covariate)
        status_var_exists <- ("status.varID" %in% attributes(forest)$names)
        if (status_var_exists && forest$status.varID %in% missing_idxs) {
            missing_idxs <- missing_idxs[c(-forest$status.varID)]
        }
        # depending on variable ID, add it to the start/end
        for (i in missing_idxs) {
            if (i < min(vars_used)) {
                counts <- c(0, counts)
            } else {
                counts <- c(counts, 0)
            }
        }
    }
    return(counts)
}


#' Forest Averaged Maximal Subtree
#'
#' Given a result from the Ranger package (write.forest must
#' be set to TRUE), this function will traverse the trees and calculate the
#' first and second order average minimal depth of a maximal subtree.
#'
#' @param ranger_obj a ranger object from the ranger package, which was created
#' by setting param write.forest to TRUE. In other words, it must have a 
#' 'forest' property.
#' @return a data.frame with two columns: averaged first and second order 
#' minimal depth of a maximal subtree 
#' @export
forestAvgMaxSubtree <- function(ranger_obj) {
    if(!("forest" %in% names(ranger_obj))){
        stop("no forest attribute present in ranger result. 
             Please run Ranger with write_forest set to TRUE")
    }
    
    forest <- ranger_obj$forest
    binned_forest <- binForestByDepth(forest)
    # use average forest depth for variables that are never used to split
    avg_forest_depth <- mean(binned_forest[[2]])
    # forest averaged First and Second Order Minimal Depth
    avg_fom_depths <- c()
    avg_som_depths <- c()
    # iterate over depth_bins to calculate first and second order minimal
    # maximal subtree
    for (var_depth_bins in binned_forest[[1]]) {
        var_fom_depths <- c()
        var_som_depths <- c()
        for (tree_depths in var_depth_bins) {
            if (length(tree_depths) > 0) {
                var_fom_depths <- c(var_fom_depths, tree_depths[1])
            }
            if (length(tree_depths) > 1) {
                var_som_depths <- c(var_som_depths, tree_depths[2])
            }
        }
        # in the case where the variable was never used to split, assign it's
        # depth to be the average depth of the forrest
        if (length(var_fom_depths) > 0) {
            fom_depth <- mean(var_fom_depths)
        } else {
            fom_depth <- avg_forest_depth
        }
        if (length(var_som_depths) > 0) {
            som_depth <- mean(var_som_depths)
        } else {
            som_depth <- avg_forest_depth
        }
        avg_fom_depths <- c(avg_fom_depths, fom_depth)
        avg_som_depths <- c(avg_som_depths, som_depth)
    }
    result <- data.frame(avg_fom_depths, avg_som_depths)
    names(result) <- c("first_order", "second_order")
    # count number of times that each variable was split
    splits_per_var <- countSplitsPerVar(forest)
    # assign the rownames
    rownames(result) <- forest$independent.variable.names
    # add splits per variable to the df with rowname key
    splits_per_var <- splits_per_var[match(rownames(result),
                                           rownames(splits_per_var)), ]
    result <- cbind(result, splits_per_var)
    # sort by first order
    result <- result[order(result$first_order), ]
    return(result)
}
