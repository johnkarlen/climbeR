#' Recursive Depth Binning
#'
#' Function to recursively traverse depths of a tree.
#'
#' @param node_list Must be in the format of elements in the Ranger package's
#' forest$split.varIDs, which represents one tree in the forest.
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
#' @return A list with 2 elements. The first is a list of vectors - 
#' one for each independent variable ocurring in the forest (this may not 
#' be the complete set of independent variables, but we will account for any
#' variables that do not occur in the forest later). Each vector contains all
#' minimal depths of maximal subtrees in the forest, for the corresponding 
#' independent variable. The second element is a vector of tree 
#' heights 'forest_depths'.
binForestByDepth <- function(ranger_obj) {
    # forest properties
    trees <- ranger_obj$split.varIDs
    num_trees <- ranger_obj$num.trees

    # return these data structures, once populated
    depth_bins <- list()
    forest_depths <- c()

    # get all non-zero var ID's, (0 represents a leaf node)
    var_id_dump <- unlist(ranger_obj$split.varIDs)
    var_id_set <- unique(var_id_dump[var_id_dump != 0])
    # number of vars that occur in the forest
    num_vars <- length(var_id_set)
    
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
                # find variable index
                var_idx <- match(var_id,var_id_set)
                depth_bins[[var_idx]][[tree_idx]] <-
                    c(depth_bins[[var_idx]][[tree_idx]], depth)
            }
        }
    }
    return(list(depth_bins = depth_bins, 
                forest_depths = forest_depths, 
                var_idxs = var_id_set))
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

    # dump all the split ID's into one container
    dump_split_IDs <- unlist(trees)
    dump_split_IDs <- dump_split_IDs[dump_split_IDs != 0]
    
    # get the list of var id's that occurred in the forest
    sorted_var_id <- sort(unique(dump_split_IDs))
    
    # it's possible for a var ID to be absent from the forest because 
    # it was never used to split. In this case, we need to build the complete
    # set of var ID's, as anticipated in binForestByDepth
    
    # create a vector of the range of these ID's.
    vars_used <- min(sorted_var_id):max(sorted_var_id)
    # (this may find some of the var ID's that were not used to split in the
    # forest, but it may still be incomplete, which will be fixed below)
    
    # exclude the status var ID, if it's in the list of var ID's
    if (status_var_exists) {
        if (ranger_obj$status.varID %in% vars_used) {
            vars_used <- vars_used[c(-ranger_obj$status.varID)]
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

    # call helper to look for missed variables
    counts <- lookForVarsAbsentInForest(counts, vars_used,
                                        num_ind_vars, ranger_obj)
    # normalize the counts
    normalized_counts <- counts / sum(counts)
    # ready to return
    result <- data.frame(normalized_counts = normalized_counts, 
                         counts = counts, 
                         var_ids = vars_used)
    rownames(result) <- ranger_obj$independent.variable.names
    return(result)
}


#' Look for Variable ID's that didn't occur in the Forest.
#'
#' Find any remaining vars, if missing. Vars can be absent in the forest, if
#' they were never used to split. This function does some bookkeeping, to find
#' elements in the count vector that should be 0. If there weren't enough vars
#' observed, their indeces must be either at the end of vars_used, or the
#' beginning.
#'
#' @param counts A vector of split counts in the forest. This may need to be
#' updated with 0's for variables that didn't occur in the forest.
#' @param vars_used The current list of varID's that have been found in the
#' forest.
#' @param num_ind_vars The number of independent vars. Counts must have this
#' many elements.
#' @param forest Pass this to access the 'status.varID' if necessary.
#' @return updated counts vector.
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
    binned_forest <- binForestByDepth(forest)
    var_ids <- binned_forest[[3]]
    # forest averaged First and Second Order Minimal Depth
    avg_fom_depths <- c()
    avg_som_depths <- c()
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
    result <- data.frame(avg_fom_depths, avg_som_depths, var_ids)
    names(result) <- c("first_order", "second_order", "variable_id")
    
    # count number of times that each variable was split
    splits_per_var <- countSplitsPerVar(forest)
    # assign the rownames
    # match splits per variable to the df with variable ID key
    splits_per_var <- splits_per_var[match(result[["variable_id"]],
                                           splits_per_var[["var_ids"]]), ]
    result <- cbind(result, splits_per_var)
    # sort by first order
    result <- result[order(result$first_order), ]
    return(result)
}
