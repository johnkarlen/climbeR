countSplitsPerVar <- function(forest) 
{
  trees <- forest$split.varIDs 
  for (i in seq_along(trees)) {
    trees[[i]] <- trees[[i]][!is.na(trees[[i]])]
  }
  counts <- c()
  num_ind_vars <- length(forest$independent.variable.names)
  dump_split_IDs <- unlist(trees)
  sorted_var_id <- sort(unique(dump_split_IDs))
  vars_used <- sorted_var_id

  for (i in vars_used) {
    if (!(i %in% sorted_var_id)) {
      counts <- c(counts, 0)
    }
    else {
      counts <- ifelse(length(dump_split_IDs[dump_split_IDs == 
                                               i]),
        c(counts, length(dump_split_IDs[dump_split_IDs == 
                                                  i])),
        c(counts,0))
    }
  }
  # counts <- lookForVarsAbsentInForest(counts, vars_used, num_ind_vars, 
  #                                     forest)
  normalized_counts <- counts/sum(counts)
  result <- data.frame(normalized_counts = normalized_counts, 
                       counts = counts, var_ids = 1:num_ind_vars)
  rownames(result) <- forest$independent.variable.names
  return(result)
}
