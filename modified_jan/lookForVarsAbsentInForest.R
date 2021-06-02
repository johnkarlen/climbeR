lookForVarsAbsentInForest <- function(counts, vars_used, num_ind_vars, forest) 
{
  num_missing <- num_ind_vars - length(counts)
  if (num_missing > 0) {
    if (min(vars_used) == 1) {
      missing_at_start <- c()
    } else{ 
      missing_at_start <- 1:(min(vars_used) - 1)
    }
    
    if (length(missing_at_start) < num_missing) {
      missing_at_end <- (max(vars_used) + 1):num_ind_vars
    } else {
      missing_at_end <- c()
    }
    
    missing_idxs <- c(missing_at_start, missing_at_end)
    status_var_exists <- ("status.varID" %in% attributes(forest)$names)
    if (status_var_exists && forest$status.varID %in% missing_idxs) {
      missing_idxs <- missing_idxs[c(-forest$status.varID)]
    }
    for (i in missing_idxs) {
      if (i < min(vars_used)) {
        counts <- c(0, counts)
      }
      else {
        counts <- c(counts, 0)
      }
    }
  }
  return(counts)
}
