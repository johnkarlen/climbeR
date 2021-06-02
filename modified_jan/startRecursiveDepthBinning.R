startRecursiveDepthBinning <- function(tree_split_varIDs) 
{
  binned_depths <- list()
  binned_depths <- recursiveDepthBinning(tree_split_varIDs, 
                                         0, 1, binned_depths)
  return(binned_depths)
}
