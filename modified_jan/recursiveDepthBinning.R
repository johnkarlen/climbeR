recursiveDepthBinning <- function(node_list, depth, expected_num_children,
                                   binned_depths) 
{
  children <- node_list[1:expected_num_children]
  node_list <- node_list[(expected_num_children + 1):length(node_list)]
  nodes <- children[children != 0]
  expected_num_children <- 2 * length(nodes)
  if (expected_num_children == 0) {
    return(binned_depths)
  }
  binned_depths[[depth + 1]] <- nodes
  binned_depths <- recursiveDepthBinning(node_list, depth + 1,
                                         expected_num_children,
                                         binned_depths)
  return(binned_depths)
}
