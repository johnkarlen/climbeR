# climbeR version 0.2.0

* plotFirstAndSecondOrderMetric now uses some base R pipes to organize plotting data, and the color scale code has changed slightly.

* countSplitsPerVar was simplified to work with dplyr verbs. 

* lookForVarsAbsentInForest was no longer necessary after fixing countSplitsPerVar 

* calculateAMDMS was edited to acomodate changes in the Ranger package. It now makes use of ranger::treeInfo to retrieve the tree representation that most closely matches the representation we used to rely on in forest$split.varIDs. It also writes these representations to forest$split.VarIDs to reduce the amount of changes needed to the package. This edit was contributed by Jan Brederecke. Some refactoring with dplyr verbs has been done here as well.

