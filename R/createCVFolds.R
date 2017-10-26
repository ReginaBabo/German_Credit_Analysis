
library(caret)


createCVFolds = function(y, k = 10) {
    eq_folds = createFolds(y, k = k)
    out = list()
    for (idx in 1:k) {
        folds = 1:k
        folds = folds[-idx]
        out[[idx]] = unlist(eq_folds[folds])
        names(out[[idx]]) = NULL
    }
    names(out) = names(eq_folds)
    return(out)
}


