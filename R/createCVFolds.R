
set.seed(123)
temp = createFolds(credit_select$creditability, k = 10)

for (idx in 1:10) {
    folds = 1:10
    folds = folds[-idx]
    temp[[idx]] = unlist(temp[folds])
    names(temp[[idx]]) = NULL
}

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

temp = createCVFolds(credit_select$creditability)
