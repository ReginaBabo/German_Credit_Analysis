
train_cv = function(formula, data, method, cutoff = 0.5, k = 10, fold_index = myFolds, verboseIter = TRUE, ...) {
    results = data.frame()
    for (idx in 1:k) {
        folds = 1:k
        folds = folds[-idx]

        train = data[fold_index[[idx]],]
        test = unlist(fold_index[folds])
        names(test) = NULL
        test = data[test,]

        myControl <- trainControl(summaryFunction = p35,
                                  classProbs = TRUE, # IMPORTANT!
                                  verboseIter = verboseIter,
                                  savePredictions = TRUE,
                                  method = "none"
        )
        ### Model Fit ###
        mod_fit = train(formula,
                        method = method,
                        data = train,
                        trControl= myControl,
                        metric = "P35",
                        ...)
        ##################

        pred = predict(mod_fit, test, type="prob")[,1]
        pred = ifelse(pred > cutoff, "X1", "X0")

        out = data.frame(obs=test$creditability, pred = pred)

        new = p35(out)

        results = rbind(results, new)
    }
    names(results) = c("Accuracy","Specificity","P35")
    return(results)
}


cutoff_seq = function(low, mid, high, formula, data, method, k = 10, fold_index = myFolds, verboseIter = TRUE, ...) {
    cutoff_options = list()
    cutoff_vec = c(low,mid,high)
    for (cutoff in cutoff_vec) {
        cutoff_options[[as.character(cutoff)]] = train_cv(formula = formula, data = data, method = method, cutoff = cutoff, k = k, fold_index = fold_index, verboseIter = verboseIter, ...)
    }
    return(cutoff_options)
}





cutoff_seq(from = 0.25, to = 0.75, by = 0.25, creditability ~ ., data = credit_select, method = "xgbTree", tuneGrid = tuneGridXGB)

get_next_cutoffs = function(by, index_max, key_max) {
    new_by = by - 0.10
    if (index_max == 1) {
        ##LEFT
        new_high = as.numeric(key_max)
        new_mid = new_high - new_by
        if (new_mid - new_by > 0) {
            new_low = new_mid - new_by
        }
        else {
            new_low = 0.01
        }
        return(list(low = new_low, mid = new_mid, high = new_high, by = new_by))
    }
    else if (index_max == 2) {
        ##CENTER
        new_mid = as.numeric(key_max)
        if (as.numeric(key_max) + new_by < 1) {
            new_high = as.numeric(key_max) + new_by
        }
        else {
            new_high = 0.99
        }
        if (as.numeric(key_max) - new_by > 0) {
            new_low = as.numeric(key_max) - new_by
        }
        else {
            new_low = 0.05
        }

        return(list(low = new_low, mid = new_mid, high = new_high, by = new_by))
    }
    else {
        ##RIGHT
        new_low = as.numeric(key_max)
        new_mid = new_low + new_by
        if (new_mid + new_by < 1) {
            new_high = new_mid + new_by
        }
        else {
            new_high = 0.95
        }

        return(list(low = new_low, mid = new_mid, high = new_high, by = new_by))

    }
}

get_best_cutoffs = function(low = 0.25, mid = 0.5, high = 0.75, by = 0.25, formula = formula, data = data, method = method, ...) {
    while(by > 0) {
        medians = lapply(cutoff_seq(low = low, mid = mid, high = high, formula = formula, data = data, method = method, ...), function(x) { median(x$P35) })
        if (by == 0.25) {
            print(list(init_cutoff = as.numeric(names(medians)[2]), init_metric = medians[[2]]))
        }
        index_max = which.max(medians)
        key_max = names(medians)[index_max]
        value_max = medians[[index_max]]

        new_cutoffs = get_next_cutoffs(by = by, index_max = index_max, key_max = key_max)
        low = new_cutoffs$low
        mid = new_cutoffs$mid
        high = new_cutoffs$high
        by = new_cutoffs$by
        print(list(round_cutoff = as.numeric(key_max), round_metric = value_max))
    }
    return(list(best_cutoff = mid, best_metric = value_max))
}



# log
get_best_cutoffs(low = 0.25, mid = 0.5, high = 0.75, by = 0.25, formula = creditability ~ ., data = credit_select, method = "glm", family="binomial", verbose=FALSE)

# 0.7 0.095

#rpart
get_best_cutoffs(low = 0.25, mid = 0.5, high = 0.75, by = 0.25, formula = creditability ~ ., data = credit_select, method = "rpart", verbose= FALSE)

#rf
tuneGridRF = data.frame(mtry=11)

get_best_cutoffs(low = 0.25, mid = 0.5, high = 0.75, by = 0.25, formula = creditability ~ ., data = credit_select, method = "rf", tuneGrid = tuneGridRF, verbose= FALSE)
# 0.5 0.0345
# 0.7 0.09

#log_reg
tuneGridLogReg = expand.grid(
    cost = 2,
    loss = "L1",
    epsilon = 0.001)

get_best_cutoffs(low = 0.25, mid = 0.5, high = 0.75, by = 0.25, formula = creditability ~ ., data = credit_select, method = "regLogistic", tuneGrid = tuneGridLogReg, verbose=TRUE)

#rlda
get_best_cutoffs(low = 0.25, mid = 0.5, high = 0.75, by = 0.25, formula = creditability ~ ., data = credit_select, method = "rlda", verbose= FALSE)

#xgb
tuneGridXGB <- expand.grid(
    nrounds=150,
    max_depth = 4,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.7,
    subsample = 0.75,
    min_child_weight = 1)

get_best_cutoffs(low = 0.25, mid = 0.5, high = 0.75, by = 0.25, formula = creditability ~ ., data = credit_select, method = "xgbTree", tuneGrid = tuneGridXGB, verbose = 0)





