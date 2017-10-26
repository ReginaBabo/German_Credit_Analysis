
train_cv = function(formula, data, method, cutoff = 0.5, k = 10, reps = 10, verboseIter = TRUE, seed = 123, ...) {
    results = data.frame()
    set.seed(seed)
    for (idx in 1:reps) {
        ## Create unique set of folds
        y = data[,all.vars(as.formula(formula))[1]][[1]]
        cv_folds = createCVFolds(y, k = k)

        for (fold_idx in cv_folds) {

            train = data[fold_idx,]
            test = data[-fold_idx,]

            ## Fit Model to Fold and produce results
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

            pos_class_idx = 1
            if (method == "rlda") {
                pos_class_idx = 2
            }

            pred = predict(mod_fit, test, type="prob")[,pos_class_idx]
            pred = ifelse(pred > cutoff, "X1", "X0")

            out = data.frame(obs = test[,all.vars(as.formula(formula))[1]][[1]],
                             pred = factor(pred, levels=levels(test[,all.vars(as.formula(formula))[1]][[1]])))

            new = p35(out)

            results = rbind(results, new)
        }

    }
    names(results) = c("Accuracy","Specificity","P35")
    return(results)
}

train_cv(creditability ~ ., data = credit, method = "rf", tuneGrid = tuneGridRF, cutoff = 0.5)

cutoff_seq = function(formula, data, method, low = 0.25, mid = 0.5, high = 0.75, k = 10, reps = 10, verboseIter = TRUE, seed = 123, ...) {
    cutoff_options = list()
    cutoff_vec = c(low, mid, high)
    for (cutoff in cutoff_vec) {
        cutoff_options[[as.character(cutoff)]] = train_cv(formula = formula,
                                                          data = data,
                                                          method = method,
                                                          cutoff = cutoff,
                                                          k = k,
                                                          reps = reps,
                                                          verboseIter = verboseIter,
                                                          seed = seed,
                                                          ...)
    }
    return(cutoff_options)
}


cutoff_seq(formula = creditability ~ ., data = credit, method = "rf", tuneGrid = tuneGridRF)

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

get_best_cutoffs = function(formula, data, method, low = 0.25, mid = 0.5, high = 0.75, by = 0.25, k = 10, reps = 10, seed = 123, ...) {
    while(by > 0) {
        medians = lapply(cutoff_seq(low = low, mid = mid, high = high, formula = formula, data = data, method = method, k = k, reps = reps, seed = seed,...), function(x) { median(x$P35) })
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
get_best_cutoffs(formula = creditability ~ ., data = credit, method = "glm", family="binomial", verbose=FALSE)

# 0.7 0.09025 - credit_select
# 0.7 0.0915 - credit_select


#rf
tuneGridRF = data.frame(mtry=54)

get_best_cutoffs(formula = creditability ~ ., data = credit_select, method = "rf", tuneGrid = tuneGridRF, verbose= FALSE)


#log_reg
tuneGridLogReg = expand.grid(
    cost = 2,
    loss = "L1",
    epsilon = 0.001)

get_best_cutoffs(formula = creditability ~ ., data = credit_select, method = "regLogistic", tuneGrid = tuneGridLogReg, verbose=FALSE)

#rlda
tuneGridRLDA = data.frame(estimator="Schafer-Strimmer")

get_best_cutoffs(formula = creditability ~ ., data = credit, method = "rlda", tuneGrid=tuneGridRLDA,verbose=FALSE)

get_best_cutoffs(formula = creditability ~ ., data = credit, method = "qda", verbose=FALSE)

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





