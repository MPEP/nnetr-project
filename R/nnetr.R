euclid_norm <- function(x) {sqrt(sum(x^2))}

unit_step <- function(data, weight) {
    distances <- vector(length = nrow(data))
    distances <- apply(data, 1, distance_from_separator, weight)
    ifelse(distances < 0, -1, 1)
}

distance_from_separator <- function(data, weight) {
    distance <- data %*% weight
    distance
}

new_perceptron <- function(formula, data, learning_rate = 1, activation = unit_step) {
    if(!is.data.frame(data)) stop("Input data must be dataframe")
    #if(!is.numeric()) stop("Input must be numeric")
    mf <- model.frame(formula, data)
    mm <- model.matrix(formula, mf)
    y <- as.matrix(model.response(mf))
    x <- mm
    w <- vector(length = ncol(mm))
    c <- 0
    weight_update <- TRUE    
    R <- max(apply(x, 1, euclid_norm))
    
    while (weight_update) {
        weight_update <- FALSE
        y_classified <- activation(x, w)
        for (i in 1:nrow(x)) {
            if (y[i] != y_classified[i]) {
                w[-1] <- w[-1] + learning_rate * y[i] * x[i,-1]
                w[1] <- w[1] + learning_rate * y[i] * R^2
                c <- c + 1
                weight_update <- TRUE
            }
        }
    }
    s <- euclid_norm(w)
    weights <- w[-1]/s
    bias <- w[1]/s
    updates <- c

    attr(- w[1] / w[2], "intercept")
    attr(- w[1] / w[2], "slope")
    return(structure(list(weights, bias, updates),
                     class = "perceptron",
                     intercept = -w[1] / w[3],
                     slope = -w[2] / w[3]
                     )
           )
}                



## print.perceptron
## summary.perceptron
## plot.perceptron
