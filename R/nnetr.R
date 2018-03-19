data(iris)
head(iris, n=20)

iris_sub = iris[1:100, c(1, 3, 5)]
names(iris_sub) = c("sepal", "petal", "species")
head(iris_sub)

euclid_norm <- function(x) {sqrt(x %*% x)}

classifier <- function(data, weight, bias) {
    distances <- vector(length = nrow(data))
    distances <- apply(data, 1, distance_from_separator, weight, bias)
    ifelse(distances < 0, -1, +1)
}

distance_from_separator <- function(data, weight, bias) {
    distance <- data %*% weight + bias
    distance
}

perceptron <- function(x, y, learning_rate = 1) {
    weight <- vector(length = ncol(x))
    bias <- 0
    counter <- 0
    weight_update <- TRUE

    while (weight_update) {
        weight_update <- FALSE
        y_classified <- classifier(x, weight, bias)
        for (i in 1:nrow(x)) {
            if (y[i] != y_classified[i]) {
                weight <- weight + learning_rate * y[i] * x[i,]
                bias <- bias + learning_rate * y[i] * x[i]
            }
        }
    }
    return(list(w=weight, b=bias, updates=counter))
}                

x <- cbind(iris_sub$sepal, iris_sub$petal)
y <- rep(+1, length(iris_sub$species))
y[x == "setosa"] <- -1
    
linear_classifier(x, c(0,0), 1)


