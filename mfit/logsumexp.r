logsumexp = function(x, dim = NULL) {

    if (is.null(dim)) {
        dim = dim(x)[which(dim(x)!=1)]
        if (is.null(dim)) dim = 1
    }

    y = apply(x,dim,max)
    x = sweep(x,2,y)
    if (dim == 1) {
        s = y + log(rowSums(exp(x)))
    } else {
        s = y + log(colSums(exp(x)))
    }
    i = which(!is.finite(y));
    if (!is.null(i)) s[i] = y[i]
    return (s);
}
