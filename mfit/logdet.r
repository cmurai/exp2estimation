logdet = function(A, ol = NULL) {

    if (all(A %% 1 != 0) && (nrow(A) == ncol(A))) {
        if (is.null(ol)) {
            use_chol = FALSE
        } else {
            if (ol == 'chol') {
                use_chol = TRUE
            } else {
                stop('The second argument can only be a string "chol" if it is specified.')
            }
        }
        if (use_chol) {
            v = 2 * sum(log(diag(chol(A))))
        } else {
            lum = expand(lu(A))
            du = diag(lum$U)
            c = det(lum$P) * prod(sign(du))
            v = log(c) + sum(log(abs(df)))
        }
        return (v)
    } else {
        stop('A should be a square matrix of double or single class.')
    }
}