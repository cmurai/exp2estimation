r_values = function(T) {
    x = matrix(0, nrow = T, ncol = 4) 

    # set initial values 
    x[1,] = runif(4)

    # sample drift for gaussian random walk
    w = matrix(rnorm(T*4, sd = 0.025), nrow=T, ncol=4)
    
    for (t in 2:T) {
        x[t,] = x[t-1,] + w[t,]

        if (x[t,1] > 0.75 || x[t,1] < 0.25) x[t,1] = x[t-1,1] - w[t,1]
        if (x[t,2] > 0.75 || x[t,2] < 0.25) x[t,2] = x[t-1,2] - w[t,2]
        if (x[t,3] > 0.75 || x[t,3] < 0.25) x[t,3] = x[t-1,3] - w[t,3]
        if (x[t,4] > 0.75 || x[t,4] < 0.25) x[t,4] = x[t-1,4] - w[t,4] 
    }
    colnames(x) = c("B1","B2","C1","C4")
    return (x)
}