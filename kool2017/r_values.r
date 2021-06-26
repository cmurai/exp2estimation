r_values = function(T){
    x = matrix(0, nrow = T, ncol = 2)
  
    # set initial values
    if (sample(2,1) == 1){
        x[1,1] = sample(c(0:4),1)
        x[1,2] = sample(c(5:9),1)
    } else {
        x[1,1] = sample(c(5:9),1)
        x[1,2] = sample(c(0:4),1)
    }
    
    # sample drift for gaussian random walk
    w1 = rnorm(T,0,2)
    w2 = rnorm(T,0,2)
    
    for(t in 2:T){
        x[t,1] = x[t-1,1] + w1[t]
        x[t,2] = x[t-1,2] + w2[t]
        
        # adjust values to stay within limit
        if (x[t,1] < 0) x[t,1] = 0
        if (x[t,1] > 9) x[t,1] = 9
        if (x[t,2] < 0) x[t,2] = 0
        if (x[t,2] > 9) x[t,2] = 9
    }
    x = round(x)
    return (x)
}