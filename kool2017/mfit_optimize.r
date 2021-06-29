mfit_optimize = function(param,df,nstarts=5){
    # Optimze data from SARSA(lambda)-TD model
    # 
    # USAGE : df = mfit_optimize(likfun,param,df,nstarts=5)
    # 
    # INPUTS :
    #   likfun = likelihood function handle
    #   param = [K x 1] parameter structure
    #   df = [T x 11] dataframe of action data
    #   nstarts (optional) = number of random starts
    # 
    # OUTPUTS :
    #   results = structure with the following fields 
    #               x 
    # 
    # Chie Murai, April 2021

    # Set parameter values 
    beta = param[1]
    alpha = param[2]
    lambda = param[3]
    pie = param[4]  # distinguishing from special value pi in R
    rho = param[5]
    w_l = param[6]
    w_h = param[7]

    s1 = df$s1
    s2 = df$s2
    c = df$c
    r = df$r
    rep1 = df$rep1
    rep2 = df$rep2
    resp1 = df$resp1
    resp2 = df$resp2 
    stake = df$stake
    s1L = df$s1L

    T = nrow(df)

    # Initialize state-action value Q
    Q_MF = matrix(0,nrow=3,ncol=2)
    Q_MB = numeric(2)
    Q_H = numeric(2)

    # Trace decay values 
    e1 = matrix(0,nrow=2,ncol=2)
    e2 = numeric(2)
    
    ll = 0  # log-likelihood

    # Calculate Q
    for (t in 1:T) {
        p = 1 / (1 + exp(beta * ((Q_H[2] + pie * rep2[t] + rho * resp2[t]) - (Q_H[1] + pie * rep1[t] + rho * resp1[t]))))
        p = max(min(p, 0.9999), 0.00001)

        ll = ll + (c[t] == 1) * log(p) + (c[t] == 2) * log(1-p)

        # Update Q
        if (t < T) {
            # Model-free
            delta2 = r[t] - Q_MF[3,s2[t]-2]
            e2[s2[t]-2] = e2[s2[t]-2] + 1
            Q_MF[3,s2[t]-2] = Q_MF[3,s2[t]-2] + alpha * delta2 * e2[s2[t]-2]

            delta1 = Q_MF[3,s2[t]-2] - Q_MF[s1[t],c[t]]
            e1[s[t],c[t]] = e1[s1[t],c[t]] + 1
            Q_MF[s1[t],c[t]] = Q_MF[s1[t],c[t]] + alpha * delta1 * lambda * e1[s1[t],c[t]]

            # Model-based
            Q_MB[c[t]] = Q_MF[3,s2[t]-2]

            # Hybrid model
            if (stake[t] == 1){
                Q_H[c[t]] = w_l * Q_MB[c[t]] + (1-w_l) * Q_MF[s1[t],c[t]]
            } else {
                Q_H[c[t]] = w_h * Q_MB[c[t]] + (1-w_h) * Q_MF[s1[t],c[t]]
            }
        }
    }
    return(list(negll = -ll))
}