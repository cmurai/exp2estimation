mfit_optimize = function(param,df,prior){
    # Optimze data from SARSA(lambda)-TD model
    # 
    # USAGE : df = mfit_optimize(likfun,param,df,prior)
    # 
    # INPUTS :
    #   likfun = likelihood function handle
    #   param = [K x 1] parameter structure
    #   df = [T x 11] dataframe of action data
    #   prior = information about prior distribution of parameters
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
    Q_MF = matrix(4.5,nrow=3,ncol=2)
    Q_MB = numeric(2)
    Q_H = numeric(2)

    # Trace decay values 
    e1 = matrix(0,nrow=2,ncol=2)
    e2 = numeric(2)
    
    ll = 0  # log-likelihood

    # Calculate Q
    for (t in 1:T) {
        if (r[t] < 0) next  # if no response, skip trial calculation

        p = 1 / (1 + exp(beta * ((Q_H[2] + pie * rep2[t] + rho * resp2[t]) - (Q_H[1] + pie * rep1[t] + rho * resp1[t]))))
        p = max(min(p, 0.9999), 0.00001)

        ll = ll + (c[t] == 1) * log(p) + (c[t] == 2) * log(1-p)

        # Update Q
        if (t < T) {
            # Model-free
            delta1 = Q_MF[3,s2[t]] - Q_MF[s1[t],c[t]]
            Q_MF[s1,c[t]] = Q_MF[s1,c[t]] + alpha*delta1

            delta2 = r[t] - Q_MF[3,s2[t]]
            
            Q_MF[3,s2[t]] = Q_MF[3,s2[t]] + alpha * delta2
            Q_MF[s1[t],c[t]] = Q_MF[s1[t],c[t]] + alpha * lambda * delta2

            # delta2 = r[t] - Q_MF[3,s2[t]]
            # e2[s2[t]] = e2[s2[t]] + 1
            # Q_MF[3,s2[t]] = Q_MF[3,s2[t]] + alpha * delta2 * e2[s2[t]]

            # delta1 = Q_MF[3,s2[t]] - Q_MF[s1[t],c[t]]
            # e1[s1[t],c[t]] = e1[s1[t],c[t]] + 1
            # Q_MF[s1[t],c[t]] = Q_MF[s1[t],c[t]] + alpha * delta1 * lambda * e1[s1[t],c[t]]

            # Model-based
            Q_MB[c[t]] = Q_MF[3,s2[t]]

            # Hybrid model
            w = ifelse(stake[t] == 1, w_l, w_h)
            Q_H[c[t]] = w * Q_MB[c[t]] + (1-w) * Q_MF[s1[t],c[t]]
            # if (stake[t] == 1){
            #     Q_H[c[t]] = w_l * Q_MB[c[t]] + (1-w_l) * Q_MF[s1[t],c[t]]
            # } else {
            #     Q_H[c[t]] = w_h * Q_MB[c[t]] + (1-w_h) * Q_MF[s1[t],c[t]]
            # }
        }
    }
    if (is.null(prior)) {
        lprior = 0
    } else {
        lprior = dgamma(beta, shape = prior$gamma_shape, rate = prior$gamma_rate, log = TRUE) + 
            dunif(alpha, log = TRUE) + dunif(lambda, log = TRUE) + 
            dnorm(pie, mean = prior$normal_mean, sd = sqrt(prior$normal_var), log = TRUE) + 
            dnorm(rho, mean = prior$normal_mean, sd = sqrt(prior$normal_var), log = TRUE) + 
            dunif(w_l, log = TRUE) + dunif(w_h, log = TRUE)
    }
    return(list(negll = - ll - lprior))
}