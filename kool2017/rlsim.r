rlsim = function(x,R=NULL,T) {
    # Simulate data from SARSA(lambda)-TD model 
    #
    # USAGE: df = rlsim(x,T)
    #
    # INPUTS: 
    #   x = parameter vector: 
    #       x[1] = inverse temperature beta
    #       x[2] = learning rate alpha
    #       x[3] = trace decay lambda 
    #       x[4] = stickiness pi for perservation/switching
    #       x[5] = stickiness rho for repetition/alternation
    #       x[6] = weight to MB behavior for low stake; w_low
    #       x[7] = weight to MB behavior for high stake; w_high
    #   R = rewards obtained per state
    #   T = number of trials
    # 
    # OUTPUTS:
    #   df = [T x 11] dataframe structure with the following fields 
    #           t     = trials
    #           s1    = states in stage 1 (1 or 2)
    #           s2    = states in stage 2 (3 or 4)
    #           c     = actions taken in stage 1 (1 or 2)
    #           r     = reward obtained (0~9)
    #           rep1  = indicator func for consecitive choice 1 
    #           rep2  = indicator func for consecitive choice 2
    #           resp1 = indicator func for consecitive left choice 
    #           resp2 = indicator func for consecitive right choice 
    #           stake = high (5) or low (1) stake 
    #           s1L   = choice leading to state 3 shown left 
    # 
    # Chie Murai, April 2021

    # Set parameter values
    beta = x[1]
    alpha = x[2]
    lambda = x[3]
    pie = x[4]  # distinguishing from special value pi in R
    rho = x[5]
    w_l = x[6]  # for low stake trials
    w_h = x[7]  # for high stake trials

    # Initialize state-action value Q
    Q_MF = matrix(0, nrow=3, ncol=2)  # [[(s1,a1),(s1,a2)], [(s2,a1),(s2,a2)], [(s3,a1),(s4,a1)]] 
    Q_MB = numeric(2)  # [a1, a2] at Stage 1
    Q_H = numeric(2)   # [a1, a2] at Stage 1

    s1 = numeric(T)  # state in stage 1
    s2 = numeric(T)  # state in stage 2
    c = numeric(T)   # choice
    r = numeric(T)   # obtained reward

    # Indicator func if same choice was chosen for s1
    rep1 = numeric(T)  # 1 if action 1 was taken consecutively
    rep2 = numeric(T)  # 1 if action 2 was taken consecutively

    # Indicator func if same key (action) was pressed at previous trial in stage 1 regardless of state 
    resp1 = numeric(T)  # 1 if LEFT key was pressed consecutively
    resp2 = numeric(T)  # 1 if RIGHT key was pressed consecutively 

    # Trace decay values 
    e1 = matrix(0,nrow=2,ncol=2)  # trace decay for stage 1
    e2 = numeric(2)               # trace decay for stage 2

    if (is.null(R)) R = r_values(T)     # return reward for states in stage 2
    stake = sample(c(1,5),T,replace=TRUE)  # high stake (5) or low stake (1) against reward
    s1L = numeric(T)

    # Conduct simulation 
    for (t in 1:T) { 

        e = numeric(2)  # eligibility trace

        # Calculate probability to take action 1 in stage 1 (regardless of state)
        p = 1 / (1 + exp(beta * ((Q_H[2] + pie * rep2[t] + rho * resp2[t]) - (Q_H[1] + pie * rep1[t] + rho * resp1[t]))))
        p = max(min(p, 0.9999), 0.00001)

        # Determine which state the subject lands on 
        s1[t] = sample(c(1,2),1)  # state 1 or 2 for stage 1
        
        # Determine how the action choices will be placed
        s1L[t] = rbinom(1,1,0.5)  # Action 1 shown at right(0) or left(1)

        # Determine the action made by the subject 
        if (runif(1) < p) {  # Action 1 taken -> State 3
            c[t] = 1
            s2[t] = 3
            r[t] = R[t,1] * stake[t]  # Give reward for state 3
        } else {                 # Action 2 taken -> State 4
            c[t] = 2
            s2[t] = 4
            r[t] = R[t,2] * stake[t]  # Give reward for state 4
        }

        # Update state-action value
        if (t < T) {
            # Model-free
            delta2 = r[t] - Q_MF[3,s2[t]-2]
            e2[s2[t]-2] = e2[s2[t]-2] + 1
            Q_MF[3,s2[t]-2] = Q_MF[3,s2[t]-2] + alpha * delta2 * e2[s2[t]-2]

            delta1 = Q_MF[3,s2[t]-2] - Q_MF[s1[t],c[t]]
            e1[s1[t],c[t]] = e1[s1[t],c[t]] + 1
            Q_MF[s1[t],c[t]] = Q_MF[s1[t],c[t]] + alpha * delta1 * lambda * e1[s1[t],c[t]]

            # Model-based
            Q_MB[c[t]] = Q_MF[3,s2[t]-2]

            # Hybrid model
            if (stake[t] == 1){
                Q_H[c[t]] = w_l * Q_MB[c[t]] + (1-w_l) * Q_MF[s1[t],c[t]]
            } else {
                Q_H[c[t]] = w_h * Q_MB[c[t]] + (1-w_h) * Q_MF[s1[t],c[t]]
            }

            # Update indicator functions 
            if (t > 1 && s2[t] == 3) {
                if (s2[t-1] == s2[t]) rep1[t+1] = 1
                if (s1L[t-1] == s1L[t]) resp1[t+1] = 1
            } else if (t > 1 && s2[t] == 4) {
                if (s2[t-1] == s2[t]) rep2[t+1] = 1
                if (s1L[t-1] == s1L[t]) resp2[t+1] = 1            
            }
        }
    }

    df = data.frame(
        t = c(1:T),
        s1 = s1, 
        s2 = s2, 
        c = c, 
        r = r,
        rep1 = rep1, rep2 = rep2,
        resp1 = resp1, resp2 = resp2, 
        stake = stake,
        s1L = s1L
    )

    return (df)
}