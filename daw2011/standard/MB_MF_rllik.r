MB_MF_rllik = function(x,df) {
    # parameters
    b = x[1]
    lr = x[2]
    lambda = x[3]
    w_lo = x[4]
    w_hi = x[5]
    st = x[6]      # pi
    respst = x[7]  # rho 

    # initialization
    Qd = list()
    Qd[[1]] = matrix(0.5,3,2)     # Q[s,a]: state-action value function for Q-learning in task A
    Qd[[5]] = matrix(0.5,3,2)     # Q[s,a]: state-action value function for Q-learning in task B
    Tm = matrix(c(0.7,0.3,0.3,0.7),2,2)  # transition matrix
    M = matrix(0,2,1)                    # last choice structure
    R = matrix(0,2,1)                    # last choice structure
    N = length(df$c1)
    LL = 0

    dtQ = numeric(2)

    N = length(df)

    # loop through trials
    for (t in 1:N) {
        if (df$r[t] < 0) next   # if no response within timelimit

        s2 = df$s2[t] + 1

        if (df$stim1_l[t] %% 2 == 0) {
            R = apply(R,2,rev)  # flip matrix if in state *b
        }

        w = ifelse(df$stake[t] == 1, w_lo, w_hi)

        maxQ = apply(Qd[[df$stake[t]]][2:3,],1,max)
        Qm = t(Tm) %*% maxQ

        Q = w*Qm + (1-w)*matrix(Qd[[df$stake[t]]][1,],2,1) + st*M + respst*R

        p1 = 1 / (1 + exp(-b * (Q[df$c1[t]] - Q[ifelse(df$c1[t] == 1, 2, 1)])))
        p1 = max(min(p1,0.9999), 0.00001)
        LL = LL + log(p1)
        # LL = LL + b*Q[df$c1[t]] - log(sum(exp(Q)))
        
        p2 = 1 / (1 + exp(-b * (Qd[[df$stake[t]]][s2,df$c2[t]] - Qd[[df$stake[t]]][s2,ifelse(df$c2[t] == 1, 2, 1)])))
        p2 = max(min(p2,0.9999), 0.00001)
        LL = LL + log(p2)
        # LL = LL + b*Qd[[df$stake[t]]][s2,df$c2[t]] - log(sum(exp(Qd[[df$stake[t]]][s2,])))

        if (t > 1 && df$trial[t] != df$trial[t-1]) M = matrix(0,2,1)
        M[df$c1[t]] = 1

        R = matrix(0,2,1)
        if (df$c1[t] == df$stim1_l[t]) {
            R[1] = 1
        } else {
            R[2] = 1
        }

        dtQ[1] = Qd[[df$stake[t]]][s2,df$c2[t]] - Qd[[df$stake[t]]][1,df$c1[t]]
        Qd[[df$stake[t]]][1,df$c1[t]] = Qd[[df$stake[t]]][1,df$c1[t]] + lr*dtQ[1]

        dtQ[2] = df$r[t] - Qd[[df$stake[t]]][s2,df$c2[t]]

        Qd[[df$stake[t]]][s2,df$c2[t]] = Qd[[df$stake[t]]][s2,df$c2[t]] + lr*dtQ[2]
        Qd[[df$stake[t]]][1,df$c1[t]] = Qd[[df$stake[t]]][1,df$c1[t]] + lambda*lr*dtQ[2]
    }
    return (LL)
}
