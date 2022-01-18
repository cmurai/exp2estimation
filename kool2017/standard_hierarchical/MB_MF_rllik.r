MB_MF_rllik = function(x,df) {
    # parameters
    b = x[1]
    lr = x[2]
    lambda = x[3]
    w_lo = x[4]
    w_hi = x[5]
    st = x[6] 
    respst = x[7]  # rho 

    # initialization
    Q_MF = matrix(4.5,4,2)  # MF Q-value for stage 1 choices
    Q2 = matrix(4.5,4,1)    # Q-value for stage 2 choices
    Tm = list()
    # Tm[[1]] = rbind(c(1,0),c(0,1))      # transition matrix (実際固定でわざわざ確率を更新する必要はない)
    # Tm[[2]] = rbind(c(1,0),c(0,1))      # transition matrix
    for (i in 1:4) { Tm[[i]] = rbind(c(1,0),c(0,1))}  # set transition matrix for each state
    M = matrix(0,4,2)                   # last choice structure
    R = matrix(0,2,1)                   # last choice structure
    N = length(df$c)
    LL = 0

    dtQ = numeric(2)                    # RPE for stages 1,2

    # loop through trials
    for (t in 1:N) {
        if (df$r[t] < 0) next   # if no response within timelimit

        if (df$stim_l[t] %% 2 == 0) {
            R = apply(R,2,rev)  # flip matrix if in state *b
        }

        s1 = df$s1[t]
        s2 = df$s2[t]
        a = df$c[t]
        action = a                  # {1,2,3,4}
        a = a - (s1 %% 2 == 0)*2    # {1,2}

        # Q_MB = t(Tm[[s1]]) %*% Q2     # compute model-based value function
        if (s1 < 3) {
            Q_MB = t(Tm[[s1]]) %*% Q2[1:2,]
        } else {
            Q_MB = t(Tm[[s1]]) %*% Q2[3:4,]
        }

        w = ifelse(df$stake[t] == 1, w_lo, w_hi)

        Q = w*Q_MB + (1-w)*matrix(Q_MF[s1,],2,1) + st*matrix(M[s1,],2,1) + respst*R

        # LL = LL + b*Q[a] - logsumexp(b*Q)
        p = 1 / (1 + exp(-b * (Q[a] - Q[ifelse(a==1, 2, 1)])))
        p = max(min(p, 0.9999), 0.00001)
        LL = LL + log(p)

        if (t > 1 && df$trial[t] != df$trial[t-1]) M = matrix(0,4,2)
        M[s1,a] = 1

        R = matrix(0,2,1)
        if (action == df$stim_l[t]) {
            R[1] = 1
        } else {
            R[2] = 1
        }

        dtQ[1] = Q2[s2] - Q_MF[s1,a]
        Q_MF[s1,a] = Q_MF[s1,a] + lr*dtQ[1]

        dtQ[2] = df$r[t] - Q2[s2]

        Q2[s2] = Q2[s2] + lr*dtQ[2]
        Q_MF[s1,a] = Q_MF[s1,a] + lambda*lr*dtQ[2]
    }
    return (LL)
}
