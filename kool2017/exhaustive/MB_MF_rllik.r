MB_MF_rllik = function(x,df) {
    # parameters
    b = x[1]           # softmax inverse temperature
    lr = x[2]          # learning rate
    lambda = x[3]      # eligibility trace decay
    w = x[4]           # mixing weight
    st = x[5]          # stickiness
    respst = x[6]      # stickiness
    
    # initialization
    Q_MF = matrix(4.5, 2, 2)
    Q2 = matrix(4.5, 2, 1)
    Tm = list()
    Tm[[1]] = rbind(c(1,0),c(0,1))
    Tm[[2]] = rbind(c(1,0),c(0,1))
    M = matrix(0,2,2)                   # last choice structure
    R = matrix(0,2,1)                   # last choice structure
    N = length(df$c)
    LL = 0

    dtQ = numeric(2)                    # RPE for stages 1,2

    # loop through trials
    for (t in 1:N) {
        if (df$r[t] < 0) next   # if no response within timelimit

        if (df$s1L[t]) {
            R = apply(R,1,rev)  # flip matrix if in state *b
        }

        s1 = df$s1[t]
        s2 = df$s2[t]
        a = df$c[t]
        action = a
        a = a - (s1 %% 2 == 0)*2

        Q_MB = t(Tm[[s1]]) %*% Q2     # compute model-based value function
        
        Q = w*Q_MB + (1-w)*matrix(Q_MF[s1,],2,1) + st*M[s1,] + respst*R

        # LL = LL + b*Q[a] - logsumexp(b*Q)
        p = 1 / (1 + exp(-b * (Q[a] - Q[ifelse(a==1, 2, 1)])))
        p = max(min(p, 0.9999), 0.00001)
        LL = LL + log(p)

        if (t > 1 && df$trial[t] != df$trial[t-1]) M = matrix(0,4,2)
        M[s1,a] = 1;

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