MB_MF_rllik = function(x,df) {
    # parameters
    b = x[1]
    lr = x[2]
    lambda = x[3]
    w_lo = x[4]
    w_hi = x[5]
    st = x[6]
    respst = x[7]

    # initialization 
    Q_MF = matrix(4.5,4,3)      # MF Q-value for stage 1 choices
    Q2 = matrix(4.5,5,1)        # Q-value for stage 2 choices
    Tm = list() 
    for (i in 1:2) { Tm[[i]] = rbind(c(1,0),c(0,1)) }               # set transition matrix for task A
    for (i in 3:5) { Tm[[i]] = rbind(c(1,0,0),c(0,1,0),c(0,0,1)) }  # set transition matrix for task B
    M = matrix(0,4,3)           # last choice structure
    R = matrix(0,3,1)           # last choice structure
    N = length(df$c)            # overall length of choices
    LL = 0

    dtQ = numeric(2)            # RPE for stages 1,2

    # loop through trials
    for (t in 1:N) {
        if (df$r[t] < 0) next   # if no response within time-limit 

        stake = df$stake[t]
        n_s2 = ifelse(stake == 1, 2, 3)  # number of states for stage 2 depending on task

        if (stake == 1) {
            R = matrix(R[c(1,3),],2,1)                      # don't consider middle option when calculating Q value
            index = c(df$stim_l[t], df$stim_r[t])
        } else {
            index = c(df$stim_l[t], df$stim_m[t], df$stim_r[t])
        }
        R = matrix(R[order(index),],n_s2,1)         # reorder R to match choice and key

        s1 = df$s1[t]
        s2 = df$s2[t]
        a = df$c[t]
        action = a
        a = a - (s1 %% 2 == 0)*n_s2     # {1,2} for stake 1 and {1,2,3} for stake 5

        if (s1 < 3) {
            Q_MB = t(Tm[[s1]]) %*% Q2[1:2,]
        } else {
            Q_MB = t(Tm[[s1]]) %*% Q2[3:5,]
        }

        w = ifelse(stake == 1, w_lo, w_hi)

        Q = w*Q_MB + (1-w)*matrix(Q_MF[s1,],n_s2,1) + st*matrix(M[s1,],n_s2,1) + respst*R

        p = exp(b*Q[a]) / sum(exp(b*Q)) 
        if (is.na(p)) browser()
        p = max(min(p, 0.9999), 0.00001)
        LL = LL + log(p)

        if (t > 1 && df$trial[t] != df$trial[t-1]) M = matrix(0,4,3)
        M[s1,a] = 1

        R = matrix(0,3,1)
        R[which(index == action)] = 1

        dtQ[1] = Q2[s2] - Q_MF[s1,a]
        Q_MF[s1,a] = Q_MF[s1,a] + lr*dtQ[1]

        dtQ[2] = df$r[t] - Q2[s2]

        Q2[s2] = Q2[s2] + lr*dtQ[2]
        Q_MF[s1,a] = Q_MF[s1,a] + lambda*lr*dtQ[2]
    }
    return (LL)
}