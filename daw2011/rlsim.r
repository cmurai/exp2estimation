rlsim = function(x,T,R=NULL) {
    # Set parameter values
    beta1 = x[1]
    beta2 = x[2]
    alpha1 = x[3]
    alpha2 = x[4]
    p = x[5]
    lambda = x[6]
    w = x[7]

    if (is.null(R)) R = r_values(T)

    # Initialize state-action value Q 
    Q_MB= numeric(2)
    Q_MF= matrix(0, nrow = 3, ncol = 2)
    Q_H = matrix(0, nrow = 3, ncol = 2)

    s1 = rep(1,T)        # state in stage 1
    s2 = numeric(T)      # state in stage 2
    ch1 = numeric(T)     # action at stage 1
    ch2 = numeric(T)     # action at stage 2
    reward = numeric(T)  # obtained reward at stage 2

    # Indicator func if same choice was chosen for s1
    rep_A1 = numeric(T)
    rep_A2 = numeric(T)

    # Indicator func counting # of times A1/A2 transitioned to B/C
    A1toB = numeric(T)
    A1toC = numeric(T)
    A2toB = numeric(T)
    A2toC = numeric(T)

    for (t in 1:T) {
        if (t != 1 && ch1[t-1] == 1) {
            pA = 1 / (1 + exp(beta1 * (Q_H[1,2] - (Q_H[1,1] + p))))
        } else if (t != 1 && ch1[t-1] == 2) {
            pA = 1 / (1 + exp(beta1 * ((Q_H[1,2] + p) - Q_H[1,1])))
        } else {
            pA = 1 / (1 + exp(beta1 * (Q_H[1,2] - Q_H[1,1])))
        }
        pB = 1 / (1 + exp(beta2 * (Q_H[2,2] - Q_H[2,1])))
        pC = 1 / (1 + exp(beta2 * (Q_H[3,2] - Q_H[3,1])))

        pA = max(min(pA,0.999),0.0001)
        pB = max(min(pB,0.999),0.0001)
        pC = max(min(pC,0.999),0.0001)

        bc = rbinom(1,1,0.7)  # common or rare transition to stage 2

        # Decide choice with uniform random; left if smaller than pA
        if (runif(1) < pA) {  # choose left
            ch1[t] = 1

            if (bc) {  # state 2
                s2[t] = 2
                A1toB[t] = ifelse(t != 1, A1toB[t-1] + 1, 1)
                if (t != 1) {
                    A1toC[t] = A1toC[t-1]
                    A2toB[t] = A2toB[t-1]
                    A2toC[t] = A2toC[t-1]
                }
                ch2[t] = ifelse(runif(1) < pB, 1, 2)
            } else {  # state 3
                s2[t] = 3
                A1toC[t] = ifelse(t != 1, A1toC[t-1] + 1, 1)
                if (t != 1) {
                    A1toB[t] = A1toB[t-1]
                    A2toB[t] = A2toB[t-1]
                    A2toC[t] = A2toC[t-1]
                }
                ch2[t] = ifelse(runif(1) < pC, 1, 2)
            }
            if (t != 1 && ch1[t-1] == 1) rep_A1[t] = 1
        } else {  # choose right
            ch1[t] = 2

            if(bc) {
                s2[t] = 3
                A2toC[t] = ifelse(t != 1, A2toC[t-1] + 1, 1)
                if (t != 1) {
                    A1toB[t] = A1toB[t-1]
                    A1toC[t] = A1toC[t-1]
                    A2toB[t] = A2toB[t-1]
                }
                ch2[t] = ifelse(runif(1) < pC, 1, 2)
            } else {
                s2[t] = 2
                A2toB[t] = ifelse(t != 1, A2toB[t-1] + 1, 1)
                if (t != 1) {
                    A1toB[t] = A1toB[t-1]
                    A1toC[t] = A1toC[t-1]
                    A2toC[t] = A2toC[t-1]
                }
                ch2[t] = ifelse(runif(1) < pB, 1, 2)
            }
            if(t != 1 && ch1[t-1] == 2) rep_A2[t] = 1
        }

        # Decide reward
        tmp = ifelse(s2[t] == 2, ch2[t], ch2[t] + 2)
        reward[t] = rbinom(1,1,R[t,tmp])

        # Update action value
        if (t < T) {
            # MF update
            Q_MF[1,ch1[t]] = Q_MF[1,ch1[t]] + alpha1 * (Q_MF[s2[t],ch2[t]] - Q_MF[1,ch1[t]])      # stage 1
            Q_MF[s2[t],ch2[t]] = Q_MF[s2[t],ch2[t]] + alpha2 * (reward[t] - Q_MF[s2[t],ch2[t]])   # stage 2
            Q_MF[1,ch1[t]] = Q_MF[1,ch1[t]] + alpha1 * lambda * (reward[t] - Q_MF[s2[t],ch2[t]])  # re-update stage 1

            # MB update 
            pA1B = ifelse(A1toB[t] >= A1toC[t], 0.7, 0.3)
            pA2B = ifelse(A2toB[t] > A2toC[t], 0.7, 0.3)
            pA1C = 1 - pA1B
            pA2C = 1 - pA2B
            Q_MB[1] = pA1B * max(Q_MF[2,1],Q_MF[2,2]) + pA1C * max(Q_MF[3,1],Q_MF[3,2])
            Q_MB[2] = pA2B * max(Q_MF[2,1],Q_MF[2,2]) + pA2C * max(Q_MF[3,1],Q_MF[3,2])

            Q_H[1,] = w * Q_MB + (1-w) * Q_MF[1,]
            Q_H[2,] = Q_MF[2,]
            Q_H[3,] = Q_MF[3,]
        }
    }

    df = data.frame(
        t = c(1:T),
        s1 = s1, s2 = s2, 
        ch1 = ch1, ch2 = ch2, 
        reward = reward,
        rep_A1 = rep_A1, rep_A2 = rep_A2,
        A1toB = A1toB, A2toB = A2toB, 
        A1toC = A1toC, A2toC = A2toC
    )

    return (df)
}