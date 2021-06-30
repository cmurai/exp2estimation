rlsim = function(x,R=NULL,T) {
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
    Q_MB = numeric(2)
    Q_MF = matrix(0, nrow = 3, ncol = 2)
    Q_H = matrix(0, nrow = 3, ncol = 2)

    s1 = rep(1,T)        # state in stage 1
    s2 = numeric(T)      # state in stage 2
    ch1 = numeric(T)     # action at stage 1
    ch2 = numeric(T)     # action at stage 2
    reward = numeric(T)  # obtained reward at stage 2

    # Indicator func if same choice was chosen for s1
    rep_A1 <- numeric(T)
    rep_A2 <- numeric(T)

    # Indicator func counting # of times A1/A2 transitioned to B/C
    A1toB <- 0
    A1toC <- 0
    A2toB <- 0
    A2toC <- 0

    choices <- c(1:8)
    names(choices) <- c('A1_MB','A2_MB','A1','A2','B1','B2','C1','C2')

    for (t in 1:T) {
        pA = 1 / (1 + exp(beta2*(Q_H[s1[t],2] + p*rep_A2[t]) - (Q_H[s1[t],1] + p*rep_A1[t])))
        pB <- 1 / (1 + exp(beta2 * (Q_H[s2[t],2] - Q_H[s2[t],1])))
        pC <- 1 / (1 + exp(beta2 * (Q_H[s3[t],2] - Q_H[s3[t],1])))

        pA <- max(min(pA,0.999),0.0001)
        pB <- max(min(pB,0.999),0.0001)
        pC <- max(min(pC,0.999),0.0001)

        bc = rbinom(1,1,0.7)  # common or rare transition to stage 2

        # Decide choice with uniform random; left if smaller than pA
        if (runif(1) < pA) {  # choose left
            ch1[t] = 1

            if (bc) {
                s2[t] = 2
                A1toB = A1toB + 1
                ch2[t] = ifelse(runif(1) < pB, 1, 2)
            } else {
                s2[t] = 3
                A1toC = A1toC + 1
                ch2[t] = ifelse(runif(1) < pC, 1, 2)
            }
            if ((t != 1 && t != T) && s1[t-1] == 'A1') rep_A1[t] = 1
        } else {  # choose right
            ch1[t] = 2

            if(bc) {
                s2[t] = 3
                A1toC = A1toC + 1
                ch2[t] = ifelse(runif(1) < pC, 1, 2)
            } else {
                s2[t] = 2
                A1toB = A1toB + 1
                ch2[t] = ifelse(runif(1) < pB, 1, 2)
            }
            if((t != 1 && t != T) && s1[t-1] == 'A2') rep_A2[t] = 1
        }

        # Decide reward
        tmp = ifelse(s2[t] == 2, ch2[t], ch2[t] + 2)
        reward[t] = rbinom(1,1,R[t,tmp])

        # Update action value
        if (t < T) {
            # MF update
            delta1 = Q_MF[s2[t],ch2[t]] - Q_MF[1,ch1[t]]
            delta2 = reward[t] - Q_MF[s2[t],ch2[t]]
            Q_MF[1,ch1[t]] = Q_MF[1,ch1[t]] + alpha1 * delta1          # stage 1
            Q_MF[s2[t],ch2[t]] = Q_MF[s2[t],ch2[t]] + alpha2 * delta2  # stage 2
            Q_MF[1,ch1[t]] = Q_MF[1,ch1[t]] + alpha1 * lamb * delta2   # reupdate stage 1

            # MB update 
            pA1B = ifelse(A1toB >= A1toC, 0.7, 0.3)
            pA2B = ifelse(A2toB > A2toC, 0.7, 0.3)
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
        rep1 = rep1, rep2 = rep2,
    )

    return (df)
}