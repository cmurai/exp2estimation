#----------------------------------------#
# Model function (for std parameter fit) #
#----------------------------------------#

# 7つ全てのパラメータを推定するためのモデル(スライド14)
func_notrandom = function(param, data, prior){
    # Declare parameters 
    beta1 = param[1]
    beta2 = param[2]
    alpha1 = param[3]
    alpha2 = param[4]
    p = param[5]
    lamb = param[6]
    w = param[7]
    
    N = nrow(data)

    s1 = data$s1
    s2 = data$s2
    ch1 = data$ch1
    ch2 = data$ch2
    r = data$reward
    rep_A1 = data$rep_A1
    rep_A2 = data$rep_A2

    # Q Values
    Q_MB = numeric(2)
    Q_MF = matrix(0, nrow = 3, ncol = 2)
    Q_H = matrix(0, nrow = 3, ncol = 2)
    
    # 対数尤度のための変数を宣言
    ll = 0

    # Calculate Q
    for(t in 1:N){
        
        if (t != 1) {
            pA = 1 / (1 + exp(beta1 * ((Q_H[1,2] + p*(ch1[t-1] == 2)) - (Q_H[1,1] + p*(ch1[t-1] == 1)))))
        } else {
            pA = 1 / (1 + exp(beta1 * (Q_H[1,2] - Q_H[1,1])))
        }
        pB = 1 / (1 + exp(beta2 * (Q_H[2,2] - Q_H[2,1])))
        pC = 1 / (1 + exp(beta2 * (Q_H[3,2] - Q_H[3,1])))

        pA = max(min(pA,0.999),0.0001)
        pB = max(min(pB,0.999),0.0001)
        pC = max(min(pC,0.999),0.0001)

        B1 = as.numeric(s2[t] == 2 && ch2[t] == 1)
        B2 = as.numeric(s2[t] == 2 && ch2[t] == 2)
        C1 = as.numeric(s2[t] == 3 && ch2[t] == 1)
        C2 = as.numeric(s2[t] == 3 && ch2[t] == 2)

        ll = ll + (ch1[t] == 1)*log(pA) + (ch1[t] == 2)*log(1-pA) + B1*log(pB) + 
            B2*log(1-pB) + C1*log(pC) + C2*log(1-pC)

        # Update Q
        if(t < N){
            # MF
            Q_MF[1,ch1[t]] = Q_MF[1,ch1[t]] + alpha1 * (Q_MF[s2[t],ch2[t]] - Q_MF[1,ch1[t]])  # stage 1
            Q_MF[s2[t],ch2[t]] = Q_MF[s2[t],ch2[t]] + alpha2 * (r[t] - Q_MF[s2[t],ch2[t]])    # stage 2
            Q_MF[1,ch1[t]] = Q_MF[1,ch1[t]] + alpha1 * lamb * (r[t] - Q_MF[s2[t],ch2[t]])     # re-update stage 1 based on updated stage 2

            # MB
            pA1B = ifelse(data$A1toB[t] >= data$A1toC[t], 0.7, 0.3)
            pA2B = ifelse(data$A2toB[t] > data$A2toC[t], 0.7, 0.3)
            pA1C = 1 - pA1B
            pA2C = 1 - pA2B
            Q_MB[1] = pA1B*max(Q_MF[2,1],Q_MF[2,2]) + pA1C*max(Q_MF[3,1],Q_MF[3,2])
            Q_MB[2] = pA2B*max(Q_MF[2,1],Q_MF[2,2]) + pA2C*max(Q_MF[3,1],Q_MF[3,2])
            
            # Hybrid
            Q_H[1,] = w * Q_MB + (1-w) * Q_MF[1,]
            Q_H[2,] = Q_MF[2,]
            Q_H[3,] = Q_MF[3,]
        }
    }
    if(is.null(prior)){
        lprior = 0
    }else{
        lprior = dgamma(beta1, shape = prior$gamma_shape, scale = prior$gamma_scale, log = TRUE) + 
                  dgamma(beta2, shape = prior$gamma_shape, scale = prior$gamma_scale, log = TRUE) +
                  dnorm(p, mean = 0, sd = 1, log = TRUE) + 
                  dbeta(w, prior$beta_a, prior$beta_b, log = TRUE) + 
                  dbeta(alpha1, prior$beta_a, prior$beta_b, log = TRUE) +  
                  dbeta(alpha2, prior$beta_a, prior$beta_b, log = TRUE) + 
                  dbeta(lamb, prior$beta_a, prior$beta_b, log = TRUE)
    }
    return(list(negll = - ll - lprior))
}