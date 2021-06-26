# メモリ，グラフのクリア
rm(list=ls())
graphics.off()

# 描画のためのライブラリ読み込み
library(tidyverse)
library(gridExtra)

rw <- read.csv('data/random_walk.csv')  # 報酬の有無を決めるランダムウォーク

#データ数を数える
hh <- 201
#乱数の発生回数
R <- 1

## 乱数の発生ーハルトン数列関数による方法
#### ハルトン数列関数
halton <- function(n,x){
    no <- 1:n
    hal <- numeric(n)
    i <- 1
    while(n >= x^i){
        i <- i+1
    }
    for(j in i:1){
        hal <- hal + (no %/% (x^(j-1)))*(1/x^j)
        no <- no %% x^(j-1)
    }
    hal
}
rand1 <- matrix(qnorm(halton(hh*R,2)),hh,R)

b1 <- 5.19
b2 <- 3.69
p <- 0.11
# w_mean <- 0.51
# w_sd <- 0.31
w <- 0.39
alpha1 <- 0.54
alpha2 <- 0.42
lamb <- 0.57

# w <- w_mean + w_sd * rand1[,1]

# Number of trials
T <- 201

# Initialize Q (行動価値)
## MB and MF
Q <- matrix(numeric(T*8), nrow=T, ncol=8)  # 1-2:MB, 3-8:MF
## Hybrid
Q_H_A1 <- numeric(T)
Q_H_A2 <- numeric(T)
Q_H_B1 <- numeric(T)
Q_H_B2 <- numeric(T)
Q_H_C1 <- numeric(T)
Q_H_C2 <- numeric(T)

# 状態価値
s1 <- character(T)
s2 <- character(T)
reward <- numeric(T)

# Probability to choose left choice
pA1 <- numeric(T)
pB1 <- numeric(T)
pC1 <- numeric(T)

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

# Generate action and reward for each trial
for(t in 1:T){
    # State Aでの選択確率
    pA1[t] <- exp(b1*(Q_H_A1[t]+p*rep_A1[t])) / (exp(b1*(Q_H_A1[t]+p*rep_A1[t])) + exp(b1*(Q_H_A2[t]+p*rep_A2[t])))
#     pA1[t] <- ifelse(t!=1, 
#                      exp(b1*(Q_H_A1[t]+p*as.integer(s1[t-1]=='A1'))) / (exp(b1*(Q_H_A1[t]+p*as.integer(s1[t-1]=='A1'))) + exp(b1*(Q_H_A2[t]+p*as.integer(s1[t-1]=='A2')))),
#                      exp(b1*Q_H_A1[t]) / (exp(b1*Q_H_A1[t]) + exp(b1*Q_H_A2[t])))
    pA1[t] <- max(min(pA1[t],0.999),0.0001)
    
    # State B, C
    pB1[t] <- exp(b2*Q_H_B1[t]) / (exp(b2*Q_H_B1[t]) + exp(b2*Q_H_B2[t]))
    pB1[t] <- max(min(pB1[t],0.999),0.0001)
    pC1[t] <- exp(b2*Q_H_C1[t]) / (exp(b2*Q_H_C1[t]) + exp(b2*Q_H_C2[t]))
    pC1[t] <- max(min(pC1[t],0.999),0.0001)
    bc <- rbinom(1,1,0.7)  # See if it will move to B or C (to unify among A1 and A2)
    
    # 一様分布をふって選択を決める。乱数がpA1[t]より小さければ左を選ぶ
    if(runif(1,0,1) < pA1[t]){  # A1
        s1[t] <- 'A1'
        
        if(bc == 1){  # B1 or B2
            A1toB <- A1toB + 1
            s2[t] <- ifelse(runif(1,0,1) < pB1[t], 'B1','B2')
        }else{  # C1 or C2
            A1toC <- A1toC + 1
            s2[t] <- ifelse(runif(1,0,1) < pC1[t], 'C1','C2')
        }
        
        if(t!=T){  # update indicator function for A1
            rep_A1[t+1] <- 1
        }
    }else{  # A2
        s1[t] <- "A2"
        
        if(bc == 0){  # B1 or B2
            A2toB <- A2toB + 1
            s2[t] <- ifelse(runif(1,0,1)<pB1[t],'B1','B2')
        }else{  # C1 or C2
            A2toC <- A2toC + 1
            s2[t] <- ifelse(runif(1,0,1)<pC1[t],'C1','C2')
        }
        
        if(t!=T){  # update indicator function for A2
            rep_A2[t+1] <- 1
        }
    }
    
    # Decide reward
    prob <- rw[t,s2[t]]
    reward[t] <- as.numeric(rbinom(1,1,prob))
    
    # Insert variable name for stages 1,2 respectively
    j <- choices[[s1[t]]]
    k <- choices[[s2[t]]]
    
    # 行動価値の更新
    if(t < T){
        # Temporarily substitute same value as previous trial
        for(m in 3:8){
            Q[t+1,m] <- Q[t,m]
        }
        
        # 取られた選択の値を更新(MF)
        delta1 <- Q[t,k] - Q[t,j]
        delta2 <- reward[t] - Q[t,k]
        Q[t+1,k] <- Q[t,k] + alpha2 * delta2  # Stage 2
        Q[t+1,j] <- Q[t,j] + alpha1 * delta1 + alpha1 * lamb * delta2 # Stage 1
#         Q[t+1,j] <- Q[t,j] + alpha1 * lamb * delta2  # Stage 1 (additional stage-skipping update)
        
        # 行動価値の更新(MB)
        ## The probability 0.7/0.3 depends on whether the subject experienced more A1->B or A1->C transition or vice versa
        pA1B <- ifelse(A1toB >= A1toC, 0.7, 0.3)
        pA1C <- 1 - pA1B
        Q[t+1,1] <- pA1B*max(Q[t+1,5],Q[t+1,6]) + pA1C*max(Q[t+1,7],Q[t+1,8])
        ## Same rule applies for A2
        pA2B <- ifelse(A2toB > A2toC, 0.7, 0.3)
        pA2C <- 1 - pA2B
        Q[t+1,2] <- pA2B*max(Q[t+1,5],Q[t+1,6]) + pA2C*max(Q[t+1,7],Q[t+1,8])

        # ハイブリッドモデルの実装
        Q_H_A1[t+1] <- Q[t+1,1]*w + Q[t+1,3]*(1-w)
        Q_H_A2[t+1] <- Q[t+1,2]*w + Q[t+1,4]*(1-w)
        Q_H_B1[t+1] <- Q[t+1,5]
        Q_H_B2[t+1] <- Q[t+1,6]
        Q_H_C1[t+1] <- Q[t+1,7]
        Q_H_C2[t+1] <- Q[t+1,8]
    }
}

(data <- data.frame(s1,s2,reward,rep_A1,rep_A2))

data <- dplyr::mutate(data, A1 = if_else(s1 == 'A1',1,0))
data <- dplyr::mutate(data, A2 = if_else(s1 == 'A2',1,0))
data <- dplyr::mutate(data, B1 = if_else(s2 == 'B1',1,0))
data <- dplyr::mutate(data, B2 = if_else(s2 == 'B2',1,0))
data <- dplyr::mutate(data, C1 = if_else(s2 == 'C1',1,0))
data <- dplyr::mutate(data, C2 = if_else(s2 == 'C2',1,0))

write.csv(data, 'data/simulation_model/test1221_3.csv')

