library(tidyverse)
library(gridExtra)
set.seed(111)

###############  variables ##########

t <- 100 # how long will simulation take. 
n_banks <- 10 # number of bank agents
n_households <- 10000 # number of household agents (gives deposits)
n_borrowers <- 20000 # number of borrower agents (take loans)
equity <- 80     # starting equity for each bank
interest <- 0.0125 # interest 
CAR <- 0.08 # capital adequacy ratio
MRR <- 0.035 # minumal reserve ratio
depo_margin <- 0.1 # margin for the deposits. (1 + depo_margin) X interest
loan_margin <- 0.9  # calculated same as depo_margin
recovery_rate <- 0.4 # how much will bank get from default loans
withdrawal_prob_min <- 0.01 # minimum probability for household to change bank
withdrawal_prob_max <- 0.08 # maximum probability
default_prob_min <- 0.04 # minimal probability for a loan to defaul during 1 period
default_prob_max <- 0.08 # maximal
reserve_fire_sell <- 0.8 # for how much of a face value will bank sell loan when in need of liquidity to satisfy reserves
bankruptcy_fire_sell <- 0.5 # for how much will bank sell loans when go bankrupt
income_tax <- 0.18 # tax
risk_weight <- function(x){0.5 + 5*x} # how is risk weight of a loan calculated. x is PD
target_reserves <- 2 # multiplier to MRR for optimal reserves held
u_bound_CAR <- 2 # multiplier to CAR for optimal capital adequacy held
safety_effect <- 0.04
concentration_spread <- function(x){0.15*x^2} # less competition - bigger spread. To give additional spread at the start add intercept to the function


## shocks variables
# CAR shock. (sharpe increase and no change later on)
CAR_shock_TF <- F
p_change_CAR <- 40 # period of change
CAR_delta <- 0.02

# PD shock. (quite sharpe increase and steady decline)
PD_shock_TF <- F
p_change_PD <- 30
max_PD_delta <- 0.07
PD_decrease_rate <- 0.05

# MRR shock
MRR_shock_TF <- T
p_change_MRR <- 50
MRR_delta <- 0.03

# interest shock
interest_shock_TF <- F
p_change_interest <- 50
interest_delta <- 0.005

# spread shock

spread_shock_TF <- F
p_change_spread <- 50
l_margin_delta <- 0.02
d_margin_delta <- -0.01

############### agent matrices ############

households <- list(depo_value = matrix(NA,ncol = n_households, nrow = t),
                   which_bank = matrix(NA,ncol = n_households, nrow = t),
                   withdrawal_prob = matrix(NA,ncol = n_households, nrow = t))

borrowers <- list(loan_value = matrix(NA,ncol = n_borrowers, nrow = t),
                  which_bank = matrix(NA,ncol = n_borrowers, nrow = t),
                  pd = matrix(NA,ncol = n_borrowers, nrow = t),
                  recovery_rate = matrix(NA,ncol = n_borrowers, nrow = t),
                  cost_capital = matrix(NA,ncol = n_borrowers, nrow = t),
                  risk_weight = matrix(NA,ncol = n_borrowers, nrow = t),
                  loan_end = matrix(NA,ncol = n_borrowers, nrow = t))

banks <- list(equity = matrix(NA, ncol = n_banks, nrow = t),
              sum_deposits = matrix(NA,ncol = n_banks, nrow = t),
              loan_portfolio = matrix(NA,ncol = n_banks, nrow = t),
              reserves = matrix(NA,ncol = n_banks, nrow = t),
              exp_loss_provision = matrix(NA,ncol = n_banks, nrow = t),
              revenue = matrix(NA,ncol = n_banks, nrow = t),
              cost = matrix(NA,ncol = n_banks, nrow = t),
              income = matrix(NA,ncol = n_banks, nrow = t),
              interbank_given = matrix(NA,ncol = n_banks, nrow = t),
              interbank_taken = matrix(NA,ncol = n_banks, nrow = t),
              interbank_from = matrix(NA,ncol = n_banks, nrow = t),
              interbank_to = matrix(NA,ncol = n_banks, nrow = t),
              risk_weighted_assets = matrix(NA,ncol = n_banks, nrow = t),
              dividends = matrix(NA,ncol = n_banks, nrow = t),
              liquidity_assistance = matrix(0,ncol = n_banks, nrow = t),
              bankruptcy = matrix(0,ncol = n_banks, nrow = t),
              undercapitalized = matrix(0,ncol = n_banks, nrow = t))

# changing to a time series for incorporating endogenous shocks later on
CAR <- rep(CAR, t)
MRR <- rep(MRR, t)
interest <- rep(interest, t)
depo_margin <- rep(depo_margin, t)
loan_margin <- rep(loan_margin, t)


############## initil values #############

households$depo_value[1,] <- 1
households$which_bank[1,] <- sample(1:n_banks, size = n_households, replace = TRUE)
households$withdrawal_prob[1,] <- runif(ncol(households$withdrawal_prob), withdrawal_prob_min, withdrawal_prob_max)
for (o in 2:t) {
  households$withdrawal_prob[2,] <- households$withdrawal_prob[1,]
}

borrowers$pd[1:t,] <- runif(ncol(borrowers$pd), default_prob_min, default_prob_max)
borrowers$recovery_rate[1,] <- recovery_rate
borrowers$risk_weight[1,] <- risk_weight(borrowers$pd[1,])
borrowers$which_bank[1,] <- sample(1:n_banks, size = n_borrowers, replace = TRUE)
borrowers$loan_end[1,] <- sample(1:loan_periods, size = n_borrowers, replace = TRUE)

# calculating initial cost of capital
for (q in 1: n_borrowers) {
  
  borrowers$cost_capital[1,q] <- (1 + ((loan_margin[p]+1)*interest[1]) - borrowers$pd[1,q] * borrowers$recovery_rate[1,q])/(1-borrowers$pd[1,q])
  
}

banks$equity[1,] <- equity

for (w in 1:n_banks) {
  # sum deposits
  banks$sum_deposits[1,w] <- sum(households$depo_value[1,which(households$which_bank[1,] == w)])
  
  # initialize loan portfolio that is in line with regulations 
  # total of risk weighted assets on the market for a given bank
  RW_assets <- borrowers$risk_weight[1,which(borrowers$which_bank[1,] == w)]
  # potential number of loans that the bank have deposits to lend with reserve
  potential_share_of_market_reserves <- banks$sum_deposits[1,w] * (1-MRR[1])
  # potential number of loans that will be in line with regulations 
  potential_share_of_market <- table(c(cumsum(RW_assets * CAR[1]),banks$equity[1,w]+2) < banks$equity[1,w])[2] # the equity value added in c() is because there always need to be one false example
  # number loans that fullfils two requirements (reserves and capital requirements)
  potential_loans <- floor(min(potential_share_of_market_reserves, potential_share_of_market))
  #give loans to those borrowers minus some margin for safety 
  borrowers$loan_value[1,which(borrowers$which_bank[1,] == w)] <- c(rep(1,(potential_loans-5)),
                                                                    rep(0,(length(RW_assets)+5-potential_loans)))
  #sum of the loans
  banks$loan_portfolio[1,w] <- sum(borrowers$loan_value[1,which(borrowers$which_bank[1,] == w)])
  # calculate risk weighted assets
  banks$risk_weighted_assets[1,w] <- sum(borrowers$risk_weight[1,which(borrowers$which_bank[1,] == w)]*borrowers$loan_value[1,which(borrowers$which_bank[1,] == w)])
  
  #expected loss provisions
  banks$exp_loss_provision[1,w] <- sum(borrowers$pd[1,which(borrowers$which_bank[1,] == w)] * borrowers$loan_value[1,which(borrowers$which_bank[1,] == w)] * (1-borrowers$recovery_rate[1,which(borrowers$which_bank[1,] == w)]))
}

banks$income[1,] <- 0
banks$reserves[1,] <- banks$sum_deposits[1,] - banks$loan_portfolio[1,]

########### shocks ###########

# Pobability of default shock
if(PD_shock_TF == TRUE){
  decrease_t <- (1/PD_decrease_rate) # how long will it take for shock to vanish
  nom_pd_decrease <- max_PD_delta*PD_decrease_rate # p.p decrease of the effect every period
  borrowers$pd[p_change_PD,] <- borrowers$pd[p_change_PD-1,] + (max_PD_delta/2) # increase by half of the maximal delta at the beginning
  borrowers$pd[(p_change_PD+1):(p_change_PD+decrease_t),] <- cumsum(c(max_PD_delta,rep(-nom_pd_decrease,decrease_t))) # add the effect.
}
# Capital adequacy requirements shock

if(CAR_shock_TF == TRUE){
  CAR[p_change_CAR:t] <- CAR[p_change_CAR:t] + CAR_delta
  
}

# Minimal reserve ratio shock

if(MRR_shock_TF == TRUE){
  MRR[p_change_MRR:t] <- MRR[p_change_MRR:t] + MRR_delta
}

# interest rate shock

if(interest_shock_TF == TRUE){
  interest[p_change_interest:t] <- interest[p_change_interest:t] + interest_delta 
}
########## start of the simulation #########

# progress bar and timer
progress.bar <- winProgressBar("Simulating banking sector", "0% Done", 0, 1, 0)
ptm <- proc.time()

for (p in 2:t) {
  
  ########### random change of the deposits ##########
  bank_change <- rep(0, ncol(households$which_bank))# empty matrix
  
  # calculate the "safety effect" for a particular bank
  which_safe <- ifelse(banks$income[p-1,] < 0, safety_effect,0)
  
  # generate randomly 1 or 0 given a withdrawal probability of a household
  households$which_bank[p,] <- households$which_bank[p-1,]
  for (u in 1:n_households) {
    
    withdrawal_prob_new <- households$withdrawal_prob[1,u] + which_safe[households$which_bank[p,u]] # add safety effect 
    bank_change[u] <- sample(c(1,0),size = 1,replace = TRUE, prob = c(withdrawal_prob_new, 1-withdrawal_prob_new))
  }
  
  # randomly generating new bank for each deposit to change ( household can change bank to the same bank. IK its weird tho)
  households$which_bank[p,which(bank_change == 1)] <- sample(which(banks$bankruptcy[p-1,] == 0), replace = TRUE, size = length(which(bank_change == 1))) 
  
  # change of the total deposits
  households$depo_value[p,] <- households$depo_value[p-1,]
  for (e in which(banks$bankruptcy[p-1,] == 0)) {
    banks$sum_deposits[p,e] <- sum(households$depo_value[p,which(households$which_bank[p,] == e)])
  }

  ############ loan defaults ##########
  # same loan status as previous period
  borrowers$loan_value[p,] <- borrowers$loan_value[p-1,]
  which_default <- rep(0, ncol(borrowers$loan_value)) # empty matrix
  # generate randomly 1 or 0 given a pd of a loan for every lended loan
  for (i in which(borrowers$loan_value[p,] == 1)) {
    which_default[i] <- sample(c(1,0), size = 1,replace = TRUE, prob = c(borrowers$pd[p,i], 1-borrowers$pd[p,i]))
  }
  # defaulted loans are no longer worth anything (except initial recovery, later on calculated)
  borrowers$loan_value[p,which(which_default == 1)] <- 0
  
  # write-off loan defaults
  with_write_off <- borrowers$loan_value[p,]
  with_write_off[which(which_default == 1)] <- recovery_rate
  
  # calculating credit loss
  borrowers$which_bank[p,] <- borrowers$which_bank[p-1,]
  for (r in which(banks$bankruptcy[p-1,] == 0)) {
    banks$loan_portfolio[p,r] <- sum(with_write_off[which(borrowers$which_bank[p,] == r)])
  }
  loan_loss_t <- banks$loan_portfolio[p,]-banks$loan_portfolio[p-1,]
  
  # updating expected loss provisions
  borrowers$recovery_rate[p,] <- borrowers$recovery_rate[p-1,]
  for (a in which(banks$bankruptcy[p-1,] == 0)) {
    
    banks$exp_loss_provision[p,a] <- sum(borrowers$pd[p,which(borrowers$which_bank[p,] == a)] * borrowers$loan_value[p,which(borrowers$which_bank[p,] == a)] * (1-borrowers$recovery_rate[p,which(borrowers$which_bank[p,] == a)]))
    
  }
  ############ reserve check ##########
  # adjusting reserves to change in deposits
  banks$reserves[p,] <- banks$sum_deposits[p,] - banks$loan_portfolio[p,] + banks$equity[p-1,]
  # which bank must sell loans because of the minimal reserves ( 1 if they fullfill rhe requirements, 0 otherwise)
  reserve_fullfill <- ifelse(banks$reserves[p,] < (banks$sum_deposits[p,] * MRR[p]),0,1)
  fire_sell_loss <- rep(0,n_banks) # empty matrix for further PnL calculation
  
  for (d in which(reserve_fullfill == 0)) {
    if(banks$bankruptcy[p-1,d] == 1){next()}
    min_nr_loans_sell <- ceiling((banks$sum_deposits[p,d]*MRR[p] - banks$reserves[p,d])/reserve_fire_sell) # minimal number of loans to sell
    fire_sell_loss[d] <- ceiling(banks$sum_deposits[p,d]*MRR[p] - banks$reserves[p,d]) * (1-reserve_fire_sell) # for further PnL calculation
    
    bank_loans <- borrowers$loan_value[p,which(borrowers$which_bank[p,] == d & borrowers$loan_value[p,] == 1)]
    borrowers$loan_value[p,which(borrowers$which_bank[p,] == d & borrowers$loan_value[p,] == 1)] <- c(rep(1,length(bank_loans) - min_nr_loans_sell),
                                                                                                      rep(0,min_nr_loans_sell))
  }
  # repeating reserve calculation after adjusting
  banks$reserves[p,] <- banks$sum_deposits[p,] - banks$loan_portfolio[p,] + banks$equity[p-1,] 
  
  ########### calculating PnL and balance sheet ###############
  
  # changing spread (i.e. profitability of banks) given the consolidation/concentration of the market
  # depo_margin[p] <- depo_margin[p] - concentration_spread((n_banks-sum(banks$bankruptcy[p]))/n_banks)
  # loan_margin[p] <- loan_margin[p] + concentration_spread((n_banks-sum(banks$bankruptcy[p]))/n_banks)
  
  ## revenue of the bank
  # interest revenue
  
  for (l in 1:n_borrowers) {
    
    borrowers$cost_capital[p,l] <- ((1 + ((loan_margin[p]+1)*interest[p]) - borrowers$pd[2,l] * borrowers$recovery_rate[p,l])/(1-borrowers$pd[2,l]))
    
  }
  
  for (y in which(banks$bankruptcy[p-1,] == 0)) {
    
    # cost of capital multiplied by loan value
    revenue_loop <- sum(borrowers$cost_capital[p-1, borrowers$which_bank[p,] == y] * borrowers$loan_value[p, borrowers$which_bank[p,] == y]) - sum(borrowers$loan_value[p,borrowers$which_bank[p,] == y])  # minus length because we use a formula 1+interest 
    # adding interest from reserves invested during the previous period and minus fire sell 
    revenue_loop <- revenue_loop + (banks$reserves[p-1,y] * interest[p-1])
    
    banks$revenue[p,y] <- revenue_loop
    
  }
  
  ## costs of the bank
  # deposit cost
  for (f in which(banks$bankruptcy[p-1,] == 0)) {
    banks$cost[p,f] <- sum(households$depo_value[p-1, which(households$which_bank[p,] == f)] * interest[p] * (1+depo_margin[p])) + fire_sell_loss[f]
  }
  # liquidity assistance cost
  banks$cost[p,] <- banks$cost[p,] + (banks$liquidity_assistance[p-1,] * interest[p])
  
  # banks net income
  banks$income[p,] <- (banks$revenue[p,] - banks$cost[p,]) + loan_loss_t - (banks$exp_loss_provision[p-1,] - banks$exp_loss_provision[p,]) - fire_sell_loss
  banks$equity[p,] <- banks$equity[p-1,] + banks$income[p,]
  
  # dividends
  borrowers$risk_weight[p,] <-  risk_weight(borrowers$pd[p,])
  
  for (g in which(banks$bankruptcy[p-1,] == 0)) {
    # a bank may give a dividend if the equity is exceed capital adequacy ratio by 1.5 its minimum
    banks$risk_weighted_assets[p,g] <- sum(borrowers$loan_value[p,which(borrowers$which_bank[p,] == g)] * borrowers$risk_weight[p,which(borrowers$which_bank[p,] == g)])
    max_dividend <- banks$equity[p,g] - banks$risk_weighted_assets[p,g]*(CAR[p]*u_bound_CAR)
    
    # second requirement is bank may give dividend if the minimum reserve allow
    max_dividend_by_deposits <- banks$reserves[p,g] - (banks$sum_deposits[p,g] * (MRR[p] * target_reserves))
    
    # if one of them dont allow to give dividends (i.e. 0) then give zero
    banks$dividends[p,g] <- min(pmax(c(max_dividend, max_dividend_by_deposits),0))
    banks$equity[p,g] <- banks$equity[p,g] - min(pmax(c(max_dividend, max_dividend_by_deposits),0))
  }
  
  # update reserves after decreasing equity by a dividend
  banks$reserves[p,] <- banks$equity[p,] + banks$sum_deposits[p,] - banks$loan_portfolio[p,]
  
  ########### bankruptcy or undercapitalization check and liquidity assistance calulation ########
  
  #checking bankruptcy
  
  banks$bankruptcy[p,] <- ifelse(banks$equity[p,] <= 0,1,0)
  
  for (k in which(banks$bankruptcy[p,] == 1)) {
    
    # leave negative equity for the rest of the simulation
    banks$equity[p:t,k] <- banks$equity[p,k]
    
    # deleverage from deposits
    banks$sum_deposits[p:t,k] <- 0
    households$depo_value[p, which(households$which_bank[p,] == k)] <- 0
    
    # fire-sell loans
    banks$loan_portfolio[p:t,k] <- 0
    borrowers$loan_value[p, which(borrowers$which_bank[p,] == k)] <- 0
    
    # allow other banks to give loans to its clients
    borrowers$which_bank[p, which(borrowers$which_bank[p,] == k)] <- sample(which(banks$bankruptcy[p,] == 0),
                                                                            size = length(borrowers$which_bank[p, which(borrowers$which_bank[p,] == k)]),
                                                                            replace = TRUE)
    
    # allow other banks to get deposits from its client
    households$which_bank[p,which(households$which_bank[p,] == k)] <- sample(which(banks$bankruptcy[p,] == 0),
                                                                             size = length(households$which_bank[p,which(households$which_bank[p,] == k)]),
                                                                             replace = TRUE)
  }
  
  # checking undercapitalization
  banks$undercapitalized[p,] <-  ifelse(banks$equity[p,] < (banks$risk_weighted_assets[p,] * CAR[p]), 1,0)
  
  for (j in which(banks$undercapitalized[p,] == 1)) {
    
    if(banks$bankruptcy[p,j] == 1){next()}
    
    liquidity_need <- (banks$risk_weighted_assets[p,j]*CAR[p]) - banks$equity[p, j]
    
    banks$liquidity_assistance[p,j] <- liquidity_need
    
  }
  ############# lending new loans ##########
  
  for (h in which(banks$bankruptcy[p,] == 0)) {
    
    excess_reserves <- banks$reserves[p,h] - (banks$sum_deposits[p,h] * MRR[p])
    excess_capital <- (banks$equity[p,h] - (banks$risk_weighted_assets[p,h] * CAR[p]))/mean(borrowers$risk_weight[p,])
    if(any(c(excess_capital, excess_reserves) < 0)){next()}
    
    potential_loans <- min(pmax(c(excess_reserves, excess_capital), 0))
    # if the potential market is smaller than potential loans, then omit
    if(length(borrowers$loan_value[p,borrowers$loan_value[p,] == 0 & borrowers$which_bank[p,] == h]) < floor(potential_loans)){
      potential_loans <- length(borrowers$loan_value[p,borrowers$loan_value[p,] == 0 & borrowers$which_bank[p,] == h])-2
    }
    # replace bank's loans that are 0 with randomly given 1s. I.E give new loans randomly
    borrowers$loan_value[p,borrowers$loan_value[p,] == 0 & borrowers$which_bank[p,] == h] <- sample(c(rep(1, floor(potential_loans)),
                                                                                                      rep(0,length(borrowers$loan_value[p,borrowers$loan_value[p,] == 0 & borrowers$which_bank[p,] == h]) - floor(potential_loans))),
                                                                                                    replace = FALSE)
    # change loan portfolio
    banks$loan_portfolio[p,h] <- sum(borrowers$loan_value[p, which(borrowers$which_bank[p,] == h)]) 
    
  }
  
  # update reserves after increasing loans portfolio
  banks$reserves[p,] <- banks$equity[p,] + banks$sum_deposits[p,] - banks$loan_portfolio[p,]
  
  
  # end of the progress bar
  percentage <- p/t
  setWinProgressBar(progress.bar, percentage, "Simulating banking sector ",
                    sprintf("%d%% Done", round(100 * percentage)))
}


close(progress.bar)
proc.time()-ptm

############ visualisation ############
banks <- lapply(banks, as.data.frame)
households <- lapply(households, as.data.frame)
borrowers <- lapply(borrowers, as.data.frame)

# when was the shock
shocks <- data.frame(TF_shock = c(PD_shock_TF,
                            MRR_shock_TF,
                            interest_shock_TF,
                            spread_shock_TF,
                            CAR_shock_TF), 
                 when_shock = c(p_change_PD,
                            p_change_MRR,
                            p_change_interest,
                            p_change_spread,
                            p_change_CAR))                

which_shock <- which(c(PD_shock_TF,
                        MRR_shock_TF,
                        interest_shock_TF,
                        spread_shock_TF,
                        CAR_shock_TF))

# total of given variable for the agent
revenue  <- banks$revenue%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "revenue")+
  geom_vline(xintercept = c(shocks[which_shock,2]), linetype=2, 
             color = which_shock+9, size=1)+
  xlab(label = "t")

reserves  <- banks$reserves%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "reserves")+
  geom_vline(xintercept = c(shocks[which_shock,2]), linetype=2, 
             color = which_shock+9, size=1)+
  xlab(label = "t")

income  <- banks$income%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "income")+
  geom_smooth(level = 0.9999, method = "lm")+
  geom_vline(xintercept = c(shocks[which_shock,2]), linetype=2, 
             color = which_shock+9, size=1)+
  xlab(label = "")

loans  <- banks$cost%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "loans")+
  geom_vline(xintercept = c(shocks[which_shock,2]), linetype=2, 
             color = which_shock+9, size=1)

equity  <- banks$equity%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "equity")+
  geom_vline(xintercept = c(shocks[which_shock,2]), linetype=2, 
             color = which_shock+9, size=1)+
  xlab(label = "")

profitability  <- banks$income/banks$equity
profitability  <- profitability%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "profitablity")+
  geom_smooth()+
  geom_vline(xintercept = c(shocks[which_shock,2]), linetype=2, 
             color = which_shock+9, size=1)+
  xlab(label = "t")

divs  <- banks$dividends%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "dividends")+
  geom_smooth()+
  geom_vline(xintercept = c(shocks[which_shock,2]), linetype=2, 
             color = which_shock+9, size=1)+
  xlab(label = "t")


grid.arrange(equity,income, reserves, divs, profitability, revenue)
any(banks$bankruptcy == 1) # if false then no bankruptcy
any(!(banks$liquidity_assistance == 0)) # if flase then no liquidity assistance
any(banks$undercapitalized == 1) # if false then no undercapitalized

########## other visualisations ###########
# mean of given variable for the agent
revenue_mean  <- banks$revenue%>%
  mutate(total = rowMeans(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "revenue")

reserves_mean  <- banks$reserves%>%
  mutate(total = rowMeans(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "reserves")

income_mean  <- banks$income%>%
  mutate(total = rowMeans(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "income")

loans_mean  <- banks$loan_portfolio%>%
  mutate(total = rowMeans(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "loans")

equity_mean  <- banks$loan_portfolio%>%
  mutate(total = rowMeans(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "equity")

profitability <- banks$income/banks$equity
profitability <- profitability%>%
  mutate(total = rowMeans(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "profitability")+
  geom_smooth()

divs <- banks$dividends%>%
  mutate(total = rowMeans(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "dividends")+
  geom_smooth()

grid.arrange(revenue_mean, reserves_mean, income_mean, equity_mean, profitability, divs)


# grid of other totals for variables

revenue  <- banks$exp_loss_provision%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "revenue")

reserves  <- banks$cost%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "reserves")

income  <- banks$dividends%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "income")+
  geom_smooth()

loans  <- banks$liquidity_assistance%>%
  mutate(total = rowSums(.[,1:ncol(.)]))%>%
  ggplot(aes(x = 1:nrow(.), y = total))+
  geom_line()+
  labs(title = "loans")


grid.arrange(revenue, reserves, income, equity)

######### analysis of non-bankrupt ########

banks <- lapply(banks, as.matrix)
households <- lapply(households, as.matrix)
borrowers <- lapply(borrowers, as.matrix)

for (t in 1:length(banks)) {
  
  non <- banks[[t]]
  banks[[t]] <- non[,which(tail(banks$bankruptcy, n = 1) == 0)]
  
}



# plot every bank separately
plot(x = 1:nrow(banks$equity), y = banks$revenue[,1], type = "l")
for (a in 2:ncol(banks$loan_portfolio)) {
  lines(x = 1:nrow(banks$equity), y = banks$revenue[,a], col = a)
}

banks$sum_deposits%>%
  mutate(ind = factor(row_number()))%>%
  gather(variable, value, -ind)%>%
  ggplot(aes(x=ind, y=value,group = variable, fill=variable))+
  geom_area(position="fill")+
  scale_x_discrete()