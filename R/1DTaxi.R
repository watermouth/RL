# System State size
L <- 11
# Action Space
AS <- c(-1,1)
# Cost
cost <- function(ss,action){
  1
}
# Transition
Transition <- function(ss, a){
  sss <- ss + a
  if (sss < 1){
    return(1)
  }
  if (sss > L ){
    return(L)
  }
  sss
}
# big enough for impossible action cost
IMP_COST <- 10000
# Initialization
# System State
ss <- 1
# Post-decision value function table
VF.table <- rep(0,L)

find_optimal_action <- function(ss, AS){
  cost_plus_vfs <- sapply(X=AS,FUN=function(a){
    ss_next <- ss + a
    if (ss_next < 1 || ss_next > L){
      return(IMP_COST)
    }
    cost(ss, a) + VF.table[ss_next] # post-decision value function
  })
  a_next <- AS[which.min(cost_plus_vfs)]
  return(list(cost_plus_vfs, a_next))
}

# loop
N <- 1000
for (i in seq(1,N)){
  # step size
  alpha <- 1 / (1+i)
  # optimal action
  found.obj <- find_optimal_action(ss=ss, AS=AS)
  cost_plus_vfs <- found.obj[[1]]
  a_next <- found.obj[[2]] 
  # update value function
  VF.table[ss] <- (1-alpha)*VF.table[ss] + alpha * cost_plus_vfs[a_next]
  # next state
  ss <- Transition(ss, a_next)
  if (ss == L){
    # optimal policy
    policy_list <- lapply(X=seq(1:L), FUN=function(ss_idx){
      # extract action which is optimal from post-decision value function
      found.obj <- find_optimal_action(ss=ss_idx, AS=AS)
      list(ss=ss_idx, action=found.obj[[2]])
    })
    break
  }
}

