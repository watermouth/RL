#' ¥code{Taxi1D}
#'  
#' @param L System State size
#' @param AS Action Space (Action element vector)
#' @param Cost Cost function
#' @param Transition Transition function
#' @param IMP_COST big enough for impossible action cost
Taxi1D <- function(N=10000,
                   L=11, AS=c(-1,0,1),Cost=function(ss,action){1},
                   Transition=function(ss,a){
                     sss <- ss + a
                     if (sss < 1){
                       return(1)
                     }
                     if (sss > L ){
                       return(L)
                     }
                     sss
                   },
                   IMP_COST=10000,
                   find_optimal_action = function(ss, AS){
                     cost_plus_vfs <- sapply(X=AS,FUN=function(a){
                       ss_next <- ss + a
                       if (ss_next < 1 || ss_next > L){
                         return(IMP_COST)
                       }
                       Cost(ss, a) + VF.table[ss_next] # post-decision value function
                     })
                     a_next <- AS[which.min(cost_plus_vfs)]
                     return(list(cost_plus_vfs, a_next))
                   },
                   step.size.fun=function(x){1/x},
                   print.state=F)
{
  # Initialization
  # System State
  ss <- 1
  # Post-decision value function table
  VF.table <- rep(0,L)
  # loop
  i <- 1
  n <- 0
  while(n < N) {
    n <- n + 1
    n.end <- n
    print(paste("n =", n))
    VF.table.pre <- VF.table
    while(T){
      # step size
      alpha <- step.size.fun(1+i)
      # optimal action
      found.obj <- find_optimal_action(ss=ss, AS=AS)
      cost_plus_vfs <- found.obj[[1]]
      a_next <- found.obj[[2]] 
      # update value function
      VF.table[ss] <- (1-alpha)*VF.table[ss] + alpha * cost_plus_vfs[which(a_next == AS)]
      # next state
      ss <- Transition(ss, a_next)
      if (print.state){
        print(ss);print(VF.table)
      }
      if (ss == L){
        VF.Diff.Squared <- sum((VF.table - VF.table.pre)^2)
        print(VF.Diff.Squared)
        if (n == N || VF.Diff.Squared < 1) {
          # optimal policy
          policy_list <- lapply(X=seq(1:L), FUN=function(ss_idx){
            # extract action which is optimal from post-decision value function
            found.obj <- find_optimal_action(ss=ss_idx, AS=AS)
            list(ss=ss_idx, action=found.obj[[2]])
          })
          n.end <- n
          n <- N
        }
        ss <- 1
        break
      }
      i <- i + 1
    }
  }
  l <- list(iteration.i=i, iteration.n=n.end, VF.table=VF.table, policy=do.call(what=rbind, args=policy_list))
  l
}

Taxi1DCost2 <- function(ss,a){
  a^2 + 1
}

Taxi1DCost2Revised <- function(ss,a,obstacles=c(1,5),IMP_COST=10000){
  if (any(ss+a == obstacles)){
    return(IMP_COST)
  }
  a^2 + 1
}

Taxi1DTransition <- function(ss,a, obstacles=c(1,5)){
  sss <- ss + a
  if (a > 0 && any(sss == obstacles)) {
    return(Taxi1DTransition(ss, a-1))
  }
  if (a < 0 && any(sss == obstacles)) {
    return(Taxi1DTransition(ss, a+1))
  }
  sss
}

Taxi1DCost3 <- function(ss,a){
  a^2 - ss + 100
}

