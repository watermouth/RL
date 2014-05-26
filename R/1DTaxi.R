#' ¥code{Taxi1D}
#'  
#' @param L System State size
#' @param AS Action Space (Action element vector)
#' @param Cost Cost function
#' @param Transition Transition function
#' @param IMP_COST big enough for impossible action cost
Taxi1D <- function(N=10000,
                   L=11, AS=c(-1,0,1),Cost=Taxi1DCost,
                   Transition=Taxi1DTransitionBase,
                   IMP_COST=10000,
                   step.size.fun=function(x){1/x},
                   print.state=F)
{
  # Initialization
  # System State
  ss <- 1
  # Post-decision value function table
  VF.table <- rep(0,L)
  Transition.Fun <- function(ss,a){Transition(ss=ss,a=a,L=L)}
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
      found.obj <- find_optimal_action(L=L, ss=ss, AS=AS,Transition=Transition.Fun, Cost=Cost,IMP_COST=IMP_COST)
      cost_plus_vfs <- found.obj[[1]]
      a_next <- found.obj[[2]] 
      # update value function
      VF.table[ss] <- (1-alpha)*VF.table[ss] + alpha * cost_plus_vfs[which(a_next == AS)]
      # next state
      ss <- Transition.Fun(ss, a_next)
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
            found.obj <- find_optimal_action(L=L, ss=ss_idx, AS=AS,Transition=Transition.Fun,Cost=Cost, IMP_COST=IMP_COST)
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

#' \code{find_optimal_action}
#' Cost function argumetns are ss and a.
find_optimal_action <- function(L, ss, AS, Transition, Cost, IMP_COST){
  cost_plus_vfs <- sapply(X=AS,FUN=function(a){
    ss_next <- Transition(ss=ss,a=a)
    if ((ss_next < 1 && a <= 0 ) || (ss_next > L && a > 0)){
      return(IMP_COST)
    }
    Cost(ss, a, ss_next) + VF.table[ss_next] # post-decision value function
  })
  a_next <- AS[which.min(cost_plus_vfs)]
  return(list(cost_plus_vfs, a_next))
}

#' Cost function's API
Taxi1DCost <- function(ss,action,ss_next=NULL){1}

Taxi1DCost2 <- function(ss,a,ss_next=NULL){
  a^2 + 1
}

Taxi1DCost2Revised <- function(ss,a,ss_next=NULL,obstacles=c(1,5),IMP_COST=10000){
  if (any(ss+a == obstacles)){
    return(IMP_COST)
  }
  a^2 + 1
}

#' Transition function's API
Taxi1DTransitionBase <- function(ss,a,L){
  sss <- ss + a
  if (sss < 1){
    return(1)
  }
  if (sss > L ){
    return(L)
  }
  sss
}

Taxi1DTransition <- function(ss,a,L,obstacles=c(1,5)){
  sss <- ss + a
  if (a > 0 && any(sss == obstacles)) {
    return(Taxi1DTransition(ss, a-1, obstacles=obstacles))
  }
  if (a < 0 && any(sss == obstacles)) {
    return(Taxi1DTransition(ss, a+1, obstacles=obstacles))
  }
  sss
}

#' 
#' a > 0 => wind brows against
Taxi1DTransition2 <- function(ss, a, L, prob=0.9){
  W <- rbinom(1,1,prob)
  adjusted <- a + (if (a > 0) -W else W)
  out <- Taxi1DTransitionBase(ss, adjusted, L)
  out
}

