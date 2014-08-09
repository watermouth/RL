# state, action, reward, transition
# state : can be ignored
# action : selecting one bandit
# reward : getting value from bandit
# transition : no transition

action <- function(slot, FUN.RAND=rnorm){
  slot$e_num <- slot$e_num + 1
  n <- slot$e_num
  slot$reward <- FUN.RAND(1, slot$mu, slot$sd)
  previous_mean <- slot$e_reward_mean
  # 平均と分散の逐次更新
  slot$e_reward_mean <- (n - 1) / n * previous_mean + 1 / n * slot$reward 
  slot$e_reward_variance <- 
    (slot$reward - slot$e_reward_mean)*(slot$reward - previous_mean) +
    slot$e_reward_variance
  slot
}

get_initialized_slot <- function(mu=0, sd=1){
  list(e_num=0, mu=mu, sd=sd, e_reward_mean=0, e_reward_variance=0)
}

get_initialized_slots <- function(mu=c(0,1), sd=c(1,2)){
  lapply(X = seq(1,along.with = mu), FUN = function(i){
    get_initialized_slot(mu[i],sd[i])
  })
}
greedy_policy <- function(slots){
  mu_vec <- sapply(X = slots, FUN=function(i){i$e_reward_mean})
  which.max(mu_vec)
}

random_policy <- function(slots){
  sample(x = 1:length(slots), size = 1)
}

epsilon_greedy_policy <- function(slots, epsilon=0.1){
  judge <- rbinom(n=1, size = 1, prob = epsilon)
  if (judge == 1) {
    return(random_policy(slots))
  } else {
    return(greedy_policy(slots))
  }
}

ucb1_policy <- function(slots, coeff=2){
  selected_nums <- sapply(X = slots, FUN = function(slot){
    slot$e_num
  })
  sum_of_selected_nums <- sum(selected_nums)
  e_reward_means <- sapply(X = slots, FUN = function(slot){
    slot$e_reward_mean
  })
  values <- e_reward_means + sqrt(coeff * log(sum_of_selected_nums) / selected_nums)  
  which.max(values)
}

bandit_simulation <- function(trial_num=100, slots, action.fun=action, policy = epsilon_greedy_policy){
  num_slot <- length(slots)
  rewards <- vector(mode = "numeric", length = trial_num)
  # initial policy
  for (i in seq(from=1, along.with = slots)){
    slots[[i]] <- action.fun(slots[[i]])
    rewards[i] <- slots[[i]]$reward
  }
  # specified policy
  for (i in (num_slot+1):trial_num){
    index_slot <- policy(slots)
    slots[[index_slot]] <- action.fun(slots[[index_slot]])
    rewards[i] <- slots[[index_slot]]$reward
  }
  # slot summary
  num_selected <- sapply(slots, FUN=function(slot){slot$e_num})
  list(average_rewards=c(0,cumsum(rewards) / (1:trial_num)), slots=slots, num_selected=num_selected)
}

scenario_simulation_1 <- function(
  trial_num=100, mu_vec=1:10, sd_vec=rep(10,10), epsilons=c(0.01, 0.1, 0.2)
){
  # slots
  slots <- get_initialized_slots(mu = mu_vec, sd=sd_vec)
  # simulation
  results <- lapply(X = epsilons, FUN = function(epsilon){
    bandit_simulation(trial_num = trial_num,
                      slots = slots,
                      policy = function(slots){
                        epsilon_greedy_policy(slots = slots, epsilon = epsilon
                        )}
    )
  })
  results
}

scenario_simulation <- function(
  trial_num=100, mu_vec=1:10, sd_vec=rep(10,10), policies
){
  # slots
  slots <- get_initialized_slots(mu = mu_vec, sd=sd_vec)
  # simulation
  results <- lapply(X = policies, FUN = function(policy){
    bandit_simulation(trial_num = trial_num, slots = slots, policy = policy)
  })
  results
}

extract_column <- function(results, column = average_rewards){
  values <- sapply(X = results, FUN = function(i){i[[column]]})
}

plotter_1 <- function(data, start_time=0, upperbound, cols = c("red", "blue", "green")){
  ts.plot(ts(data, start = start_time), gpars = list(col=cols))
  abline(h = upperbound, col="grey")
}

