Average_rewards_for_Policies <- function(trial_num=100, mu_vec=1:10, sd_vec=rep(2,10)){
  epsilons <- c(0.01, 0.1, 0.2)
  policies <- lapply(seq(from=1, along.with=epsilons), FUN=function(i){
    # Do not allow delayed evaluation
    force(i)
    function(slots){epsilon_greedy_policy(slots = slots,epsilon = epsilons[i])}
  })
  policies <- c(policies, ucb1_policy)
  cols <- c("red", "blue", "green", "gold")
  results <- scenario_simulation(
    trial_num = trial_num, mu_vec = mu_vec, sd_vec = sd_vec, policies=policies)
  average_rewards <- extract_column(results, column = "average_rewards")
  plotter_1(data = average_rewards,
            start_time = 0, cols = cols, upperbound = tail(mu_vec,1))
  plotter_1(data = average_rewards[(1:(trial_num / 10)),],
            start_time = 0,  cols = cols, upperbound = tail(mu_vec,1))
  plotter_1(data = average_rewards[-(1:(trial_num / 10)),],
            start_time = (trial_num / 10), cols = cols, upperbound = tail(mu_vec,1))
  nums_selected <- extract_column(results, column = "num_selected")
  lapply(X = seq(from=1, along.with = policies), FUN = function(i){
    barplot(height = nums_selected[,i])
  })
  results
}

Various_Bandit_Simulations <- function(do.return=F){
  case.list <- list()
  print("relatively small variance")
  case.list[[1]] <- Average_rewards_for_Policies(trial_num = 5000, mu_vec = 1:10, sd_vec = rep(1,10))
  print("bigger variance makes it difficult to bandit better")
  case.list[[2]] <- Average_rewards_for_Policies(trial_num = 5000, mu_vec = 1:10, sd_vec = 1:10)
  if (do.return){
    return(case.list)
  }
  NULL
}