Average_Rewards_for_Epsilon_Greedy <- function(trial_num=100){
  mu_vec <- 1:10
  sd_vec <- rep(2,10)
  epsilons <- c(0.01, 0.1, 0.2)
  cols <- c("red", "blue", "green")
  results <- scenario_simulation_1(
    trial_num = trial_num, mu_vec = mu_vec, sd_vec = sd_vec, epsilons = epsilons)
  average_rewards <- extract_column(results, column = "average_rewards")
  plotter_1(data = average_rewards,
            start_time = 0, cols = cols, upperbound = tail(mu_vec,1))
  plotter_1(data = average_rewards[(1:(trial_num / 10)),],
            start_time = 0,  cols = cols, upperbound = tail(mu_vec,1))
  plotter_1(data = average_rewards[-(1:(trial_num / 10)),],
            start_time = (trial_num / 10), cols = cols, upperbound = tail(mu_vec,1))
  nums_selected <- extract_column(results, column = "num_selected")
  lapply(X = seq(from=1, along.with = epsilons), FUN = function(i){
    barplot(height = nums_selected[,i])
  })
  NULL
}