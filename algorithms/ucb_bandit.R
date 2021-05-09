# upper confidence bound bandit


reward_fun <- function( action )
{
  list( "1" = 5 + rnorm(1, sd = 10),
        "2" = 0 + rnorm(1, sd = 100), 
        "3" = 2 + rnorm(1, sd = 20), 
        "4" = -2 + rnorm(1,sd = 200))[[action]]
}

ucb_action <- function( rewards, time, actions_taken, exploration )
{
  
  action <- rewards + exploration*sqrt( log(time)/actions_taken ) 
  action[is.nan(action)] <- Inf
  
  which.max( action )[1]
}

ucb_bandit_stationary <- function( n_actions, exploration, reward_fun, tolerance= 0.001, max_iter = 500 )
{
  rewards <- rep(0, n_actions)
  times_action_taken <- rep(0, n_actions)
  
  iter <- 1
  while(TRUE)
  {
    action <- ucb_action( rewards, iter, times_action_taken, exploration = exploration )
    
    reward <- reward_fun( as.character(action) )
    times_action_taken[action] <- times_action_taken[action] + 1
    # calculate the change in reward
    change <- 1/times_action_taken[action] * ( reward - rewards[action] )
    # update the reward for this specific action 
    rewards[action] <- rewards[action] + change
    
    if( abs(change) < tolerance || iter > max_iter)
      break;
    iter <- iter + 1
  }
  
  return( list( rewards = rewards, times_action_taken = times_action_taken, best_action = which.max(rewards), best_reward = rewards[which.max(rewards)] ))
}

test <- ucb_bandit_stationary(4, exploration = 5.3, reward_fun = reward_fun)

ucb_actions <- replicate( 1000, ucb_bandit_stationary(4, exploration = 1.3, reward_fun = reward_fun)[["best_action"]] )

