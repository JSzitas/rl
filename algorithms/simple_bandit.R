# we call this to get our reward - this is called Bandit in the book 
reward_fun <- function( action )
{
  list( "1" = 5 + rnorm(1, sd = 10),
        "2" = 0 + rnorm(1, sd = 100), 
        "3" = 2 + rnorm(1, sd = 20), 
        "4" = -2 + rnorm(1,sd = 200))[[action]]
}

simple_bandit <- function( n_actions, epsilon, reward_fun, tolerance= 0.001, max_iter = 500 )
{
  rewards <- rep(0, n_actions)
  times_action_taken <- rep(0, n_actions)
  
  iter <- 1
  while(TRUE)
  {
    # decide if we are exploring or exploiting
    explore_vs_exploit <- runif(1)
    if( explore_vs_exploit <= epsilon )
    {
      # pick random action with probability 1-epsilon (which is really equal to 
      # epsilon if we are sampling from a random uniform)
      action <- sample( 1:n_actions, 1 )
    }
    else
    {
      # otherwise exploit 
      action <- which.max( rewards )
    }
    
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
# to see what the algorithm chooses typically
actions <- replicate( 1000, simple_bandit(4, epsilon = 0.3, reward_fun = reward_fun)[["best_action"]] )
