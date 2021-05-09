
# we just bake the nonstationary reward function in - I am too lazy to make an R6 generator class
nonstationary_bandit <- function( n_actions, epsilon = 0.1, alpha = 0.1, tolerance= 0.001, max_iter = 500 )
{
  rewards <- rep(0, n_actions)
  times_action_taken <- rep(0, n_actions)
  
  iter <- 1
  
  
  reward_list <- list( "1" = 1,
                       "2" = 1,
                       "3" = 1,
                       "4" = 1)
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
    
    reward <- reward_list[[action]]
    reward_list <- lapply( reward_list, function(i) i + rnorm(1, sd = 0.01) )
    times_action_taken[action] <- times_action_taken[action] + 1
    # calculate the change in reward
    almost_alpha <- 1/times_action_taken[action]
    if(!is.null(alpha))
    {
      almost_alpha <- alpha
    }
    change <- almost_alpha * ( reward - rewards[action] )
    # update the reward for this specific action 
    rewards[action] <- rewards[action] + change
    
    if( #abs(change) < tolerance || 
      iter > max_iter)
      break;
    iter <- iter + 1
  }
  
  return( list( rewards = rewards,
                times_action_taken = times_action_taken,
                best_action = which.max(rewards),
                best_reward = rewards[which.max(rewards)],
                reward_list ))
}
# to see what the algorithm chooses typically
actions_avg <- nonstationary_bandit(4, alpha = NULL, max_iter = 10000 )
actions_alpha <- nonstationary_bandit(4, alpha = 0.1, max_iter = 10000 )


# exponential recency weighed average without initial bias
nonstationary_bandit_wo_bias <- function( n_actions, epsilon = 0.1, alpha = 0.1, use_beta = TRUE, tolerance= 0.001, max_iter = 500 )
{
  rewards <- rep(0, n_actions)
  times_action_taken <- rep(0, n_actions)
  
  iter <- 1
  
  
  reward_list <- list( "1" = 1,
                       "2" = 1,
                       "3" = 1,
                       "4" = 1)
  o_n <- alpha
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
    
    reward <- reward_list[[action]]
    reward_list <- lapply( reward_list, function(i) i + rnorm(1, sd = 0.01) )
    times_action_taken[action] <- times_action_taken[action] + 1
    # calculate the change in reward
    almost_alpha <- 1/times_action_taken[action]
    if(!is.null(alpha))
    {
      almost_alpha <- alpha
    }
    
    if(use_beta)
    {
      almost_alpha <- alpha/o_n
      # since o_n is conveniently recomputed each step.... we can compute next 
      # steps o_n here 
      o_n <- o_n + alpha*(1-o_n)
    }
    
    change <- almost_alpha * ( reward - rewards[action] )
    # update the reward for this specific action 
    rewards[action] <- rewards[action] + change
    
    if( #abs(change) < tolerance || 
      iter > max_iter)
      break;
    iter <- iter + 1
  }
  
  return( list( rewards = rewards,
                times_action_taken = times_action_taken,
                best_action = which.max(rewards),
                best_reward = rewards[which.max(rewards)],
                reward_list ))
}

actions_beta <- nonstationary_bandit_wo_bias( 4, alpha = 0.1, max_iter = 10000 )

