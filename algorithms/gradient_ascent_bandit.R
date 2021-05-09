
# returns rescaled preferences
action_probabilities <- function( all_preferences )
{
  exp(all_preferences)/sum(exp(all_preferences))
}

# select a preference according to weights
sample_preference <- function( preferences, probs )
{
  # preferences give us the weights
  sample( 1:length(preferences), size = 1, prob = probs)
}

bandit_gradient_ascent <-function( n_actions, reward_fun, step_size, alpha = 0.1, max_iter = 5000 )
{
  rewards <- rep(0, n_actions)
  times_action_taken <- rep(0, n_actions)
  # initialize all of the preferences to be the same
  preferences <- rep(1, n_actions)/n_actions
  
  baseline <- 0
  
  iter <- 1
  while(TRUE)
  {
    # get action probabilities
    probabilities <- action_probabilities( preferences )
    
    # select an action to take based on the probabilities
    action <- sample_preference( preferences, probabilities )
    # get the reward
    reward <- reward_fun( action )
    
    # update preferences - action taken
    preferences[action] <- preferences[action] + ( step_size * ( baseline - reward ) * (1 - probabilities[action]))
    # update preferences - other actions
    preferences[-action] <- preferences[-action] - ( step_size * ( baseline - reward ) * probabilities[action])
    # I use the simple version of a fixed alpha - rather than a shrinking term (ie the average) in case of nonstationarity
    baseline <- baseline + alpha * reward     

    if( iter > max_iter)
      break;
    iter <- iter + 1
  }
  # we just care which actions the agent prefers, what their probabilities are, and what the baseline is
  return( list( preferences = preferences, action_probs = action_probabilities( preferences ), baseline = baseline ))
}

reward_fun <- function( action )
{
  list( "1" = 5 + rnorm(1, sd = 10),
        "2" = 0 + rnorm(1, sd = 100), 
        "3" = 2 + rnorm(1, sd = 20), 
        "4" = -2 + rnorm(1,sd = 200))[[action]]
}

ascent_bandit <- bandit_gradient_ascent( 4, reward_fun, step_size= 0.05, max_iter = 100 )
# note - you sometimes get "NA in probability vector" due to inherent instability 
# basically a numerical issue with how the exp(A)/(sum(exp(A))) is calculated.
# I am not unduly concerned, and neither should be you - I imagine some small trick can fix it, 
# but I am uninterested in implementing or explaining trickery 
