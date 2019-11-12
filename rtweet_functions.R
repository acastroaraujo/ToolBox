
## **********************************
## Packages
## **********************************

library(tidyverse)
library(rtweet) ## create token(s) separately


## **********************************
## Many token functions
## **********************************

# To do:
#   
#   make a manyToken variant of lookup_users

manyTokens_get_followers <- function(u, token) {
  
  user_info <- lookup_users(u, token = token)
  fc <- user_info$followers_count
  rl <- rate_limit(token, "get_followers")
  rl$remaining <- rl$remaining * 5e3
  
  message(user_info$screen_name, " has ", scales::comma(fc), " followers ")
  message("number of queries: ",  ceiling(fc / 75000))
  
  if (fc == 0) return(list(NULL))
  
  # ******************************************************
  # Case 1: one token is enough
  # ******************************************************
  
  if (any(rl$remaining >= fc)) {
    i <- which(rl$remaining >= fc)[[1]]
    followers <- get_followers(u, n = fc, token = token[[i]]) %>% 
      pull(user_id)
    
    # ******************************************************
    # Case 2: a combination of the existing tokens is enough
    # ******************************************************
    
  } else if (sum(rl$remaining) >= fc) {
    token_index <- which(rl$remaining > 0) 
    output <- vector("list", length(token_index))
    
    output[[1]] <- get_followers(
      user = u, n = rl$remaining[token_index[1]], 
      token = token[token_index[1]]
    )
    
    for (i in seq_along(token_index)[-1]) {
      output[[i]] <- get_followers(
        user = u, n = rl$remaining[token_index[i]], 
        token = token[token_index[i]],
        page = next_cursor(output[[i - 1]])
      )
    }
    
    followers <- bind_rows(output) %>% 
      drop_na() %>% 
      distinct() %>% 
      pull(user_id)
    
    # ******************************************************
    # Case 3: none of the tokens are enough (make loop)
    # ******************************************************
    
  } else {
    
    ## wait some time if all tokens are exhausted...
    
    if (sum(rl$remaining) == 0) { 
      message("Token reset in ", round(median(rl$reset), digits = 2), " mins...")
      Sys.sleep(as.numeric(median(abs(rl$reset)), "secs") + 2)  ## abs is to mitigate a quirky error
      rl <- rate_limit(token, "get_followers")                  ## in which reset has negative values
      rl$remaining <- rl$remaining * 5e3
      
    }
    
    output <- list()
    i <- 1
    output[[i]] <- get_followers(
      user = u, n = max(rl$remaining), 
      token = token[[which(rl$remaining == max(rl$remaining ))[1]]]
    )
    
    repeat {
      
      i <- i + 1
      rl <- rate_limit(token, "get_followers")
      rl$remaining <- rl$remaining * 5e3
      
      if (sum(rl$remaining) == 0) {   # wait min time possible if all tokens are exhausted
        message("Wait for ", round(median(rl$reset), digits = 2), " mins...")
        Sys.sleep(as.numeric(median(abs(rl$reset)), "secs") + 2)  ## abs is to mitigate a quirky error
        
      }
      
      rl <- rate_limit(token, "get_followers")
      
      while (sum(rl$remaining) == 0) {            ## Sometimes the tokens don't reset when they're supposed to.
        Sys.sleep(30)                             ## If all works well, this little snippet should never execute.
        rl <- rate_limit(token, "get_followers") 
      }
      
      rl$remaining <- rl$remaining * 5e3
      token_index <- which(rl$remaining == max(rl$remaining))[[1]]
      
      output[[i]] <- get_followers(
        user = u, n = max(rl$remaining),
        token = token[[token_index]],
        page = next_cursor(output[[i - 1]])
      )
      
      if (nrow(output[[i]]) < max(rl$remaining)) {
        break
      }
    }
    
    followers <- bind_rows(output) %>% 
      drop_na() %>% 
      distinct() %>% 
      pull(user_id)
  }
  
  output <- list(user_id = followers)   ## go back here!!
  names(output) <- user_info$user_id
  output
}


## -----------------------------------------

# manyTokens_get_friends <- function(u) {
#   
#   user_info <- lookup_users(u)
#   fc <- user_info$friends_count
#   rl <- rate_limit(token, "get_friends") %>% 
#     pull(remaining) * 5e3
#   
#   message(paste0("\n", user_info$screen_name, " is following ", scales::comma(fc), " users ", "(approx. ",  ceiling(fc / 75000), " queries are required)"))
#   
#   if (fc == 0) return(list(NULL))
#   
#   # ******************************************************
#   # Case 1: one token is enough
#   # ******************************************************
#   
#   if (any(rl >= fc)) {
#     i <- which(rl >= fc)[[1]]
#     friends <- get_friends(u, n = fc, token = token[[i]]) %>% 
#       pull(user_id)
#     
#     # ******************************************************
#     # Case 2: a combination of the existing tokens is enough
#     # ******************************************************
#     
#   } else if (sum(rl) >= fc) {
#     token_index <- which(rl > 0) 
#     output <- vector("list", length(token_index))
#     
#     output[[1]] <- get_friends(
#       user = u, n = rl[token_index[1]], 
#       token = token[token_index[1]]
#     )
#     
#     for (i in seq_along(token_index)[-1]) {
#       output[[i]] <- get_friends(
#         user = u, n = rl[token_index[i]], 
#         token = token[token_index[i]],
#         page = next_cursor(output[[i - 1]])
#       )
#     }
#     
#     friends <- bind_rows(output) %>% 
#       drop_na() %>% 
#       distinct() %>% 
#       pull(user_id)
#     
#     # ******************************************************
#     # Case 3: none of the tokens are enough (make loop)
#     # ******************************************************
#     
#   } else {
#     
#     ## wait some time if all tokens are exhausted...
#     
#     if (sum(rl) == 0) { 
#       cat("Token reset in", median(rate_limit(token, "get_friends") %>% pull(reset)), "mins...\n")
#       Sys.sleep(median(rate_limit(token, "get_friends") %>% pull(reset)) * 60 + 10)
#       rl <- rate_limit(token, "get_friends") %>% pull(remaining) * 5e3
#     }
#     
#     output <- list()
#     i <- 1
#     output[[i]] <- get_friends(
#       user = u, n = max(rl), 
#       token = token[[which(rl == max(rl))[1]]]
#     )
#     
#     repeat {
#       
#       i <- i + 1
#       df_rate_limit <- rate_limit(token, "get_friends")
#       rl <- df_rate_limit %>% pull(remaining)
#       
#       if (sum(rl) == 0) {   # wait min time possible if all tokens are exhausted
#         message("Wait for", median(df_rate_limit %>% pull(reset)), "mins...")
#         t0 <- df_rate_limit %>% pull(timestamp)
#         t1 <- df_rate_limit %>% pull(reset_at)
#         Sys.sleep(median(difftime(t1, t0, units = "secs")) + 5)
#       }
#       
#       rl <- rate_limit(token, "get_friends") %>% pull(remaining)
#       
#       ## Sometimes the tokens don't reset when they're supposed to.
#       ## If all works well, this little snippet should never execute.
#       while (sum(rl) == 0) { 
#         Sys.sleep(30)
#         rl <- rate_limit(token, "get_friends") %>% pull(remaining)
#       }
#       
#       token_index <- which(rl == max(rl))[[1]]
#       
#       output[[i]] <- get_friends(
#         user = u, n = (max(rl) * 5e3),
#         token = token[[token_index]],
#         page = next_cursor(output[[i - 1]])
#       )
#       
#       if (nrow(output[[i]]) < max(rl) * 5e3) {
#         break
#       }
#     }
#     
#     friends <- bind_rows(output) %>% 
#       drop_na() %>% 
#       distinct() %>% 
#       pull(user_id)
#   }
#   return(tibble(from = user_info$user_id, to = friends))
# }


# manyTokens_lookup_users <- function(user_list, token) {
#   
#   rl <- rate_limit(token, "lookup_users")
#   
#   ruprimny <- lookup_users(, token = token[[3]])
#   
#   
# }

## **********************************
## Quick influence ------- 
## **********************************

influence_data <- function(user, token) {
    
    user_info <- lookup_users(user, token = token[[sample(length(token), size = 1)]])
    
    user_followers <- manyTokens_get_followers(user_info$user_id, token = token)
    followers_details <- lookup_users(unlist(user_followers), token = token[[sample(length(token), size = 1)]])
    
    message(user_info$name, " has a primary influence of ",
            scales::comma(sum(c(followers_details$followers_count, user_info$followers_count))))
  
  return(list(user_info = user_info, follower_details = followers_details))
  
}

#me_df <- influence_snapshot("acastroaraujo", token = token)

ruprimny <- influence_data("RodrigoUprimny", token = token)


