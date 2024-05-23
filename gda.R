
###################################################### RUN THIS TO GENERATE MARKET CONDITIONS ###############################################

# generate demand data for year 1970 to 1979

d_1970 <- c(rnorm(20, 1, 1))
d_1971 <- c(rnorm(20, 1.1, 1))
# demand burst due to news of Picasso's illness
d_1972 <- c(rnorm(50, 2, 2))  
# demand burst due to Picasso's death 
d_1973 <- c(rnorm(80, 2.7, 1)) 
# fetish money gradually die down 
d_1974 <- c(rnorm(50, 2.5, 1))
d_1975 <- c(rnorm(30, 2, 1))
d_1976 <- c(rnorm(30, 1.9, 2))  
# price adjust back to normal and steadily increase
d_1977 <- c(rnorm(20, 2.1, 1)) 
d_1978 <- c(rnorm(10, 2.2, 0.5))
d_1979 <- c(rnorm(10, 2.3, 0.2)) 

# combine demand into one list 
demand_data <- list(d_1970, d_1971, d_1972, d_1973, d_1974, d_1975, d_1976, d_1977, d_1978, d_1979)

###############################################################  END  ###########################################################################


####################
#                  #
#                  #
#                  #
#    discrete      #.              
#                  #
#                  #
#                  #
#                  #
####################

###################################################### RUN THIS TO SET UP DISCRETE GDA ##########################################################

# discrete gda, return price given t and num_sold 
p_discrete_gda <- function(k, alpha, lambda, t, total_stock, num_sold) {
  n <- num_sold + 1 
  if (n < total_stock + 1) {
    discrete_ask_price <- k * alpha^(n) * exp(-lambda*t)
    return(discrete_ask_price)
  } else {
    return(10^6) 
  }
}

# filter possible bids by calculating lowest ask price given num_sold and t 
get_possible_buy_bids <- function(k, alpha, lambda, t, num_sold) {
  # print("step 2: getting price")
  possible_buy_bids <<- demand_data[[t]][which(demand_data[[t]] > p_discrete_gda(k, alpha, lambda, t, total_stock, num_sold))] 
  # print("step 3: gotten price")
  return(possible_buy_bids)
}


# return the number of units being sold at t. this function update price based on num_sold
op_discrete_gda <- function(k, alpha, lambda, t, num_sold, possible_buy_bids) {
  # the initial possible_buy_indices is based on the lowest available price at time t. Since price moves after each purchase, 
  # the following line is filtering based on the the updated num_sold, which leads to updated n and price  
  print("step 4: bid price transferred") 
  # while loop filter: num_sold - last element of num_sold_log -> number of units sold in the current t cycle 
  while (num_sold - num_sold_log[[length(num_sold_log)]] < length(possible_buy_bids)) {
    # filter possible buy bids list with the updated price with the new num_sold 
    possible_buy_bids <- possible_buy_bids[possible_buy_bids > p_discrete_gda(k, alpha, lambda, t, total_stock, num_sold)]
    if (length(possible_buy_bids) > 0) { 
      settle_price_log <<- c(settle_price_log, p_discrete_gda(k, alpha, lambda, t, total_stock, num_sold))
      buy_bids_log <<- c(buy_bids_log, possible_buy_bids[1]) 
      possible_buy_bids <- possible_buy_bids[-1] # assumption: first index gets the first unit. in reality, it depends on the bidder's tip, which depends on the bidder's perception of other bidders' tips
      num_sold <- num_sold + 1 
    } else {
      num_sold_log <<- c(num_sold_log, num_sold) 
      return(num_sold)
    }
  }
  num_sold_log <<- c(num_sold_log, num_sold) 
  return(num_sold)
  }
  
#start_discrete_gda <- function(k, alpha, lambda, start_t, end_t, t, total_stock, num_sold, num_sold_log) {
    #print(paste(current_t <- start_t + t))   

###############################################################  END  ###########################################################################
###################################################### RUN THIS TO SET PARAMS FOR DISSCRETE GDA #################################################

t <- 1
start_year <- 1969 
end_year <- 1979
alpha <- 1.05 
lambda <- 0.1
num_sold <- 0 
total_stock <- 100
k <- 1.05 
num_sold_log <- c(0) 
settle_price_log <- c(0) 
buy_bids_log <- c(0) 
era<- seq(1969, 1979)  

###############################################################  END  ###########################################################################
###################################################### RUN THIS TO START THE DISCRETE GDA function ###############################################

while (t < end_year - start_year + 1) {
  # return possible indices based on the lowest price at t 
  possible_buy_bids <- get_possible_buy_bids(k, alpha, lambda, t, num_sold)  
  num_sold <- op_discrete_gda(k, alpha, lambda, t, num_sold, possible_buy_bids) # update num_sold at t 
  t <- t + 1 # update t 
}
 
cat("Total revenue from Discrete:", sum(settle_price_log))  
cat("Average revenue from Discrete:", sum(settle_price_log) / num_sold)  
cat("Paintings left from Discrete:", total_stock - num_sold) 
cat("Primium Loss from Discrete:", sum(buy_bids_log) - sum(settle_price_log))

#print(buy_bids_log) 
#print(settle_price_log) 
#print(num_sold_log) 
#print(num_sold)   


################################################################## END ###########################################################################
###################################################### RUN THIS TO PRINT DISCRETE GDA PLOTS ######################################################
# PLOT 1: YEAR vs NUM_SOLD in each YEAR 
discrete_plot1 <- plot(era, num_sold_log, type = "o", main = "Total Number of Units Sold in Each Year (Discrete GDA)", xlab = "Year", ylab = "Number of units sold")
print(discrete_plot1) 

# PLOT 2: DIFF BETWEEN BUY_BIDS_LOG and SETTLE_PRICE_LOG 
diff <- buy_bids_log - settle_price_log  
index <- seq_along(diff) 
discrete_plot2 <- plot(index, diff, type = "o", xlab = "Index", ylab = "Premium Loss (Bid Price - Settle Price)", main = "Premium Loss vs. Unit Index (Discrete GDA)")
print(discrete_plot_2)

# print numbers 
# total revenue   


print(sum(settle_price_log))  



################################################################## END ###########################################################################

####################
#                  #
#                  #
#                  #
#   continuous     #
#                  #
#                  # 
#                  #
#                  #
####################

###################################################### RUN THIS TO SET UP CONTINOUS GDA ###############################################

# return the price at time t for continuous gda 
p_continuous_gda <- function(k, lambda, t) {   
  continuous_ask <- k * exp(-lambda*t) 
  return(continuous_ask) 
}    

################################################################# END ###########################################################################

##################################################### RUN THIS TO SET PARAMS FOR CONTINOUS GDA ###############################################
t <- 1 
start_year <- 1969 
end_year <- 1979 
lambda <- 0.1
num_sold <- 0 
total_stock <- 100 
k <- 1.1 
continuous_num_sold_log <- c(0) 
continuous_settle_price_log <- c(0) 
continuous_buy_bid_log <- c(0) 
era <- seq(1969, 1979)

###################################################### RUN THIS TO START THE CONTINUOUS GDA function ###############################################

while (t < end_year - start_year + 1) {
  out <- t * r
  available <- out - num_sold  
  buy_bids <<- demand_data[[t]][which(demand_data[[t]] > p_continuous_gda(k, lambda, t))]
  if (length(buy_bids) >= available) {  
    # demand > supple, sold out available units 
    # log buy_bids, settle_price for each t, num_sold for each t   
    continuous_buy_bid_log <- c(continuous_buy_bid_log, head(buy_bids, available)) 
    continuous_settle_price_log <- c(continuous_settle_price_log, rep(p_continuous_gda(k, lambda, t), available))
    continuous_num_sold_log <- c(continuous_num_sold_log, num_sold)
    # update num_sold 
    num_sold <- num_sold + available  
  } else {
    # demand < supply, sold based on demand 
    # log buy_bids, settle_price for each t, num_sold for each t  
    continuous_buy_bid_log <- c(continuous_buy_bid_log, buy_bids) 
    continuous_settle_price_log <- c(continuous_settle_price_log, rep(p_continuous_gda(k, lambda, t), length(buy_bids)))
    continuous_num_sold_log <- c(continuous_num_sold_log, num_sold)
    # update num_sold 
    num_sold <- num_sold + length(buy_bids) 
  }
  t <- t + 1 
}

print(continuous_buy_bid_log) 
print(continuous_settle_price_log) 
print(continuous_num_sold_log)  

cat("Total revenue from continuous:", sum(continuous_settle_price_log))  
cat("Average revenue from continuous:", sum(continuous_settle_price_log) / num_sold)  
cat("Paintings left from continuous:", total_stock - num_sold) 
cat("Premium Loss from continuous:", sum(continuous_buy_bid_log) - sum(continuous_settle_price_log))

################################################################# END ###########################################################################

###################################################### RUN THIS TO PRINT DISCRETE GDA PLOTS ######################################################
# PLOT 1: YEAR vs NUM_SOLD in each YEAR 
cont_plot1 <- plot(era, continuous_num_sold_log, type = "o", main = "Number of units sold in each year (Continuous GDA)", xlab = "Year", ylab = "Number of units sold")
print(cont_plot1) 

# PLOT 2: DIFF BETWEEN BUY_BIDS_LOG and SETTLE_PRICE_LOG 
diff <- continuous_buy_bid_log - continuous_settle_price_log  
index <- seq_along(diff) 
cont_plot2 <- plot(index, diff, type = "o", xlab = "Index", ylab = "Premium Loss (Bid Price - Settle Price)", main = "Premium Loss vs. Unit Index (Continous GDA)")
print(cont_plot2)


################################################################## END ###########################################################################



####################
#                  #
#                  #
#                  #
#      VRGDA       #
#                  #
#                  #
#                  #
#                  #
####################

###################################################### RUN THIS TO SET UP VRGDA ##########################################################
# vrgda 
p_vr_gda <- function(k, alpha, t, n, r) { 
  # scheduled time of selling nth unit, linear emission schedule
  s_n <- n/r   
  if (n > total_stock) {
    return(10^6)
  } else {
    # calculate ask price that depends on n and t  
    vrgda_ask_price <- k * (1-alpha)^(t-s_n) 
    return(vrgda_ask_price)  
  }
}

# filter possible bids by calculating lowest ask price given num_sold and t 
get_possible_buy_bids <- function(k, alpha, t, n, r) {
  # print("step 2: getting price")
  possible_buy_bids <<- demand_data[[t]][which(demand_data[[t]] > p_vr_gda(k, alpha, t, n, r))] 
  # print("step 3: gotten price")
  return(possible_buy_bids)
}


# return the number of units being sold at t. this function update price based on num_sold
op_vr_gda <- function(k, alpha, t, n, r, possible_buy_bids) {
  # the initial possible_buy_indices is based on the lowest available price at time t. Since price moves after each purchase, 
  # the following line is filtering based on the the updated num_sold, which leads to updated n and price  
  print("step 4: bid price transferred") 
  # while loop filter: num_sold - last element of num_sold_log -> number of units sold in the current t cycle 
  while (n - n_log[[length(n_log)]] < length(possible_buy_bids)) {
    # filter possible buy bids list with the updated price with the new num_sold 
    possible_buy_bids <- possible_buy_bids[possible_buy_bids > p_vr_gda(k, alpha, t, n, r)]
    if (length(possible_buy_bids) > 0) { 
      vr_settle_price_log <<- c(vr_settle_price_log, p_vr_gda(k, alpha, t, n, r))
      vr_buy_bids_log <<- c(vr_buy_bids_log, possible_buy_bids[1]) 
      possible_buy_bids <- possible_buy_bids[-1] # assumption: first index gets the first unit. in reality, it depends on the bidder's tip, which depends on the bidder's perception of other bidders' tips
      n <- n + 1 
    } else {
      n_log <<- c(n_log, n) 
      return(n)
    }
  }
  n_log <<- c(n_log, n) 
  return(n)
}

################################################################# END ###########################################################################
###################################################### RUN THIS TO SET UP VRGDA PARAMS ##########################################################

r = 10 
alpha = 0.1 
k = 1.1 
start_year = 1969 
end_year = 1979
n = 1 
t = 1
n_log <- c(0) 
vr_settle_price_log <- c(0) 
vr_buy_bids_log <- c(0)   
total_stock <- 100  
era <- seq(1969, 1979)

################################################################# END ###########################################################################
###################################################### RUN THIS TO START VRGDA #####################################################################
while (t < end_year - start_year + 1) {
  # return possible indices based on the lowest price at t 
  possible_buy_bids <- get_possible_buy_bids(k, alpha, t, n, r)  
  n <- op_vr_gda(k, alpha, t, n, r, possible_buy_bids) # update num_sold at t 
  t <- t + 1 # update t 
} 


print(vr_buy_bids_log) 
print(vr_settle_price_log) 
print(n_log) 
print(n)  


cat("Total revenue from VRGDA:", sum(vr_settle_price_log))  
cat("Average revenue from VRGDA:", sum(vr_settle_price_log) / n)  
cat("Paintings left from VRGDA:", total_stock - n + 1) 
cat("Premium Loss from VRGDA:", sum(vr_buy_bids_log) - sum(vr_settle_price_log))

################################################################# END ###########################################################################

###################################################### RUN THIS TO PRINT VRGDA PLOTS ######################################################

# PLOT 1: YEAR vs NUM_SOLD in each YEAR 
vr_plot1 <- plot(era, n_log, type = "o", main = "Number of units sold in each year (VRGDA)", xlab = "Year", ylab = "Number of units sold")
print(vr_plot1) 

# PLOT 2: DIFF BETWEEN BUY_BIDS_LOG and SETTLE_PRICE_LOG 
vr_diff <- continuous_buy_bid_log - continuous_settle_price_log  
vr_index <- seq_along(vr_diff) 
vr_plot2 <- plot(vr_index, diff, type = "o", xlab = "Index", ylab = "Premium Loss (Bid Price - Settle Price)", main = "Premium Loss vs. Unit Index (VRGDA)")
print(vr_plot2) 




################################################################# END ###########################################################################


