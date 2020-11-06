#' Function to clean up the toy heigh data chunk
#' 
#' This function is just sugar to make the toy height data section easier to deal with
#' 
#' @details This function has no parameters and generates all results from simulated data. 
#' The function outputs the toy_height_data dataframe that is used to plot the psychadelic beard which demonstrates
#' that the metic mean edge capacity is not great.
#' 
#' @export



generate_toy_height_data <- function(){
  
  #The function that calculate the force from the angle
  ForceV_from_angle <- function(target_angle, k, d){
    #allows us to tune an angle to get a specific force. alternatively we can tune k and d!
    tibble(H = sqrt(d^2 * (1 + tan(target_angle)^2)),
           ForceT = k*(H-d), 
           ForceV = ForceT*sin(target_angle)) %>%
      pull(ForceV)
    
  }
  
  #Get all the possible combinations of each edge excluding height data
  fixed_mean_alpha  <- 20:80 %>%
    map_df(~{
      BranchEC <-100-.x -20
      
      tibble(A = .x,
             B =10 + BranchEC*(0:100)/100,
             C = 10 +  BranchEC*(100:0)/100,)
      
    } ) %>%
    mutate(groupID = 1:n()) %>%
    gather(key = edge, value = capacity, - groupID) %>%
    left_join(tibble(edge = c("A", "B", "C"), 
                     flow = c(20,10,10)), #edge flow is always postive! 
              by = "edge") %>%
    mutate(alpha = capacity/abs(flow)) %>% #calculate alpha from the flow and edge capacity
    group_by(groupID) %>%
    mutate(mean_alpha = mean(alpha),
           mean_load = mean(1/alpha), #The harmonic mean of alpha aka mean load
           flow_fract = abs(flow)/sum(ifelse(flow>0,flow,0)),
           excess_cap = sum(alpha*flow_fract)/3) %>%
    group_by(mean_alpha) %>%
    mutate(counts = n(),
           k = 100*(10-1)*(1-1/alpha)+100,
           rank = rank(excess_cap, ties.method = "random")) %>% 
    ungroup 
  
  #Minimise the data down to only the usefully distinct data
  Edge_combos <- fixed_mean_alpha  %>%
    distinct(alpha, flow, .keep_all = TRUE) %>%
    mutate(groupID2 = 1:n())
  
  #Calculate heights for each node pair combination
  Edge_combos_delta_z <- Edge_combos$groupID2  %>% 
    map_df(~{  
      if((.x/100)%%1 == 0){ (print(paste(.x, "of", nrow(Edge_combos))))} #print every 100
      
      current_settings <- Edge_combos   %>%
        filter(groupID2 == .x)
      
      solution_angle <- nlsLM(Force ~ ForceV_from_angle(target_angle, k = k, d = 1), 
                              start = c(target_angle = pi/4), 
                              data = list(Force = current_settings$flow, k = current_settings$k), 
                              upper = pi/2)
      
      
      Out <- current_settings %>%
        mutate( theta_rads = coef(solution_angle)[1],
                theta_degs = theta_rads*360/(2*pi),
                delta_z = tan(theta_rads),
                delta_h = sqrt(delta_z^2 + 1)-1,
                strain = delta_h # this is because the distance is 1 and strain = (H-d)/d and H = delta_h+d
        ) %>%
        select(groupID, theta_rads, theta_degs, delta_z, flow, alpha, k, delta_h, strain)
    })  
  
  #calculate the ratio of excess capacity split between B and C then join with the alpha value by group
  #using capcity or 1/alpha gives the same value 
  toy_theta_temp <-  fixed_mean_alpha %>%
    select(groupID, edge, alpha, counts) %>%
    spread(key = edge, value = alpha) %>%
    mutate(
      B= 1/B, #It appears to make no difference if alpha or load level is used
      C = 1/C,
      ratio = (B)/(C+B)) %>%
    left_join(fixed_mean_alpha %>%
                select(groupID, mean_alpha, mean_load) %>%
                distinct) %>%
    select(-A, -B, -C)
  
  #get combine the previous df's together to get the strain across the system for all combinations
  toy_height_data <- fixed_mean_alpha %>%
    select(groupID, edge, flow, alpha) %>%
    #add in the height data
    left_join(Edge_combos_delta_z %>% select(-groupID))  %>%
    select(groupID, edge, strain) %>%
    #Use spread to keep the delta values for each edge
    spread(key = edge, value = strain) %>%
    mutate(mean_strain = (A+B+C)/3) %>%
    left_join(toy_theta_temp, by = "groupID")
  
  
  entropy_data <- fixed_mean_alpha %>%
    group_by(groupID) %>%
    mutate(p_capacity = capacity/sum(capacity), #fraction of total capacity
           p_alpha = alpha/sum(alpha), #fraction of total alpha
           entrop_capacity = -p_capacity*log(p_capacity), #
           entrop_alpha = -p_alpha*log(p_alpha)) %>%
    summarise(capacity = sum(entrop_capacity),
              alpha = sum(entrop_alpha)) %>%
    #normalise the entropy relative to the highest value
    mutate(capacity = capacity/max(capacity), 
           alpha = alpha/max(alpha))
  
entropy_data_out =   toy_height_data %>%
    select(groupID, mean_strain, mean_alpha) %>%
    left_join(entropy_data) %>%
    gather(key = type, value = div, -mean_strain, - mean_alpha,-groupID ) %>%
    mutate(mean_alpha = signif(mean_alpha,5))
  
  out <- list(toy_height_data = toy_height_data, entropy_data = entropy_data_out)
  
  return(out)
  
}