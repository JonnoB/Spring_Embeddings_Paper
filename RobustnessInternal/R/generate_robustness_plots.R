#' generate robustness plots
#' 
#' Creates the main plot and plots for the appendix
#' 
#' @param PL_SETSe_emebeddings dataframe
#' @param stellar_embeds dataframe
#' @param all_SETSe_emebeddings dataframe
#' 
#' @export

generate_robustness_plots <- function(PL_SETSe_emebeddings, stellar_embeds, all_SETSe_emebeddings ){
  
pl_plot_df <- PL_SETSe_emebeddings %>%
  filter(graph == "IEEE_118_igraph",
         carrying_capacity !=1.010,
         average_type == "mean")

# Add in the pl stellar if you want but yikes what a horror show. 
# pl_plot_df <- pl_plot_df %>% select(graph, carrying_capacity, average_type, attack_round) %>%
#   distinct() %>%
#   mutate(graph = str_remove(graph, "_igraph")) %>%
#   left_join(stellar_embeds_pl %>% select(graph, carrying_capacity , value, metric)) %>%
#   bind_rows(pl_plot_df %>% select(graph, carrying_capacity , value, metric, average_type, attack_round))

stellar_embeds %>% 
  group_by(graph, metric) %>%
  mutate(
    value = kappa(value),
    average_type = "mean",
    robin_hood_mode = as.character(robin_hood_mode)) %>%
  bind_rows(. ,
            all_SETSe_emebeddings %>%
              mutate(graph = str_remove(graph, "_igraph"))
  ) %>%
  filter(
    graph == "IEEE_118",
    grepl("mean", average_type),
    metric !="elev"
    #average_type == average_type2
  ) %>%
  #select(metric, value, attack_round, metric, carrying_capacity)
  mutate(metric = str_replace(metric, "_", " "),
         error_spike = carrying_capacity>1.5 & as.logical(robin_hood_mode) & fract == 1) %>%
  ggplot(aes(x = value, y = attack_round, group = metric)) + 
  geom_point(aes( colour =  as.factor(carrying_capacity))) +
  geom_line(data = pl_plot_df %>% filter(metric != "elev"), size = 1) +
  geom_smooth(method = "gam",se = FALSE) +
  facet_wrap(~metric) +
  #facet_wrap(~metric, scales = "free_x") +
  labs(colour = "Capacity", y = "Attack round", x = "Fraction of total range")+
  labs(title = "The relationship between robustness metrics and number of attacks until failure",
       y = "Number of rounds until loss of giant component") 
ggsave(file.path(FiguresFolder, "robustness_and_attacks_118.pdf"))



temp <- stellar_embeds %>% 
  group_by(graph, metric) %>%
  mutate(
    value = kappa(value),
    average_type = "mean",
    robin_hood_mode = as.character(robin_hood_mode)) %>%
  bind_rows(. ,
            all_SETSe_emebeddings2 %>%
              mutate(graph = str_remove(graph, "_igraph"))
  ) %>%
  filter(grepl("mean", average_type)) %>%
  mutate(metric = ifelse(metric =="elev", "elevation" ,metric))

unique(temp$metric) %>%
  walk(~{
  
    title_name <- paste("The relationship between", .x,  "and number of attacks until failure")
    
    temp %>%
      mutate(graph_rank = ifelse(graph == "UK_high_voltage", 999, str_remove(graph, "IEEE_"))%>% as.integer(.),
             graph = str_replace_all(graph, "_", " ") ) %>%
      filter(
        metric ==.x
        #average_type == average_type2
      ) %>%
      mutate(metric = str_replace(metric, "_", " "),
             error_spike = carrying_capacity>1.5 & as.logical(robin_hood_mode) & fract == 1) %>%
      ggplot(aes(x = value, y = attack_round, group = metric)) + 
      geom_point(aes( colour =  as.factor(carrying_capacity))) +
      #geom_line(data = pl_plot_df, size = 1) +
      geom_smooth(method = "gam",se = FALSE) +
      facet_wrap(~graph %>% fct_reorder(., graph_rank), scales = "free_y") +
      labs(colour = "Capacity", y = "Attack round", x = "Fraction of total range")+
      labs(title = title_name,
           y = "Number of rounds until loss of giant component") 
    
    ggsave(file.path(FiguresFolderAppendix, paste0(.x, "_robustness.pdf")))
    
  })


}


