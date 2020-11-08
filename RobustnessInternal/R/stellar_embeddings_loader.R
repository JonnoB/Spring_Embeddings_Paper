#' load stellar embedding csv's
#'
#' This function loads the csvs that are created when the embeddings are made by the stellar python library.
#' The function assumes that the graphs were created from graphml files. The scripts that created the embeddings
#' 
#' 
#' @param folder_path
#'
#'
#' @export

stellar_embeddings_loader <- function(folder_path){
  
  #regex pattern. Removes the last bit of the file name which is of the structure _2.csv, _54.csv etc
  pattern <- "_\\d+.csv"
  
  list.files(path = folder_path, 
             full.names = T, pattern = ".csv") %>%
    map_df(~{
      
      out <- read_csv(.x) %>%
        mutate(file = file %>% str_remove(., ".graphml")) %>%
        separate(., col = file, into = c("drop1", "fract",
                                         "drop2", "carrying_capacity",
                                         "drop3", "largest",
                                         "drop4", "smallest",
                                         "drop5", "drop6", "robin_hood_mode"),
                 sep = "_", convert = TRUE) %>%
        select(-contains("drop")) %>%
        mutate(graph = basename(.x) %>% str_remove(., pattern))%>%
        rename(value = edge_mean_length)
      
      names(out) <- make.names(names(out))
      
      return(out)
      
    }) 
  
}