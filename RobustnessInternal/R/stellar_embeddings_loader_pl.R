#' load stellar embedding csv's
#'
#' This function loads the csvs that are created when the embeddings are made by the stellar python library.
#' The function assumes that the graphs were created from graphml files. The scripts that created the embeddings
#' This function is just for the proportionally loaded files
#' 
#' 
#' @param folder_path
#'
#'
#' @export

stellar_embeddings_loader_pl <- function(folder_path){
  
  #regex pattern. Removes the last bit of the file name which is of the structure _2.csv, _54.csv etc
  pattern <- "_\\d+.csv"
  
  list.files(path = folder_path, 
             full.names = T, pattern = ".csv") %>%
    map_df(~{
      
      out <- read_csv(.x) %>%
        mutate(file = file %>% str_remove(., ".graphml")) %>%
       separate(., col = file, into = c(
                                        "drop2", "carrying_capacity"),
                sep = "_", convert = TRUE) %>%
       select(-contains("drop")) %>%
       mutate(graph = basename(.x) %>% str_remove(., ".csv")) %>%
        rename(value = edge_mean_length)
      
      names(out) <- make.names(names(out))
      
      return(out)
      
    }) 
  
}