
###########################
###########################
# # 
# # MYRIAD RUN SCRIPT
# # 
# # This is an R script that runs a single attack to failure accroding to a set of input parameters. It is designed to be run on
# # an HPC cluster, allowing each attack to run independtly when scheduled
# # 
# # A question is how much time is spent loading the necessary packages and data as this may mean that several attacks should be combined
# # to reduce the overhead.
# # 
# # 
###########################
###########################

packages <- c("rlang", "dplyr", "tidyr", "purrr", "tibble", "forcats", "igraph", "devtools", "minpack.lm" )

new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

sapply(packages, library, character.only = TRUE)



#install_github("JonnoB/PowerGridNetworking")
library(PowerGridNetworking)

#Set up file system to read the correct folders this switches between aws and windows mode

#creates the correct root depending on whether this is on the cloud or not
if(dir.exists("/home/jonno")){
  #This folder is for use on my machine
  Project_folder <- "/home/jonno/Dropbox/IEEE_Networks"
  basewd <- "/home/jonno"
}else{
  #This is for the folder that is on the cloud
  Project_folder <- "~/Dropbox/IEEE_Networks"
  basewd <- "~/Dropbox"
}

IEEE_networks <- file.path(Project_folder, "IEEE_network_files")

#Load some other useful functions
list.files(file.path(basewd, "Useful_PhD__R_Functions"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))

list.files(file.path(basewd, "Flow_Spring_System"), pattern = ".R", full.names = T) %>%
  walk(~source(.x))


##
##
#only loads IEEE_118 and the wrong one at that! delete and replace with a parametrised version!
##
##
g <- readRDS(file = graph_path) #read the target graph


#Proportionally load the network
g <- Proportional_Load(g, alpha = ec)

#For most of the simulations we will scramble the proportionally loaded networks,
#However when we are calculating the limits of 1 and Inf as well as the proportional line for the test cases
#scrambling is not required. As a result these lines of code make the overall script more flexible.
if(scramble_network){
#scramble the excess capaacity
edge_order_df <- Create_scrambled_edges(g, scramble_seed, fract = fract)

g <- g %>%
  set.edge.attribute(., "Link.Limit", value = edge_order_df$Link.Limit)

}

#Set the seed for the random order and generate the edge deletion order
set.seed(deletion_seed)
DeletionOrder <- RandomAttack(g, Target = "Edges", Number = ecount(g), Name = "Link")
#Construction the deletion order function.
FixedNodes <- quo(FixedStrategyAttack(g, DeletionOrder, "Edge", Name =  "Link"))

AttackSeries <- suppressMessages(AttackTheGrid(NetworkList = list(list(g)),
                              AttackStrategy = FixedNodes,
                              referenceGrid = NULL,
                              MinMaxComp = 0.0,
                              TotalAttackRounds=1000,
                              CascadeMode = TRUE,
                              CumulativeAttacks = NULL,
                              Demand = "Load_MW",
                              Generation = "Generation_MW",
                              EdgeName = "Link",
                              VertexName = "name",
                              Net_generation = "Net_Generation"))


#This is becuase doing all the extraction at the end would take a very long time, so I break it up over all the jobs.
#This way I only need to combine the saved files but not operate on them.
AttackSeriesSummary <- AttackSeries %>%
  ExtractNetworkStats(Generation = "Generation_MW", EdgeName = "Link", PowerFlow = "PowerFlow", Link.Limit = "Link.Limit") %>%
  mutate(attack_id = attack_id,
         GridLoading = ifelse(Blackout==1, 0, GridLoading))

#Both the attack series and the summary are saved to store as much information as possible in one go.

#The structure is generated as needed and so any new paths can just be created at this point.
#There is very little overhead in doing it this way
c(dirname(Iter_collapse_path), dirname(Iter_collapse_summary_path) ) %>% walk(~{
  if(!file.exists(.x)) dir.create(.x, recursive = T)
})

saveRDS(AttackSeries, file = Iter_collapse_path)

saveRDS(AttackSeriesSummary, file = Iter_collapse_summary_path)


#After the simulation and its summary are saved to the drive the script is complete

#######################
#######################
##
##
##END
##
##
########################
########################