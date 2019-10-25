#Create Gbase 2 is to use the naming convention that came after artificial waterfalls was publsihed.

setwd(basewd)

GenerationData <- read_excel("data_set_1.xlsm", sheet =10, skip = 33) %>%
  setNames(make.names(names(.))) %>%
  mutate(Site = str_sub(Node.1,1,4))

LocalAssetData <- read_excel("data_set_1.xlsm", sheet =11, skip = 11) %>%
  setNames(make.names(names(.)))

TransportData <- read_excel("data_set_1.xlsm", sheet =12, skip = 11) %>%
  setNames(make.names(names(.)))

#Clean and organise the data
trans1 <-TransportData[,1:16] %>% 
  filter(!is.na(Bus.ID)) %>%
  mutate(Bus.Name = gsub("-", "_", Bus.Name)) #having "-" means that edge names become inseperable. thus all "-" are converted to "_"

VertexMetaData <- trans1 %>%
  mutate(node_name = str_sub(Bus.Name, 1, 4)%>% gsub("-|_", "",.)) %>% #converts to site level for nodes, this reduces by about 450 nodes almost half
  rename(generation =Generation.B.....Year.Round...Transport.Model.,#rename generation for ease of taking away negative demand
         #these are renamed so that it smooths the rest of the cleaning
         demand =Demand,
         voltage = Voltage,
         bus_order = Bus.Order) %>%
  mutate(generation = if_else(demand<0, generation-demand, generation),
         demand = if_else(demand<0, 0, demand)) %>%
  group_by(node_name) %>%
  summarise(voltage = max(voltage),
            demand = sum(demand),
            generation = sum(generation),
            #Peak_Gen = sum(Generation.A.....Peak.Security...Transport.Model.),
            net_generation = sum(BusTransferB),
            bus_order = min(bus_order)) 

trans2<- TransportData[,17:59] %>%
  setNames(make.names(names(.))) %>% as_tibble %>%
  filter(!is.na(Bus.1)) %>% #remove emty rows
  mutate(Bus.1 = str_sub(Bus.1, 1, 4) %>% gsub("-|_", "",.),   #convert to site from individual nodes, this simplifies a lot a prevents sites being taken down piecemeal
         Bus.2 = str_sub(Bus.2, 1, 4)%>% gsub("-|_", "",.)) %>%
  #converting into an undirected graph and back afain puts all the node names in alphanumeric order, this prevents
  #The grouping from not correctly aggregating due to node pairs such as B to A followed by from A to B. This operation makes all
  #from A to B.
  graph_from_data_frame(., directed = FALSE) %>%
  as_data_frame()  %>%
  rename(Bus.1 = from, Bus.2 = to) %>%
  filter(!(Bus.1==Bus.2))%>% #removes internal connections, about 400 edges
  group_by(Bus.1, Bus.2) %>% #trying to stop the non-unique identifier problem
  summarise(
    y = sum(1/X..Peak.Security.), #create susceptance, The addmitance of parallel lines is y1+y2+y3+..+Yn
    edge_limit = sum(Link.Limit),
    length = mean(OHL.Length + Cable.Length), #They are all pretty much the same length when they match like this, so this isn't very important
    combines = n() #tells howmany parallel edges between two different sites were combined together, is about 300 edges
  ) %>% 
  ungroup %>%
  mutate(edge_name = paste(Bus.1,Bus.2, sep = "-"),
         #These f lines have a lower limit than thier initial values. They are given the median value for thier voltage
         edge_limit = case_when(
           edge_name == "BONB-BRAC" ~1090,
           edge_name == "FAUG-LAGG" ~329, #This is just an arbitrary number as this line is loaded so much more than median
           edge_name == "KEIT-KINT" ~1090,
           edge_name == "LAGG-MILW" ~203,
           TRUE ~ edge_limit
         )
  )

#make sure everything is under limit
# as_data_frame(gbase) %>%
#   group_by(voltage) %>%
#   summarise(mean = mean(edge_limit),
#             median = median(edge_limit))
# 
# as_data_frame(gbase) %>%
#   filter(edge_limit<power_flow)

gbase <- graph_from_data_frame(trans2, directed=FALSE, vertices = VertexMetaData)

gbase <- set.edge.attribute(gbase, "edge_name", value = get.edge.attribute(gbase, "edge_name")) %>%
  set.edge.attribute(., "weight", value = get.edge.attribute(gbase, "edge_limit")) %>% #weight should probably be the admittance but whatever.
  set.vertex.attribute(., "component", value = components(gbase)$membership)


slack_ref <- slack_ref_func(gbase) #find the most appropriate node to be the slack bus

#Add in edge Voltages
gbase <- set_edge_attr(gbase, "voltage", 
                       #Takes the minimum voltage when the nodes do not agree
                       #If statement only necessary if you need to take a value that is neither max nor min, E.g. 0
                       value = pmin(get.vertex.attribute(gbase, "voltage", get.edgelist(gbase)[,1]), 
                                    get.vertex.attribute(gbase, "voltage", get.edgelist(gbase)[,2]))
                       ) %>%
  power_flow(., slack_ref$node_name) #calculate power flow

rm(GenerationData);rm(LocalAssetData);rm(TransportData);rm(trans1);rm(trans2);rm(slack_ref);rm(VertexMetaData)
