import os
import networkx as nx
import pandas as pd
import numpy as np
from stellargraph import StellarGraph
from stellargraph.data import BiasedRandomWalk
from gensim.models import Word2Vec

# number of dimensions to embed into
# this is only going to be two for this experiment

dims = int(os.getenv("DIMENSIONS"))
task_id = int(os.getenv("SGE_TASK_ID")) - 1

number_of_sub_jobs = 64

# Defines the grid that will be loaded and also the indices that will be embedded
job_params = np.array(np.meshgrid(["IEEE_14", "IEEE_30", "IEEE_57", "IEEE_118", "IEEE_300", "UK_high_voltage"],
                                  np.arange(1, number_of_sub_jobs + 1))).T.reshape(-1, 2)

grid_name = job_params[task_id - 1, 0]

grid_sub_task_id = int(job_params[task_id - 1, 1])

job_indices = list(np.arange(0, 3457, 3456 / number_of_sub_jobs, dtype=int))

# The index of the files that will be used in this sub job
start_index = job_indices[0:number_of_sub_jobs][grid_sub_task_id - 1]
end_index = job_indices[1:(number_of_sub_jobs + 1)][grid_sub_task_id - 1]

# Get the working directory and store it so I can save the embedding there
project_folder = os.getcwd()

# The folder the data is stored in
# grid_folder = "/home/jonno/Dropbox/IEEE_Networks/power_grid_graphs/graphml" + "/" + grid_name + "_graphml"
grid_folder = "/home/ucabbou/power_grid_graphs/graphml" + "/" + grid_name + "_graphml"

# save path of the embedded data
save_path = project_folder + "/" + grid_name + "_" + str(grid_sub_task_id) + ".csv"

files_to_embed = os.listdir(grid_folder)[start_index:end_index]

list_of_dataframes = []
for grid_instance in files_to_embed:
    G_graphml = nx.read_graphml(grid_folder + "/" + grid_instance)
    # get the node features as a dataframe, these will then be added to the stellar graph.
    # This seems to work better than trying to put them in directly
    nodefeatures = pd.DataFrame.from_dict(dict(G_graphml.nodes(data=True)), orient='index')
    # Convert the networkx graph to a Stellargraph
    G = StellarGraph.from_networkx(G_graphml, node_features=nodefeatures)
    # The features aren't used by node2vec but it makes changing to DGI easier

    rw = BiasedRandomWalk(G)

    walks = rw.run(
        nodes=list(G.nodes()),  # root nodes
        length=30,  # maximum length of a random walk
        n=100,  # number of random walks per root node
        p=0.5,  # Defines (unormalised) probability, 1/p, of returning to source node
        q=2.0,  # Defines (unormalised) probability, 1/q, for moving away from source node
        weighted=True,
        seed=423
    )
    print("Number of random walks: {}".format(len(walks)))

    str_walks = [[str(n) for n in walk] for walk in walks]

    model = Word2Vec(str_walks, size=dims, window=10, min_count=0, sg=1, workers=1, iter=1)

    node_ids = model.wv.index2word  # list of node IDs
    node_embeddings = (
        model.wv.vectors
    )  # numpy.ndarray of size number of nodes times embeddings dimensionality

    node_embeddings_df = pd.DataFrame(data=node_embeddings)

    # uses a list comprehension inside a list comprehension to append "n" to all values
    # betweem 0 and the number of nodes in the network

    node_names = ["n" + i for i in [str(i) for i in range(len(G.nodes()))]]

    from_index = [node_names.index(i) for i in G.edge_arrays()[0]]
    to_index = [node_names.index(i) for i in G.edge_arrays()[1]]

    # subtract one edge df from the other. I can only make it work
    # converting to arrays though
    df_edge_diff = node_embeddings_df.loc[from_index,].to_numpy() - \
                   node_embeddings_df.loc[to_index,].to_numpy()

    # get the euclidean length of each edge
    df_edge_diff = np.square(df_edge_diff)
    df_edge_diff = np.sum(df_edge_diff, axis=1)
    df_edge_diff = np.sqrt(df_edge_diff)

    mean_df = pd.DataFrame(node_embeddings_df.abs().mean())
    mean_df['vars'] = pd.Series(range(0, dims))
    mean_df['id'] = 1
    mean_df = mean_df.pivot(index='id', columns='vars', values=0)

    abs_mean_df = pd.DataFrame(node_embeddings_df.abs().mean())
    abs_mean_df['vars'] = pd.Series(range(0, dims))
    abs_mean_df['id'] = 1
    abs_mean_df = abs_mean_df.pivot(index='id', columns='vars', values=0)

    # concatencate the dataframes together by column
    network_embeds = pd.concat([mean_df, abs_mean_df], axis=1)
    # add the mean edge length from the embeddings together
    network_embeds["edge_mean_length"] = np.mean(df_edge_diff)

    # add this iteration to the dataframe of embeddings
    list_of_dataframes.append(network_embeds)

# exit the inner loop

# make a single dataframe from the list
out = pd.concat(list_of_dataframes)
# Add the file names as column to allow easy identification
out['file'] = files_to_embed

# Save it all as a CSV to be loaded back into R
out.to_csv(save_path)
