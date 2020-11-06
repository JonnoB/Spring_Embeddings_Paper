# Measuring power grid robustness under variable line loads using graph embedding


This is the code used for my paper applying SETSe to cascading failures in the power-grid. The paper attacks 6 different power grids

This paper explores how graph embedding can be used as a proxy for the robustness of power-grids to cascading failure. It focuses on the Strain Elevation Tension Spring embeddings (SETSe) algorithm and compares it to two other graph embedding algorithms as well as the metrics mean edge capacity and Line Load. These 5 methods are used to model the collapse point of the giant component of the network under random attack. The analysis uses 6 power-grid networks, the IEEE load flow test cases 14, 30, 57, 118, 300 as well as the UK high voltage network.  
Usually, cascading failure research uses loadings proportional to the initial flow of the network; this paper deviates from that by loading the edges of the network so that they have different range of tolerances.
Over 300,000 attack scenarios are generated for each network and the point that the giant component is lost in each case. The paper finds that SETSe and Line Load are appropriate methods to model robustness with an $R^2$ of 0.915 and 0.95 respectively. Mean edge capacity and the two other embedding methods are not able to effectively model robustness. This is despite the other embeddings methods using 32 dimensional embeddings compared to SETSe's 1. What's more the SETSe algorithm provides a geographical embedding which allows meaningful analysis of the state of the power grid. 
Overall this paper provides two effective measures for understanding the robustness of the giant component on the power grid and shows the value of network embeddings methods for critical infrastructure.

# Code and calculation

The main code file is setse_robustness_code.Rmd.
The majority of the code is written in R a small amount uses Python and the [Stellar graph library](https://stellargraph.readthedocs.io/en/stable/README.html).
Most of the calculations of this project were done on the UCL [Myriad HPC system](https://wiki.rc.ucl.ac.uk/wiki/Myriad).
For ease of use the project has a small self-contained R package called RobustnessInternal. This package is documented but has no examples as it is used primarily to simplify the code in the script.

# Related repos

This repo depends on the following to other repo's

* [Power Grid Networking](https://github.com/JonnoB/PowerGridNetworking)
* [rSETSe](https://github.com/JonnoB/rSETSe)
* [Useful PhD R Functions](https://github.com/JonnoB/Useful_PhD__R_Functions)

This work follows on from the paper [The spring bounces back](https://doi.org/10.1007/s41109-020-00329-4). The repository for the code used in that paper is found in [SETSe_assortativity_and_clusters](https://github.com/JonnoB/SETSe_assortativity_and_clusters)
