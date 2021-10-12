# DOS-Project2 - Gossip Simulator
Team Members: Blas Kojusner and Michael Perez

COP5615 - Dr. Alin Dobra

October 11 2021

How to run:
dotnet fsi Project2.fsx numNodes topology algorithm

numNodes is the number of actors involved (for 3D based topologies
you can round up until you get a perfect cube), topology is one of full, 3D, line,
imp3D, algorithm is one of gossip, push-sum.

What is working:
We implemented both algorithms, gossip and push-sum, for all topologies: full, 3D, line, and imp3D. Everything is working, and convergence is guaranteed. We define convergence in the gossip protocol to be when each actor has received the rumor 10 times. We define convergence in the push-sum protocol to be when all actors either have s/w ratios that did not change more than 10^-10 in 3 consecutive rounds or if an actor's neighbor's all converged. Nodes send messages every round, not only upon receiving messages, ensuring that all actors remain busy performing computation. 

Largest network we managed to deal with for each topology and algorithm:

(Michael's 6-core 2019 MacBook Pro)
|   |  Full |  3D | Line  | Imperfect 3D  |
|---|---|---|---|---|
| Gossip  | 50,000 nodes, 371.332 sec  | 1,000,000 nodes (rounded to 1,000,000), 595.219 sec  | 5,000 nodes (rounded to 5,832), 463.643 sec  | 2,000,000 nodes (rounded to 2,000,376), 1,137.153 sec  | 
|  Push-Sum | 20,000 nodes (rounded to 21,952), 1,027.658 sec  |  10,000 nodes (rounded to 10,648), 236.414 sec |  5,000 nodes (rounded to 5,832), 515.882 sec | 100,000 nodes (rounded to 103,823), 275.235 sec  |
