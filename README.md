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
We implemented both algorithms, gossip and push-sum, for all topologies: full, 3D, line, and imp3D. Everything is working, and convergence is guaranteed. 

Largest network we managed to deal with for each topology and algorithm:

(Michael's 6-core 2019 MacBook Pro)
|   |  Full |  3D | Line  | Imperfect 3D  |
|---|---|---|---|---|
| Gossip  | 50,000 nodes, 371.332 sec  | 1,000,000 nodes (rounded to 1,000,000), 595.219 sec  | 5,000 nodes (rounded to 5,832), 463.643 sec  | 2,000,000 nodes (rounded to 2,000,376), 1,137.153 sec  | 
|  Push-Sum | 20,000 nodes (rounded to 21,952), 1,027.658 sec  |  10,000 nodes (rounded to 10,648), 236.414 sec |  5,000 nodes (rounded to 5,832), 515.882 sec | 50,000 nodes (rounded to 50,653), 370.300 sec  |


Graphs:
Gossip
full: (10,000 nodes, 0.862 sec), (20,000 nodes, 2.539 sec), (50,000 nodes, 371.332 sec)

3D: (5,000 nodes (rounded to 5,832), 0.622 sec), (10,000 nodes (rounded to 10,648), 1.676 sec), (20,000 nodes (rounded to 21,952), 3.987 sec), (50,000 nodes (rounded to 50,653), 13.249 sec), (100,000 nodes (rounded to 103,823), 28.82 sec), (200,000 nodes (rounded to 205,379), 65.592 sec), (500,000 nodes (rounded to 512,000), 216.189 sec), (1,000,000 nodes (rounded to 1,000,000), 595.219 sec), (2,000,000 nodes (rounded to 2,000,376),  sec)

line: (5,000 nodes, 463.643 sec)

imp3D: (5,000 nodes (rounded to 5,832), 0.599 sec), (10,000 nodes (rounded to 10,648), 1.510 sec), (20,000 nodes (rounded to 21,952), 4.073 sec), (50,000 nodes (rounded to 50,653), 11.860 sec), (100,000 nodes (rounded to 103,823), 42.246 sec), (200,000 nodes (rounded to 205,379), 78.167 sec), (500,000 nodes (rounded to 512,000), 223.884 sec), (1,000,000 nodes (rounded to 1,000,000), 555.205 sec), (2,000,000 nodes (rounded to 2,000,376), 1,137.153 sec)

Push-sum
full: (1,000 nodes, 1.049 sec), (2,000 nodes (rounded to 2,197), 4.291 sec), (5,000 nodes (rounded to 5,832), 35.712 sec), (10,000 nodes (rounded to 10,648), 200.703 sec), (20,000 nodes (rounded to 21,952), 1,027.658 sec), (50,000 nodes (rounded to 50,653),  sec), (100,000 nodes (rounded to 103,823),  sec), (200,000 nodes (rounded to 205,379),  sec), (500,000 nodes (rounded to 512,000),  sec), (1,000,000 nodes (rounded to 1,000,000),  sec), (2,000,000 nodes (rounded to 2,000,376),  sec)

3D: (1,000 nodes, 2.334 sec), (2,000 nodes (rounded to 2,197), 10.504 sec), (5,000 nodes (rounded to 5,832), 64.282 sec), (10,000 nodes (rounded to 10,648), 236.414 sec), (20,000 nodes (rounded to 21,952),  sec), (50,000 nodes (rounded to 50,653),  sec), (100,000 nodes (rounded to 103,823),  sec), (200,000 nodes (rounded to 205,379),  sec), (500,000 nodes (rounded to 512,000),  sec), (1,000,000 nodes (rounded to 1,000,000),  sec), (2,000,000 nodes (rounded to 2,000,376),  sec)

line: (1,000 nodes, 53.677 sec), (2,000 nodes (rounded to 2,197), 112.251 sec), (5,000 nodes (rounded to 5,832), 515.882 sec), (10,000 nodes (rounded to 10,648),  sec), (20,000 nodes (rounded to 21,952),  sec), (50,000 nodes (rounded to 50,653),  sec), (100,000 nodes (rounded to 103,823),  sec), (200,000 nodes (rounded to 205,379),  sec), (500,000 nodes (rounded to 512,000),  sec), (1,000,000 nodes (rounded to 1,000,000),  sec), (2,000,000 nodes (rounded to 2,000,376),  sec)

imp3D: (1,000 nodes, 1.066 sec), (2,000 nodes (rounded to 2,197), 3.42 sec), (5,000 nodes (rounded to 5,832),  9.32 sec), (10,000 nodes (rounded to 10,648), 23.801 sec), (20,000 nodes (rounded to 21,952), 91.291 sec), (50,000 nodes (rounded to 50,653), 370.300 sec), (100,000 nodes (rounded to 103,823),  sec), (200,000 nodes (rounded to 205,379),  sec), (500,000 nodes (rounded to 512,000),  sec), (1,000,000 nodes (rounded to 1,000,000),  sec), (2,000,000 nodes (rounded to 2,000,376),  sec)
