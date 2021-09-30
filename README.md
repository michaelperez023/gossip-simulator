# DOS-Project2 - Gossip Simulator
Team Members: Blas Kojusner and Michael Perez

COP5615 - Dr. Alin Dobra

October 8 2021

How to run:
dotnet fsi Project2.fsx numNodes topology algorithm

Where numNodes is the number of actors involved (for 3D based topologies
you can round up until you get a perfect cube), topology is one of full, 3D, line,
imp3D, algorithm is one of gossip, push-sum.

What is working:
We implemented both algorithms, gossip and push-sum, for all topologies: full, 3D, line, and imp3D.

Largest network we managed to deal with for each topology and algorithm:
(Michael's 6-core 2019 MacBook Pro)
Gossip
full: (50,000 nodes, 371.332 sec)
3D: (2,000,000 nodes (rounded to 2,000,376),  sec)
line: (5,000 nodes, 463.643 sec)
imp3D: (2,000,000 nodes (rounded to 2,000,376), 1,137.153 sec)

Push-sum
full:
3D: 
line: 
imp3D: 

Graphs:
Gossip
full: (10,000 nodes, 0.862 sec), (20,000 nodes, 2.539 sec), (50,000 nodes, 371.332 sec)

3D: (5,000 nodes (rounded to 5,832), 0.622 sec), (10,000 nodes (rounded to 10,648), 1.676 sec), (20,000 nodes (rounded to 21,952), 3.987 sec), (50,000 nodes (rounded to 50,653), 13.249 sec), (100,000 nodes (rounded to 103,823), 28.82 sec), (200,000 nodes (rounded to 205,379), 65.592 sec), (500,000 nodes (rounded to 512,000), 216.189 sec), (1,000,000 nodes (rounded to 1,000,000), 595.219 sec), (2,000,000 nodes (rounded to 2,000,376),  sec)

line: (5,000 nodes, 463.643L sec)

imp3D: (5,000 nodes (rounded to 5,832), 0.599 sec), (10,000 nodes (rounded to 10,648), 1.510 sec), (20,000 nodes (rounded to 21,952), 4.073 sec), (50,000 nodes (rounded to 50,653), 11.860 sec), (100,000 nodes (rounded to 103,823), 42.246 sec), (200,000 nodes (rounded to 205,379), 78.167 sec), (500,000 nodes (rounded to 512,000), 223.884 sec), (1,000,000 nodes (rounded to 1,000,000), 555.205 sec), (2,000,000 nodes (rounded to 2,000,376), 1,137.153 sec)

Push-sum
full:
3D: 
line: 
imp3D: 
