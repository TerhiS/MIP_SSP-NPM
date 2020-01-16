# MIP_SSP-NPM

Mixed Integer Linear Programming (MIP) models for the job sequencing and tool switching problem with non-identical parallel machines.

Three different MIP models are presented in [GAMS_models](https://github.com/TerhiS/MIP_SSP-NPM/tree/master/GAMS_models). 
1) precedence-based
2) position-based
3) time-index-based

# Running the tests
Experiments were conducted on a 3.20 GHz Intel Core i8-8700 processor with 32 GB of memory running under Windows 10. 
IBM CPLEX 12.8 on GAMS 25.1.3 was used to implement and solve the mathematical model while allowing CPLEX to use all cores in parallel. 

## Acknowledgement 
The models are part of a contribution published in the International Journal of Operational Research 2020, 
published by Inderscience Publishing, Switzerland. 
The definitive authenticated version will be available online via [IJOR/Inderscience Publishing](https://www.inderscience.com/jhome.php?jcode=ijor).
# Abstract
```
This paper addresses the generalisation of the NP-hard job sequencing and tool switching problem with non-identical parallel machines and sequence-dependent setup times where a set of jobs is to be scheduled on a set of unrelated parallel machines with machine-dependent processing and tool switching times. Three different mathematical models are presented for two different objectives and are applied to a set of newly generated test instances, publicly available. The instances are compared and analysed using a standard commercial solver and a simple iterated local search heuristic. Overall, it is shown that the solution quality obtained by the mathematical models depends on the size of the problem instance as well as the tool requirements. Computational results show that, based on the given computation time limit, the precedence-based formulation in general is superior to the position-based and time-index-based formulation for dense problem instances while the position-based formulation works well for sparse problems. With an increasing problem size, however, the ILS metaheuristic requires significantly less time to find near-optimal solutions than the mathematical models.
```

# Test instances
The problem instances and an explanation are provided on [Mendeley](http://dx.doi.org/10.17632/ggr36f5gd5.2)

## Authors
[**D. Calmels**](https://www.researchgate.net/profile/Dorothea_Calmels)
