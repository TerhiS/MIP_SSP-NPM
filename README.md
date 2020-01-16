# MIP_SSP-NPM
Mixed Integer Linear Programming (MIP) models for the job sequencing and tool switching problem with non-identical parallel machines (SSP-NPM).
## Abstract
*This work addresses the generalisation of the NP-hard job sequencing and tool switching problem with non-identical parallel machines 
and sequence-dependent setup times where a set of jobs is to be scheduled on a set of unrelated parallel machines with machine-dependent processing and tool switching times. 
Three different mathematical models are presented for two different objectives and are applied to a set of newly generated test instances, publicly available. 
The instances are compared and analysed using the IBM CPLEX solver and a simple iterated local search heuristic.*

## Acknowledgement 
The models are part of a contribution published in the International Journal of Operational Research 2020, 
published by Inderscience Publishing, Switzerland. 
The definitive authenticated version will be available online via [IJOR/Inderscience Publishing](https://www.inderscience.com/jhome.php?jcode=ijor).

## Authors
[**D. Calmels**](https://www.researchgate.net/profile/Dorothea_Calmels)

## Content
Three different MIP models are presented in [GAMS_models](https://github.com/TerhiS/MIP_SSP-NPM/tree/master/GAMS_models). 
1) [precedence-based](https://github.com/TerhiS/MIP_SSP-NPM/tree/master/GAMS_models/precedence-based.gms)
2) [position-based](https://github.com/TerhiS/MIP_SSP-NPM/tree/master/GAMS_models/position-based.gms)
3) [time-index-based](https://github.com/TerhiS/MIP_SSP-NPM/tree/master/GAMS_models/time-index-based.gmx)

## Built With
[GAMS IDE](https://www.gams.com/download/) - Integrated Development Environment
## Solved With
[IBM ILOG CPLEX](https://www.ibm.com/de-de/products/ilog-cplex-optimization-studio) - Solver
´´´
Note: A valid CPLEX licence is required for running the models
´´´

## Test instances
The problem instances and an explanation are provided on [Mendeley](http://dx.doi.org/10.17632/ggr36f5gd5.2)


