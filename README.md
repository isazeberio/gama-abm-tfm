# gama-abm-tfm
This repository contains all the necessary resources to replicate my final thesis for the Master's program in Computational Social Sciences at Carlos III University.

* gama-models: includes the scripts to replicate the agent-based models in GAMA. 
* figures: includes the R script to replicate the plots and map in the thesis. 
* data: includes the databases that are used in the R script.

## Abstract
This study aims to explore the complex dynamics of media and interpersonal communication in order to gain a deeper understanding of their role in shaping public opinion. It focuses specifically on the unique scenario of the Brexit campaign in London, where the decision of the United Kingdom to leave the European Union not only highlighted the significance of public opinion on European integration but also revealed the fragility of EU regime support. Despite being a pro-Remain city within a pro-Leave country, London's exceptional case has been overlooked in the existing literature that attributes the outcome of Brexit to the influence of Eurosceptic media. This raises the question of whether the influence of media has been overstated, as studies often examine its role without considering the broader context and without sufficiently balancing the potential counter-effects, such as interpersonal communication. To address these gaps, we proposes the application of a data-driven agent-based modeling (ABM) framework using GAMA (GIS Agent-based Modeling Architecture) platform. The ABM framework is based on a simple political information flow model that acknowledges both media and interpersonal communication as important sources of political information, and on the filter-hypothesis, which suggests that the discussion network has the ability to moderate the effect of the media depending on the composition of the network (homogenous or heterogenous) and the content of the message (congruent or dissonant with respect to prior attitudes). The results are not conclusive at this stage, as the work is still in progress. The primary contribution of this article is the use of ABM built in GAMA to simulate the conditional effect of media and interpersonal communication. This study aims to demonstrate the analytical value of computational social science approaches in addressing the limitations of traditional experimental and observational studies.
 
## ABM Set up

Public opinion is modeled as a function of: 
1. Prior attitudes
2. Media exposure
3. Interpersonal communication.

During each time step, agents read the newspaper (media exposure) and interact with other agents (interpersonal communication). Through this iterative process, they update their opinions by either increasing or decreasing their inclination to vote leave, which represents their original attitude. This updating mechanism continues until the campaign referendum comes to an end (time > days, being days equal to 70).

The *leave* variable assigns an attitude towards the Brexit to the agents indicating their voting preference. This variable is defined as a float type, a decimal value ranging from 0 to 1. Within this continuous range:
* A value of 0 represents a 0% pro-Leave attitude (or 100% pro-Remain attitude)
* A value of 1 represents a 100% pro-Leave attitude (or 0% pro-Remain attitude)
* A value of 0.5 represents undecided voters (critical threshold)

