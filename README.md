# Forecasting airport transfer passenger flow using real-time data and machine learning
This repository provides the code files for our paper "Forecasting airport transfer passenger flow using real-time data and machine learning".

### Files:
1. Fit regression trees.ipynb --- Fit and tune the regression tree model
2. Empirical distribution of terminal nodes.R --- Fit empirical distributions to each of the terminal nodes. The resulting distributions are the probabilistic forecasts of connection times.
3. Empirical distribution binomial calculation.R --- This script calculates the distribution of passenger flows (# of passengers arriving in each 15 min interval) when assuming passenger arrivals are independent. Here we show the example for domestic passenger flows.
4. Domestic pax flows - Copula.R --- This script illustrates the method of predicting distributions of passenger flows when assuming dependent passenger arrivals. Here, we use domestic flows as an example. 
5. Newsvendor staffing study at immigration.R --- This file illustrates how we evaluate the improved predictions' impact on decision making as shown in Section 6 in the paper. Here we focus on the decision-making problem at the immigration area where only international passenger flows are used. Note that domestic passengers do not need to go through immigration desks. Please see Section 6 (Eq. 1 and 2) in the paper for more details.
6. Newsvendor staffing study at security.R --- This file illustrates how we evaluate the improved predictions' impact on decision making as shown in Section 6 in the paper. Here we focus on the decision-making problem at the security area where both domestic and international passenger flows are used. Note that domestic passengers do not need to go through immigration desks. This script requires results output from "5. simulation_study_newsvendor.R". Please see Section 6 (Eq. 3 and 4) in the paper for more details. 

### Reference
Guo, Xiaojia, Yael Grushka-Cockayne, and Bert De Reyck. "Forecasting airport transfer passenger flow using real-time data and machine learning." Manufacturing & Service Operations Management (2021)
