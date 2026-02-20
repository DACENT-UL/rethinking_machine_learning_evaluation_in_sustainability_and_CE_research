# rethinking_machine_learning_evaluation_in_sustainability_and_CE_research
This is the Github repo which accompanies the manuscript 'Rethinking Machine Learning Evaluation in Sustainability &amp; CE Research'

This repo contains 2 files, the purposes of which are described below.

## Irish_data.xlsx
Irish_data.xlsx is the data used for the empirical example presented in the paper. Details of the variables are provided below (and in table 2 of the paper)

Municipal Waste - kilograms per capita - from Eurostat
General Electricity Consumption - ktoe - from the Central Statistics Office (CSO)
GDP - 2015 US$ - from the World Bank
Population - people - from the World Bank
Unemployment - % of labour force - from the World Bank
CO2 emissions - tonnes/capita - from Our World in Data
Inflation - annual % change in consumer prices - from the World Bank

## models.R
This is the R file used for the modelling and anlaysis of the empirical example presented in the paper. This file applies z score scaling to the variables in Irish_data.xlsx to prepare them for modelling, fits and evaluates the mean baseline, naive baseline, support vector machine and neural network and creates the figures used in the publication
