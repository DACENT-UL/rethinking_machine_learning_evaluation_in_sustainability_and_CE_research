# rethinking_machine_learning_evaluation_in_sustainability_and_CE_research
This is the Github repo which accompanies the manuscript 'Rethinking Machine Learning Evaluation in Sustainability &amp; CE Research'

This repo contains 2 files, the purposes of which are described below.

## Irish_data.xlsx
Irish_data.xlsx is the data used for the empirical example presented in the paper. Details of the variables are provided below (and in table 2 of the paper). The lengths of the time series used from each variable was 2000-2020.

Municipal Waste - kilograms per capita - from Eurostat (https://doi.org/10.2908/CEI_PC031) (note values for 2013 and 2015 are missing).

GDP - constant to 2015 US$ - from the World Bank - (data id: NY.GDP.MKTP.KD)

Population - people - from the World Bank - (data id: SP.POP.TOTL)

Unemployment - % of labour force - from the World Bank - (data id: SL.UEM.TOTL.ZS)

## models.R
This is the R file used for the modelling and anlaysis of the empirical example presented in the paper. This file;

->applies z score scaling to the variables in Irish_data.xlsx to prepare them for modelling

->uses linear interpolation to optain estimated municipal waste values for 2013 and 2015

->fits and evaluates the mean baseline, naive baseline, support vector machine and neural network 

->creates the figures used in the publication
