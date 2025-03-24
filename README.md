# Description  
The full potential market size of the native seed industry is currently unknown. 
While data on the amount of seed purchased - or at least ordered - by BLM per year, per field office, are available, data are not available for the remaining agencies. 
Here we create a simple estimate of the seed market using only a handful of variables, and wrapping them together in a simple equation for simulation bootstrapping. 

In all following discussions the domain, i.e. the area of analysis, is classified as all states of the Continental United States, Alaska, Puerto Rico and all Pacific Islands. 

# Overview

For some overview on this project please see the following pitch talk:  

[Seed Market Sizing](https://sagesteppe.github.io/SeedMarketSizingTalk/SeedMarketSizingTalk.html#/title-slide)  


# Workflow 

The number of fires per year, and the sizes of annual fires are calculated in the script CalculateAnnualFireSummary. 

A raster data set which classifies the entirety of the domain as an area to be drill seeded or seeded via aerial methods is created in ClassifyTopographyDrillAerial. 

An analysis of seed mix data, to determine the ratio of Forbs:Grasses:Trees used in various mixes is calculated in AnalyzeSeedMixes. 

# Scripts

These are all contained within the 'scripts' sub directory. 

- *functions.R* Contains all functions required to run the analysis herein. 
They are not R package quality functions, i.e. they are not very abstracted - instead running very specific purposes, and as a result they are not robust to unexpected input. 

- *DataBaseFields.R* This is largely used to develop a set of images which are used for visualizing the proposed database for holding survey results. 
This was done to ensure that the survey would be able to ask the 'correct' questions required to have a variety of components necessary for the analysis. 

- *DetermineNLCDClassesDOIregions.R* This looks like it was going to be used to convert the PADUS database to a set of raster files. However, it turns out the data could be kept as a vector and still be computed on. 

- *ExtremeValues.Rmd* This script was used for calling the function which calculate the GEV return levels on observed data. 

- *forecasting.R* Contains the function used to create time series forecasts, and the implementation of them. These data ended up not being used in lieu of for the quantile regression data. 

- *KDE_Play* Used kept around to document how to use kernel density estimates to smooth survey responses

- *markoveChainRegressionSlope* Used for determining if 'large' or 'small' fires, i.e. those beneath the conditional mean (linear regression) followed each other. Not used in lieu of quantile approach. 

- *ProcessDataToTabulateLandCover* Used to calculate DOI agency land ownership by regions. Note that this involved some spatial complexity as area calculations on a 'flattened' i.e. projected surface are much faster than on a geographic (round) coordinate system.
'flattened' results are very good, but I was worried about push backs from agency heads on their agencies total land surfaces.
Note that a fundamental mismatch exists between these values and official reporting as PADUS does not yet seem to have all areas incorporated yet. 

- *QuantileRegression* Initial Quantile Regression estimates of burned areas was performed here. This led to the decision to use 'growthcharts' for modelling fire sizes. 

- *RelationshipQuantilesExtremes* The process for reconciling GEV estimates with growth chart quantile estimates and simulate total burned areas in the future are in here, as well as notes on some of the troubleshooting involved to make it happen. 

- *ReplicateSurveyQuestions* This script is WEIRD! It uses `sink` to create a quarto markdown file which can be used for the survey. 
It has some nested forloops and overall is pretty unusual for the usual R user. 
Breath! 

- *SurveyGoals* Just notes on the goals of the survey, not totally fleshed out. 

- *survey-original* The first wave 'test' survey for surveydown. Probably of limited utility now and the contents in the 'survey' subdirectory should be more important. 

- *TabulateTerrestrialAreasbyDOIRegion* This is where the actual results of *ProcessDataToTabulateLandCover* come from. 