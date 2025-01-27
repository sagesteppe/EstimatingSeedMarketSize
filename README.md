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

