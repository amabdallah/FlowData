# UDWR_FlowStorageData
Automate loading cross-tabulated time series flows data from text files into an ODM database in SQL Server. Then use ODM user interface Tools to query and visualize time series data from one centeral hub. The time series data are for the Utah Division of Water Resources. 
Written and tested by Adel M. Abdallah, Utah State University  
Started on June 15, 2015   
Last updated on Jan 22, 2016   
Estimated time spent: Over a month worth of work  

**Disclaimer**   
The Utah Division of Water Resources owns this data.  
Use the script and data at your own risk.   
This project is in progress.   

###Summary
The Matlab script reads, manipulates, and loads discharge time series data from 
cross-tabulated text files into a Microsoft SQL Server ODM Blank Database

## Data provided by Craig Miller and Dave Cole at Utah Division of Water Resources      

* 1. Stream Flow: Monthly (516 text files)   average? (flow cubic foot per second cfs)
* 2. Stream Flow: Daily (50 text files)  cumultive Volume (acre feet)   
* 3. Flow: Monthly (198 text files)  cumultive Volume (acre feet)  
* 4. OutPut: Monthly (129 text files)  cumultive Volume (acre feet)  

<img src="https://github.com/amabdallah/UDWR_FlowStorageData/blob/master/DataLoadingConceptual.JPG" width= "700">     


###How does the script work?
 1. read each text file: get the name of the station, time range, and data values    
 2. convet the cross-tabulated data values to time series   
 3. add metadata: source, unit, method for each site   
 4. prepare the data and its metadata to a strucutre that matches the ODM database tables    
 5. load metadata, then data values for each site   


### Why do we need this script, or to load data into ODM? Purpose?
Craig Miller mentioned these reaons for loading their text files into a database   
"at present we have flat files that only have at most 5 digits of accuracy.   
In models we cannot see changes in systems like the Colorado River from small   
changes upstream because we just don't have enough accuracy in our data    
We would also like to have a way of accessing data from different sources    
through one data hub.  We want to be able to standardize how we keep our data.  
We also would like to be able to query our data for quick analysi."    

### How to use the database?  
First, attach the database instance you can download here to Microsoft SQL Server. Here is instructions on how to do it 
https://www.codeplex.com/Download?ProjectName=HydroServer&DownloadId=349185

Then, you can use ODM Tools software to query and visualize the database   
There are two versions of this software: C# or Python Based. But both are free. The Python one is more recent and should be compatiable with Mac, Windows, and Lunix. 

Windows C# ODM Tools    
https://hydroserver.codeplex.com/wikipage?title=ODM%20Tools&referringTitle=Documentation

Python ODM Tools    
https://github.com/ODM2/ODMToolsPython


