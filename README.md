### UDWR_FlowStorageData
Automate loading cross-tabulated time series flows data from text files into an ODM database in SQL Server. Then use ODM user interface Tools to query and visualize time series data from one centeral hub. The time series data are for the Utah Division of Water Resources.     
Written and tested by Adel M. Abdallah, Utah State University     
Started on June 15, 2015      
Last update on Jan 22, 2016     
Estimated time spent: Over a month of work and lots of dreaming...     

**Disclaimer**   
The Utah Division of Water Resources owns this data.  
Use the script and data at your own risk.   
This project is in progress.   

### The Observation Data Model (ODM)   
The ODM is a data model for the storage and retrieval of hydrologic observations in a relational database. ODM has become like the standard to organize any point time series data (e.g., snow, rain, discharge, weather, etc).   
Here I used the version ODM 1.1.1 which you can find lots of details about it at this link below. To make it simple, this schematic below highlights in dashed blue boxes the basic tables that you need to populate in the database for this specific data. The rest of tables could be used for more detailed metadata of **real-time** data. 
https://hydroserver.codeplex.com/wikipage?title=Observations%20Data%20Model&referringTitle=Documentation

<img src="https://github.com/amabdallah/UDWR_FlowStorageData/blob/master/ODM.jpg" width= "700">     

### Why do we need this script, or to load data into ODM? Purpose?
Craig Miller mentioned these reasons for loading their text files into a database
"at present we have flat files that only have at most 5 digits of accuracy.
In models we cannot see changes in systems like the Colorado River from small
changes upstream because we just don't have enough accuracy in our data
**We would also like to have a way of accessing data from different sources
through one data hub.**   
We want to be able to standardize how we keep our data.  
We also would like to be able to query our data for quick analysis."  

The ODM has successfully been used worldwide to organize time series data and there are several software tools that have been developed to load data, query, and visualize its selected data. So this is why I used ODM here. 

### Data provided by Craig Miller and Dave Cole at Utah Division of Water Resources      
Here is the original folders of all the files:
https://github.com/amabdallah/UDWR_FlowStorageData/tree/master/OriginalFiles    

For now, I needed to exclude a few text files that had problematic issues which made the Matlab script break. The final text files that I used are here 
https://github.com/amabdallah/UDWR_FlowStorageData/tree/master/PreparedFiles    
* 1. Stream Flow: Monthly (516 text files)   average? (flow cubic foot per second cfs)
* 2. Stream Flow: Daily (50 text files)  cumulative Volume (acre feet)   
* 3. Flow: Monthly (198 text files)  cumulative Volume (acre feet)  
* 4. OutPut: Monthly (129 text files)  cumulative Volume (acre feet)  

### How does the script work?
 1. read each text file: get the name of the station, time range, and data values    
 2. convert the cross-tabulated data values to time series   
 3. add metadata: source, unit, method for each site   
 4. prepare the data and its metadata to a structure that matches the ODM database tables    
 5. load metadata, then data values for each site   
Here is the script      
https://github.com/amabdallah/UDWR_FlowStorageData/blob/master/Script_Load_Time_Series_Data_Jan22_2016.m
<img src="https://github.com/amabdallah/UDWR_FlowStorageData/blob/master/DataLoadingConceptual.JPG" width= "700">     

### Result    
Here is a copy of the populated SQL Server database instance. Please follow the instructions below on how to use it.      
https://github.com/amabdallah/UDWR_FlowStorageData/raw/master/ODM_SQLServer_FlowData_Jan22_2016.rar

### How to use the database?  
First, attach the database instance you can download here to Microsoft SQL Server. Here is instructions on how to do it 
https://www.codeplex.com/Download?ProjectName=HydroServer&DownloadId=349185

Then, you can use ODM Tools software to query and visualize the database   
There are two versions of this software: C# or Python Based. But both are free. The Python one is more recent and should be compatible with Mac, Windows, and Lunix. 

Windows C# ODM Tools    
https://hydroserver.codeplex.com/downloads/get/352448

Python ODM Tools    
https://github.com/ODM2/ODMToolsPython


