# FlowData
Automate loading cross-tabulated time series flows data from text files into an ODM database

###Summary
 Matlab script to read, manipulate, and load discharge time series data from 
cross-tabulated text files into a Microsoft SQL Server ODM Blank Database

### Data provided by Craig Miller and Dave Cole at Utah Division of Water Resources      

* 1. Monthly (547 MonthlyStations or text files)   
* 2. Daily (51 MonthlyStations or text files)  

 Written and tested by Adel Abdallah   
 Started on June 15 2015   
 Finsihed on December 7th 2015   
 estimated time spent: 70 hours of work   

### There are two seprate scripts for discharge time series data.   
Daily   
Monthly   


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

