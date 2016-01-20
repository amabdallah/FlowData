# Original Files 

Craig Miller at the Utah Division of Water Resources handed these folders to Adel Abdallah on a CD on June 16, 2015

## The purpose?
Craig Miller mentioned these reaons for loading their text files into a database
"at present we have flat files that only have at most 5 digits of accuracy.
In models we cannot see changes in systems like the Colorado River from small
changes upstream because we just don't have enough accuracy in our data
**We would also like to have a way of accessing data from different sources
through one data hub.**   
We want to be able to standardize how we keep our data.  
We also would like to be able to query our data for quick analysis."  

## Files structure and format
Dave Cole at the Utah Division of Water Resources shared this format that describes how the data is organzied in the text files with Adel Abdallah on a CD on June 16, 2015   


Utah Division of Water Resources

	Daily Streamflow Format


 Record   Column    Format  Variable

   1      1 -  5      I5     IYear        Water Year.
          6 - 80      A75           Name of streamflow station.

   2      1 - 12      A12           Name of Streamflow station, can be blank.
         13 - 14      I2        IEX      Daily streamflow values are multiplied by 10**IEX.
         15 - 74    12F5.0    Q(I,J,K,L)Daily Streamflow values in cfs for a day for the months October to September.
         75 - 80      I6                  Day of the month identification.


	Record 2 is repeated 31 times for each year of input streamflow data.  The sequence of record 1 and record 2 repeated 31 times is repeated for each year of input data.  For more than one year of streamflow data the process is repeated for record 1 and record 2.
 

	Utah Division of Water Resources

	Monthly Streamflow Format


 Record   Column    Format  Variable

   1      1 - 80      A80           Name of streamflow station.

   2      1 -  8      A12           Name of Streamflow station, can be blank.
          9 - 12      I4                 Water Year, can be blank.
         13 - 14      I2        IEXPMonthly streamflow values are multiplied by 10**IEXP.
         15 - 74    12F5.0    Q(I,J,K,L)Monthly Streamflow values in acre-feet for the months October to September.
         75 - 80      F6.0                Annual streamflow, can be blank.


	Record 2 is repeated for each year of input streamflow data.  For more than one streamflow station the process is repeated for record 1 and record 

