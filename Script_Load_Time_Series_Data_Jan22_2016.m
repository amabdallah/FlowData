%% ReadME   

% Matlab script to read, manipulate, and load discharge time series data from 
% cross-tabulated text files into a Microsoft SQL Server ODM Blank Database

% Data provided by Craig Miller and Dave Cole at Utah Division of Water Resources    

% 1. Monthly (547 MonthlyStations or text files)
% 2. Daily (51 MonthlyStations or text files)

% Written and tested by Adel Abdallah
% Started on June 15 2015
% updated on December 7th, 2015
% Updated again on Jan 22, 2016
% estimated time spent: over a month of work....lost of keeping track :)

% There are two seprate scripts for discharge time series data. 
% --------------------------------------------------------------
% Daily 
% Monthly


% How does the script work?
% --------------------------------------------------------------
% 1. read each text file: get the name of the station, time range, and data values 
% 2. convet the cross-tabulated data values to time series 
% 3. add metadata: source, unit, method for each site
% 4. prepare the data and its metadata to a strucutre that matches the ODM database tables 
% 5. load metadata, then data values for each site 


% Why do we need this script, or to load data into ODM? Purpose?
% --------------------------------------------------------------
% Craig Miller mentioned these reaons for loading their text files into a database
% "at present we have flat files that only have at most 5 digits of accuracy.
% In models we cannot see changes in systems like the Colorado River from small 
% changes upstream because we just don't have enough accuracy in our data 
% We would also like to have a way of accessing data from different sources 
% through one data hub.  We want to be able to standardize how we keep our data.
% We also would like to be able to query our data for quick analysi."  
%
%% 1. Read and prepare needed metadata: Variables, Sources, Methods, and Sites form Excel
path(path,'C:\AdelAbdallah\01PhD\UDWRes\ProcessedData\Metadata')
clc, clear all;
% DBCC CHECKIDENT('OD.dbo.DataValues', RESEED, 0)

% ALTER TABLE [Variables] AUTO_INCREMENT = 1
% DBCC CHECKIDENT ("Variables", RESEED, 1);
% Variables 
[num,txt,Variables] = xlsread('Variables','Variables');
 % avoid the headlines  
Variables=Variables(2:end,:);% avoid the first row: headline 

% Sources 
[num,txt,Sources] = xlsread('Sources','Sources');
 % avoid the headlines  
Sources=Sources(2:end,:);% avoid the first row: headline 

% Methods 
[num,txt,Methods] = xlsread('Methods','Methods');
 % avoid the headlines  
Methods=Methods(2:end,:);% avoid the first row: headline 
%% 2. Load metadata 
% Enter your SQLServer Password below e.g., Pass='UDWR2016'
% this password will be reused many times throughout the script to connect
% to your database 

Pass=''
% Methods 
conn = database('OD','sa',Pass,...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
    
fastinsert(conn,'Methods',{'MethodDescription','MethodLink'},Methods)

% Add a new controlled vocabulary variable term called "Storage/delivered volume"
fastinsert(conn,'VariableNameCV',{'Term','Definition'},{'Storage/delivered volume','Storage/delivered volume'})

fastinsert(conn,'Variables',{'VariableCode','VariableName','Speciation','VariableUnitsID',...
    'SampleMedium','ValueType','IsRegular','TimeSupport','TimeUnitsID','DataType',...
    'GeneralCategory','NoDataValue'},Variables(:,:))

fastinsert(conn,'Sources',{'Organization','SourceDescription','SourceLink',...
    'ContactName','Phone','Email','Address','City','State','ZipCode','Citation','MetadataID'},Sources)

%% First: "Streamfl-Daily" Folder
% Read daily discharge data values from 51 text files and load them to an ODM Microsfot SQL Server database
%% 1. Import data from 51 text file to Matlab workspace

clear all, clc;% clear workspace and clean memory from previos work


path(path,'C:\AdelAbdallah\01PhD\UDWRes\ProcessedData\Daily')% go to data folder

list=dir(pwd);  %get info of files/folders in current directory
isfile=~[list.isdir]; %determine index of files vs folders
DailyFileNames={list(isfile).name}; %create cell array of file names
DailyFileNames={list(~[list.isdir]).name}';
% use the for loop in paralell on the four cores of this machine 
% NOTE: to use the Parallel Pool untility in Matlab, you need to have the
% appropriate toolbox 
parfor fls=1:length(DailyFileNames);
% Initialize variables.
filename=DailyFileNames{fls};
% Initialize variables.
startRow = 2;

% Read columns of data as strings:
% For more information, see the TEXTSCAN documentation.
formatSpec = '%6s%6s%1s%1s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%s%[^\n\r]';

% Open the text file.
fileID = fopen(filename,'r');

% Read columns of data according to format string.
% This call is based on the structure of the file used to generate this
% code. If an error occurs for a different file, try regenerating the code
% from the Import Tool.
dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '', 'HeaderLines' ,startRow-1, 'ReturnOnError', false);

% Close the text file.
fclose(fileID);

% Convert the contents of columns containing numeric strings to numbers.
% Replace non-numeric strings with NaN.
raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
for col=1:length(dataArray)-1;
    raw(1:length(dataArray{col}),col) = dataArray{col};
end
numericData = NaN(size(dataArray{1},1),size(dataArray,2));

for col=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17];
    % Converts strings in the input cell array to numbers. Replaced non-numeric
    % strings with NaN.
    rawData = dataArray{col};
    for row=1:size(rawData, 1);
        % Create a regular expression to detect and remove non-numeric prefixes and
        % suffixes.
        regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
        try
            result = regexp(rawData{row}, regexstr, 'names');
            numbers = result.numbers;
            
            % Detected commas in non-thousand locations.
            invalidThousandsSeparator = false;
            if any(numbers==',');
                thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                if isempty(regexp(thousandsRegExp, ',', 'once'));
                    numbers = NaN;
                    invalidThousandsSeparator = true;
                end
            end
            % Convert numeric strings to numbers.
            if ~invalidThousandsSeparator;
                numbers = textscan(strrep(numbers, ',', ''), '%f');
                numericData(row, col) = numbers{1};
                raw{row, col} = numbers{1};
            end
        catch me
        end
    end
end


% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells

% Create output variable
Untitled = cell2mat(raw);
% Clear temporary variables


% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells

% Create output variable
DailyFilesData{fls} = cell2mat(raw);
% Clear temporary variables
% clearvars startRow formatSpec fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp me R isfile list Untitled ;

% read the station name of each time sereis 
endRow2 = 1;

formatSpec2 = '%*7s%9s%s%[^\n\r]';

fileID2 = fopen(filename,'r');

dataArray2 = textscan(fileID2, formatSpec2, endRow2, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);


dataArray2{1} = strtrim(dataArray2{1});
dataArray2{2} = strtrim(dataArray2{2});

fclose(fileID2);

Station = [dataArray2{1:end-1}];
% clearvars  endRow2 formatSpec2 fileID2 dataArray2 ans;

MonthlyStations{fls}=Station;

end
% report the station mames 
StationNames=vertcat(MonthlyStations{:});

ODMSites=StationNames;


% Column headings in the DailyODMsitesMetadata Array
% Column#1 is for the Site Code 
% Column#2 is for the Site Name 
% Column#3 is for the Site file name
% 
% %% clear unneeded files 
% clear isfile 
% clear list
% clear StationNames
% clear MonthlyStations
% clear DailyFileNames
%% 2. Convert cross-tabulated discharge data into seires 

% For loop to go over each file (time series of a site)

for fls=1:length(DailyFilesData);
     
      m=DailyFilesData{fls};
      
% Delete the rows which have the title and year based on having NaN at
% column 17 (the days column)
m(isnan(m(:,17)) , :)=[];

NumYears=length(m)/31;

for yrs=1:NumYears;

     % vector of daily values for each selected month
    col=[5,6,7,8,9,10,11,12,13,14,15,16];
    mth=[10,11,12,1,2,3,4,5,6,7,8,9];
   
    if yrs==1;
    st=1;
    ed=31;
else 
    st=(yrs-1)*31+1;
    ed=(yrs-1)*31+31;
    
    end
    
     for icol=1:length(col);
         col(icol);
         mth(icol);
    
    %Get the data values for one month in the same year 

    
    x=m(st:ed,col(icol));
    % Find the indix of data valus 
    z=find (~isnan(x));
    % vector of 31 days (always, regardless of the month length)  
    h=(m(st:ed,17));
    % vector of years
    t=(m(st:ed,2));
    % vector of exponants 
    e=(m(st:ed,4));
% 
    %Get the days of the month based on the month length
    days=h(z(~isnan(z)));

    % Generate the month vector that corresponds to the number of days in
    % that month
    months=repmat(mth(icol),1,length(z))';
    
    %Get the data year's vector 
    years=t(z(~isnan(z)))  ;
    
    %Get the data values of the month
    Values=x(z(~isnan(z)));

    %Get the coresponding exponent of the data values
    exponent=e(z(~isnan(z)));

    % Multiply the data value with the corresponsing exponent 
    TrueValues=Values.*10.^exponent;

%     Mix1{jj,col(icol)-4}=[days,months,years,TrueValues] 
%     A1 = cell2mat(Mix1')
      DateValues{yrs,col(icol)-4}=[days,months,years,TrueValues];
       
 %       MatrixTimeSereis{k}=TimeSereis
%       K should be part of this loop so we can rest the Mix after every file 
     
     end % months  

end % years  
      % transpose the matrix of cells so we can convert the cells in a serial way    
      DateValuesTrans{fls}=transpose(DateValues);
      clear DateValues % its important to clear "DateValues" variable one after every file 
      TimeSereis=DateValuesTrans{fls};
% gather all the serial time series data in one Matrix 

MatrixTimeSereisFiles{1,fls} =DailyFileNames{fls};
% convert each DateValuesTrans to a serial one 

MatrixTimeSereisFiles{2,fls} = cat(1,TimeSereis{:});


%  merge days/month/year in one column
      x=MatrixTimeSereisFiles{2,fls};
 
       Date=datestr(datenum([x(:,3),x(:,2),x(:,1)]));
       da=cellstr(Date);
       val=num2cell(x(:,4));
       ODMTimeSeries{1,fls}=DailyFileNames{fls};
       ODMTimeSeries{2,fls}=[da,val];
 end %ODMTimeSeries files 
        ODMTimeSeries=transpose(ODMTimeSeries);

       DailyODMTimeSeriesMatrix=[ODMSites,ODMTimeSeries];      
% clear uneeded variables/prameters  
% clear e
% clear mth
% clear NumYears
% clear TrueValues
% clear z
% clear yrs
% clear m
% clear h
% clear mth
% clear ed
% clear icol
% clear x
% clear exponent
%% 3. Load Sites
conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);    
% % Sites
% prepare attributes for the Sites table 
B=num2cell(repmat(NaN,length(DailyODMTimeSeriesMatrix),1));
E=num2cell(repmat('T',length(DailyODMTimeSeriesMatrix),1));

% generate dummy longitude and latitudes because the files dont have them
% BUT ODM requires them. Mandatory attributes 
lat=num2cell(repmat(41.718473,1,length(DailyODMTimeSeriesMatrix)))';
long=num2cell(repmat(-111.946402,1,length(DailyODMTimeSeriesMatrix)))';

Dat=num2cell(repmat(0,1,length(DailyODMTimeSeriesMatrix)))';
Sites={DailyODMTimeSeriesMatrix(:,1),DailyODMTimeSeriesMatrix(:,2),lat,long,Dat,...
    B,B,B,B,B,B,B,B,DailyODMTimeSeriesMatrix(:,3)};

Sites2=horzcat(Sites{:});


fastinsert(conn,'Sites',{'SiteCode','SiteName','Latitude','Longitude',...
    'LatLongDatumID','Elevation_m',...
'VerticalDatum','LocalX','LocalY','LocalProjectionID',...
'PosAccuracy_m','State','County','Comments'},Sites2(:,:))
%% 5. Prepare Data Values for the ODM strucutre   
% File #34 has a problem with dublicate decemeber 1992 reading
% and March 2nd and 3rd the same year

% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys

for fls=1:length(DailyFilesData);
     
x=DailyODMTimeSeriesMatrix(fls,4)  ;  
new_data = cat(1, x{:});

DataValue=new_data(:,2);
ValueAccuracy=num2cell(repmat(NaN,1,length(DataValue)))';
LocalDateTime=new_data(:,1);
UTCOffset=num2cell(repmat(0,1,length(DataValue)))';

% this part is not finished.....just an idea
% The site ID should be looked up based on either the site code
% Pass the array of site code and get back the site IDs
% Sites2(:,1)
% sqlquery = 'select SiteID* from OD';
% curs = exec(connPostgres2015,sqlquery)
% curs = fetch(curs);
% x=curs.Data(end,1)

SiteID=num2cell(repmat(fls,1,length(DataValue)))';


DateTimeUTC=LocalDateTime;
% VaribleID=1 for discharge, average?, day, cubic food per second (cfs)  
VariableID=num2cell(repmat(1,1,length(DataValue)))';
OffsetValue=num2cell(repmat(NaN,1,length(DataValue)))';
OffsetTypeID=num2cell(repmat(NaN,1,length(DataValue)))';
CensorCode=(repmat({'nc'},1,length(DataValue)))';
QualifierID=num2cell(repmat(NaN,1,length(DataValue)))';

% Method ID= 1 for UDWR: Derived daily discharge data from different sources. Contact Graig Miller and Dave Cole for details 
MethodID=num2cell(repmat(1,1,length(DataValue)))';

% Source ID=1 for the source of Utah Division of Water Resources/State Agency/Craig Miller   
SourceID=num2cell(repmat(1,1,length(DataValue)))';
SampleID=num2cell(repmat(NaN,1,length(DataValue)))';
DerivedFromID=num2cell(repmat(NaN,1,length(DataValue)))';
QualityControlLevelID=num2cell(repmat(0,1,length(DataValue)))';

DataValues{fls}=[DataValue,ValueAccuracy,LocalDateTime,UTCOffset,...
DateTimeUTC,SiteID,VariableID,OffsetValue,...
OffsetTypeID,CensorCode,QualifierID,MethodID,...
SourceID,SampleID,DerivedFromID,...
QualityControlLevelID];

end
%% 6. Load Daily Values into the ODM Database  

% connetc to the ODM database 
conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
% loop over the time series files and load each of them to the database    
for fls=1:length(DailyFilesData);

fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValues{fls})

end

% Error using database/fastinsert (line 287)
% Java exception occurred:
% java.sql.BatchUpdateException: Violation of UNIQUE KEY constraint 'UNIQUE_DataValues'. Cannot insert duplicate key
% in object 'dbo.DataValues'. The duplicate key value is (0, <NULL>, Dec  1 1992 12:00AM, 0, Dec  1 1992 12:00AM,
% 34, 42, <NULL>, <NULL>, nc, <NULL>, 26, 6, <NULL>, <NULL>, 0).
% 
% 	at com.microsoft.sqlserver.jdbc.SQLServerPreparedStatement.executeBatch(SQLServerPreparedStatement.java:1178)

%% Second: "Streamfl-Monthly" Folder  /Annual
% Read monthly discharge data values from Text files and load them to an ODM Microsfot SQL Server database
%% 1. Import data from 517 text file to Matlab workspace
% clear the workspace and command window
clear all, clc;
path(path,'C:\AdelAbdallah\01PhD\UDWRes\ProcessedData\Monthly') % go to data folder 

%get info of files/folders in current directory
list=dir(pwd);  
%determine index of files vs folders
isfile=~[list.isdir]; 
%create cell array of file names
MonthlyFileNames={list(isfile).name}; 
MonthlyFileNames={list(~[list.isdir]).name}';

for fls=1:length(MonthlyFileNames);
% Initialize variables.
filename=MonthlyFileNames{fls};

% Initialize variables.
filename=MonthlyFileNames{fls};
startRow = 2;

% Read columns of data as strings:
% For more information, see the TEXTSCAN documentation.
% formatSpec = '%12s%2s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%s%[^\n\r]';
formatSpec = '%8s%4s%2s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%s%[^\n\r]';

% Open the text file.
fileID = fopen(filename,'r');

% Read columns of data according to format string.
% This call is based on the structure of the file used to generate this
% code. If an error occurs for a different file, try regenerating the code
% from the Import Tool.
dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '',  'ReturnOnError', false);

% Close the text file.
fclose(fileID);

% Convert the contents of columns containing numeric strings to numbers.
% Replace non-numeric strings with NaN.
% Replace non-numeric strings with NaN.
raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
for col=1:length(dataArray)-1
    raw(1:length(dataArray{col}),col) = dataArray{col};
end
numericData = NaN(size(dataArray{1},1),size(dataArray,2));

for col=[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
    % Converts strings in the input cell array to numbers. Replaced non-numeric
    % strings with NaN.
    rawData = dataArray{col};
    for row=1:size(rawData, 1);
        % Create a regular expression to detect and remove non-numeric prefixes and
        % suffixes.
        regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
        try
            result = regexp(rawData{row}, regexstr, 'names');
            numbers = result.numbers;
            
            % Detected commas in non-thousand locations.
            invalidThousandsSeparator = false;
            if any(numbers==',');
                thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                if isempty(regexp(thousandsRegExp, ',', 'once'));
                    numbers = NaN;
                    invalidThousandsSeparator = true;
                end
            end
            % Convert numeric strings to numbers.
            if ~invalidThousandsSeparator;
                numbers = textscan(strrep(numbers, ',', ''), '%f');
                numericData(row, col) = numbers{1};
                raw{row, col} = numbers{1};
            end
        catch me
        end
    end
end

% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells

% Create output variable
MonthlyFilesData{fls} = cell2mat(raw);

% read the station name of each time sereis 
endRow2 = 1;

formatSpec = '%8s%50s%[^\n\r]';
formatSpec2 = '%1s%50s%[^\n\r]';

fileID2 = fopen(filename,'r');

dataArray = textscan(fileID2, formatSpec, endRow2, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);

dataArray{1} = strtrim(dataArray{1});
dataArray{2} = strtrim(dataArray{2});

% Allocate imported array to column variable names
VarName1 = dataArray{:, 1};

VarName2 = dataArray{:, 2};

fclose(fileID2);

Station = [dataArray{1:end-1}];

clearvars  endRow formatSpec fileID dataArray ans;

MonthlyStations{fls}=Station;

% Clear temporary variables
clearvars  formatSpec fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp me R startRow list isfile ;

end
clear filename
%% 2. Convert cross-tabulated discharge data into seires

for fls=1:length(MonthlyFilesData);
    m=MonthlyFilesData{fls};

% Delete the first row in the first column if it is not a year or it is NAN
m(isnan(m(:,3)) , :) =[];
m(isnan(m(:,2)) , :) =[];

% Read the streamflow values out of the text file
Tnew=m(:,4:15);

% find size of the matrix of streamflow values 
[R,C]=size(Tnew); % R:Row, %C:Column

% Read the exponant column (multiplier of streamflow values)
Expo=m(:,3);

% Generate a matrix of exponants that correspond for each month and each
% year 

ValuesExpo=repmat(Expo,1,12)'; % 12 is number of months 

% transpose the matrix to a vector 

ValuesExpo=ValuesExpo(:);


% Period=m(1,1)-m(end,1)
% Start=m(1,1);
% End=m(end,1);
% n=End-Start ;
n=length(m);

% Generate a vector of years 
y=m(:,2);

ym=repmat(y,1,12)';

Tm(:,1)=ym(:);
 % Generate a vector of months in a water year format
Tm(:,2)=repmat([10,11,12,1,2,3,4,5,6,7,8,9],1,R)';

% Transpose the matix to vector
Tm(:,3) = reshape(Tnew.',1,[])'; % m: Matrix
h=Tm(:,3);

% Multiply the streamflow variavbles with the exponant 
Values=h.*10.^ValuesExpo;
% extract the year 
year=Tm(:,1);
month=Tm(:,2);
%Combine Month/Year and add the first day of the month so it YYYYMMDD time
%stamp is accpeted in the ODM database
Date=datestr(datenum(year,month,1));

% there are three ways to handle YYYYMM in a database  
% 1) Add the 1st day of the month as store it along with month and year. so it will be YYYYMMDD
% 2) Use varchar data type and  only store month and year in it. YYYYMM
% 3) Store the year and the month values as two separate columns as integers. i.e. for month and year.

% I prefere to the use the first method (YYYYMMDD) date for these Advantages:
% Sorting is straight forward where is there is a default format (compared to storing as varchar)
% Date operations are straight forward (compared to storing as varchar/two integer columns)
% Data stays in one column (compared to storing as two integer columns)
%-----------------------------
% Monthly DataValues
%Convert data values to cell arrays 
Values = num2cell(Tm(:,3));
%TimeStamp
Date=cellstr(Date);

% combine time stamp and data values to check them with origian data in the
% text file 
DateValues=[Date,Values];

Monthly22{fls}=DateValues;

%-----------------------------
% find the vector of the Annual steamflow values 
Annual=m(:,16);
% find the vector of the years  
YearsPeriod=m(:,2);

% combine time stamp and data Annual values to check them with origian data in the
% text file 
DateValuesAnnual=[YearsPeriod,Annual];

Annual22{fls}=DateValuesAnnual;

clear DateValuesMonthly DateValues Date ym Tm m % its important to clear "DateValues" variable one after every file 

%-----------------------------

end

MonthlyFileNames{fls};

MonthlyStationNames=vertcat(MonthlyStations{:});

% Merge the site name the site code to make a uniqe site name 
MonthlyStationNamesUnq=strcat(MonthlyFileNames,MonthlyStationNames(:,2));


% Monthly Data
ODMMonthlyTimeSeries=transpose(Monthly22);

% % Replace empty cells with "Unknown"
% MonthlyStationNamesCode=MonthlyStationNames(:,1);
% emptyCellsCode= cellfun(@isempty,MonthlyStationNamesCode); 
% MonthlyStationNamesCode(emptyCellsCode)=num2cell(rand(1));
% 
% % Replace empty cells with "Unknown"
% MonthlyStationNamesName=MonthlyStationNames(:,2);
% emptyCellsName = cellfun(@isempty,MonthlyStationNamesName); 
% MonthlyStationNamesName(emptyCellsName)=num2cell(rand(1));

% MonthlyStationNames1=[MonthlyStationNamesCode,MonthlyStationNamesName];

ODMTimeSeriesMonthlyMatrix=[MonthlyFileNames,MonthlyStationNamesUnq,ODMMonthlyTimeSeries];
% Column headings in the ODMTimeSeriesMonthlyMatrix Array
% Column#1 is for the Site Name 
% Column#2 is for the Site code 
% Column#3 is for the Site file name
% Column#4 is for the matrix of data values 

% Annual Data (derived average monthlty)
ODMAnnualTimeSeries=transpose(Annual22);
ODMTimeSeriesAnnualMatrix=[MonthlyFileNames,MonthlyStationNamesUnq,ODMAnnualTimeSeries];

% Column headings in the ODMTimeSeriesAnnualMatrix Array
% Column#1 is for the Site Name 
% Column#2 is for the Site code 
% Column#3 is for the Site file name
% Column#4 is for the matrix of data values 
%% 3. Read and prepare (Sites) to match the ODM Database
% Methods, Sources, Variables are already defined and loaded in the previos First step above 
% Sites (the same site has Monthly and Annual)

% prepare attributes for the Sites table 
B=num2cell(repmat(NaN,length(ODMMonthlyTimeSeries),1));
E=num2cell(repmat('T',length(ODMTimeSeriesMonthlyMatrix),1));

% generate dummy longitude and latitudes because the files dont have them
% BUT ODM requires them. Mandatory attributes 
lat=num2cell(repmat(41.718473,1,length(ODMTimeSeriesMonthlyMatrix)))';
long=num2cell(repmat(-111.946402,1,length(ODMTimeSeriesMonthlyMatrix)))';

Dat=num2cell(repmat(0,1,length(ODMTimeSeriesMonthlyMatrix)))';

% I used the file name as a site code becuase it is unquie. The site code
% inside the files is not uniqe and many files share the same code. This is
% problomatic for the ODM database. The site name and code must be unque
% across the database
SitesMonthly={ODMTimeSeriesMonthlyMatrix(:,1),ODMTimeSeriesMonthlyMatrix(:,2),lat,long,Dat,...
   B,B,B,B,B,B,B,B,B};

SitesMonthly2=horzcat(SitesMonthly{:});
%% 4. Load monthly data Sites to the ODM database  


% connect Matlab workspace with the SQL Server ODM blank Database 
conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
    

fastinsert(conn,'Sites',{'SiteCode','SiteName','Latitude','Longitude',...
    'LatLongDatumID','Elevation_m',...
'VerticalDatum','LocalX','LocalY','LocalProjectionID',...
'PosAccuracy_m','State','County','Comments'},SitesMonthly2(:,:));

% % % Check dublicate site Names  
% A=SitesMonthly2(:,2);
% [~,idx]=unique(strcat(A(:,1)) );
% withoutduplicates=A(idx,:);
% 
%  % % Check dublicate site codes  
% BB=SitesMonthly2(:,1);
% [~,idxCode]=unique(strcat(BB(:,1)) );
% withoutduplicatesCode=BB(idxCode,:);

%% 5.1 Prepare Monthly Data Values to match the ODM Database strucutre 
% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys 

% This site 09183500 reads years before the 1910 with wired format
% I had to delete them manulaly
% this one too 094052SM.MON
% this site too 10241430.mon

for fls=1:length(MonthlyFilesData);
     
x=ODMTimeSeriesMonthlyMatrix(fls,3) ;  
new_data = cat(1, x{:});

% remove empty values (Null) because ODM doesnt allows empty data values
for k = 1:numel(new_data)
  if isnan(new_data{k})
    new_data{k} = [];
  end
end
%remove empty cell array contents
new_data(any(cellfun(@isempty,new_data),2),:) = []; %delete row with any empty cells


DataValue=new_data(:,2);
ValueAccuracy=num2cell(repmat(NaN,1,length(DataValue)))';
LocalDateTime=new_data(:,1);
UTCOffset=num2cell(repmat(0,1,length(DataValue)))';
SiteID=num2cell(repmat(fls+50,1,length(DataValue)))'; % 51 are previous daily sites
DateTimeUTC=LocalDateTime;
% Varible ID= 2 for Storage/delivered volume, cumulitve (total), month, acre-ft unit  
VariableID=num2cell(repmat(2,1,length(DataValue)))';
OffsetValue=num2cell(repmat(NaN,1,length(DataValue)))';
OffsetTypeID=num2cell(repmat(NaN,1,length(DataValue)))';
CensorCode=(repmat({'nc'},1,length(DataValue)))';
QualifierID=num2cell(repmat(NaN,1,length(DataValue)))';
% Method ID= 2 for UDWR: Derived discharge data from different sources. Contact Graig Miller and Dave Cole for details 
MethodID=num2cell(repmat(2,1,length(DataValue)))';
% Source ID=1 for the source of Utah Division of Water Resources/State Agency/Craig Miller   
SourceID=num2cell(repmat(1,1,length(DataValue)))';
SampleID=num2cell(repmat(NaN,1,length(DataValue)))';
DerivedFromID=num2cell(repmat(NaN,1,length(DataValue)))';
QualityControlLevelID=num2cell(repmat(0,1,length(DataValue)))';

DataValues{fls}=[DataValue,ValueAccuracy,LocalDateTime,UTCOffset,...
DateTimeUTC,SiteID,VariableID,OffsetValue,...
OffsetTypeID,CensorCode,QualifierID,MethodID,...
SourceID,SampleID,DerivedFromID,...
QualityControlLevelID];

end
%% 5.1.1 Load the data values into the ODM database
% connect Matlab workspace with the SQL Server ODM blank Database 
conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
     
for fls=500:length(MonthlyFilesData);
fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValues{fls})
end
%% 5.2 Prepare the Annual Data Values to match the ODM Database strucutre 
% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys 

for flsAnnual=1:length(MonthlyFilesData);
     
xAnnual=ODMTimeSeriesAnnualMatrix(flsAnnual,3) ;  
new_dataAnnual = cat(1, xAnnual{:});
new_dataAnnual = num2cell(new_dataAnnual);


% remove empty values (Null) because ODM doesnt allows empty data values
for kAnnual = 1:numel(new_dataAnnual);
  if isnan(new_dataAnnual{kAnnual});
    new_dataAnnual{kAnnual} = [];
  end
end

%remove empty cell array contents
new_dataAnnual(any(cellfun(@isempty,new_dataAnnual),2),:) = []; %delete row with any empty cells


DataValueAnn=new_dataAnnual(:,2);
ValueAccuracyAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';

LocalDateTimeAnn=cell2mat(new_dataAnnual(:,1)); % convert the cell to double 
% because the date functon to conver the year 1999 to 1/1/1999 works only
% with Double
LocalDateTimeAnn=cellstr(datestr(datenum(LocalDateTimeAnn,1,1)));


UTCOffsetAnn=num2cell(repmat(0,1,length(DataValueAnn)))';
SiteIDAnn=num2cell(repmat(flsAnnual+50,1,length(DataValueAnn)))';
DateTimeUTCAnn=LocalDateTimeAnn;

% Varible ID =3 for Storage/delivered volume, cumulitve (total), year, acre-ft unit  
VariableIDAnn=num2cell(repmat(3,1,length(DataValueAnn)))';

OffsetValueAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
OffsetTypeIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
CensorCodeAnn=(repmat({'nc'},1,length(DataValueAnn)))';
QualifierIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';

% Method ID= 3 for UDWR: Derived volume data from different sources. Contact Graig Miller and Dave Cole for details 
MethodIDAnn=num2cell(repmat(3,1,length(DataValueAnn)))';

% Source ID=1 for the source of Utah Division of Water Resources/State Agency/Craig Miller   
SourceIDAnn=num2cell(repmat(1,1,length(DataValueAnn)))';
SampleIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
DerivedFromIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
QualityControlLevelIDAnn=num2cell(repmat(0,1,length(DataValueAnn)))';

DataValuesAnn{flsAnnual}=[DataValueAnn,ValueAccuracyAnn,LocalDateTimeAnn,UTCOffsetAnn,...
DateTimeUTCAnn,SiteIDAnn,VariableIDAnn,OffsetValueAnn,...
OffsetTypeIDAnn,CensorCodeAnn,QualifierIDAnn,MethodIDAnn,...
SourceIDAnn,SampleIDAnn,DerivedFromIDAnn,...
QualityControlLevelIDAnn];

end
%% 5.2.1 Load the annual data values int othe ODM database         
% connect Matlab workspace with the SQL Server ODM blank Database 

conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
for flsAnnual=1:length(MonthlyFilesData);
fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValuesAnn{flsAnnual});
end

%% Third: "Flow" folder Monthly data
% Read monthly discharge data values from Text files and load them to an ODM Microsfot SQL Server database
%% 1. Import data from 201 text file to Matlab workspace
% clear the workspace and command window
clear all, clc;
path(path,'C:\AdelAbdallah\01PhD\UDWRes\ProcessedData\Flow') % go to data folder 

%get info of files/folders in current directory
list=dir(pwd);  
%determine index of files vs folders
isfile=~[list.isdir]; 
%create cell array of file names
MonthlyFileNames={list(isfile).name}; 
MonthlyFileNames={list(~[list.isdir]).name}';

for fls=1:length(MonthlyFileNames);
% Initialize variables.
filename=MonthlyFileNames{fls};

% Initialize variables.
filename=MonthlyFileNames{fls};
startRow = 2;

% Read columns of data as strings:
% For more information, see the TEXTSCAN documentation.
% formatSpec = '%12s%2s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%s%[^\n\r]';
formatSpec = '%8s%4s%2s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%s%[^\n\r]';

% Open the text file.
fileID = fopen(filename,'r');

% Read columns of data according to format string.
% This call is based on the structure of the file used to generate this
% code. If an error occurs for a different file, try regenerating the code
% from the Import Tool.
dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '',  'ReturnOnError', false);

% Close the text file.
fclose(fileID);

% Convert the contents of columns containing numeric strings to numbers.
% Replace non-numeric strings with NaN.
% Replace non-numeric strings with NaN.
raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
for col=1:length(dataArray)-1
    raw(1:length(dataArray{col}),col) = dataArray{col};
end
numericData = NaN(size(dataArray{1},1),size(dataArray,2));

for col=[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
    % Converts strings in the input cell array to numbers. Replaced non-numeric
    % strings with NaN.
    rawData = dataArray{col};
    for row=1:size(rawData, 1);
        % Create a regular expression to detect and remove non-numeric prefixes and
        % suffixes.
        regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
        try
            result = regexp(rawData{row}, regexstr, 'names');
            numbers = result.numbers;
            
            % Detected commas in non-thousand locations.
            invalidThousandsSeparator = false;
            if any(numbers==',');
                thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                if isempty(regexp(thousandsRegExp, ',', 'once'));
                    numbers = NaN;
                    invalidThousandsSeparator = true;
                end
            end
            % Convert numeric strings to numbers.
            if ~invalidThousandsSeparator;
                numbers = textscan(strrep(numbers, ',', ''), '%f');
                numericData(row, col) = numbers{1};
                raw{row, col} = numbers{1};
            end
        catch me
        end
    end
end

% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells

% Create output variable
MonthlyFilesData{fls} = cell2mat(raw);

% read the station name of each time sereis 
endRow2 = 1;

formatSpec = '%150s%[^\n\r]';

fileID2 = fopen(filename,'r');

dataArray = textscan(fileID2, formatSpec, endRow2, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);

dataArray{1} = strtrim(dataArray{1});
dataArray{2} = strtrim(dataArray{2});

% Allocate imported array to column variable names
VarName1 = dataArray{:, 1};

VarName2 = dataArray{:, 2};

fclose(fileID2);

Station = [dataArray{1:end-1}];

clearvars  endRow formatSpec fileID dataArray ans;

MonthlyStations{fls}=Station;

% Clear temporary variables
clearvars  formatSpec fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp me R startRow list isfile ;

end
clear filename
%% 2. Convert cross-tabulated discharge data into seires

for fls=1:length(MonthlyFilesData);
    m=MonthlyFilesData{fls};

% Delete the first row in the first column if it is not a year or it is NAN
m(isnan(m(:,3)) , :) =[];
m(isnan(m(:,2)) , :) =[];

% Read the streamflow values out of the text file
Tnew=m(:,4:15);

% find size of the matrix of streamflow values 
[R,C]=size(Tnew); % R:Row, %C:Column

% Read the exponant column (multiplier of streamflow values)
Expo=m(:,3);

% Generate a matrix of exponants that correspond for each month and each
% year 

ValuesExpo=repmat(Expo,1,12)'; % 12 is number of months 

% transpose the matrix to a vector 

ValuesExpo=ValuesExpo(:);


% Period=m(1,1)-m(end,1)
% Start=m(1,1);
% End=m(end,1);
% n=End-Start ;
n=length(m);

% Generate a vector of years 
y=m(:,2);

ym=repmat(y,1,12)';

Tm(:,1)=ym(:);
 % Generate a vector of months in a water year format
Tm(:,2)=repmat([10,11,12,1,2,3,4,5,6,7,8,9],1,R)';

% Transpose the matix to vector
Tm(:,3) = reshape(Tnew.',1,[])'; % m: Matrix
h=Tm(:,3);

% Multiply the streamflow variavbles with the exponant 
Values=h.*10.^ValuesExpo;
% extract the year 
year=Tm(:,1);
month=Tm(:,2);
%Combine Month/Year and add the first day of the month so it YYYYMMDD time
%stamp is accpeted in the ODM database
Date=datestr(datenum(year,month,1));

% there are three ways to handle YYYYMM in a database  
% 1) Add the 1st day of the month as store it along with month and year. so it will be YYYYMMDD
% 2) Use varchar data type and  only store month and year in it. YYYYMM
% 3) Store the year and the month values as two separate columns as integers. i.e. for month and year.

% I prefere to the use the first method (YYYYMMDD) date for these Advantages:
% Sorting is straight forward where is there is a default format (compared to storing as varchar)
% Date operations are straight forward (compared to storing as varchar/two integer columns)
% Data stays in one column (compared to storing as two integer columns)
%-----------------------------
% Monthly DataValues
%Convert data values to cell arrays 
Values = num2cell(Tm(:,3));
%TimeStamp
Date=cellstr(Date);

% combine time stamp and data values to check them with origian data in the
% text file 
DateValues=[Date,Values];

Monthly22{fls}=DateValues;

%-----------------------------
% find the vector of the Annual steamflow values 
Annual=m(:,16);
% find the vector of the years  
YearsPeriod=m(:,2);

% combine time stamp and data Annual values to check them with origian data in the
% text file 
DateValuesAnnual=[YearsPeriod,Annual];

Annual22{fls}=DateValuesAnnual;

clear DateValuesMonthly DateValues Date ym Tm m % its important to clear "DateValues" variable one after every file 

%-----------------------------

end

MonthlyFileNames{fls};

MonthlyStationNames=vertcat(MonthlyStations{:});

% Merge the site name the site code to make a uniqe site name 
MonthlyStationNamesUnq=strcat(MonthlyFileNames,MonthlyStationNames);


% Monthly Data
ODMMonthlyTimeSeries=transpose(Monthly22);

% % Replace empty cells with "Unknown"
% MonthlyStationNamesCode=MonthlyStationNames(:,1);
% emptyCellsCode= cellfun(@isempty,MonthlyStationNamesCode); 
% MonthlyStationNamesCode(emptyCellsCode)=num2cell(rand(1));
% 
% % Replace empty cells with "Unknown"
% MonthlyStationNamesName=MonthlyStationNames(:,2);
% emptyCellsName = cellfun(@isempty,MonthlyStationNamesName); 
% MonthlyStationNamesName(emptyCellsName)=num2cell(rand(1));

% MonthlyStationNames1=[MonthlyStationNamesCode,MonthlyStationNamesName];

ODMTimeSeriesMonthlyMatrix=[MonthlyFileNames,MonthlyStationNamesUnq,ODMMonthlyTimeSeries];
% Column headings in the ODMTimeSeriesMonthlyMatrix Array
% Column#1 is for the Site Name 
% Column#2 is for the Site code 
% Column#3 is for the Site file name
% Column#4 is for the matrix of data values 

% Annual Data (derived average monthlty)
ODMAnnualTimeSeries=transpose(Annual22);
ODMTimeSeriesAnnualMatrix=[MonthlyFileNames,MonthlyStationNamesUnq,ODMAnnualTimeSeries];

% Column headings in the ODMTimeSeriesAnnualMatrix Array
% Column#1 is for the Site Name 
% Column#2 is for the Site code 
% Column#3 is for the Site file name
% Column#4 is for the matrix of data values 
%% 3. Read and prepare (Sites) to match the ODM Database
% Methods, Sources, Variables are already defined and loaded in the previos First step above 
% Sites (the same site has Monthly and Annual)

% prepare attributes for the Sites table 
B=num2cell(repmat(NaN,length(ODMMonthlyTimeSeries),1));
E=num2cell(repmat('T',length(ODMTimeSeriesMonthlyMatrix),1));

% generate dummy longitude and latitudes because the files dont have them
% BUT ODM requires them. Mandatory attributes 
lat=num2cell(repmat(41.718473,1,length(ODMTimeSeriesMonthlyMatrix)))';
long=num2cell(repmat(-111.946402,1,length(ODMTimeSeriesMonthlyMatrix)))';

Dat=num2cell(repmat(0,1,length(ODMTimeSeriesMonthlyMatrix)))';

% I used the file name as a site code becuase it is unquie. The site code
% inside the files is not uniqe and many files share the same code. This is
% problomatic for the ODM database. The site name and code must be unque
% across the database
SitesMonthly={ODMTimeSeriesMonthlyMatrix(:,1),ODMTimeSeriesMonthlyMatrix(:,2),lat,long,Dat,...
   B,B,B,B,B,B,B,B,B};

SitesMonthly2=horzcat(SitesMonthly{:});
%% 4. Load Sites of monthly data  to the ODM database  


% connect Matlab workspace with the SQL Server ODM blank Database 
conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
    

fastinsert(conn,'Sites',{'SiteCode','SiteName','Latitude','Longitude',...
    'LatLongDatumID','Elevation_m',...
'VerticalDatum','LocalX','LocalY','LocalProjectionID',...
'PosAccuracy_m','State','County','Comments'},SitesMonthly2(1:end,:));

% % % Check dublicate site Names  
% A=SitesMonthly2(:,2);
% [~,idx]=unique(strcat(A(:,1)) );
% withoutduplicates=A(idx,:);
% 
%  % % Check dublicate site codes  
% BB=SitesMonthly2(:,1);
% [~,idxCode]=unique(strcat(BB(:,1)) );
% withoutduplicatesCode=BB(idxCode,:);

%% 5.1 Prepare Monthly Data Values to match the ODM Database strucutre 
% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys 


for fls=1:length(MonthlyFilesData);
     
x=ODMTimeSeriesMonthlyMatrix(fls,3) ;  
new_data = cat(1, x{:});

% remove empty values (Null) because ODM doesnt allows empty data values
for k = 1:numel(new_data)
  if isnan(new_data{k})
    new_data{k} = [];
  end
end
%remove empty cell array contents
new_data(any(cellfun(@isempty,new_data),2),:) = []; %delete row with any empty cells


DataValue=new_data(:,2);
ValueAccuracy=num2cell(repmat(NaN,1,length(DataValue)))';
LocalDateTime=new_data(:,1);
UTCOffset=num2cell(repmat(0,1,length(DataValue)))';
SiteID=num2cell(repmat(fls+566,1,length(DataValue)))'; % 51 are previous daily sites
DateTimeUTC=LocalDateTime;
% Varible ID= 2 for Storage/delivered volume, cumulitve (total), month, acre-ft unit  
VariableID=num2cell(repmat(2,1,length(DataValue)))';
OffsetValue=num2cell(repmat(NaN,1,length(DataValue)))';
OffsetTypeID=num2cell(repmat(NaN,1,length(DataValue)))';
CensorCode=(repmat({'nc'},1,length(DataValue)))';
QualifierID=num2cell(repmat(NaN,1,length(DataValue)))';
% Method ID= 2 for UDWR: Derived discharge data from different sources. Contact Graig Miller and Dave Cole for details 
MethodID=num2cell(repmat(2,1,length(DataValue)))';
% Source ID=1 for the source of Utah Division of Water Resources/State Agency/Craig Miller   
SourceID=num2cell(repmat(1,1,length(DataValue)))';
SampleID=num2cell(repmat(NaN,1,length(DataValue)))';
DerivedFromID=num2cell(repmat(NaN,1,length(DataValue)))';
QualityControlLevelID=num2cell(repmat(0,1,length(DataValue)))';

DataValues{fls}=[DataValue,ValueAccuracy,LocalDateTime,UTCOffset,...
DateTimeUTC,SiteID,VariableID,OffsetValue,...
OffsetTypeID,CensorCode,QualifierID,MethodID,...
SourceID,SampleID,DerivedFromID,...
QualityControlLevelID];



end
%% 5.1.1 Load the data values into the ODM database
conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
            
for fls=1:length(MonthlyFilesData);
fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValues{fls})
end
%% Find a way to skip the empty data for annual files 
%% 5.2 Prepare the Annual Data Values to match the ODM Database strucutre 
% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys 

for flsAnnual=1:length(MonthlyFilesData);
     
xAnnual=ODMTimeSeriesAnnualMatrix(flsAnnual,3) ;  
new_dataAnnual = cat(1, xAnnual{:});
new_dataAnnual = num2cell(new_dataAnnual);


% remove empty values (Null) because ODM doesnt allows empty data values
for kAnnual = 1:numel(new_dataAnnual);
  if isnan(new_dataAnnual{kAnnual});
    new_dataAnnual{kAnnual} = [];
  end
end

%remove empty cell array contents
new_dataAnnual(any(cellfun(@isempty,new_dataAnnual),2),:) = []; %delete row with any empty cells


DataValueAnn=new_dataAnnual(:,2);
ValueAccuracyAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';

LocalDateTimeAnn=cell2mat(new_dataAnnual(:,1)); % convert the cell to double 
% because the date functon to conver the year 1999 to 1/1/1999 works only
% with Double
LocalDateTimeAnn=cellstr(datestr(datenum(LocalDateTimeAnn,1,1)));


UTCOffsetAnn=num2cell(repmat(0,1,length(DataValueAnn)))';
SiteIDAnn=num2cell(repmat(flsAnnual+566,1,length(DataValueAnn)))';
DateTimeUTCAnn=LocalDateTimeAnn;

% Varible ID =3 for Storage/delivered volume, cumulitve (total), year, acre-ft unit  
VariableIDAnn=num2cell(repmat(3,1,length(DataValueAnn)))';

OffsetValueAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
OffsetTypeIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
CensorCodeAnn=(repmat({'nc'},1,length(DataValueAnn)))';
QualifierIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';

% Method ID= 3 for UDWR: Derived volume data from different sources. Contact Graig Miller and Dave Cole for details 
MethodIDAnn=num2cell(repmat(3,1,length(DataValueAnn)))';

% Source ID=1 for the source of Utah Division of Water Resources/State Agency/Craig Miller   
SourceIDAnn=num2cell(repmat(1,1,length(DataValueAnn)))';
SampleIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
DerivedFromIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
QualityControlLevelIDAnn=num2cell(repmat(0,1,length(DataValueAnn)))';

DataValuesAnn{flsAnnual}=[DataValueAnn,ValueAccuracyAnn,LocalDateTimeAnn,UTCOffsetAnn,...
DateTimeUTCAnn,SiteIDAnn,VariableIDAnn,OffsetValueAnn,...
OffsetTypeIDAnn,CensorCodeAnn,QualifierIDAnn,MethodIDAnn,...
SourceIDAnn,SampleIDAnn,DerivedFromIDAnn,...
QualityControlLevelIDAnn];

end
%% 5.2.1 Load the annual data values int othe ODM database         
% connect Matlab workspace with the SQL Server ODM blank Database 

conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
for flsAnnual=1:length(MonthlyFilesData);
fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValuesAnn{flsAnnual});

end

%% Forth: "OutPut" folder of Monthly data
% Read monthly discharge data values from Text files and load them to an ODM Microsfot SQL Server database
%% 1. Import data from 133 text file to Matlab workspace
% clear the workspace and command window
clear all, clc;
path(path,'C:\AdelAbdallah\01PhD\UDWRes\ProcessedData\Flow') % go to data folder 

%get info of files/folders in current directory
list=dir(pwd);  
%determine index of files vs folders
isfile=~[list.isdir]; 
%create cell array of file names
MonthlyFileNames={list(isfile).name}; 
MonthlyFileNames={list(~[list.isdir]).name}';

for fls=1:length(MonthlyFileNames);
% Initialize variables.
filename=MonthlyFileNames{fls};

% Initialize variables.
filename=MonthlyFileNames{fls};
startRow = 2;

% Read columns of data as strings:
% For more information, see the TEXTSCAN documentation.
% formatSpec = '%12s%2s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%s%[^\n\r]';
formatSpec = '%8s%4s%2s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%5s%s%[^\n\r]';

% Open the text file.
fileID = fopen(filename,'r');

% Read columns of data according to format string.
% This call is based on the structure of the file used to generate this
% code. If an error occurs for a different file, try regenerating the code
% from the Import Tool.
dataArray = textscan(fileID, formatSpec, 'Delimiter', '', 'WhiteSpace', '',  'ReturnOnError', false);

% Close the text file.
fclose(fileID);

% Convert the contents of columns containing numeric strings to numbers.
% Replace non-numeric strings with NaN.
% Replace non-numeric strings with NaN.
raw = repmat({''},length(dataArray{1}),length(dataArray)-1);
for col=1:length(dataArray)-1
    raw(1:length(dataArray{col}),col) = dataArray{col};
end
numericData = NaN(size(dataArray{1},1),size(dataArray,2));

for col=[2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
    % Converts strings in the input cell array to numbers. Replaced non-numeric
    % strings with NaN.
    rawData = dataArray{col};
    for row=1:size(rawData, 1);
        % Create a regular expression to detect and remove non-numeric prefixes and
        % suffixes.
        regexstr = '(?<prefix>.*?)(?<numbers>([-]*(\d+[\,]*)+[\.]{0,1}\d*[eEdD]{0,1}[-+]*\d*[i]{0,1})|([-]*(\d+[\,]*)*[\.]{1,1}\d+[eEdD]{0,1}[-+]*\d*[i]{0,1}))(?<suffix>.*)';
        try
            result = regexp(rawData{row}, regexstr, 'names');
            numbers = result.numbers;
            
            % Detected commas in non-thousand locations.
            invalidThousandsSeparator = false;
            if any(numbers==',');
                thousandsRegExp = '^\d+?(\,\d{3})*\.{0,1}\d*$';
                if isempty(regexp(thousandsRegExp, ',', 'once'));
                    numbers = NaN;
                    invalidThousandsSeparator = true;
                end
            end
            % Convert numeric strings to numbers.
            if ~invalidThousandsSeparator;
                numbers = textscan(strrep(numbers, ',', ''), '%f');
                numericData(row, col) = numbers{1};
                raw{row, col} = numbers{1};
            end
        catch me
        end
    end
end

% Replace non-numeric cells with NaN
R = cellfun(@(x) ~isnumeric(x) && ~islogical(x),raw); % Find non-numeric cells
raw(R) = {NaN}; % Replace non-numeric cells

% Create output variable
MonthlyFilesData{fls} = cell2mat(raw);

% read the station name of each time sereis 
endRow2 = 1;

formatSpec = '%150s%[^\n\r]';

fileID2 = fopen(filename,'r');

dataArray = textscan(fileID2, formatSpec, endRow2, 'Delimiter', '', 'WhiteSpace', '', 'ReturnOnError', false);

dataArray{1} = strtrim(dataArray{1});
dataArray{2} = strtrim(dataArray{2});

% Allocate imported array to column variable names
VarName1 = dataArray{:, 1};

VarName2 = dataArray{:, 2};

fclose(fileID2);

Station = [dataArray{1:end-1}];

clearvars  endRow formatSpec fileID dataArray ans;

MonthlyStations{fls}=Station;

% Clear temporary variables
clearvars  formatSpec fileID dataArray ans raw col numericData rawData row regexstr result numbers invalidThousandsSeparator thousandsRegExp me R startRow list isfile ;

end
clear filename
%% 2. Convert cross-tabulated discharge data into seires

for fls=1:length(MonthlyFilesData);
    m=MonthlyFilesData{fls};

% Delete the first row in the first column if it is not a year or it is NAN
m(isnan(m(:,3)) , :) =[];
m(isnan(m(:,2)) , :) =[];

% Read the streamflow values out of the text file
Tnew=m(:,4:15);

% find size of the matrix of streamflow values 
[R,C]=size(Tnew); % R:Row, %C:Column

% Read the exponant column (multiplier of streamflow values)
Expo=m(:,3);

% Generate a matrix of exponants that correspond for each month and each
% year 

ValuesExpo=repmat(Expo,1,12)'; % 12 is number of months 

% transpose the matrix to a vector 

ValuesExpo=ValuesExpo(:);


% Period=m(1,1)-m(end,1)
% Start=m(1,1);
% End=m(end,1);
% n=End-Start ;
n=length(m);

% Generate a vector of years 
y=m(:,2);

ym=repmat(y,1,12)';

Tm(:,1)=ym(:);
 % Generate a vector of months in a water year format
Tm(:,2)=repmat([10,11,12,1,2,3,4,5,6,7,8,9],1,R)';

% Transpose the matix to vector
Tm(:,3) = reshape(Tnew.',1,[])'; % m: Matrix
h=Tm(:,3);

% Multiply the streamflow variavbles with the exponant 
Values=h.*10.^ValuesExpo;
% extract the year 
year=Tm(:,1);
month=Tm(:,2);
%Combine Month/Year and add the first day of the month so it YYYYMMDD time
%stamp is accpeted in the ODM database
Date=datestr(datenum(year,month,1));

% there are three ways to handle YYYYMM in a database  
% 1) Add the 1st day of the month as store it along with month and year. so it will be YYYYMMDD
% 2) Use varchar data type and  only store month and year in it. YYYYMM
% 3) Store the year and the month values as two separate columns as integers. i.e. for month and year.

% I prefere to the use the first method (YYYYMMDD) date for these Advantages:
% Sorting is straight forward where is there is a default format (compared to storing as varchar)
% Date operations are straight forward (compared to storing as varchar/two integer columns)
% Data stays in one column (compared to storing as two integer columns)
%-----------------------------
% Monthly DataValues
%Convert data values to cell arrays 
Values = num2cell(Tm(:,3));
%TimeStamp
Date=cellstr(Date);

% combine time stamp and data values to check them with origian data in the
% text file 
DateValues=[Date,Values];

Monthly22{fls}=DateValues;

%-----------------------------
% find the vector of the Annual steamflow values 
Annual=m(:,16);
% find the vector of the years  
YearsPeriod=m(:,2);

% combine time stamp and data Annual values to check them with origian data in the
% text file 
DateValuesAnnual=[YearsPeriod,Annual];

Annual22{fls}=DateValuesAnnual;

clear DateValuesMonthly DateValues Date ym Tm m % its important to clear "DateValues" variable one after every file 

%-----------------------------

end

MonthlyFileNames{fls};

MonthlyStationNames=vertcat(MonthlyStations{:});

% Merge the site name the site code to make a uniqe site name 
MonthlyStationNamesUnq=strcat(MonthlyFileNames,MonthlyStationNames)


% Monthly Data
ODMMonthlyTimeSeries=transpose(Monthly22);

% % Replace empty cells with "Unknown"
% MonthlyStationNamesCode=MonthlyStationNames(:,1);
% emptyCellsCode= cellfun(@isempty,MonthlyStationNamesCode); 
% MonthlyStationNamesCode(emptyCellsCode)=num2cell(rand(1));
% 
% % Replace empty cells with "Unknown"
% MonthlyStationNamesName=MonthlyStationNames(:,2);
% emptyCellsName = cellfun(@isempty,MonthlyStationNamesName); 
% MonthlyStationNamesName(emptyCellsName)=num2cell(rand(1));

% MonthlyStationNames1=[MonthlyStationNamesCode,MonthlyStationNamesName];

ODMTimeSeriesMonthlyMatrix=[MonthlyFileNames,MonthlyStationNamesUnq,ODMMonthlyTimeSeries];
% Column headings in the ODMTimeSeriesMonthlyMatrix Array
% Column#1 is for the Site Name 
% Column#2 is for the Site code 
% Column#3 is for the Site file name
% Column#4 is for the matrix of data values 

% Annual Data (derived average monthlty)
ODMAnnualTimeSeries=transpose(Annual22);
ODMTimeSeriesAnnualMatrix=[MonthlyFileNames,MonthlyStationNamesUnq,ODMAnnualTimeSeries];

% Column headings in the ODMTimeSeriesAnnualMatrix Array
% Column#1 is for the Site Name 
% Column#2 is for the Site code 
% Column#3 is for the Site file name
% Column#4 is for the matrix of data values 
%% 3. Read and prepare (Sites) to match the ODM Database
% Methods, Sources, Variables are already defined and loaded in the previos First step above 
% Sites (the same site has Monthly and Annual)

% prepare attributes for the Sites table 
B=num2cell(repmat(NaN,length(ODMMonthlyTimeSeries),1));
E=num2cell(repmat('T',length(ODMTimeSeriesMonthlyMatrix),1));

% generate dummy longitude and latitudes because the files dont have them
% BUT ODM requires them. Mandatory attributes 
lat=num2cell(repmat(41.718473,1,length(ODMTimeSeriesMonthlyMatrix)))';
long=num2cell(repmat(-111.946402,1,length(ODMTimeSeriesMonthlyMatrix)))';

Dat=num2cell(repmat(0,1,length(ODMTimeSeriesMonthlyMatrix)))';

% I used the file name as a site code becuase it is unquie. The site code
% inside the files is not uniqe and many files share the same code. This is
% problomatic for the ODM database. The site name and code must be unque
% across the database
SitesMonthly={ODMTimeSeriesMonthlyMatrix(:,1),ODMTimeSeriesMonthlyMatrix(:,2),lat,long,Dat,...
   B,B,B,B,B,B,B,B,B};

SitesMonthly2=horzcat(SitesMonthly{:});
%% 4. Load monthly data Sites to the ODM database  


% connect Matlab workspace with the SQL Server ODM blank Database 
conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
    

fastinsert(conn,'Sites',{'SiteCode','SiteName','Latitude','Longitude',...
    'LatLongDatumID','Elevation_m',...
'VerticalDatum','LocalX','LocalY','LocalProjectionID',...
'PosAccuracy_m','State','County','Comments'},SitesMonthly2(:,:));

% % % Check dublicate site Names  
% A=SitesMonthly2(:,2);
% [~,idx]=unique(strcat(A(:,1)) );
% withoutduplicates=A(idx,:);
% 
%  % % Check dublicate site codes  
% BB=SitesMonthly2(:,1);
% [~,idxCode]=unique(strcat(BB(:,1)) );
% withoutduplicatesCode=BB(idxCode,:);

%% 5.1 Prepare Monthly Data Values to match the ODM Database strucutre 
% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys 

for fls=1:length(MonthlyFilesData);
     
x=ODMTimeSeriesMonthlyMatrix(fls,3) ;  
new_data = cat(1, x{:});

% remove empty values (Null) because ODM doesnt allows empty data values
for k = 1:numel(new_data)
  if isnan(new_data{k})
    new_data{k} = [];
  end
end
%remove empty cell array contents
new_data(any(cellfun(@isempty,new_data),2),:) = []; %delete row with any empty cells


DataValue=new_data(:,2);
ValueAccuracy=num2cell(repmat(NaN,1,length(DataValue)))';
LocalDateTime=new_data(:,1);
UTCOffset=num2cell(repmat(0,1,length(DataValue)))';
SiteID=num2cell(repmat(fls+764,1,length(DataValue)))'; % 51 are previous daily sites
DateTimeUTC=LocalDateTime;
% Varible ID= 2 for Storage/delivered volume, cumulitve (total), month, acre-ft unit  
VariableID=num2cell(repmat(2,1,length(DataValue)))';
OffsetValue=num2cell(repmat(NaN,1,length(DataValue)))';
OffsetTypeID=num2cell(repmat(NaN,1,length(DataValue)))';
CensorCode=(repmat({'nc'},1,length(DataValue)))';
QualifierID=num2cell(repmat(NaN,1,length(DataValue)))';
% Method ID= 2 for UDWR: Derived discharge data from different sources. Contact Graig Miller and Dave Cole for details 
MethodID=num2cell(repmat(2,1,length(DataValue)))';
% Source ID=1 for the source of Utah Division of Water Resources/State Agency/Craig Miller   
SourceID=num2cell(repmat(1,1,length(DataValue)))';
SampleID=num2cell(repmat(NaN,1,length(DataValue)))';
DerivedFromID=num2cell(repmat(NaN,1,length(DataValue)))';
QualityControlLevelID=num2cell(repmat(0,1,length(DataValue)))';

DataValues{fls}=[DataValue,ValueAccuracy,LocalDateTime,UTCOffset,...
DateTimeUTC,SiteID,VariableID,OffsetValue,...
OffsetTypeID,CensorCode,QualifierID,MethodID,...
SourceID,SampleID,DerivedFromID,...
QualityControlLevelID];

end
%% 5.1.1 Load the data values into the ODM database
conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
            
for fls=1:length(MonthlyFilesData);
fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValues{fls})
end
%% 5.2 Prepare the Annual Data Values to match the ODM Database strucutre 
% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys 

for flsAnnual=1:length(MonthlyFilesData);
     
xAnnual=ODMTimeSeriesAnnualMatrix(flsAnnual,3) ;  
new_dataAnnual = cat(1, xAnnual{:});
new_dataAnnual = num2cell(new_dataAnnual);


% remove empty values (Null) because ODM doesnt allows empty data values
for kAnnual = 1:numel(new_dataAnnual);
  if isnan(new_dataAnnual{kAnnual});
    new_dataAnnual{kAnnual} = [];
  end
end

%remove empty cell array contents
new_dataAnnual(any(cellfun(@isempty,new_dataAnnual),2),:) = []; %delete row with any empty cells


DataValueAnn=new_dataAnnual(:,2);
ValueAccuracyAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';

LocalDateTimeAnn=cell2mat(new_dataAnnual(:,1)); % convert the cell to double 
% because the date functon to conver the year 1999 to 1/1/1999 works only
% with Double
LocalDateTimeAnn=cellstr(datestr(datenum(LocalDateTimeAnn,1,1)));


UTCOffsetAnn=num2cell(repmat(0,1,length(DataValueAnn)))';
SiteIDAnn=num2cell(repmat(flsAnnual+764,1,length(DataValueAnn)))';
DateTimeUTCAnn=LocalDateTimeAnn;

% Varible ID =3 for Storage/delivered volume, cumulitve (total), year, acre-ft unit  
VariableIDAnn=num2cell(repmat(3,1,length(DataValueAnn)))';

OffsetValueAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
OffsetTypeIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
CensorCodeAnn=(repmat({'nc'},1,length(DataValueAnn)))';
QualifierIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';

% Method ID= 3 for UDWR: Derived volume data from different sources. Contact Graig Miller and Dave Cole for details 
MethodIDAnn=num2cell(repmat(3,1,length(DataValueAnn)))';

% Source ID=1 for the source of Utah Division of Water Resources/State Agency/Craig Miller   
SourceIDAnn=num2cell(repmat(1,1,length(DataValueAnn)))';
SampleIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
DerivedFromIDAnn=num2cell(repmat(NaN,1,length(DataValueAnn)))';
QualityControlLevelIDAnn=num2cell(repmat(0,1,length(DataValueAnn)))';

DataValuesAnn{flsAnnual}=[DataValueAnn,ValueAccuracyAnn,LocalDateTimeAnn,UTCOffsetAnn,...
DateTimeUTCAnn,SiteIDAnn,VariableIDAnn,OffsetValueAnn,...
OffsetTypeIDAnn,CensorCodeAnn,QualifierIDAnn,MethodIDAnn,...
SourceIDAnn,SampleIDAnn,DerivedFromIDAnn,...
QualityControlLevelIDAnn];

end
%% 5.2.1 Load the annual data values int othe ODM database         
% connect Matlab workspace with the SQL Server ODM blank Database 

conn = database('OD','sa','Pass',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
for flsAnnual=1:length(MonthlyFilesData);
fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValuesAnn{flsAnnual});

end

