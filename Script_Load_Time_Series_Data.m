%% ReadME   

% Matlab script to read, manipulate, and load discharge time series data from 
% cross-tabulated text files into a Microsoft SQL Server ODM Blank Database

% Data provided by Craig Miller and Dave Cole at Utah Division of Water Resources    

% 1. Monthly (547 MonthlyStations or text files)
% 2. Daily (51 MonthlyStations or text files)

% Written and tested by Adel Abdallah
% Started on June 15 2015
% Finsihed on December 7th 2015
% estimated time spent: 70 hours of work

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
%% First: Daily
% Read daily discharge data values from 51 text files and load them to an ODM Microsfot SQL Server database
%% 1. Import data from 51 text file to Matlab workspace

clear all, clc;% clear workspace and clean memory from previos work

path(path,'C:\AdelAbdallah\UDWRes\ProcessedData\Daily') % go to data folder

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
%% 3. Read and prepare needed metadata: Variables, Sources, Methods, and Sites form Excel
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
%% 4. Load metadata 
% Methods 
    
conn = database('OD','sa','Adel2010',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
    
fastinsert(conn,'Methods',{'MethodDescription','MethodLink'},Methods)


fastinsert(conn,'Variables',{'VariableCode','VariableName','Speciation','VariableUnitsID',...
    'SampleMedium','ValueType','IsRegular','TimeSupport','TimeUnitsID','DataType',...
    'GeneralCategory','NoDataValue'},Variables)

fastinsert(conn,'Sources',{'Organization','SourceDescription','SourceLink',...
    'ContactName','Phone','Email','Address','City','State','ZipCode','Citation','MetadataID'},Sources)


fastinsert(conn,'Sites',{'SiteCode','SiteName','Latitude','Longitude',...
    'LatLongDatumID','Elevation_m',...
'VerticalDatum','LocalX','LocalY','LocalProjectionID',...
'PosAccuracy_m','State','County','Comments'},Sites2(:,:))
%% 5. Load Data Values into ODM Database  
% File #34 has a problem with dublicate decemeber 1992 reading
% and March 2nd and 3rd the same year

% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys

for fls=1:length(DailyFilesData);
     
x=ODMTimeSeriesMatrix(fls,4)  ;  
new_data = cat(1, x{:});

DataValue=new_data(:,2);
ValueAccuracy=num2cell(repmat(NaN,1,length(DataValue)))';
LocalDateTime=new_data(:,1);
UTCOffset=num2cell(repmat(0,1,length(DataValue)))';
SiteID=num2cell(repmat(fls,1,length(DataValue)))';
DateTimeUTC=LocalDateTime;
VariableID=num2cell(repmat(42,1,length(DataValue)))';
OffsetValue=num2cell(repmat(NaN,1,length(DataValue)))';
OffsetTypeID=num2cell(repmat(NaN,1,length(DataValue)))';
CensorCode=(repmat({'nc'},1,length(DataValue)))';
QualifierID=num2cell(repmat(NaN,1,length(DataValue)))';
MethodID=num2cell(repmat(26,1,length(DataValue)))';
SourceID=num2cell(repmat(6,1,length(DataValue)))';
SampleID=num2cell(repmat(NaN,1,length(DataValue)))';
DerivedFromID=num2cell(repmat(NaN,1,length(DataValue)))';
QualityControlLevelID=num2cell(repmat(0,1,length(DataValue)))';

DataValues{fls}=[DataValue,ValueAccuracy,LocalDateTime,UTCOffset,...
DateTimeUTC,SiteID,VariableID,OffsetValue,...
OffsetTypeID,CensorCode,QualifierID,MethodID,...
SourceID,SampleID,DerivedFromID,...
QualityControlLevelID];


fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValues{fls})

end

%% Second: Monthly/Annual
% Read monthly discharge data values from Text files and load them to an ODM Microsfot SQL Server database
%% 1. Import data from 547 text file to Matlab workspace
clear all, clc;
path(path,'C:\AdelAbdallah\UDWRes\ProcessedData\Monthly') % go to data folder

list=dir(pwd);  %get info of files/folders in current directory
isfile=~[list.isdir]; %determine index of files vs folders
MonthlyFileNames={list(isfile).name}; %create cell array of file names
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

% Monthly Data
ODMMonthlyTimeSeries=transpose(Monthly22);

% Replace empty cells with "Unknown"
MonthlyStationNamesCode=MonthlyStationNames(:,1);
emptyCellsCode= cellfun(@isempty,MonthlyStationNamesCode); 
MonthlyStationNamesCode(emptyCellsCode)=num2cell(rand(1));

% Replace empty cells with "Unknown"
MonthlyStationNamesName=MonthlyStationNames(:,2);
emptyCellsName = cellfun(@isempty,MonthlyStationNamesName); 
MonthlyStationNamesName(emptyCellsName)=num2cell(rand(1));

MonthlyStationNames1=[MonthlyStationNamesCode,MonthlyStationNamesName];

ODMTimeSeriesMonthlyMatrix=[MonthlyStationNames1,MonthlyFileNames,ODMMonthlyTimeSeries];
% Column headings in the ODMTimeSeriesMonthlyMatrix Array
% Column#1 is for the Site Name 
% Column#2 is for the Site code 
% Column#3 is for the Site file name
% Column#4 is for the matrix of data values 

% Annual Data (derived average monthlty)
ODMAnnualTimeSeries=transpose(Annual22)
ODMTimeSeriesAnnualMatrix=[MonthlyStationNames1,MonthlyFileNames,ODMAnnualTimeSeries]

% Column headings in the ODMTimeSeriesAnnualMatrix Array
% Column#1 is for the Site Name 
% Column#2 is for the Site code 
% Column#3 is for the Site file name
% Column#4 is for the matrix of data values 
%% 3. Read and prepare needed metadata: (Sites) form Excel

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

% double check on the attributes and right matrix coluim here match with
% ODM
SitesMonthly={ODMTimeSeriesMonthlyMatrix(:,1),ODMTimeSeriesMonthlyMatrix(:,2),lat,long,Dat,...
    B,B,B,B,B,B,B,B,ODMTimeSeriesMonthlyMatrix(:,3)};

SitesMonthly2=horzcat(SitesMonthly{:});
%% 4. Load Sites metadata (sites)
% Methods 

% connect Matlab workspace with the SQL Server ODM blank Database 
conn = database('OD','sa','Adel2010',...
                'Vendor','Microsoft SQL Server','Server','localhost',...
                'AuthType','Server','PortNumber',52158);
    


fastinsert(conn,'Sites',{'SiteCode','SiteName','Latitude','Longitude',...
    'LatLongDatumID','Elevation_m',...
'VerticalDatum','LocalX','LocalY','LocalProjectionID',...
'PosAccuracy_m','State','County','Comments'},SitesMonthly2(:,:))
%% 5.1 Load Monthly Data Values into ODM Database  
% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys 

for fls=1:length(MonthlyFilesData);
     
x=ODMTimeSeriesMonthlyMatrix(fls,4) ;  
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
SiteID=num2cell(repmat(fls,1,length(DataValue)))';
DateTimeUTC=LocalDateTime;
VariableID=num2cell(repmat(42,1,length(DataValue)))';
OffsetValue=num2cell(repmat(NaN,1,length(DataValue)))';
OffsetTypeID=num2cell(repmat(NaN,1,length(DataValue)))';
CensorCode=(repmat({'nc'},1,length(DataValue)))';
QualifierID=num2cell(repmat(NaN,1,length(DataValue)))';
MethodID=num2cell(repmat(26,1,length(DataValue)))';
SourceID=num2cell(repmat(6,1,length(DataValue)))';
SampleID=num2cell(repmat(NaN,1,length(DataValue)))';
DerivedFromID=num2cell(repmat(NaN,1,length(DataValue)))';
QualityControlLevelID=num2cell(repmat(0,1,length(DataValue)))';

DataValues{fls}=[DataValue,ValueAccuracy,LocalDateTime,UTCOffset,...
DateTimeUTC,SiteID,VariableID,OffsetValue,...
OffsetTypeID,CensorCode,QualifierID,MethodID,...
SourceID,SampleID,DerivedFromID,...
QualityControlLevelID];


fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValues{fls})

end
%% 5.2 Load Annual Data Values into ODM Database  
% for loop over the sites: get the site name, get the corresponding forien
% key, create a vector of forign keys for its lenght. Insert Data Values
% for the site. Always the same unit, method, source, and variable. Just
% adjust the lengh of their vector of forighn keys 


for fls=1:length(MonthlyFilesData);
     
x=ODMTimeSeriesAnnualMatrix(fls,4)  ;  
new_data = cat(1, x{:});

DataValue=new_data(:,2);
ValueAccuracy=num2cell(repmat(NaN,1,length(DataValue)))';
LocalDateTime=new_data(:,1);
UTCOffset=num2cell(repmat(0,1,length(DataValue)))';
SiteID=num2cell(repmat(fls,1,length(DataValue)))';
DateTimeUTC=LocalDateTime;
VariableID=num2cell(repmat(42,1,length(DataValue)))';
OffsetValue=num2cell(repmat(NaN,1,length(DataValue)))';
OffsetTypeID=num2cell(repmat(NaN,1,length(DataValue)))';
CensorCode=(repmat({'nc'},1,length(DataValue)))';
QualifierID=num2cell(repmat(NaN,1,length(DataValue)))';
MethodID=num2cell(repmat(26,1,length(DataValue)))';
SourceID=num2cell(repmat(6,1,length(DataValue)))';
SampleID=num2cell(repmat(NaN,1,length(DataValue)))';
DerivedFromID=num2cell(repmat(NaN,1,length(DataValue)))';
QualityControlLevelID=num2cell(repmat(0,1,length(DataValue)))';

DataValues{fls}=[DataValue,ValueAccuracy,LocalDateTime,UTCOffset,...
DateTimeUTC,SiteID,VariableID,OffsetValue,...
OffsetTypeID,CensorCode,QualifierID,MethodID,...
SourceID,SampleID,DerivedFromID,...
QualityControlLevelID];


fastinsert(conn,'DataValues',{'DataValue','ValueAccuracy','LocalDateTime','UTCOffset',...
'DateTimeUTC','SiteID','VariableID','OffsetValue',...
'OffsetTypeID','CensorCode','QualifierID','MethodID',...
'SourceID','SampleID','DerivedFromID','QualityControlLevelID'},...
DataValues{fls})

end
