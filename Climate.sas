*read each file one at a time, changing the names each time: one, two
PROC IMPORT OUT= WORK.two DATAFILE= "/home/u59285623/my_content/ceei.xls" 
            DBMS=xls REPLACE;
     SHEET="2010"; 
     GETNAMES=YES;
RUN;
*concat, format 
data climate;
set one two;
CO2Et = input('CO2E (t)'n, 12.);
Connects=input(Connections, 12.);
Consump=input(Consumption, 12.);
GJ=input('Energy (GJ)'n, 12.);
Year=put('Report Year'n, 4.);
Drop 'CO2E (t)'n Connections Consumption 'Energy (GJ)'n R 'Avg VKT'n 'Data Type'n 
'Local Govt ID'n 'Measurement Desc'n 'Report Year'n Units;
run;
*if needed at this point, for inspection as shown below
%let categorical = 'Local Govt Name'n 'Local Govt Type'n 'Regional District'n 
'Sector'n 'Subsector Desc'n Year;
%let interval= percapem percapen Connects Consump GJ Population;

*select columns for further analysis
proc sql;   
   create table climate2 as
      select 'Local Govt Name'n, 'Local Govt Type'n, Sector, Year, CO2Et
         from climate;

*unpivot the years, find the reduction <30%, select 
PROC SORT DATA=climate2 OUT=climate3 NODUPRECS;
BY 'Local Govt Name'n 'Local Govt Type'n  Sector Year;
RUN ;
proc transpose data=climate3 out=climate4 prefix=T LET;
    by 'Local Govt Name'n 'Local Govt Type'n  Sector;
    id Year;
    var CO2Et;
run;
Data climate4;
set climate4;
DIF = 100*(T2010 - T2007)/T2007;
if missing(DIF) then policy =.;
eslse if DIF< -3 then policy="achieved";
else policy="no";
run;
Data climate5;
set climate4;
if 'Local Govt Type'n IN ('Regional District', 'Regional District Unincorporated Areas') then delete;
if 'Local Govt Type'n in ('Island Municipality','Island Trust Area','Indian Government District', 
'Regional Municipality' 'Town' 'Village' ) 
then Local='Island/Indian/RM/Twn/Vill';
else Local='Local Govt Type'n;
if Sector in ('Agriculture','Land-use Change - Deforestation','Solid Waste') then Sectr='Luluc+waste';
else Sectr=Sector;
drop 'Local Govt Type'n Sector Secor Secot_c 'Local Govt Type_c'n Sector_c T2007 T2010 _NAME_;
run;

*inspect for climate 4 or 5 (can also be done before above analysis ) 
PROC CONTENTS DATA=climate5;
RUN;
proc means data = climate5 n nmiss mean median;
run;
%let categorical = 'Local Govt Name'n Local Sectr Policy;
ods graphics ;
proc freq data=climate5;
tables &categorical/ plot=freqplot;
run;
*for interval vars 
%let interval= DIF;
ods select histogram;
proc univariate data=climate5 noprint;
var &interval;
histogram &interval/ normal kernel;
inset n mean median std/position=ne;
run;
proc sgplot data=climate5;
vbox DIF /category=Sectr ;
*vbox DIF /category=Local ;
run;

*cross tabulation
proc freq data=climate5;
  tables Local Sectr Policy
  Local*Policy Sectr*Policy/
  plots(only)=freqplot(scale=percent); 
run;

*simple logistic regression
ods graphics on;
proc logistic data=climate5 alpha=0.05 
plots(only)=(effect oddsratio);
class Sectr;
model Policy (event="achieved")=Sectr/clodds=pl;
run;
*multiple logistic
ods graphics on;
proc logistic data=climate5 alpha=0.05 plots(only)=(effect oddsratio);
class local(ref="Island/Indian/RM/Twn/Vill") Sectr (ref="Luluc+waste") /param=ref;
model policy(event="achieved")=local sectr/clodds=pl; 
run;

ods graphics on;
proc logistic data=climate5;
class local (param=ref ref='Island/Indian/RM/Twn/Vill') Sectr (param=ref ref='Luluc+waste');
model Policy(event='achieved')=local|Sectr/
selection=backward clodds=pl slstay=0.10;
run;

*group by and subtract (if needed)
proc sql;
create table work.climate2 as
    select 'Local Govt Name'n, year,
        mean(GJ) as MeanGJ
    from work.climate
    group by 'Local Govt Name'n, year;
quit;
DATA climate3;
SET climate2;   
LAG_MeanGJ = LAG(MeanGJ);   
if Year=2007 then LAG_MeanGJ =.;
DIF_GJ = MeanGJ - LAG_MeanGJ;   
PER_INCREASE = (DIF_GJ/LAG_MeanGJ)*100; 
RUN;  



