# DoD_Discharge

**Overview** <br>
This repository reads in discharge data and generates predicted discharge throughout the year. The code is separated into R markdowns for each year. Raw pressure transducer (PT) and calculated measured discharge (Q) are pulled in from the coresponding year's sensor folder on the "DoD project" Google Drive. Cleaning the PT data included removing out-of-water points, shifting jumps in pressure due to cleaning of sensor houseing, removing beaver dams, combining data from paired PTs when neither had a complete record, and filling in small gaps in data using the 'na_kalman' fuction from the 'imputeTS' R package. Air pressure data was collected by the Harms' lab, or downloaded from NEON or Eielson Airforce Base. For some years we have water level data (water level above sensor, not stream depth) that was calculated in HOBOware instead of pressure data. Water pressure - air pressure or water level was regressed against the measured Q to fit a linear relationship and the formula is used to predict Q from the PT data. Pressure units are kPa and discharge units are L/s. AKDT is the time zone for all output files.

**Folders**
- PT_data: Once the PT data are cleaned they are exported to this folder. All date+times are in AKDT.
- Predicted_Discharge: The predicted Q is exported to this folder. For every year there is a file labeled 
  'Predicted_Q_YEAR' that contains date+time, Q and site, and a file labeled 'Q.daily.YEAR' where Q is the 
  average Q for each day. Most years have the same data saparated into separate folder by site. All date+times 
  are in AKDT!!
- Rmd summaries: Contains HTML RMarkdown files for each year.

**2015 Q**
- Predicted Q exported as 15 minute averages
- Raw PT input time zone: GMT - 8 (AKDT)
- All PT data at 5 minute intervals
- Air pressure data from Eielson, 1 hour intervals
- FRCH:
  - Field book PT start and end times: 5/14/2015 to 9/21/2015
  - Raw PT data start and end times: 2015-05-14 12:05 to 2015-09-22 10:20
  - Predicted Q start and end times: 5/18/2015 3:15 - 9/21/2015 12:45
  - PT data level with air pressure until 5/18/15 15:15
  - Quadratic relationship
- MOOS:
  - Field book PT start and end times: 5/14/2015 to 9/21/2015
  - Raw PT data start and end times: 2015-05-14 12:05 to 2015-09-22 10:20
  - Predicted Q start and end times: 5/20/2015 10:15 to 9/21/2015 15:50
  - PT data level with air pressure until 2015-05-20 22:10
  - Spike in pressure difference at start removed because it doesn't match French
  - Quadratic relationship
- Gaps filled up to 50 x 15 minutes using na_kalman
- Checks at end of Rmarkdown:
  - Plot of length of timesteps for all sites: 15 minutes
  - Plot of missing values for all sites 
  
**2018 Q**
- Predicted Q at 30 minute intervals
- Raw PT input time zone: GMT - 9 (AKST)!!
- PT data time interval: 30 minutes
- Field book PT start and end times: 
  - MOOS: 5/30/18 10:55 to 10/26/18 11:30 
  - FRCH: 5/30/18 ~15:10 to 10/12/18 ~9:35
- FRCH pressure data was below 100 (out of water) before 5/18
- MOOS pressure data was below 100 (out of water) before 5/20
- Checks at end of Rmarkdown:
  - Plot of length of timesteps for all sites: 30 minutes
  - Plot of missing values for all sites 
 
**2019 Q**
- Raw PT input time zone: GMT - 8
- Field book PT start and end times: 
  - FRCH:
  - MOOS:
  - POKE: 5/10/19 12:26 (PT1) and 5/14/19 (PT2) to 10/17/19
  - STRT: 5/21/19 12:30 to 10/16/19 10:30
  - VAUL:

**2020 Q**
- Raw PT input time zone: GMT - 8
- Field book PT start and end times: 
  - FRCH: 6/15/20 12:45 to 10/15/20
  - MOOS: 6/15/20 17:30 to 10/14/20 ~16:15
  - POKE: 6/4/20 ~14:30 to 10/14/20 ~17:00
  - STRT: 6/17/20 16:30 to 10/13/20 ~13:30 
  - VAUL: 6/5/20 16:00 to 10/14/20 ~ 12:30
  
**2021 Q**
- Raw PT input time zone: GMT - 8
- Field book PT start and end times: 
  - FRCH: 5/4/21 ~17:00 to 9/28/21 10:15
  - POKE: 5/6/21 17:00 to 9/29/21 14:00
  - STRT: 5/7/21 17:00 to 9/30/21 13:30
  - VAUL: 5/13/21 17:00 to 9/27/21 ~ 16:00
- MOOS: 
  - 5 minute intervals 
  - Field book PT start and end times: 5/4/21 ~17:00 to 9/28/21 13:00
  - Raw water level data start and end times PT1: 2021-06-30 12:00 to 2021-09-28 10:30
  - Raw water level data start and end times PT2: 2021-06-30 12:00 to 2021-09-28 10:30
  - PT1 absolute pressure is above 100 untill 2021-09-28 12:45
  - Last few points of MOOS predicted Q are negative, but not removed because pressure data well above 100 during that time for PT1 (lots of PT2 pressure data is below 100).
  - Sharp fall at 2021-08-11 14:00:00 - was the site visited?
  
**2022 Q**
- Predicted Q exported as 15 minute averages
- Raw PT input time zone: GMT - 8
- Air pressure data is from NEON, with small gaps filled by Eielson
- Field book PT start and end times: 
  - POKE: 5/9/22 ~13:15 to 10/8/22 ~10:45
  - STRT: 6/2/22 13:15 to 10/8/22 ~14:15
  - VAUL: 5/25/22 11:15 (PT left) and 6/6/22 10:40 (PT right) to 10/11/22 ~ 14:30
 -FRCH:
  - Field book PT start and end times: 5/11/22 13:30 to 10/10/22 10:25
  - Raw PT data start and end times:
  - Predicted Q start and end times:
 - MOOS:
  - 5 minute intervals
  - Field book PT start and end times: 5/10/22 ~13:15 to 10/7/22 ~10:45 
  - Raw PT2 data start and end times: 2022-05-09 10:00 to 2022-10-10 16:00
  - Predicted Q start and end times: 5/25/2022 12:05 to 10/8/2022 8:15
  - Only PT2 was used because PT1 had a big gap in July and August
  - PT2 < 100 until 2022-05-24 13:45
- Checks at end of Rmarkdown:
  - Plot of length of timesteps for all sites: 15 minutes
  - Plot of missing values for all sites  
