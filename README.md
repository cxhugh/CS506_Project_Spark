# CS506 Project: Using Permit, Parcel Data to Investigate Residence Energy Systems

## Team: 
Zixin "Cindy" Ding (cindydzx@bu.edu)<br/>
Fang "Hugh" Qu (hughqu@bu.edu)<br/>
Jason Lu (jasonlu6@bu.edu)<br/>

## Coordinators:
Ziba Cranmer (zcranmer@bu.edu)<br/>
Professor Dora Erdos (edori@bu.edu)<br/> 
Professor Adam Smith (ad22smith@bu.edu)<br/>

## Abstract:
BU’s Institute for Sustainable Energy (ISE) is a University-wide institute that promotes faculty research. We have partnered with Michael Walsh and Adam Pollack to design and analyze a database of tax parcels and building permits for populating a building energy systems inventory and investigating the trends and distribution of heating and energy efficient systems. The goals of the project are: 1) determine which communities are more likely to have heat pumps, based on various demographics, 2) determine which communities are benefitting from energy efficient systems or not, and 3) implement a predictive model which determines which parcel building has a particular type of heating system. 4) Visualize the distribution of heating systems in Boston. We incorporate the permit and parcel database, and the parcel data key dataset to help BU ISE determine which buildings are most likely to have heat pumps.

## Data Link
All the original data is downloaded from City of Boston, and we created a separate Google drive folder:
https://drive.google.com/drive/folders/1riQF7L2c6BuY6Wx6-dzQphNFREMNLdti?usp=sharing
Or you can access the data in the link displayed in our report.

## File Description:
### (Cleaning_Parcel_Data)_Zixin_Ding.ipynb
This is how we cleaned up the parcel data(Boston Property Assessment Data), and the process is described in 3.1 of our final report. We used the cleaned-up version for most of data analysis and classification model.

### Produce_Permit_Solar_Zixin_Ding.ipynb and Permit_Solar.csv and Permit_Solar_Stack_Bar_Chart(Fig5.14).R
See Section 5.3 and Fig 5.14. We first filtered out Permit_Solar.csv to get the capacity of solar energy used for household in comments column of Boston Permits Dataset.(produced by filtering in the numbers before kilowatts). Then we convert the dataset to Permit_Solar.csv and produce the Permit_Solar_Stack_Bar_Chart, in ascending order of average housing price per sq ft(in zipcode).

### Map_Visualization.R (Zixin Ding)
Code for 5.1.3 Map Visualization of Property Assessment Data

### Zixin_Ding_Rate_of_Change.ipynb
See Section 5.2 for the rate of change table for type of heating systems.


