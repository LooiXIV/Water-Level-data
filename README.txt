This repository contains all water level data collected, water level analysis, and
the water level quantile/water budget analysis conducted on all the TIBS wetlands.
Below describes the contants of each folder with a brief description of each file.
####################################################################################

1) R_Scripts: Contains all R scripts pertaining to analysis, calculations, or data
	acquisition.
	a) DL NOAA WL data.R: this downloads NOAA water level data from the NOAA
	tides and currents data repository (http://tidesandcurrents.noaa.gov/).
	b) Match NOAA and Site WL.R: This script reads in the downloaded NOAA water
	level data and matches the daily water level from the NOAA water level station
	to the water level at the TIBS wetland site. If the data the TIBS wetland
	site are higher than daily resolution, these data are aggregated to daily
	means. The data are plotted against each other, regressed, and analyzed. 
	Additionally, the end of the script produces necessary parameters used in the
	Wetland Model.
	c) WL Quantiles v3.R: This code organizes and creates figures to show WL
	and water budget relationships, comparing the Moses Saunders Dam water 
	contributions to the wetland watershed contributions.

2) Download NOAA WL Data.R: Within this folder there are two additional folders.
	One is labeled "Cape Vincent" and the other "Oswego". Both of these folders
	essentially contain the same type of data, daily water level data for 
	every year available at either site. The Oswego folder has additional data
	files within it, in addition to the daily water level means. These include:
	a) Oswego Water Level 1906-2014.csv: All concatenated daily mean water level
	data of Oswego from 1906 to 2014.
	b) Oswego Water Level 1933-1948.csv: All concatenated daily mean water level
	data of Oswego from 1933 to 1948.
	c) Oswego WLs_1970-2014.csv: All concatenated daily mean water level
	data of Oswego from 1970 to 2014. 
	d) Oswego WLs_2000-2014.csv: All concatenated daily mean water level 
	data of Oswego from 2000 to 2014.

3) Quantile Analysis: This folder contains all the plots created by the WL Quantiles v3.R
	Script. Additionally, there are folders for each TIBS wetland site that contain
	all the Wetland Model outputs for the four modeled scenarios (Post-MSD low prcp,
	Post-MSD high prcp, Pre-MSD low prcp, Pre-MSD high prcp).

4) Site WL Data: this folder contains all the hourly water level data for each wetland
	site that TIB monitors, and that was part of this thesis. 