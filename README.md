# Project: Open Data NYC - an RShiny app development project
### [Project Description](doc/project2_desc.md)

Term: Fall 2016

+ Team 6
+ Projec title: NYC Crime Analysis
+ Team members
	+ Weichuan Wu
	+ Jiwen You
	+ Youzhu Liu
	+ Yueqi Zhang
	+ Minghao Dai
	
+ Project summary: Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

In this second project of Applied Data Science, we will carry out *Exploratory Data Analysis and Visualization* via a shiny app on a topic about open data released from the [New York City open data portal](https://nycopendata.socrata.com/). See [Project 2 Description](doc/project2_desc.md) for more details.  

```diff
+ **After your finish your shiny app, please replace the screenshot below with one from your own app.**
```

![screenshot](doc/screenshot2.png)

The **learning goals** for this project is 
- business intelligence for data science
- systems development/design life cycle
- shiny app/shiny server
	
**Contribution statement**: 
+ Jiwen You :
	+ Transformed the longitude and latitude data in the crime data set to zipcode
	+ Extracted raw data and create RData file for project use
	+ Completed the statistical analysis and R Shiny demonstration for the public facility allocation section
	+ Edited, consolidated and cleaned statements and scripts

+ Minghao Dai: 
         
	+ Extracted daily crime data set by crime type, saved as preddata.csv
	+ Ploted crime count time series plot by date and crime type
	+ Ploted all type crime number heat map by years and months
	+ Made prediction for each crime type time serie by fitting a TBATS model

+ Weichuan Wu: 
         
	+ Processed the raw crime data for crime map and built the first version of ui.R and server.R by plot the crime map
	+ Collected the statistics data by zip code and use it to draw the crime against income plot
	+ Analyized the crime data and draw the 30 days accumulated crime plot
	+ Analyized the crime interval distribution and make the plot
	+ Ploted the scatterD3 map in Prediction section to analysize whether can use crime to predict murder or not
	+ Helped teammates to learn the Shiny and help them to debug.
	

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.

