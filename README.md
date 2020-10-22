# 2018-Duke-Data-Fest

### Team BunchLadies: Chunxiao Li, Xuetong Li, Yunxuan Li, Junwen Huang

### Award: Best Visualization Award

This repository for code, data, and figures were made during Duke ASA Datafest 2018. The Duke ASA Datafest 2018 was presented by Indeed.com. Indeed.com asked “What advice would you give a new high school about what major to choose in college? How does Indeed’s data compare to official government data on the labor market? Can it be used to provide good economic indicators?”. 

Our team conducted an end-to-end data analysis including data cleaning, feature engineering, temporal and spatial modelling. and visualizing the job searching and job posting statistics in the United States. The final results were presented as a Shiny app, which provides annual statistics map, dynamic trends, a Gaussian Process model visualzation, and a Spatial Autoregressive model visualization.

### 1 Project Summary
1. Conducted data cleaning,feature selection and creation, and data exploratory analysis (EDA) on a large dataset.
2. Built Gaussian Processmodels and Spatial Autoregressive models respectively for the temporal and spatial data. 
3. Used ggplot2 to visual geographical data, and presented the data visualization results as a Shiny App in R.


### 2 File Documentation
The [DataFest 2018 presented by BrunchLadies.pdf](https://github.com/lichunxiao9501/2018-Duke-Data-Fest/blob/master/DataFest%202018%20presented%20by%20BrunchLadies.pdf) file is the final presentation slide, which provides a summary of the shiny app, with links to YouTube video demonstrations.

The [gauss_process.R](https://github.com/lichunxiao9501/2018-Duke-Data-Fest/blob/master/codes/gauss_process.R) and [Spatial_Model.R](https://github.com/lichunxiao9501/2018-Duke-Data-Fest/blob/master/codes/Spatial_Model.R) files are the scrips for building the temporal and spatial models respectively.

The [ShinyApp.r](https://github.com/lichunxiao9501/2018-Duke-Data-Fest/blob/master/codes/ShinyApp.r) file is the main script for the shiny app.


### 3 Data Visualization examples and Shiny App Screenshots

Pic 1: Total clicks of job posts by __states__ over time
![total clicks gif](https://github.com/lichunxiao9501/2018-Duke-Data-Fest/blob/master/pics/total_clicks.gif)

Pic 2: Total clicks of job posts by __industry__ over time
![total clicks by ind gif](https://github.com/lichunxiao9501/2018-Duke-Data-Fest/blob/master/pics/animation_1.gif)

Pic 3: Shiny App Screenshot
![shiny screenshot](https://github.com/lichunxiao9501/2018-Duke-Data-Fest/blob/master/pics/shiny_app_screenshot1.png)
