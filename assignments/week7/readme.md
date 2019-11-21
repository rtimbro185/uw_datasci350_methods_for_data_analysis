**Author:** Ryan Timbrook <br>
**Project:** <br>
**Date:** <br>

**Description:**   
Assignment Objective:
    Do the following to create, execute and compare linear regression models for the price from the automotive data set. Use all features for each of your models. Make sure you remove the symbolizing and normalized losses columns first.

    1.) Stepwise Regression. 
    2.) SVD Regression. 
    3.) Elastic Net Regression. 

    To evaluate and compare these models use both summary statistics and plots of the residuals. 
    What to answer with this data:
        How similar or different is the performance? 
        Do you consider any of these models to be a good fit to the data?

Observations:
    The results are similar for each of the models. For this data set these methods appear to be a good fit. This is shown below in the Stepwise Regression summary data table, the SVD Regression summary data table and the Elastic Net Regression summary data table.
    
    The stepwise regression model produced a high Adjusted R-squared value of 0.9829 along with a significantly low p-value of < 2.2e-16; Step: AIC=-1015.98
    The Elastic Net Regression summary data:
        SSE = 49.6263868117087 
        SSR = 0.629650293920823 
        SST = 50.2560371056296 
        RMSE = 0.257131537884501 
        Adjusted R^2 = 0.987207385886177

**Approach:**
* see R jupyter notebook DataScience_HW_7-RyanTimbrook



