# Code
An R package called RCmodels was made from this project. The package includes the following functions written to run the models:
   
* Both models   
    + clean() - Cleans the file input data. When file is txt it cleans the data in accordance to the from the Icelandic Met office. If you have other types of data the clean function also cleans a custom xlsx file with columns Date, Time,Quality,W,Q in this order.
    + priors() - Defines the prior parameters for a given country. If you want to add your country to use the model please contact us. Contact information: sor16@hi.is and aoj8@hi.is
* Model 1   
    + plotmodel1() - makes use of clean() and model1BH() to plot a rating curve calculated with model 1
        + model1BH() - Makes use of Densevalm11() to determine the fit and confidence intervals of a rating curve
            + Densevalm11() - Evaluates the log density p for given theta
    
* Model 2   
    + plotmodel2() - makes use of clean() and model2BH() to plot a rating curve calculated with model 2
    + model2BH() - Makes use of Densevalm22() to determine the fit and confidence intervals of a rating curve. It uses predict_u() to calculate predictive values and predictive confidence intervals for the unobserved stages of a rating curve. 
    + Densevalm22() - Evaluates the log density p for given theta
    + Adist() - Extracts unique elements of water level measurements and creates a distance matrix from them
    + B_splines - Test the B-splines in a rating curve
    + W_unobserved() - Returns the stages needed to make an equally spaced grid  
    + predict_u() - Calculates predictive values for unobserved stages

All of the functions written to run the models are available for everyone [here](https://github.com/sor16/RCmodels).      
To download the package RCmodels, type the following in your R console:           


```r
    #if not yet installed
    install.packages('devtools')

    devtools::install_github('sor16/RCmodels'). 
```
          
To learn about input arguments into the functions, type ? in front of the function name.
