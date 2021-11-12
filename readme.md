# tecan_analysis

This is a bare-bones repository for calculting maximum growth rates from 
tecan-like growth data. Basically all it does is fit the "baranyi" growth
curve to OD data. It also gives a few lines of code for plotting the fit
over the original data. 

It assumes you have saved a .csv of the tecan data that looks like "25oct16_OD.csv"
which is used as the example in fit_growth_curves_baranyi.r.  

It doesn't do any stats, or apply treatment names to the wells, figuring it 
is most simple to leave that to the user. 

We (members of the Harcombe lab) have used variations of this code in many of our analyses. Hope its useful for you! 

Best, Jeremy Chacon