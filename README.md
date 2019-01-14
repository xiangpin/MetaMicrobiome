#MetaMicrobiome: an R package for the meta-analysis and visualization of microbiome.
MetaMicrobiome was designed to performance the meta-analysis, visualization and the module building. MetaMicrobiome provides function for computing the count data for the measures of risk and a chi-squared test. the package also provides a function for creating forest plots for the results of the measures of risk. Moreover, the package also provides functions for the module building and testing based on the randomforest, and a function for the visualization for the result with the ROC curves.
##Getting Started
These instructions will get you a copy of the package and running on your local machine for using, development and testing purpose.
###Dependencies
MetaMicrobiome requires the packages of [metafor](http://www.metafor-project.org/doku.php/installation), [eipR](https://cran.r-project.org/web/packages/epiR/index.html), [pROC](https://cran.r-project.org/web/packages/pROC/index.html), [caret](https://cran.r-project.org/web/packages/caret/index.html), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html) and [tidyr](https://cran.r-project.org/web/packages/tidyr/index.html)
###Installing and Testing
####Install the developement version from GitHub:
```
require("devtools")
devtools::install_github('xiangpin/MetaMicrobiome')
```
Or 
```
git clone https://github.com/xiangpin/MetaMicrobiome.git
R CMD build MetaMicrobiome
R CMD INSTALL MetaMicrobiome_version.tar.gz
```
####Try it out
```
require("MetaMicrobiome")
example(ggforest)
svg("multi_measure_OR.svg")
p2
dev.off()
```
![multiforest](./test/multi_measure_OR.png)
Forest plot of the alpha diversity metrics (Or other your interesting variables, such as phylum or genera), The length of the error bar represents the 95% confidence interval. The left of dashed lines depicts that the metirc of the case is higher than the control. if there is not overlap between the dashed lines and the error bar, which shows that there are significantly difference between the case and the normal.
##Documentation


##Authors
Shunagbin Xu  
Email: xshuangbin@163.com  
Github: https://github.com/xiangpin/
##Citation

##Contributing

