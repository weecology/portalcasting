# How to run test out parallel processing in R

## Setup Portalcasting

In order to get the library up and running from source, 

1. Install R and RStudio. (Can follow [A Installing R and RStudio | Hands-On Programming with R](https://rstudio-education.github.io/hopr/starting.html) for reference on installation.) 

2. Clone the source code present at [GitHub - PrayasJ/portalcasting at multi-process](https://github.com/prayasj/portalcasting/tree/multi-process)

3. Open the cloned repository in RStudio. (We are doing this for a single click installation.)

4. After opening up the repository in RStudio, go to Build > Install and Restart.

5. Done. Portalcasting has been setup on the system.



## Install Microbenchmark package for comparison

In RStudio, running `install.packages("microbenchmark")` installs microbenchmark on your R instance.



## Testing out Portalcasting with parallel

Create a new `.R` file or simply run the following lines of code in RStudio to get a statistical comparison between the casting functions.



```r
library(microbenchmark)
library(portalcasting)
main <- "/path/to/data/dir"

microbenchmark(
    non_parallel = {
        setup_production(main = main)
        portalcast(main = main, models = c("ESSS", "AutoArima", "NaiveArima"), end_moons = 520:525)
    }, 
    windows = {
        setup_production(main = main)
        portalcast(main = main, models = c("ESSS", "AutoArima", "NaiveArima"), end_moons = 520:525, multiprocess = 'windows')
    }, 
    unix = {
        setup_production(main = main)
        portalcast(main = main, models = c("ESSS", "AutoArima", "NaiveArima"), end_moons = 520:525, multiprocess = 'unix')
    }, 
    times = 5
)
```


