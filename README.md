
<!-- README.md is generated from README.Rmd. Please edit that file -->

# carfollowingmodels

<!-- badges: start -->
<!-- badges: end -->

The goal of `carfollowingmodels` is to make several car following models
available in R for numerical simulation.

## Installation

`carfollowingmodels`is not on CRAN yet. You can download the development
version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("durraniu/carfollowingmodels")
```

## Example

To use any car-following model, you need to provide the lead vehicle
data, initial position, speed and/or acceleration of following
vehicle(s), and model parameters.

Following shows an example with 5 following vehicles. The lead vehicle
is moving at 13.9 m/s at the reference position of 100 m.

``` r
# Time
last_time <- 3000 ## s
time_frame <- 0.1 ## s
Time <- seq(from = 0, to = last_time, by = time_frame)
time_length <- length(Time)



## Lead vehicle
vn1_first <- 13.9 ## first speed m/s
xn1_first <- 100 ## position of lead vehicle front center m
bn1_complete <- c(rep(0, 29500),
                 rep(-5, time_length - 29500))



#############################################
### Complete speed trajectory of Lead vehicle
#############################################

vn1_complete <- rep(NA_real_, time_length) ### an empty vector
xn1_complete <- rep(NA_real_, time_length) ### an empty vector

vn1_complete[1] <- vn1_first
xn1_complete[1] <- xn1_first

for (t in 2:time_length) {

 ### Lead vehicle calculations
 vn1_complete[t] <- vn1_complete[t-1] + (bn1_complete[t-1] * time_frame)

 vn1_complete[t] <- ifelse(vn1_complete[t] < 0, 0, vn1_complete[t])


 xn1_complete[t] <- xn1_complete[t-1] + (vn1_complete[t-1] * time_frame) +
  (0.5 * bn1_complete[t-1] * (time_frame)^2)

}

## Lead vehicle data in a dataframe
ldf <- data.frame(Time, bn1_complete, xn1_complete, vn1_complete)
```

## Intelligent Driver Model (IDM)

To predict the trajectories of the 5 following vehicles, you can use any
car-following model available in this package. For example, the
`simulate_idm()` function uses the Intelligent Driver Model as shown
below. For more details on input arguments, type `?simulate_idm`in the
console.

``` r
library(carfollowingmodels)

## Run the IDM function:
results_idm <- simulate_idm(

resolution=0.1,
N=5,

dfn1=ldf,
xn1="xn1_complete",
vn1="vn1_complete",

xn_first=list(85, 70, 55, 40, 25),
vn_first=list(12, 12, 12, 12, 12),
ln=list(5, 5, 5, 5, 5),

a=2,
v_0=14.4,
small_delta=1,
s_0=4,
Tg=1,
b=1.5
)


head(results_idm)
#>   fvn Time    xn1  vn1 ln1  sn_star      v_dot       xn       vn       sn
#> 1   1  0.0 100.00 13.9   5 9.418207 -1.4407191 85.00000 12.00000 10.00000
#> 2   1  0.1 101.39 13.9   5 8.860068 -1.1565350 86.19280 11.85593 10.19720
#> 3   1  0.2 102.78 13.9   5 8.420694 -0.9399018 87.37261 11.74027 10.40739
#> 4   1  0.3 104.17 13.9   5 8.069309 -0.7704464 88.54193 11.64628 10.62807
#> 5   1  0.4 105.56 13.9   5 7.785079 -0.6351232 89.70271 11.56924 10.85729
#> 6   1  0.5 106.95 13.9   5 7.553349 -0.5252076 90.85646 11.50573 11.09354
#>      deltav
#> 1 -1.900000
#> 2 -2.044072
#> 3 -2.159725
#> 4 -2.253716
#> 5 -2.330760
#> 6 -2.394273
```

Now you can plot the results:

``` r
library(tidyverse)
#> Warning: package 'tidyverse' was built under R version 4.0.4
#> -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.0.6     v dplyr   1.0.4
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()

## Position
results_at_time_0_LV <- subset(results_idm, fvn==1 & Time ==0)
results_at_time_0_FV <- subset(results_idm, Time ==0)

ggplot() +
  geom_rect(data = results_at_time_0_LV,
            aes(xmin = xn1 - ln1,
                xmax = xn1,
                
                ymin = 0.628,
                ymax = 3.028)) +
  geom_rect(data = results_at_time_0_FV,
            aes(group = fvn,
                fill = as.factor(fvn),
                xmin = xn - 5,
                xmax = xn,
                
                ymin = 0.628,
                ymax = 3.028)) +
  geom_hline(yintercept = 3.6, linetype = "longdash") +
  coord_fixed(ratio=1) +
  theme(legend.title = element_blank())
```

<img src="man/figures/README-plots-1.png" width="100%" />

``` r

## Speed
ggplot(data = results_idm) +
  geom_line(aes(x = Time, y = vn, color = as.factor(fvn), group=fvn)) +
  geom_line(data = subset(results_idm, fvn==1),
            aes(x = Time, y = vn1, color = "LV Speed")) +
  theme(legend.title = element_blank())
```

<img src="man/figures/README-plots-2.png" width="100%" />

## Gipps Model

``` r
results_gipps <- simulate_gipps(


resolution=0.1,  
N=5,

dfn1=ldf, 
xn1="xn1_complete", 
vn1="vn1_complete", 



xn_first=list(85, 70, 55, 40, 25),
vn_first=list(12, 12, 12, 12, 12), 
ln=list(6.5, 6.5, 6.5, 6.5, 6.5),



an=2, 
Vn=14.4, 
tau=0.1, 
bn=-1.5, 
bcap=-2 
)


head(results_gipps)
#>   fvn Time    xn1  vn1 ln1 bcap   bn      bn_v       xn       vn       sn
#> 1   1  0.0 100.00 13.9 6.5   -2 -1.5 0.7815591 85.00000 12.00000 8.500000
#> 2   1  0.1 101.39 13.9 6.5   -2 -1.5 0.7585004 86.20391 12.07816 8.686092
#> 3   1  0.2 102.78 13.9 6.5   -2 -1.5 0.7359608 87.41552 12.15401 8.864484
#> 4   1  0.3 104.17 13.9 6.5   -2 -1.5 0.7139400 88.63460 12.22760 9.035404
#> 5   1  0.4 105.56 13.9 6.5   -2 -1.5 0.6924367 89.86093 12.29900 9.199074
#> 6   1  0.5 106.95 13.9 6.5   -2 -1.5 0.6714485 91.09429 12.36824 9.355712
#>      deltav
#> 1 -1.900000
#> 2 -1.821844
#> 3 -1.745994
#> 4 -1.672398
#> 5 -1.601004
#> 6 -1.531760


## Speed
ggplot(data = results_gipps) +
  geom_line(aes(x = Time, y = vn, color = as.factor(fvn), group=fvn)) +
  geom_line(data = subset(results_gipps, fvn==1),
            aes(x = Time, y = vn1, color = "LV Speed")) +
  theme(legend.title = element_blank())
#> Warning: Removed 2180 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

### Compare Models

Following shows the speed of first following vehicle as predicted by
different models:

``` r
results_gipps_fv1 <- results_gipps %>% 
  filter(fvn == 1)

results_idm_fv1 <- results_idm %>% 
  filter(fvn == 1)

ggplot() +
  geom_line(data = results_gipps_fv1 ,
            aes(x = Time, y = vn, color = "Gipps Speed")) +
  geom_line(data = results_idm_fv1 ,
            aes(x = Time, y = vn, color = "IDM Speed")) +
  geom_line(data = results_idm_fv1 ,
            aes(x = Time, y = vn1, color = "LV Speed"), linetype = "longdash")
#> Warning: Removed 438 row(s) containing missing values (geom_path).
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />