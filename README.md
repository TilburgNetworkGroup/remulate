# remulate (Version: 1.0.0)

### About the package
A package to generate Relational Event History Data

### Programming Languages
The package contains code written in:
* R (>= 4.0.0)
* Rcpp (>= 1.0.4.6) and RcppArmadillo (>= 0.9.860.2.0)
* C++11 (Compiler Version: GCC-8.1.0)
	
### Installing the package
To install the package in R using `devtools`:

```
library(devtools)
install_github("TilburgNetworkGroup/remulate")

# load the package
library(remulate)
```

### Example

```r
library(remulate)

<<<<<<< HEAD
=======

>>>>>>> 956fb3f5643884b95463d4253fa19105dedcaf55
#Number of possible actors in the network
N <- 35
#vector of actor names or ids
actors = c(1:N)

# Create a data frame with exogenous information, first column must contain actor names, second column the time at which the exogenous information changes (if it doesn't change keep it 0), subsequent columns contain the covariates for the actors
cov <- data.frame(id = actors,time=rep(0,N),stringsAsFactors=F)
cov$sex <- sample(c(0,1), N, replace = T, prob = c(0.7, 0.3))
cov$age <- sample(c(18:55),N,replace=T)

#To generate a network with N actors and M events and 1000 random or burn in events 
M <- 1000
burn_in <- 500 

#define the formula input for generating the data using a tie or dyad oriened model
form <- ~  baseline(-5)+isp(0.18)+osp(0.049)+itp(0.08)+otp(-0.226)+inertia(-0.86)+psABAY(1.9)+psABXB(1.16,)+psABXA(1.69)+send(0.2,"age",cov)+same(0.1,"sex",cov)

dat <- remulateDyad(form,actors,M,burn_in)

#define the formulae inputs for generating the data using an actor oriented model

s_form <- ~baseline(-5)+indegreeSender(0.1,scaling="std")+outdegreeSender(-0.05,scaling="std")+send(0.2,"age",cov)
d_form <- ~baseline(-2)+inertia(0.2)+itp(0.08)+otp(-0.226)+same(0.1,"sex",cov)

dat <- remulateActor(s_form,d_form,actors,M)


```
