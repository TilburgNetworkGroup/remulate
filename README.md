# remulate

remulate is a R library to simulate dynamic temporal networks based on Relational Event Models. Remulate can be used to simulate network data with time-varying effects, time-varying exogenous predictors under actor and tie-oriented relational event models. The package can also be used to simualte mixed-effects models, stochastic block models, and latent class relational event models.



### Programming Languages
The package contains code written in:
* R (>= 4.0.0)
* Rcpp (>= 1.0.4.6) and RcppArmadillo (>= 0.9.860.2.0)
* C++11 (Compiler Version: GCC-8.1.0)
	
## Installation

To install the package in R using `devtools`:

```R
library(devtools)
install_github("TilburgNetworkGroup/remulate")

#load the package
library(remulate)
```

## Usage
```R
effects <- ~ baseline(-4) + inertia(0.01) + reciprocity(-0.04) + itp(0.01,scaling="std")

remulateTie(effects, actors = 1:25, time = 20, events = 500, initial = 200)

```
## Support
```R
#To view all help files in the remulate package
help(package='remulate')

#To view available effects for remulateTie
help('remulateTieEffects')

#To view available effects for remulateActor
help('remulateActorEffects')

```

## Citing this Work

If you use this R package in your research or in any publications, please cite it as follows to help support our work:

```bibtex
@misc{lakdawala2024simulating,
    title={Simulating Relational Event Histories - Why and How}, 
    author={Rumana Lakdawala and Joris Mulder and Roger Leenders},
    year={2024},
    eprint={2403.19329},
    archivePrefix={arXiv},
    primaryClass={cs.SI}
}
```

## Contributing
Pull requests and bug reports are welcome. For major changes, please open an issue first to discuss what you would like to change.

### Acknowledgement
The development of this package was supported by a Vidi Grant (452-17-006) awarded by the Netherlands Organization for Scientific Research (NWO) Grant and an ERC Starting Grant  (758791).
