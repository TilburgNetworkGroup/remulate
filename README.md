# remulate

**remulate** is an R package designed to simulate dynamic temporal networks based on Relational Event Models (REM).

## Key Features

**remulate** provides a range of features to facilitate the simulation of relational event data:

- **Support for Dyadic and Actor-Oriented Models**: Simulate both dyadic relational event models and actor-oriented relational event models.
- **Rich Collection of Endogenous Statistics**: Pre-computed endogenous statistics with multiple normalization and standardization options, along with support for interaction terms.
- **Flexible Memory Decay Functions**: Includes exponential decay, step-wise decay, and other functions to model the impact of past events on future interactions.
- **Custom Risk Sets**: Enables simulations with custom risksets where certain actor pairs cannot interact.
- **Time-Varying Network Parameters**: Allows for variations in network effects over time.
- **Time-Varying Nodal Covariates**: Allows for variations in node attributes over time.
- **Support for Advanced REM Variants**:
  - Dyadic latent class relational event models (DLC-REM)
  - Frailty relational event models
  - Relational event block models

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
@article{lakdawalaSimulatingRelationalEvent2025,
  title = {Simulating Relational Event History Data: Why and How},
  shorttitle = {Simulating Relational Event History Data},
  author = {Lakdawala, Rumana and Mulder, Joris and Leenders, Roger},
  year = {2025},
  month = aug,
  journal = {Journal of Computational Social Science},
  volume = {8},
  number = {4},
  pages = {92},
  issn = {2432-2725},
  doi = {10.1007/s42001-025-00427-2},
  urldate = {2025-08-28},
  langid = {english},
  keywords = {Actor-oriented models,Dyadic interaction models,Interventions,Model fit assessment,Relational events,Simulation techniques,Temporal social networks}
}
```

## Contributing
Pull requests and bug reports are welcome. For major changes, please open an issue first to discuss what you would like to change.

### Acknowledgement
The development of this package was supported by a Vidi Grant (452-17-006) awarded by the Netherlands Organization for Scientific Research (NWO) Grant and an ERC Starting Grant  (758791).
