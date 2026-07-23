# temperature_competition

## Set up repository

In order to perform simulations, first we must run the following two scripts:

1. Alphas - set alpha values.R

This script defines which range of parameter space we will be running simulations over. Saves the object 'combos_dat.Rdata'.

2. Source - replicates to simulate.R

This script uses the previously defined combos.Rdata to set how many simulations we will fun. Saves the object 'replicate_dat.Rdata'

3. FIXME Temperatures across the us?? - PRISM pull

Now the script' Source - set basic controls.R is ready to use, and simulations are able to be initiated.

## Defining temperature-dependence

The scripts in Scripts LV/ that deal with temperature dependence are the 'Intrinsic growth' folder, where we define Thermal performance curves, and the 'Alphas' folder, where we define the three functional forms that competition coefficients can take across temperature (constant, gradual, abrupt).

### Intrinsic growth

Along with evaluating competiiton between a temperature specialist and a generalist, we also tested the situation where two temperature specialists compete, such that their optimal temperature are offset.


## Simulations

To run the simulations for all competition functional forms, we have separate scripts, in the 'Source' folder, and their associated .sh script to run them on a cluster.

Within each of these scripts, we source the parameter scripts associated with the scenario, the scrips 'Source - functions.R', 'Source - set basic controls.R' and 'Source - pull params together.R'.

These include analyses found in the main text (specialist vs generalist), and the two-specialist offset case, from the supplement. The latter scripts are indicated by '...offset TI' in the script name.

We save the resulting dataframes of a simulation to the folder...

## Final dataframes

The output of the simulations can be found in the 'Final dataframes' folder.


## Figures (using to keep track of which I've ran in this new repo so far)

Before running the figures, we must make a few dataframes by gathering the simulation scenario outputs together (i.e. those found in the 'Final dataframes' folder). This is done within the Figures folder, using scripts: 'Source - params for figures.R', and 'Source - proportions for figures.R'. These scripts produce the objects 'parameter_dat.Rdata' and 'proportion_dat.Rdata' respectively, which are saved to the 'Rdata' folder.

All figures are saved to the main folder, 'Figures-outputs'. 

- Supp Figure - environmental conditions & map.R (ran)

