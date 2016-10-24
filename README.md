# TPDS
Solve households' choice to (1) participate in the experiment, (2) exchange coupons, (3) purchase commodities to fit the observed choices in Rajasthan choice experiment data.



---------
globvar.f90
---------

Declare all parameter values and data matrices that will be used throughout the estimation and the simulation called by the main file.


---------
Main.f90
---------

Call "get_ntheta" to get the number of parameters in the model.

Read in initial parameter guess "initialguess.txt".

Call "theta_to_param" to map the numbers in "initiaguess.txt" to the model parameters.

"get_ntheta" and "theta_to_param" are subroutines declared in "mappings.f90".

Call "ReadData" to read in the data.

Call "Likelihood" for one-time computation of likelihood value or call "dfpmin" for the estimation of "Likeihood" on the desktop. "dfpmin" is a subroutine declared in "minimization.f90". 

Call "Simulation" to run counterfactual policy simulation given the parameter values.

---------
Likelihood.f90
---------

Loop over households and unobserved marginal utility type space. 

For treatment households, (1) call "SolveParticipationProblem" to compute the participation choice probability. (2) Call "LikelihoodVoucher" to solve utility maximization when choice between cash and in-kind transfers is available and compute the likelihood of observed choices of the experiment participants. (3) Also call "LikelihoodNoVoucher" to solve utility maximization under the status quo (no cash option) and compute the likelihood of the observed choices among the non participants.

For control households, just call "LikelihoodNoVoucher". 


---------
Simulation.f90
---------

Run three different policy simulations.

(1) Compute cash equivalent value of in-kind transfers for 1 month of switch to cash only and for permanent switch to cash only

(2) Compute welfare gain from having 5 e-Mitra kiosks every villages + switching to cash only permanently

(3) Compute welfare gain from correcting all scales used by FPS dealers and enforcing longer than 75 hours of FPS open hours.
