# Basic model, adapted from Spiegel et al. 2016 MEE
# Original code by Orr Spiegel
# Adapted by Kaija Gahm


# PARAMETERS --------------------------------------------------------------
N_indv <- 50
Scl = 300000 # length of one edge of the simulation area (arbitrary, in meters). Full simulation area will be 300000^2 m^2. We'll assume it's centered around an origin point with coords [0,0]. Value based *loosely* on area of Israel mask.
N_tmStp <- 10
EtaCRW <- 0.7 #the weight of the CRW component in the BCRW used to model the Indiv movement
Kappa_ind <- 3 #oncentration parameters of von Mises directional distributions used for individuals' movement

# INITIALIZE DATA STORAGE -------------------------------------------------
XYind <- vector("list", length = N_indv) # to store indiv locs at each time step
HRCenters = matrix(data = NA, nrow = N_indv, ncol = 3) # home range centers
Phi_ind <- rep(0, N_indv) # direction of the last step for the individuals


# STARTING LOCATIONS ------------------------------------------------------
for(i in 1:N_indv){
  XYind[[i]] <- matrix(rep(NA, 2 * N_tmStp), ncol = 2) # empty matrices
  XYind[[i]][1, ] <-  c(runif(n = 1, min = -Scl/3, max = Scl/3 ),#X random initial location of each individual is uniformly distributed in the range of the observed ones
                        runif(n = 1, min = -Scl/3, max = Scl/3 ))#Y random initial location of each individual is uniformly distributed in the range of the observed ones
  HRCenters[i,1:2] <- XYind[[i]][1,]
  # KG: I think it's Scl/3 so that indivs start in center and have room to move.
}

# (PLOT HOME RANGE CENTERS) KG --------------------------------------------
as.data.frame(HRCenters) %>% ggplot(aes(x = V1, y = V2))+geom_point()+ylim(-Scl, Scl)+xlim(-Scl, Scl)


# RUN SIMULATION ----------------------------------------------------------
## Loop on time steps ##
for(Curr_tmStp in 1:(N_tmStp-1)){
  ## Loop on individuals ##
  for(Curr_indv in 1:N_indv){
    BiasPoint <- HRCenters[Curr_indv, 1:2] # its home range center
    coo <- BiasPoint  - XYind[[Curr_indv]][Curr_tmStp, ] #cheking direction to the Bias point of this individual, change the second ccomp to have another bias
    mu <- Arg(coo[1] + (0+1i) * coo[2])#
    if(mu < 0){ # make sure direction is not negative
      mu <- mu + 2 * pi  
    }
    mu.av <- Arg(EtaCRW * exp(Phi_ind[Curr_indv] * (0+1i)) + (1 - EtaCRW) * exp(mu * (0+1i))) #bias to initial location + CRW to find the von mises center for next step
  }
}