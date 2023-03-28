# Basic model, adapted from Spiegel et al. 2016 MEE
# Original code by Orr Spiegel
# Adapted by Kaija Gahm
library(CircStats) # for `rvm`, selecting from von Mises distribution

# PARAMETERS --------------------------------------------------------------
N_indv <- 10
Scl = 120000 # length of one edge of the simulation area (arbitrary, in meters). Full simulation area will be 30000^2 m^2. We'll assume it's centered around an origin point with coords [0,0]. Value based *loosely* on area of Israel mask.
N_tmStp <- 250
EtaCRW <- 0.7 #the weight of the CRW component in the BCRW used to model the Indiv movement
Kappa <- 5 # concentration parameter of von Mises directional distributions used for individuals' movement
Social_range <- 2000 #detection range (in meters) - individuals within this range will be considered in the bias point in the relevant scenario of sociable agents. Set to 0 if you want socially indifferent agents.
startScl = 8
gammaShape = 0.308
gammaRate = 0.0035


runSim <- function(N_indv = 5, Scl = 120000, N_tmStp = 250, EtaCRW = 0.7, Kappa = 3, Social_range = 2000, startScl = startScl, gammaShape = gammaShape, gammaRate = gammaRate){
  # INITIALIZE DATA STORAGE -------------------------------------------------
  XYind <- vector("list", length = N_indv) # to store indiv locs at each time step
  HRCenters = matrix(data = NA, nrow = N_indv, ncol = 3) # home range centers
  Phi_ind <- rep(0, N_indv) # direction of the last step for the individuals. For the first run of the model, will be 0; later gets updated.
  
  
  # STARTING LOCATIONS ------------------------------------------------------
  for(i in 1:N_indv){
    XYind[[i]] <- matrix(rep(NA, 2 * N_tmStp), ncol = 2) # empty matrices
    XYind[[i]][1, ] <-  c(runif(n = 1, min = -Scl/startScl, max = Scl/startScl ),#X random initial location of each individual is uniformly distributed in the range of the observed ones
                          runif(n = 1, min = -Scl/startScl, max = Scl/startScl ))#Y random initial location of each individual is uniformly distributed in the range of the observed ones
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
      
      ## Calculate distance of this indiv to all other individuals
      Dist <- rep(NA, N_indv)
      for(ii in 1:N_indv){
        Dist[ii] <- dist(rbind(XYind[[ii]][Curr_tmStp, ], 
                               c(XYind[[Curr_indv]][Curr_tmStp, ])))
        Dist[Dist == 0] <- NA # get rid of distance to self
      }
      
      ## start generation of indiv location at time k+1
      
      # SET BIAS POINT
      BiasPoint <- HRCenters[Curr_indv, 1:2] # its home range center
      
      ## If social: update bias point with social info
      if(min(Dist, na.rm = T) < Social_range){ # if another indiv is in range...
        BiasPoint <- XYind[[which.min(Dist)]][Curr_tmStp, ] # set bias point to loc of nearest indiv
      }
      
      # GET DIRECTION OF MOVEMENT
      coo <- BiasPoint  - XYind[[Curr_indv]][Curr_tmStp, ] # Checking direction to the Bias point of this individual, change the second ccomp to have another bias
      mu <- Arg(coo[1] + (0+1i) * coo[2])#
      if(mu < 0){ # make sure direction is not negative
        mu <- mu + 2 * pi  
      }
      mu.av <- Arg(EtaCRW * exp(Phi_ind[Curr_indv] * (0+1i)) + (1 - EtaCRW) * exp(mu * (0+1i))) # Bias to initial location + CRW to find the von Mises center for the next step
      Phi_ind[Curr_indv] <- rvm(n = 1, mean = mu.av, k = Kappa) # Choose current step direction from von Mises distribution centered around the direction `mu.av` selected above.
      
      
      # TAKE NEXT STEP
      #selection of step size for this indiv in this state from the specific gamma  # XXX need to fit gamma distribution to the step lengths for real data        
      step.len <- rgamma(1, shape = gammaShape, rate = gammaRate) # select step length
      step <- step.len * c(Re(exp((0+1i) * Phi_ind[Curr_indv])), Im(exp((0+1i) * Phi_ind[Curr_indv]))) # calculate amount by which to change x and y components
      next.loc <- XYind[[Curr_indv]][Curr_tmStp, ] + step # calculate coords of next location
      XYind[[Curr_indv]][Curr_tmStp + 1, ] <- next.loc # save next location
    } # close loop on individuals
    
    cat(paste("Done with timestep", Curr_tmStp, "of", N_tmStp, "\n"))
  } # close loop on time steps
  
  outDF <- purrr::imap_dfr(XYind, ~.x %>% as.data.frame() %>% mutate(ind = .y, timestamp = 1:nrow(.x),
                                                                     K = Kappa, E = EtaCRW))
  return(outDF)
}

kappas <- c(1, 3, 5)
EtaCRWs <- c(0.5, 0.7, 0.9, 1)
params <- expand.grid(K = kappas, E = EtaCRWs)

out <- pmap_dfr(params, ~runSim(N_indv = 10, Scl = Scl, N_tmStp = 500, EtaCRW = .y, Kappa = .x, gammaShape = gammaShape, gammaRate = gammaRate, Social_range = 0, startScl = startScl))

out %>%
  ggplot(aes(x = V1, y = V2, col = factor(ind)))+
  #geom_point(alpha = 0.5)+
  geom_path(linewidth = 0.2, aes(group = factor(ind)))+
  facet_grid(rows = vars(K), cols = vars(E))+
  ylab("Kappa")+ # kappa is roughly how direct
  xlab("EtaCRW")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(axis.text = element_blank())



nonSoc <- runSim(N_indv = N_indv, Scl = Scl, N_tmStp = 500, EtaCRW = 0.7, Kappa = 3, gammaShape = gammaShape, gammaRate = gammaRate, Social_range = 0, startScl = startScl)

nonSoc %>%
  ggplot(aes(x = V1, y = V2, col = factor(ind)))+
  geom_path(linewidth = 0.5, aes(group = factor(ind)))+
  theme_minimal()+
  theme(legend.position = "none")+
  coord_equal()+
  ggtitle("Non-sociable Agents")


soc <- runSim(N_indv = N_indv, Scl = Scl, N_tmStp = 500, EtaCRW = 0.7, Kappa = 3, gammaShape = gammaShape, gammaRate = gammaRate, Social_range = 2500, startScl = startScl)

soc %>%
  ggplot(aes(x = V1, y = V2, col = factor(ind)))+
  geom_path(linewidth = 0.5, aes(group = factor(ind)))+
  theme_minimal()+
  theme(legend.position = "none")+
  coord_equal()+
  ggtitle("Sociable Agents")













# # sample animation code
# a <- df %>%
#   ggplot(aes(x = V1, y = V2, col = factor(ind)))+
#   geom_point(alpha = 0.5)+
#   geom_path(linewidth = 0.5, aes(group = factor(ind)))+
#   transition_reveal(timestamp)+
#   NULL
# animate(a, fps = 10)



