## this code generates data for testing the method of social network randomizations ###
## the code generates a population of agents with given movement rules ###
## currently it is set on the paired agents scenario, see details below for switching between different scenarios or exploring the parameter spcae fo other values
## writen by Orr Spiegel Jan 2016. contact me at orr.spiegel@mail.huji.ac.il if you need further help ###

#Required packages #######
rm(list=ls())
require(CircStats); require(spatstat); require(sp); require(R.matlab);


#### model parameters and protocol selection for users to choose #######
ToPlot=1#can be 0 (no plot) or 1 (create plots)- note this will slow the proccess a lot!!
Color_indv=c(3,5,6,7,8,2,3,5,6,7,8,2)#if ToPlot=01 this determines how many individfuals to plot (currently 12). The colors for plotted individuas: 2-=red 3=green 5= light green 6 purpule 7 yelow 8 grey

DayLength=50;#how many time step for each day (currently it is limited to 59 since i considered it as minutes within an hour)
DaysToSimulate=100 ;#how many days to simulate
N_indv=60#number of individuals in the population 
Social_Pecrt_rng=2000#detection range (in meters) - indivduals within this range will be considered in the bias point in the relevan scenario of sociable agents. set to 0 if you want socially indiferent agents (or in the fixed pairs scenario) 

DriftHRCenters=0#1 or 0 for a drift of the center in space along the simulation (i.e. the changing environment scenarios), 0 is no drift.
DriftStrength=c(1,0)#if DriftHRCenters==1 this defines drift in m per day for X and Y values in the simulation  
DriftasOnce=2;#if DriftHRCenters==1 this defines when does the drift occur. can be 0,1,2. if 0 it will drift daily; if 1 it will drift in the midle of the run if 2 it will drift from begining. 
PropDriftIndiv=1;#proportion of drifting individuals.
PairedAgents=1; #if 1 then simulates the paired agent scenario where agents are paired in their intial location and show attraction to their pair only, indiferent to the others. 
PairStartDist=200;#if PairedAgents=1 this sets the Upper limit of the distance of the pairs. For the second individual in each pair the location will be random within this range from the first mate. 
set.seed(1234) #comment it for new randomizations start points


#### agents movement rules: #### 
EtaCRW=0.7 #the weight of the CRW component in the BCRW used to model the Indiv movement
StpSize_ind=7 #Mean step lengths of individuals;
StpStd_ind=5 # Sandard deviations of step lengths of individuals
Kappa_ind=3 #oncentration parameters of von Mises directional distributions used for individuals' movement
# plotting step size distribution:
if (ToPlot==1) {plot(density(rgamma(10000, shape = StpSize_ind^2/StpStd_ind^2,scale = StpStd_ind^2/StpSize_ind)))  
    rose.diag(rvm(n=10000, mean = 0, k = Kappa_ind),bins=72, pch = 16, cex = 1, shrink = 1)  }


#### prepraration of variables for storing data ####
if (PairedAgents==1){Social_Pecrt_rng=0}#if i work in a paired design than no social perception range for other agents

N_tmStp=DaysToSimulate*DayLength #  total namber of time steps that witll be simulated for each iteration
N_Rows=N_indv*N_tmStp
XYind_log2=as.data.frame(t(rbind(
  rep(seq(1:N_indv),length.out=N_Rows,each=N_tmStp),#indiviuals within iteration
  rep(seq(1:N_tmStp),length.out=N_Rows),#cumul time steps for each indiv
  rep(seq(1:DaysToSimulate),length.out=N_Rows,each=DayLength),#day in the simulation
  rep(seq(1:DayLength),length.out=N_Rows,each=1),#step within a day
  rep(seq(1:(DaysToSimulate*N_indv)),length.out=N_Rows,each=DayLength),#day in the simulation#burst (i.e., each day as a unique value, not same for two indiv)
  matrix(data=NA,nrow=3,ncol=N_Rows))));#state, x, y pseudoSex

names(XYind_log2)=c("indiv","step","Day","StepInDay","burst","x","y","pseudoSex")
XYind_log2$indiv=as.factor(XYind_log2$indiv);
XYind_log2$burst=as.factor(XYind_log2$burst);
startIndx=1;
Phi_ind <- rep(0, N_indv) #dierction of the last step for the individuals
XYind <- vector("list")#this list will store the matrixes of locations of all individual
HRCnt=matrix(data=NA,nrow=N_indv,ncol=3)
HRCnt[,3]=rep(c(1,2),N_indv/2) #since sex is listed in this parameter i update it to be pairs of diferent sexes for the paired design
HRcenterDist=rep(0,N_indv/2);#Distance between pairs of agents at the begining


#### setting drift of HR center (bias point) for the drifting scenarios ####
if (DriftasOnce==0){ #0 it will drift daily;
  Xdrift=cumsum(rep(c(DriftStrength[1],rep(0,(DayLength-1))),DaysToSimulate))#for each day i have the drift, then steps with no drift ubtil the next day. i work with Cumsum since the drift grows 
  Ydrift=cumsum(rep(c(DriftStrength[2],rep(0,(DayLength-1))),DaysToSimulate)) 
  print('using drift daily')
} else if (DriftasOnce == 1){ # if 1 it will drift in the midle of the run  
  Xdrift=rep(0,N_tmStp);Xdrift[N_tmStp/2]=DriftStrength[1]*DaysToSimulate;Xdrift=cumsum(Xdrift);
  Ydrift=rep(0,N_tmStp);Ydrift[N_tmStp/2]=DriftStrength[2]*DaysToSimulate;Ydrift=cumsum(Ydrift);  
  print('using drift from middle step of the run')
} else if (DriftasOnce == 2){#if 2 it will drift from begining.
  Xdrift=rep(DriftStrength[1]*DaysToSimulate,N_tmStp);
  Ydrift=rep(DriftStrength[2]*DaysToSimulate,N_tmStp);
  #print('using drift from start')
}else{print('check your params')}
DrifByStep=rbind(Xdrift,Ydrift)


#Determining indivduals to drift.
DriftingYorN=c(rep(1,PropDriftIndiv*N_indv),rep(-1,N_indv-PropDriftIndiv*N_indv))
DriftingYorN=sample(DriftingYorN, N_indv);

#setting the HR centerDrift for this time step (cumulative from beggining)
if (DriftHRCenters==1){CurDrift=DrifByStep[,Curr_tmStp];names(CurDrift)=NULL}else{CurDrift=c(0,0)}


#### generating landscape for this iteration #######      
N_FoodPatches=50 #how many patches - this is just to be consistent with the code. currently individuals do not response to resources. 
Scl=2000;#the size of the area for simuation. not important since there is not boundry. just to be consistent with other codes.
PointsStrc <- rsyst(nx=sqrt(N_FoodPatches))#generation of a uniform landscape
FoodItems=SpatialPoints(coords(PointsStrc)*Scl-Scl/2) # These are the points on scale of 0 to 1 no -Scl/2 to Scl/2
if (ToPlot==1) {plot(FoodItems,col='white',pch=1); title('the simulated landscape')}#the number of point will be more or less kappa*mu    


#### a loop on individuals for initial conditions ####
for (k in 1:N_indv) {
  #placing agents in their initial positions 
  XYind[[k]] <- matrix(rep(NA, 2 * N_tmStp), ncol = 2)#this matrix will store the location of each individual
  XYind[[k]][1, ] <-  c(runif(n=1,min=-Scl/3,max=Scl/3 ),#X random initial location of each individual is uniformly distributed in the range of the observed ones
                         runif(n=1,min=-Scl/3,max=Scl/3 ))#Y random initial location of each individual is uniformly distributed in the range of the observed ones
  HRCnt[k,1:2] =XYind[[k]][1, ];#logging the HR center 
   
  if ((PairedAgents==1) & (k %% 2 == 0)){#if this is a paired scenario than i switch every other individual to be a pair with nearby location to the previous one
     XYind[[k]][1, ]=XYind[[k-1]][1, ]+c(runif(n=2,min=-PairStartDist/2,max=PairStartDist/2))#setting the start tlocation for the pair
     HRcenterDist[k/2]=round(dist(rbind(XYind[[k-1]][1, ],c(XYind[[k]][1, ]))),digit=1);#storing the initial distance from this pair
     print(paste("pair:",k-1,"and",k, "distance",HRcenterDist[k/2] ))
   }#if paired design than use nearby coordinate for every second individual

   if (ToPlot==1 ) {
      plot(SpatialPoints(coords=matrix(data=XYind[[k]][1, ],nrow=1)),add=T,pch=HRCnt[k,c(3)]+23,col=HRCnt[k,c(3)],bg=HRCnt[k,c(3)], cex=1.5) 
      if ((PairedAgents==1) & (k %% 2 == 0)){#plot also line between pairs
        plot(SpatialLines(list( Lines( list(Line(rbind(XYind[[k-1]][1, ],c(XYind[[k]][1, ])))), ID=k/2) )),add=T) # This is the river 'line' 
        }#plot also line between pairs
    }#plot the line
}#loop on individuals 
          
      


#### loop on time steps and individuals to run the simulation ####
for (Curr_tmStp in 1:(N_tmStp-1)){
  ## loop on individuals ##
  for (Curr_indv in 1:N_indv){
          
  #### distance of this Curr_indv to all other individuals 
  Dist=rep(NA,N_indv ); 
  for (ii in 1:N_indv) {Dist[ii] = dist(rbind(XYind[[ii]][Curr_tmStp, ],c(XYind[[Curr_indv]][Curr_tmStp, ])))}
  Dist[Dist==0]=NA;#getting rid of the distance to self
          
  ## start generation of indiv location at time k+1
          
  ##### selecting direction ##########
  #calculating the direction to the initial location (bias point )+ now with drift for the current step
  BiasPoint=(XYind[[Curr_indv]][1, ]+CurDrift*DriftingYorN[Curr_indv])#this bias point is the origin+ the current cumulative bias
  if ((PairedAgents==1) & (Curr_indv %% 2 == 1)){BiasPoint=colMeans(rbind(BiasPoint,XYind[[Curr_indv+1]][Curr_tmStp, ]))}#updating bias point as the mean of HR center and mates last location
  if ((PairedAgents==1) & (Curr_indv %% 2 == 0)){BiasPoint=colMeans(rbind(BiasPoint,XYind[[Curr_indv-1]][Curr_tmStp-1, ]))}#updating bias point as the mean of HR center and mates previous location (before last step since he moved already)  
          
  if (min(Dist,na.rm=T)< Social_Pecrt_rng) {#if there is another invividual in range, 
    #if i want the mean of direction to the HR and nearest neighbor: 
    #BiasPoint=colMeans(rbind(BiasPoint,XYind[[which.min(Dist)]][Curr_tmStp, ]))}#then updated  this for the bias also
    BiasPoint=XYind[[which.min(Dist)]][Curr_tmStp, ]
    }#then updated  this for the bias also
          
          
  coo <- BiasPoint  - XYind[[Curr_indv]][Curr_tmStp, ] #cheking direction to the Bias point of this individual, change the second ccomp to have another bias
  mu <- Arg(coo[1] + (0+1i) * coo[2])#
  if (mu < 0)  {   mu <- mu + 2 * pi  }#just making sure direction is not negative
  mu.av <- Arg(EtaCRW * exp(Phi_ind[Curr_indv] * (0+1i)) + (1 - EtaCRW) * exp(mu * (0+1i))) #bias to initial location + CRW to find the von mises center for next step
  Phi_ind[Curr_indv] <- rvm(n=1, mean = mu.av, k = Kappa_ind)#choosing curr step direction from vonMises centered around the direction selected above 
          
          
  ##### preforming step #########
  #selection of step size for this indiv in this state from the specific gamma          
  step.len <- rgamma(1, shape = StpSize_ind^2/StpStd_ind^2,scale = StpStd_ind^2/StpSize_ind)
  step <- step.len * c(Re(exp((0+1i) * Phi_ind[Curr_indv])), Im(exp((0+1i) * Phi_ind[Curr_indv])))
  XYind[[Curr_indv]][Curr_tmStp + 1, ] <- XYind[[Curr_indv]][Curr_tmStp, ] + step#the indiv next location

  ###### ploting the steps ########
  if (Curr_indv<=length(Color_indv) & ToPlot==1 ){#plot only up to 6 indiv and only if ToPlot==1
    StepAsLine = SpatialLines(list( Lines( list(Line(XYind[[Curr_indv]][Curr_tmStp:(Curr_tmStp+1) , ])), ID=Curr_tmStp) )) # #Conver line to spatial object for plot
    plot(StepAsLine,add=T, col= Color_indv[Curr_indv],lwd=4  ) #plot the buffer
  }#plot only up to 10 indiv
  
  }#loop on invdividuals
  print(c("done with timestep",Curr_tmStp,"out of", N_tmStp))
      
}#loop on time steps
      
      
#### after the nested loops -how many times out of the box? ####
startIndx=1;
for (k in 1:N_indv) {        
  endIndx=startIndx+N_tmStp-1;
  XYind_log2[c(startIndx:endIndx),c("x","y")]=XYind[[k]]        
  XYind_log2[c(startIndx:endIndx),c("pseudoSex")]=  HRCnt[k,3] #storing the sex of this agent sbased on the HR center it got was it a male1 or a female2 there?
  startIndx=endIndx+1;
}        
OutOfTheBox=sum((XYind_log2$x<  -Scl/2) | 
                  (XYind_log2$x>   Scl/2) |
                  (XYind_log2$y<  -Scl/2) |
                  (XYind_log2$y>   Scl/2) ) /length(XYind_log2$x)
print(paste("out of the box rate",round(OutOfTheBox,digit=5)))
rm("Curr_indv","Curr_tmStp","k","Color_indv","PointsStrc","CurDrift","BiasPoint")
rm("Phi_ind","step","step.len","mu","mu.av","coo","StepAsLine","Dist", "endIndx","startIndx","ToPlot")


###### Saving results for matlab and R#####
Name1=paste("xyFromSimulationForSNanalysis", N_tmStp , N_indv,  100*EtaCRW, StpSize_ind,DriftHRCenters,".rdata",sep="_")
Name2="xyFromSimulationForSNanalysis.mat"
save(list=ls(),file=Name1)
writeMat(Name2,XY=XYind_log2,HRCntXY=HRCnt) 
print('saved simulation data, go to matlab to run the nalysis... ')

