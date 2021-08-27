############# Performing a MonteCarlo simulation to predict the Oil in place #################
#Reservoir characterisitcs Bulk volume Uniform Min.= 70 MMft3 Max= 80 MMft3, unifrom distribution as Vb#
#Porosity Triangular Min.=0.13; Most likely=0.15; Max.=0.20 , Triangular distribution as Por#
#Net-to-gross Ratio Normal µ = 0.8; ??=0.05, Normal distribution as ntg#
#Formation volume factor at initial conditions was measured as 1.2 rb/STB#
#Inital water saturation was approximatelyfound to be a direct function of porosity, ??:Swi = 0.05/??#

##### LOAD DATA PACKAGE ####

install.packages("triangle")
install.packages("ggfortify")
library(triangle)
library(ggfortify)

#### Montecarlo simulation with n= 1000 for the parameters ###
Vb <- runif(n=1000,min=70E6,max=80E6)
plot(Vb)
hist(Vb)
Por <- rltriangle(n=1000, a=0.13, b=0.20, c=0.15)
plot(Por)
hist(Por)
ntg <- rnorm(n=1000,mean=0.8,sd=0.05)
plot(ntg)
hist(ntg)

#### Calculating Oil in place ####

swi=0.05/Por
boi=1.2
ooip <- Vb*ntg*Por*(1-swi)/(1.2*5.615)
plot(ooip)
hist(ooip)

#### Getting the Proven, Probable and Possible reserves ####  

ecdf1<-ecdf(ooip)
plot(ooip,1-ecdf1(ooip))
abline(h = 0.9, lty = 3)
abline(h = 0.5, lty = 3)
abline(h = 0.1, lty = 3)
quantile(ooip, probs = 0.1)
quantile(ooip, probs = 0.5)
quantile(ooip, probs = 0.9)

############################################################