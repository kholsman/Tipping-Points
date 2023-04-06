#######Code to perform the stochastic cusp model######

###Camilla Sguotti, camilla.sguotti@unipd.it, Christian Moellmann, chritsian.moellmann@uni-hamburg.de###

#####The stochastic cusp model: BACKGROUND INFO#####
##The stochastic cusp model was developed by the mathematician Rene Thom in the 1980s and
#allows to model the dynamics of a state variable depending on two interactive drivers.
#The model is based on a cubic differential equation extended with a Wiener´s process to add stochasticity. 

#-V(z_t,α,β)= -1/4 z_t^4+1/2 〖βz〗_t^2+αz_t							(1)

#where V(z_t,α,β) is a potential function whose slope represents the rate of change of the system (the system state variable is called zt), depending on the two control variables (α,β).

#-δV(z,α,β)/δz=(-z_t^3+βz_t+α)dt+ σ_z dW_t		

#The two control variables, α and β, are the factors that can cause the rise of non-linear discontinuous dynamics.
#z is the state variable (the variable we want to model) and is modelled as a linear function of for example a a time series of an ecosystem or a population
#α is the asymmetry variable that controls the dimension of the state variable. Therefore, if α increases the state variable will change accordingly.
#Instead, β is the so-called bifurcation variable and can modify the relationship between zt and α from linear and continuous to non-linear and discontinuous45. 
#In the model α, β and z are modelled as linear function of the original variables we want to fit in the model such as fishing capacity, temperature (SST), ecosystem state of the Northern Adriatic Sea (PC1 and PC2). 
#A combination of different drivers can also be used to model the three factors fitted in the stochastic cusp model. 

    #α= α_0+α_1 Fishing Capacity
    # β=β_0+β_1 SST
    #zt=w_0+w_1 State of Northern Adriatic Ecosystem (PC1 and PC2)							
  
 # where α0, β0 and w0 are the intercepts, and α1, β1 and w1 the slopes of the models.
  #The stochastic cusp model is thus able to detect three different types of dynamics of the state variable, 
#linear and continuous, non-linear and continuous and discontinuous

#Moreoevr the model compare the stochastic cusp model to two continuous alternatives, a linear model and a logistic model 

###for more info see Sguotti et al., 2019, 2022; Moellmann et al., 2022; Diks and Wang 2016, Petraitis et al., 2016; Dakos and Kefi 2022. 

####applying the model#######

###before applying the model we usually do the typical screening (change point analysis, driver-state plots, bimodality of the state variable).
#This preliminary screening allows us to hypothesise the presence of tipping points in our state variable. 
#To confirm the presence of tipping points and in particular the presence of hysteresis, we then perform the stochastic cusp model 

library(cusp)
library(ggplot2)
library(ggrepel)
#load the dataset: PCA results and drivers of the Northern Adriatic Community. More info about how these data were processed 
#can be found in Sguotti et al., 2022 (Journal of Animal Ecology)
load("data_ex.RData") 

#fit the stochastic cusp model 
fit_community = cusp(y ~ pc2, alpha ~ Effort, beta ~ SST,  data=tot)
#summary of the model ouput
summary(fit_community, logist=T)
#the output show the coefficients of the three variables (alpha, beta and z). 
#At the end the output show the results of the three alternative models. 
plot(fit_community)
#plot the output. The grey area is the area below the fold or the area where 3 equilibria are possible two stables one unstable
#the blue dots show the high state of the state variable while the greeb dots the small state. 
#the dimension of the points show the probability of being in the same state the next year (but more analyses needed to fully understand how this is calculated)

###now I do various steps to improve the visualization of the model.
pred.community = fit_community$linear.predictors
pred.community = as.data.frame(pred.community)
pred.community$Y = fit_community$y
head(pred.community)

# calculate Cardan´s discriminant (delta): 27*alpha^2-4beta^3 => this one help us to define the presence of multiple equilibria.
delta.community = 27*(pred.community$alpha^2) - 4* (pred.community$beta^3)
pred.community$delta = delta.community 

# add values of SSB, F and T 
pred.community$ssbpred = (fit_community$fitted.values-fit_community$coefficients[5])/fit_community$coefficients[6]
pred.community$Fpred = (pred.community$alpha-fit_community$coefficients[1])/fit_community$coefficients[2]
pred.community$Tpred = (pred.community$beta-fit_community$coefficients[3])/fit_community$coefficients[4]

# add a column indicating multimodality 
MM = ifelse(pred.community$delta <= 0, "multimod", "stable")
pred.community$MM = MM 
pred.community$fMM = as.factor(pred.community$MM)
str(pred.community)
pred.community$Year = c(1980:2018)
head(pred.community)

# extract the bifurcation set, so the limit of the area below the fold! These in theory are also the values where,
#the tipping points occurred or were supposed to occurr depending on the data
bif.community <- cusp.bifset(pred.community$beta)
bif.community <- as.data.frame(bif.community)
head(bif.community)
Flow <- (bif.community$alpha.l-fit_community$coefficients[1])/fit_community$coefficients[2]
Fup <- (bif.community$alpha.u-fit_community$coefficients[1])/fit_community$coefficients[2]
pred.community$Flow<- Flow
pred.community$Fup <- Fup
pred2.community <- pred.community[!is.nan(pred.community$Flow),]
table(pred.community$fMM)#10 stable, 33multimod 76.7% in 

# define the area of the bifurcation set (the grey area)
ord1 = order(pred2.community$Flow)
ord2 = order(rev(pred2.community$Fup))
#create the polygons
d = data.frame(x=c(pred2.community$Flow[ord1], rev(pred2.community$Fup)[ord2]),
               y =c(pred2.community$Tpred[ord1], rev(pred2.community$Tpred)[ord2]))

#theme to make plot nicer 
cami_theme <- function() {
  theme(
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = "black", family = "Helvetica", size=8),
    plot.title = element_text(colour = "black", face = "bold", size = 10, hjust = 0.5, family = "Helvetica"),
    axis.title = element_text(colour = "black", face = "bold", size = 8, family = "Helvetica"),
    #panel.grid.major.x = element_line(colour = "dodgerblue3"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Helvetica", colour = "white", size=12),
    strip.background = element_rect(fill = "black"),
    axis.ticks = element_line(colour = "black")
  )
}

p1 = ggplot(data=pred.community , aes(x=Fpred, y=Tpred)) +
  geom_polygon(data=d,aes(x=x,y=y), color="lightblue", fill="lightblue") +
  coord_cartesian(xlim=c(7000,10000))+
  geom_path(color="darkgrey") +
  geom_point(data=pred.community, aes(x=Fpred, y=Tpred), size= 3*tot$pc2, col="blue") +
  #scale_color_manual(breaks = c("1", "2","3","4"),values=c("#1b7837","#313695", "#d73027","orange","black"))+
  geom_text_repel(label=tot$Year, size=3)+
  ylab("SST (°C)")+xlab("Fishing Capacity (GT)")+
  theme_bw()+
  theme(legend.position="none")+
  cami_theme()
p1

####### Interpretation from the presentation and Sguotti et al., 2022 (Journal of Animal Ecology)
