Repo maintained by: Kirstin Holsman  
Alaska Fisheries Science Center  
NOAA Fisheries, Seattle WA  
**<kirstin.holsman@noaa.gov>**

[“5th Effects of Climate Change on the World’s Oceans
Meeting”](https://meetings.pices.int/meetings/international/2023/eccwo-5/Background)

[“S-CCME/SICCME Workshop on integrated modeling to identify climate
change tipping points in marine
ecosystems.”](https://meetings.pices.int/meetings/international/2023/eccwo-5/program#W5)

April 16th, 9:00 am – 5:00 pm

**Overview**

Marine ecosystems are increasingly impacted by multiple climate change
and non-climate stressors that are pushing some systems and species
towards or past tipping points (critical points where a small change in
a pressure or driver can induce a disproportionate change in system
dynamics). The goal of this workshop is to draw upon recent PICES and
ICES working group efforts to synthesize findings and outputs from
recent integrated modeling projects across the globe. In particular,
this SCCME/S-CCME ([“ICES/PICES The Strategic Initiative on Climate
Change Impacts on Marine
Ecosystems”](https://www.ices.dk/community/groups/pages/siccme.aspx))
workshop will review evidence and case studies for historical and future
tipping points and thresholds in marine ecosystems to help support
climate-informed management advice.

**Potential Outcomes**

1.  Github repository with example R code for identifying tipping points
    in marine systems.  
2.  Peer review publication of emergent approaches, case studies, and
    considerations of tipping points and thresholds for climate informed
    fisheries and ecosystem management advice.

# Download the Tipping Points repo

To run this tutorial first clone the git repository to your local drive:

## Option 1: Use R

This set of commands, run within R, downloads the repository and unpacks
it, with the Tipping Points directory structure being located in the
specified `download_path`. This also performs the folder renaming
mentioned in Option 2.

``` r
    # Specify the download directory
    main_nm       <- "Tipping-Points"

    # Note: Edit download_path for preference
    download_path <-  path.expand("~")
    dest_fldr     <- file.path(download_path,main_nm)
    
    url           <- "https://github.com/kholsman/Tipping-Points/archive/refs/heads/main.zip"
    dest_file     <- file.path(download_path,paste0(main_nm,".zip"))
    download.file(url=url, destfile=dest_file)
    
    # unzip the .zip file (manually unzip if this doesn't work)
    setwd(download_path)
    unzip (dest_file, exdir = download_path,overwrite = T)
    
    #rename the unzipped folder from ACLIM2-main to ACLIM2
    file.rename(paste0(main_nm,"-main"), main_nm)
    setwd(main_nm)
```

## Option 2: Download the zipped repo

Download the full zip archive directly from the [**Tipping-Points
Repo**](https://github.com/kholsman/Tipping-Points) using this link:
[**https://github.com/kholsman/Tipping-Points/archive/refs/heads/main.zip**](https://github.com/kholsman/Tipping-Points/archive/refs/heads/main.zip),
and unzip its contents while preserving directory structure.

**Important!** If downloading from zip, please **rename the root
folder** from `Tipping-Points-main` (in the zipfile) to `Tipping-Points`
(name used in cloned copies) after unzipping, for consistency in the
following examples.

Your final folder structure should look like this:

<!-- ![](Figs/dir.png){ width=100%} -->

## Option 3: Use git commandline

If you have git installed and can work with it, this is the preferred
method as it preserves all directory structure and can aid in future
updating. Use this from a **terminal command line, not in R**, to clone
the full Tipping-Points directory and sub-directories:

``` bash
    gh repo clone kholsman/Tipping-Points
```

------------------------------------------------------------------------

# Set up the Workspace

Open R() and used ‘setwd()’ to navigate to the root ACLIM2 folder (.e.g,
\~/mydocuments/ACLIM2)

``` r
    # set the workspace to your local ACLIM2 folder
    # e.g.
    # setwd( path.expand("~/Documents/GitHub/Tipping-Points") )
   
    # --------------------------------------
    # SETUP WORKSPACE
    tmstp  <- format(Sys.time(), "%Y_%m_%d")
    main   <- getwd()  #"~/GitHub_new/Tipping-Points"
    
    # loads packages, data, setup, etc.
    suppressWarnings(source("R/make.R"))
```

    ##  Loading Holsman et al. 2020 dataset from 
    ##  https://github.com/kholsman/EBM_Holsman_NatComm
    ## --------------------------------

------------------------------------------------------------------------

# Tipping Point Methods Demo

This document provides an overview of accessing, plotting, and demoing
Tipping Point analyses. Below are three example vignettes for tipping
point analysis : Thresholds analysis courtesy of Kirstin Holsman
(<kirstin.holsman@noaa.gov>), IRA courtesy of Manuel Hidalgo
(<jm.hidalgo@ieo.csic.es>), and Stochastic CUSP modelling (SCM) courtesy
of Camilla Sguotti (<camilla.sguotti@unipd.it>).

## Threshold analysis

### Thresholds summary

Below is an example analysis from (Holsman et
al. 2020)\[<https://www.nature.com/articles/s41467-020-18300-3>\] where
threshold analysis was used to detect a thermal tipping point in biomass
and catch for Bering Sea groundfish.

![From Holsman et al. 2020](Figs/Holsman2.png)

This approach fits a gam (with or without autocorrelation) to a response
variable as a function of an environmental driver (e.g., temperature).
The first and second derivatives of the gam are used to detect the
presence of a threshold and tipping point. See (Large et
al. 2013)\[<https://academic.oup.com/icesjms/article/70/4/755/727721>\]
and (Samhouri et
al. 2017)\[<https://esajournals.onlinelibrary.wiley.com/doi/full/10.1002/ecs2.1860>\]
for more information and a stepwise approach to threshold analysis.

From Large et al. 2013: *“The shape of the relationship between a
response and pressure is captured in the smoothing function *S*(*x*).
Values of the pressure variable that influence the response in a
particular direction can be enumerated by recognizing qualities of the
shape of the smoothing function. The first derivative *Ŝ*(*x*)′ of
*s*(*X*) indicates regions where a pressure variable causes a negative
\[*Ŝ*(*x*)′, 0\] or positive \[*Ŝ*(*x*)′ . 0\] response to an ecological
indicator. Further, the second derivative *Ŝ*(*x*)″ denotes regions
where *Ŝ*(*x*)′ changes sign and a threshold is crossed \[0 , *Ŝ*(*x*)″
. 0\]. To measure the uncertainty surrounding both *Ŝ*(*x*)′ and
*Ŝ*(*x*)″, we estimated the first and second derivatives using finite
differences for each bootstrap replicated smoothing term *s**b**r*(*x*).
Both $\hat{S_i}(x)'$ and$\hat{S_i}(x)''$ were sorted into ascending
order and the value of the 2.5% and 97.5% quantiles of $\hat{S_i}(x)'$
and$\hat{S_i}(x)''$ were considered the 95% CI for the first and second
derivative of the smoothing function (Buckland, 1984). A significant
trend *Ŝ*(*x*)′ or threshold *Ŝ*(*x*)″ was identified when the 95% CI
crossed zero for either derivative.”*

### Step 1: Fit gam()

Let’s first explore the tipping point calculations using Pacific cod
biomass (‘B_thresh_12_2’) or catch (‘C_thresh_12_2’) for scenarios with
the 2 MT cap effects (’\_13_1’).

``` r
  # Load setup, packages, functions and data
  source("R/make.R") 

  # Alternatives to play around with:
  # ------------------------------------
  # change in catch from persistence scenario for P. cod without effects of 2 MT cap
  # datIN        <- C_thresh_12_2$datIN  
  # change in Biomass from persistence scenario for P. cod without effects of 2 MT cap
  # datIN        <- B_thresh_12_2$datIN  
  # change in catch from persistence scenario for Pollock without effects of 2 MT cap
  # datIN        <- C_thresh_12_1$datIN  


  # change in catch from persistence scenario for P. cod with effects of 2 MT cap
  datIN <- C_thresh_13_2$datIN  
  
  # set the response variable to the % difference in catch (or biomass) relative to the 
  # persistence scenario, set the driver to TempC
  datIN <- datIN%>%dplyr::select(Year,
                                 driver   = TempC,
                                 response = delta_var_prcnt,
                                 MC_n , Scenario)
  
  # Add columns for grouping (optional for autocorrelation grouping in gamm)
  datIN$rand   <- 1    
  datIN$group  <- paste0(datIN$MC_n,"_",datIN$Scenario,"_", datIN$sp)
  datIN$groupN <- as.numeric(as.factor(datIN$group)) 
  datIN$YearN  <- datIN$Year- min(datIN$Year)+1
  
  # Based on code from E. Hazen and M. Hunsicker WG36 (PICES)

  # Fit gam with and without autocorrelation 
  #------------------------------------
  tmp_gam          <- gam(response ~ s(driver,k=t_knots,bs="tp"),data = datIN)
  datIN$dev.resid  <- residuals(tmp_gam,type='deviance')
  
  # # AR gam
  # tmp_gamAR        <-  gamm(response ~ s(TempC,k=t_knots,bs="tp"),
  #                           random=list(groupN=~1),correlation=corAR1(form=~YearN),data = datIN)
  # datIN$dev.resid  <- residuals(tmp_gamAR$gam,type='deviance')


  # fit the null model
  null      <- gam(response ~ 1,family='gaussian',data = datIN)
  dr        <- sum(residuals(tmp_gam)^2)
  dn0       <- sum(residuals(null)^2)
  gam.dev.expl    <- (dn0-dr)/dn0
  
  # from here out use the non-AR gam for the demo
  x <-  seq(-3,10,.1) # newdata TempC vector
  hat       <-  predict(tmp_gam,se.fit=TRUE, newdata = data.frame(driver=x) )
  dd        <-  datIN%>%mutate(driver = round(driver,2) )%>%select(driver, response)
  dd$num    <-  1:length(dd[,1])
```

### Step 2: Bootstrap for error

We now measure uncertainty surrounding each GAM by using a naive
bootstrap with random sampling and replacement. For each
indicator–pressure combination, bootstrap replicates were selected from
the raw data and each was fitted with a GAM.

For pressure–state relationships identified as nonlinear, we define the
location of the threshold as the inflection point, that is, the value of
the pressure where the second derivative changed sign (Large et
al. 2013). For these analyses, we calculated the 95% CI of the smoothing
function itself, along with its second derivative, via bootstrapping of
the residuals.

``` r
  # pre-allocate 'NA' Matrix
  # ------------------------------------

  # TIP: 'boot_nobsIN' is specified in R/setup.R as 1000
  # boot_nobsIN <- 500 # uncomment this line if the code is slow
    
  # pre allocate NA Matrix
    x <-  seq(-3,10,.1) # newdata TempC vector
    Deriv1 <- 
    Deriv2 <- 
    hatFit <- 
    hatse  <- matrix(NA,boot_nobsIN,length(x))
    gmlist <- list()
    
  # set bootstrap iterations to boot_nobsIN    
    boot_n    <- boot_nobsIN # number of bootstrap runs
    boot_nobs <- boot_nobsIN # optional subsample, if boot_nobs > sample nobs, is set = sample nobs
    knotsIN   <- t_knots # number of knots, set to 4 in Holsman et al. 2020
    
    sdmult    <- 1 # 1 sd
    method    <- methodIN  # Holsman et al. 2020 used method 2
    probIN    <- c(.025,.5,.975) # probablities for the quantile ranges
    spanIN    <- span_set  # default set to 0.1
    
  # Run the boot strap
  # ------------------------------------
  pb <- progress_bar$new(total = boot_n)
  for(int in 1:boot_n){
     pb$tick()
    # get bootstraped sub-sample
    nobs          <- length(dd$num)
    if(boot_nobs > nobs) 
      boot_nobs   <- nobs
    
    bootd         <- sample_n(dd,boot_nobs,replace = TRUE)
    tmpgam        <- gam(response~s(driver,k=knotsIN,bs="tp"),data = bootd)
    tmpd          <- deriv2(tmpgam,simdat=x)
    gmlist[[int]] <- tmpgam
    Deriv1[int,]  <- tmpd$fd_d1
    Deriv2[int,]  <- tmpd$fd_d2
    hatFit[int,]  <- predict(tmpgam,se.fit=TRUE,newdata=data.frame(driver=x))$fit
    hatse[int,]   <- predict(tmpgam,se.fit=TRUE,newdata=data.frame(driver=x))$se
    
  }
```

### Step 3: Get first and second derivatives of gam to determine tipping point

``` r
  # apply quantiles to bootstrap replicates
  D1_se  <- apply(Deriv1,2,quantile,probs=probIN)
  D2_se  <- apply(Deriv2,2,quantile,probs=probIN)
  qnt    <- apply(hatFit,2,quantile,probs=probIN)
  qntse  <- apply(hatse,2,quantile,probs=probIN)
  nobs   <- length(x) 
  
  # first to the gam using 1-3 methods
    hat_qnt1 <- data.frame(tmp = x,
                          up  = hat$fit+qnt[3,]-qnt[2,],
                          mn  = hat$fit,
                          dwn = hat$fit+qnt[1,]-qnt[2,],
                          method = "Method 1")
    
    hat_qnt2 <- data.frame(tmp = x,
                          up  = hat$fit+sdmult*qntse[2,],
                          mn  = hat$fit,
                          dwn = hat$fit-sdmult*qntse[2,],
                          method = "Method 2")
    
    hat_qnt3 <- data.frame(tmp = x, 
                          up  = hat$fit+sdmult*hat$se,
                          mn  = hat$fit,
                          dwn = hat$fit-sdmult*hat$se,
                          method = "Method 3")
  
  ggplot(rbind(hat_qnt1,hat_qnt2,hat_qnt3))+
    geom_ribbon(aes(x=tmp, ymin=dwn, ymax=up,fill=method))+facet_grid(method~.)+
    scale_fill_viridis_d(begin = .8, end=0)+
    theme_minimal()
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/threshold_STEP3-1.png" style="display: block; margin: auto;" />

``` r
  # use method 2
  hat_qnt <- hat_qnt2
  
  hat_qnt$smoothed_mn  <- predict(loess(mn ~ tmp, data=hat_qnt, span=spanIN)) 
  hat_qnt$smoothed_dwn <- predict(loess(dwn ~ tmp, data=hat_qnt, span=spanIN)) 
  hat_qnt$smoothed_up  <- predict(loess(up ~ tmp, data=hat_qnt, span=spanIN)) 

   ggplot(rbind(
     hat_qnt%>%select(datIN=tmp,up, mn, dwn)%>%mutate(method="not smoothed"),
     hat_qnt%>%select(datIN=tmp,up=smoothed_up, mn=smoothed_mn, dwn=smoothed_dwn)%>%mutate(method="smoothed")))+
  geom_ribbon(aes(x=datIN, ymin=dwn, ymax=up,fill=method))+
     facet_grid(method~.)+
  scale_fill_viridis_d(begin = .8, end=0)+
  theme_minimal()
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/threshold_STEP3-2.png" style="display: block; margin: auto;" />

``` r
  # first derivative quantiles
  df1_qnt<-data.frame(tmp = x,
                      up  = D1_se[3,],
                      mn  = D1_se[2,],
                      dwn = D1_se[1,])
  
  # second derivative quantiles
  df2_qnt<-data.frame(tmp = x,
                      up  = D2_se[3,],
                      mn  = D2_se[2,],
                      dwn = D2_se[1,])
  

  # get difference in signs
  getdelta<-function(xx,rnd = rndN2){
    nn        <- length(xx)
    xx        <- round(xx,rndN2)
    delta     <- rep(NA,nn)
    updn      <- c(0, diff(sign(xx)))
    ix        <- which(updn != 0)
    sign(updn)[ix]
    delta[ix] <- 1
    return(list(delta=delta,ix=ix,updn=updn,xx=xx))
  }
  
  
  # determine peaks and valleys:
  # 10% smoothing span
  df1_qnt$smoothed_mn  <- predict(loess(mn  ~ tmp, data=df1_qnt, span=spanIN)) 
  df1_qnt$smoothed_dwn <- predict(loess(dwn ~ tmp, data=df1_qnt, span=spanIN)) 
  df1_qnt$smoothed_up  <- predict(loess(up  ~ tmp, data=df1_qnt, span=spanIN)) 
  
  
  ggplot(rbind(
    hat_qnt%>%select(datIN = tmp,up=smoothed_up, mn=smoothed_mn, dwn=smoothed_dwn)%>%mutate(method="a) smoothed gam (s(x))"),
    df1_qnt%>%select(datIN = tmp,up=smoothed_up, mn=smoothed_mn, dwn=smoothed_dwn)%>%mutate(method="b) First Deriv (s'(x)")))+
  geom_ribbon(aes(x=datIN, ymin=dwn, ymax=up,fill=method))+facet_grid(method~.,scales="free_y")+
  scale_fill_viridis_d(begin = .8, end=.1)+
  theme_minimal()
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/threshold_STEP3-3.png" style="display: block; margin: auto;" />

``` r
  # find thresholds
  pks1    <- sort(c(findPeaks(df1_qnt$smoothed_mn),findPeaks(-df1_qnt$smoothed_mn)))
  signif1 <- which(!data.table::between(0, df1_qnt$dwn, df1_qnt$up, incbounds=TRUE))
  thrsh1  <- intersect(which(!data.table::between(0, df1_qnt$dwn, df1_qnt$up, incbounds=TRUE)),pks1)
  df1_qnt$tmp[thrsh1]
  
  # 10% smoothing span
  df2_qnt$smoothed_mn  <- predict(loess(mn ~ tmp, data=df2_qnt, span=spanIN)) 
  df2_qnt$smoothed_dwn <- predict(loess(dwn ~ tmp, data=df2_qnt, span=spanIN)) 
  df2_qnt$smoothed_up  <- predict(loess(up ~ tmp, data=df2_qnt, span=spanIN)) 
  pks2     <- sort(c(findPeaks(df2_qnt$smoothed_mn),findPeaks(-df2_qnt$smoothed_mn)))
  pks2_up  <- sort(c(findPeaks(df2_qnt$smoothed_up),findPeaks(-df2_qnt$smoothed_up)))
  pks2_dwn <- sort(c(findPeaks(df2_qnt$smoothed_dwn),findPeaks(-df2_qnt$smoothed_dwn)))
  

   
  hat_qnt$sig <- df1_qnt$sig <- df2_qnt$sig <-NA
  df2_qnt$sig <- !between(0, df2_qnt$dwn, df2_qnt$up, incbounds=TRUE)
  df2_qnt$sig[!df2_qnt$sig] <-NA
  signif2     <- which(!between(0, df2_qnt$dwn, df2_qnt$up, incbounds=TRUE))
  thrsh2_all  <- intersect(signif2,pks2)
  thrsh2      <- which(1==10)
 
  if(length(thrsh2_all)>0)
    thrsh2<-mean(thrsh2_all[which(abs(df2_qnt$smoothed_mn[thrsh2_all])==
                                    max(abs(df2_qnt$smoothed_mn[thrsh2_all])))],na.rm=T)
  
   
   plot1<- ggplot(rbind(
    hat_qnt%>%select(driver = tmp,up=smoothed_up, mn=smoothed_mn, 
                     dwn=smoothed_dwn,sig)%>%mutate(method="a) smoothed gam (s(x))"),
    df1_qnt%>%select(driver = tmp,up=smoothed_up, mn=smoothed_mn, 
                     dwn=smoothed_dwn,sig)%>%mutate(method="b) First Deriv (s'(x)"),
    df2_qnt%>%select(driver = tmp,up=smoothed_up, mn=smoothed_mn,
                     dwn=smoothed_dwn,sig)%>%mutate(method="c) Second Deriv (s''(x)")))+
    geom_ribbon(aes(x=driver, ymin=dwn, ymax=up,fill=method))+
    facet_grid(method~.,scales="free_y")+
    geom_hline(yintercept=0,color="white")+
    scale_fill_viridis_d(begin = .8, end=.1)+
    theme_minimal()
   
  plot1 + geom_mark_rect(aes(x=driver, y=up,fill = sig,label = "sig. range"),color=FALSE)+
    geom_vline (xintercept =df2_qnt$tmp[thrsh2], color = "red") +
    theme(legend.position="none")+ ylab("")
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/threshold_STEP3-4.png" style="display: block; margin: auto;" />

### Use the threshold() and plot_threshold() functions

``` r
   # --------------------------------------------------
   # now use the threshold function to detect thresholds
   # --------------------------------------------------
  
   threshold_example        <-  threshold(
                                knotsIN    = t_knots,
                                driver     = datIN$driver,
                                response   = datIN$response,
                                boot_nobs  = boot_nobsIN,
                                rndN       = rndNIN,
                                method     = methodIN,
                                boot_n     = nitrIN)

    plot_threshold(threshold_example)
```

## Integrative Resilience Analysis (IRA)

### IRA summary

Integrative Resilience Analysis’ (IRA) to two case studies: Western
Mediterranean and Iberian Seas in the Atlantic ([Hidalgo et al
2022](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13648),
[Polo et
al. 2022](https://academic.oup.com/icesjms/article/79/7/2017/6648917)).

From Hidalgo et al. 2022: *“The IRA is a three-step methodological
framework which applies the concepts of resilience and folded stability
landscapes in an empirical multivariate context through the combination
of multivariate analysis, non-additive modelling and a resilience
assessment (Vasilakopoulos et al.,
[2017](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13648#jane13648-bib-0042)).
This way, the IRA elucidates the system dynamics and shift mechanisms in
response to external stressors.”*

From Polo et al. 2022: *“In the IRA framework, the relationship between
PCsys and its drivers is assessed using PCsys as response variable in
generalized additive models (GAMs) and threshold-GAMs (TGAMs) (Cianelli
et al., 2004). Each of the four stressors with 0, 1, and 2 years of lag
were used as explanatory variables in separate models. Testing potential
lagged effect of the stressors was designed to detect a potential
delayed response of the sampled biomass by species. GAMs are models that
assume additive and stationary relationships between the response and
explanatory variables while TGAMs are GAMs adjusted to account for
abrupt changes in the response mechanism (Ciannelli et al., 2004). The
basic GAM function used is included in R package mgcv (Wood, 2011). The
“genuine” cross-validatory squared prediction error (gCV), a
modification of generalized cross validation proposed by Cianelli et
al. (2004) that makes the goodness of fit of GAMs and TGAMs comparable,
was computed for model selection and estimation of the threshold year.”*

The IRA was carried out in R (R Core Team, 2019) using packages vegan
(Oksanen et al.,
[2019](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13648#jane13648-bib-0027)),
mgcv (Wood,
[2017](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13648#jane13648-bib-0045))
and akima (Akima et al.,
[2015](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2656.13648#jane13648-bib-0001)).

### IRA step by step

### Step 1. PCA

**Code courtesy of Manuel Hidalgo (<jm.hidalgo@ieo.csic.es>)**

Apply PCA to the time-species matrix of each studied area to identify
the main modes of community variability.PCAs are based on the
correlation matrices of the species’ biomass, following
log-transformation.

``` r
  ######################################################################
  # PV(23.05.18) By  Paris Vasilakopoulos (JRC-EU, Ispra, Italy)
  # Resilience Assessment
  ######################################################################
  
  source("R/make.R")
  Data <- read.csv("Data/Data.csv", header = TRUE)
 
  str(Data)
  Data <- Data%>%
    mutate(year    = as.numeric(Year),
           pc1sys  = PC1,
           pc1str  = chl_win,
           pc1str1 = NA,
           pc1str2 = NA)
  
  # Create lagged pc1stressor vectors
  Data$pc1str1[c(2:NROW(Data))] <- Data$pc1str[c(1:(NROW(Data)-1))]
  Data$pc1str2[c(3:NROW(Data))] <- Data$pc1str[c(1:(NROW(Data)-2))]
  
  #get rid of years 1994-1995
  Data <- Data[5:NROW(Data),]
  Data <- Data%>%select(year,pc1sys,pc1str2)
  
  rownames(Data) <- 1:nrow(Data)

  ########################################################################
  # If x- and y-axis on different scales, now is a good time to standardise it/them
  colnames(Data)[3] <- "pc1str"
  # z-standardisation carried out here
  S1          <- scale(Data$pc1str) #save for back-calculation
  Data$pc1str <- as.numeric(scale(Data$pc1str))
```

### Step 2. Sequential regime shift detection method (STARS)

Sequential regime shift detection method (STARS), modified to account
for temporal autocorrelation is used to detect significant shifts in the
mean values of PC1 and PC2. As stated in Hidalgo et al. 2022 *“STARS
estimates a Regime Shift Index (RSI), that is, a cumulative sum of
normalized anomalies relative to each value of the time series analysed,
and uses it to test the hypothesis of a regime shift occurring in that
year (Rodionov, 2006)”*. Hidalgo et al. 2022 use a cut-off length of 3
years and a significant probability threshold of p = 0.05’ Polo et
al. 2022 used cut-off length of 15 years.

### Step 3. Apply GAM and TGAM with lags 0-2 years and gCV

Now apply GAM and TGAM with lags 0-2 years. Hidalgo et al. 2022
describes the two as *” A GAM describes a system that changes in a
continuous way in response to the corresponding change of its
stressor(s), while a TGAM represents a system response curve that is
folded backwards, forming a fold bifurcation with two tipping points
(Figure S1).”*.

They further used the following methods:

3.1. *“Fit 18 GAMs and 18 TGAMs (3 stressors × 2 seasons × 3 lags) for
each system, using either PC1 or PC2 as a response variable.”*. The fits
of relevant generalized additive models (GAMs) and non-additive
threshold GAMs (TGAMs) at 0- to 2-time lags response variable (PC1 or
PC2, normalized) to stressors (normalized) were compared.*“The effect of
the stressors on the system was examined at 0- to 2-year lags to account
for a potential delay in the environmental effect on the community
sampled given that environmental effects typically influence spawning
and early life stages, while the sampled biomass is usually dominated by
specimens older than 0 years old.”*.

3.2. *“Compute the ‘genuine’ cross-validation squared prediction error
(genuine CV; gCV), which accounts for the estimation of the threshold
line and the estimation of the degrees of freedom for the functions
appearing in all additive and non-additive formulations (Ciannelli et
al., 2004)”* in order to evaluate goodness of fit of GAMs and TGAMs.

``` r
# 3.1. fit Tgam and GAMS PC~stressor system
#      and Compute the genuine CV (gCV)
# ------------------------------------

  # use TGAM with no lag
  years.char <- paste((Data$year), sep = "")
  
  TGAM <- threshold.gam(formula(pc1sys~s(pc1str,k=3,by=I(1*(year<=r)))+s(pc1str,k=3,by=I(1*(year>r)))),
                        a    = 0.1,
                        b    = 0.9,
                        data = Data,
                        threshold.name = 'year',
                        nthd = 100)
  summary(TGAM$res)
  
  # identify the threshold year
 ggplot(data.frame(rv = TGAM$rv, gcvv = TGAM$gcvv))+
   geom_line(aes(x=rv,y=gcvv,color="Regime 1"),size=1.1)+
   geom_vline(xintercept = TGAM$mr, color = "gray",size=1.1)+
   xlab("Threshold variable")+
   ylab('GCV')+theme_minimal()
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/IRA_STEP3-1.png" style="display: block; margin: auto;" />

``` r
  #threshold year = 2007
  TGAM$mr
  
  #quick plot of the TGAM fitted vs observed values
  Dataup  <- Data%>%filter(year<=round(TGAM$mr)) # upper branch years
  Datalow <- Data%>%filter(year>round(TGAM$mr))  # lower branch years
  
  m       <- gam(pc1sys~s(pc1str, k=3), data=Dataup)
  n       <- gam(pc1sys~s(pc1str, k=3), data=Datalow)
  
  newdata1 <- data.frame(pc1str=runif(1000,min(Dataup$pc1str), max(Dataup$pc1str)))
  newdata2 <- data.frame(pc1str=runif(1000,min(Datalow$pc1str), max(Datalow$pc1str)))
  y        <- predict.gam(m,newdata1)
  z        <- predict.gam(n,newdata2)

  ggplot()+
    geom_text(data = Data, aes(x=pc1str,y =pc1sys, label = years.char))+
    geom_line(data = newdata1, aes(x=pc1str,y=y,color="Regime 1"),size=1.1)+
    geom_line(data = newdata2, aes(x=pc1str,y=z,color="Regime 2"),size=1.1)+
    coord_cartesian(
      ylim = c(min(Data$pc1sys)-sd(Data$pc1sys)/3,
               max(Data$pc1sys)+sd(Data$pc1sys)/3),
      xlim = c(min(Data$pc1str)-sd(Data$pc1str)/3,
               max(Data$pc1str)+sd(Data$pc1str)/3))+
    theme_minimal()
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/IRA_STEP3-2.png" style="display: block; margin: auto;" />

### Step 4. Calculate the position of the tipping point of each regime

Calculate the position of the tipping point of each regime along the
trajectory of its respective attractor. Note that *“the x-coordinates of
the tipping points were set so as to ensure that the lowest Resy
estimate within each regime was equal to zero…The stability landscape
with its alternate basins of attraction emerged through linear
interpolation of all rResy values onto a 100 × 100 grid.”* Resy was
scaled by dividing with the maximum value observed to calculate relative
resilience (*r**R**e**s*<sub>*y*</sub>):  
*r**R**e**s*<sub>*y*</sub> = *R**e**s*<sub>*y*</sub>/*m**a**x*(*R**e**s*<sub>*y*</sub>)

``` r
  # ----------------------------------------
  # Calculate vertical component of resilience
  # ----------------------------------------

  # here I'm simply using the residuals for this, but one could calculate actual distance
  # of point from line (but little effect on the shape of the stability landscape)
  Data_prev <- Data # archive  # Data <- Data_prev
  Data$vc   <- abs(TGAM$res$residuals)
  
  # Identify the position of tipping points
  # ----------------------------------------

  # First tipping point(F1):
  Data                <- rbind(Data, c("F1",NA,NA,0)) #F1 obviously has vc=0 
  Data[,2:ncol(Data)] <- sapply(Data[,2:ncol(Data)],as.numeric)

  
  # calculate x-value (pc1str) of F1
  # may add -1 here to force tipping year's resilience not to be calculated based on upper branch
  ff1 <- which.max(rowSums(Data[1:which(Data$year == round(TGAM$mr)),3:4])+0.1) 
  ff1 <- names(ff1)
  Data[nrow(Data),3]<- -abs(Data[ff1,3])-abs(Data[ff1,ncol(Data)])
  # this x-value for F1 ensures that all Res estimates of upper branch will be non-negative. 
  # 0.05 is arbitrary to give some extra space - can be omitted
  
  # calculate y-value (pc1sys) of F1
  o <- gam(pc1sys~s(pc1str, k=3), data=rbind(Dataup,Data[NROW(Data),1:3]))
  Data[nrow(Data),2] <- predict.gam(o, newdata=data.frame(pc1str=Data[nrow(Data),3]))
  
  #Second tipping point (F2):
  Data                <- rbind(Data, c("F2",NA,NA,0))
  Data[,2:ncol(Data)] <- sapply(Data[,2:ncol(Data)],as.numeric)
  #Data$vc<--Data$vc
  
  #calculate x-value (pc1str) of F2
  ff2 <- which.max(rowSums(Data[which(Data$year == round(TGAM$mr)+1):NROW(Data),3:4]))
  ff2 <- names(ff2)
  Data[nrow(Data),3]<-Data[ff2,3]+abs(Data[ff2,ncol(Data)]+0.1)
  #this x-value for F2 ensures that all Res estimates of lower branch will be non-negative.
  #0.05 is arbitrary to give some extra space
  
  #calculate y-value (pc1sys) of F2
  p <- gam(pc1sys~s(pc1str, k=3), data=rbind(Datalow,Data[NROW(Data),1:3]))
  Data[nrow(Data),2] <- predict.gam(p, newdata=data.frame(pc1str=Data[nrow(Data),3]))


  # ----------------------------------------
  # calculate the horizontal component of resilience
  # ----------------------------------------
  Data$hc     <- NA
  rr          <- 1:which(Data$year == round(TGAM$mr))
  Data$hc[rr] <- abs(Data$pc1str[nrow(Data)-1]-Data$pc1str[rr])

  rr          <- which(Data$year == round(TGAM$mr)+1):(nrow(Data)-2)
  Data$hc[rr] <- abs(Data$pc1str[rr]- Data$pc1str[nrow(Data)])
  Data$hc[(nrow(Data)-1):nrow(Data)] <- 0
  
  #calculate resilience
  Data$res  <- Data$hc - Data$vc
  Data$res[13] = 0  # manually set because it is unclear if part of first or second state
  
  #calculate relative resilience
  Data$relres <- Data$res/max(Data$res , na.rm =T)
  
  #prepare line extensions to tipping points
  newdata3 <- data.frame(pc1str=runif(100,Data$pc1str[NROW(Data)-1],min(Dataup$pc1str)))
  newdata4 <- data.frame(pc1str=runif(100,max(Datalow$pc1str),Data$pc1str[NROW(Data)]))
  x        <- predict.gam(o,newdata3)
  w        <- predict.gam(p,newdata4)
```

### plot results

``` r
  # -----------------------------
  # stability landscape
  # -----------------------------

  # adding two theoretical points at the beginning of 
  # each branch to stabilize the shape of the stability landscape:
 
  # unscale stressors
  Data$pc1str     <- Data$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')
  newdata1$pc1str <- newdata1$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')
  newdata2$pc1str <- newdata2$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')
  newdata3$pc1str <- newdata3$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')
  newdata4$pc1str <- newdata4$pc1str * attr(S1, 'scaled:scale') + attr(S1, 'scaled:center')

  #interpolate to build stability landscape
  xo=seq(min(Data$pc1str), max(Data$pc1str),length=500)
  yo=seq(min(Data$pc1sys), max(Data$pc1sys),length=500)
  
  xx<-interp(Data$pc1str,Data$pc1sys,-Data$relres,xo=xo,yo=yo)
  colnames(xx$z) <- yo
  rownames(xx$z) <- xo
  xx_df          <- suppressWarnings(reshape::melt(xx$z))


  #add 1994-95
  newdata <- rbind(c("1995",
                     2.862,0.785,0,NA,NA,NA),Data)
  newdata <- rbind(c("1994",3.166,
                         0.732,0,NA,NA,NA),newdata)
  Data1       <- Data
  Data        <- newdata
  Data$pc1sys <-as.numeric(Data$pc1sys)
  Data$pc1str <-as.numeric(Data$pc1str)
  Data$res    <-as.numeric(Data$res)
  Data$relres <-as.numeric(Data$relres)

  #Folded stability landscape
  years.char  <- paste((Data$year), sep = "")
  
  #So, here's the figure (first version):
  
  df2 <- data.frame(x = newdata2$pc1str[order(newdata2$pc1str)], y= z[order(newdata2$pc1str)])
  df1 <- data.frame(x = newdata1$pc1str[order(newdata1$pc1str)], y= y[order(newdata1$pc1str)])
  df3 <- data.frame(x = newdata3$pc1str[order(newdata3$pc1str)], y= x[order(newdata3$pc1str)])
  df4 <- data.frame(x = newdata4$pc1str[order(newdata4$pc1str)],y= w[order(newdata4$pc1str)])

  
  plot1 <- ggplot() + 
    geom_contour_filled(data = xx_df, aes(x = X1, y = X2, z = value), alpha=.7,bins=20)+
    geom_point(data = Data[(NROW(Data)-1):(NROW(Data)),], inherit.aes = F,
               aes(x = pc1str, y = pc1sys),size = 3)+
    geom_text(data  = Data[1:(NROW(Data)-2),], 
              aes(x = pc1str,y = pc1sys, label = years.char[1:(NROW(Data)-2)]), 
              size  = 3,
              alpha = .7)+
    geom_line(data = df1, aes(x=x,y=y),size=1.1)+
    geom_line(data = df2, aes(x=x,y=y),size=1.1)+
    geom_line(data = df3, aes(x=x,y=y),size=1.1,linetype="dotted")+
    geom_line(data = df4, aes(x=x,y=y),size=1.1,linetype="dotted")+
    geom_text(data = Data[(NROW(Data)-1),], 
              aes(x = pc1str,y = pc1sys,label = years.char[(NROW(Data)-1)]), 
              size  = 8,
              nudge_x = -.04)+
    geom_text(data = Data[NROW(Data),], 
              aes(x = pc1str,y = pc1sys, label = years.char[NROW(Data)]), 
              size = 8,
              nudge_x = .04)+
    coord_cartesian(
      ylim=c(min(Data$pc1sys)-0.2,
             max(Data$pc1sys)+0.2),
      xlim=c(min(Data$pc1str)-0.1,
             max(Data$pc1str)+0.1))+
    xlab("Stressors (chla_win; 2-year lag)")+
    ylab("Northern Spain community (PC1sys)")+
    theme_minimal()

plot1
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/IRA_STEP6-1.png" style="display: block; margin: auto;" />

## Stochastic cusp modelling (SCM)

**Camilla Sguotti(<camilla.sguotti@unipd.it> & Christian
Moellmann(<chritsian.moellmann@uni-hamburg.de>) **

### SCM Suumary

From Möllmann et al. [2021](https://doi.org/10.1038/s41598-021-93843-z):
*“SCM is based on catastrophe theory, popular in the 1970s, but recently
rediscovered in a number of research fields including fisheries
science.The cusp is one of seven geometric elements in catastrophe
theory and represents a 3D surface combining linear and non-linear
responses of a state variable to one control variable (called the
asymmetry variable) modulated by a second so-called bifurcation
variable. In SCM the cusp is represented by a potential function that
can be fit to data using the method of moments and maximum likelihood
estimators, and the state, asymmetry and bifurcation are canonical
variables fit themselves using linear models of observed quantities.
Importantly, using SCM we can identify hysteresis by distinguishing
between unstable (in fact bistable) and stable states in the dynamics of
the cod stock using a statistic called Cardan´s discriminant (see
“Methods”). Bistable dynamics exist in the non-linear part of the cusp
under the folded curve, where the state variable can flip between the
upper and lower shield, also called the cusp area (shaded in light blue
in the 3D—Supplementary Fig. S4—and 2D representations of the model
surface; Fig. 4a). Outside the cusp area the system is assumed to be
stable which indicates a high degree of irreversibility
(i.e. hysteresis). As suggested in the SCM literature, we conducted a
comprehensive model validation that revealed our fitted SCM to be
superior to alternative linear and logistic models, explaining a large
portion of the variability in the data and fulfilling additional
criteria for this model type to be valid.”*

![From Möllmann et al. 2021](Figs/fig1.png)

### Background

The stochastic cusp model was developed by the mathematician Rene Thom
in the 1980s and allows for modeling the dynamics of a state variable
depending on two interactive drivers. The model is based on a cubic
differential equation extended with a Wiener´s process to add
stochasticity.

 − *V*(*z*<sub>*t*</sub>,*α*,*β*) =  − 1/4*z*<sub>*t*</sub><sup>4</sup> + 1/2*β**z*<sub>*t*</sub><sup>2</sup> + *α**z*<sub>*t*</sub>

where *V*(*z*<sub>*t*</sub>,*α*,*β*) is a potential function whose slope
represents the rate of change of the system (the system state variable
is called *z*<sub>*t*</sub>), depending on the two control variables
(*α*, *β*).

 − *δ**V*(*z*,*α*,*β*)/*δ**z* = (−*z*<sub>*t*</sub><sup>3</sup>+*β**z*<sub>*t*</sub>+*α*)*d**t* + *σ*<sub>*z*</sub>*d**W*<sub>*t*</sub>

The two control variables, α and β, are the factors that can cause the
rise of non-linear discontinuous dynamics.*z* is the state variable (the
variable we want to model) and is modelled as a linear function of for
example a a time series of an ecosystem or a population, *α*is the
asymmetry variable that controls the dimension of the state variable.
Therefore, if *α* increases the state variable will change
accordingly.Instead, *β* is the so-called bifurcation variable and can
modify the relationship between *z*<sub>*t*</sub> and *α* from linear
and continuous to non-linear and discontinuous.

In the model *α*, *β* and *z* are modelled as linear functions of the
original variables we want to fit in the model such as fishing capacity,
temperature (SST), ecosystem state of the Northern Adriatic Sea (PC1 and
PC2). A combination of different drivers can also be used to model the
three factors fitted in the stochastic cusp model.

-   *α* = *α*<sub>0</sub> + *α*<sub>1</sub> Fishing Capacity
-   *β* = *β*<sub>0</sub> + *β*<sub>1</sub> SST
-   *z*<sub>*t*</sub> = *w*<sub>0</sub> + *w*<sub>1</sub> State of
    Northern Adriatic Ecosystem (PC1 and PC2)

where *α*<sub>0</sub>, *β*<sub>0</sub> and *w*<sub>0</sub> are the
intercepts, and *α*<sub>1</sub>,*β*<sub>1</sub> and *w*<sub>1</sub> the
slopes of the models.

The stochastic cusp model is thus able to detect three different types
of dynamics of the state variable, linear and continuous, non-linear and
continuous and discontinuous. Moreover the model compare the stochastic
cusp model to two continuous alternatives, a linear model and a logistic
model.

For more info see Sguotti et al., 2019, 2022; Moellmann et al., 2022;
Diks and Wang 2016, Petraitis et al., 2016; Dakos and Kefi 2022.

### Applying the model

Before applying the model we usually do the typical screening (change
point analysis, driver-state plots, bimodality of the state
variable).This preliminary screening allows us to hypothesize the
presence of tipping points in the state variable.

To confirm the presence of tipping points and in particular the presence
of hysteresis, we then perform the stochastic cusp model.

``` r
#load the dataset: PCA results and drivers of the Northern Adriatic Community. More info about how these data were processed 
#can be found in Sguotti et al., 2022 (Journal of Animal Ecology)


load("Data/data_ex.RData") 

# -----------------------------------
# fit the stochastic cusp model
# -----------------------------------
fit_community = cusp(y ~ pc2, alpha ~ Effort, beta ~ SST,  data=tot)

#summary of the model ouput
summary(fit_community, logist=T)

#the output show the coefficients of the three variables (alpha, beta and z). 
#At the end the output show the results of the three alternative models. 

plot(fit_community)
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/CUSP-1.png" style="display: block; margin: auto;" />

``` r
#plot the output. The grey area is the area below the fold or the area where 3 equilibria are possible two stables one unstable
#the blue dots show the high state of the state variable while the greeb dots the small state. 
#the dimension of the points show the probability of being in the same state the next year (but more analyses needed to fully understand how this is calculated)


###now I do various steps to improve the visualization of the model.
# -----------------------------------
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

# -----------------------------------
# extract the bifurcation set
# -----------------------------------
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
```

<img src="Tipping_points_getstarted_files/figure-markdown_github/CUSP-2.png" style="display: block; margin: auto;" />

``` r
####### Interpretation from the presentation and Sguotti et al., 2022 (Journal of Animal Ecology)
```

<!-- ### Download the data from Holsman et al. 2020 -->
<!-- ```{r thresholds, echo =T, eval=F, include = T} -->
<!--     cat("The download takes a few mins (large data files: 251.5 MB)...\n") -->
<!--     url <-   "https://figshare.com/ndownloader/files/24115769?private_link=81007e2dd5edee0a5a7a" -->
<!--     options(timeout = max(300, getOption("timeout"))) -->
<!--     dest_path  <-  file.path(main,"Data/data.zip") -->
<!--     download.file(url=url, destfile=dest_path,method="libcurl") -->
<!--     unzip(dest_path, exdir = ".",overwrite=T) -->
<!-- ``` -->
<!-- ### Explore the threshold function  -->
<!-- ```{r threshold_STEP0,eval=T,include=T,echo=T, results='hide',message=FALSE} -->
<!--    source("R/make.R")  # set up the Rcode and load packages and data -->
<!--    head(risk12)  # preview the risk table for "No cap" simulations -->
<!--    head(risk13)  # preview the risk table for "2 MT cap" simulations -->
<!--    C_thresh_12_1$thrsh_x # Temperature tipping point for pollock under "No cap" simulations -->
<!--    C_thresh_12_2$thrsh_x # Temperature tipping point for p cod under "No cap" simulations -->
<!--    C_thresh_12_3$thrsh_x # No tipping point was found for arrowtooth under "No cap" simulations -->
<!--  Biom_tmp <- list("No Cap" = c(  -->
<!--    B_thresh_12_1$thrsh_x, -->
<!--    B_thresh_12_2$thrsh_x, -->
<!--    B_thresh_12_3$thrsh_x), -->
<!--    "2 MT Cap" = c(  -->
<!--    B_thresh_13_1$thrsh_x, -->
<!--    B_thresh_13_2$thrsh_x, -->
<!--    B_thresh_13_3$thrsh_x)) -->
<!--  tmp <- list("No Cap" = c(  -->
<!--    C_thresh_12_1$thrsh_x, -->
<!--    C_thresh_12_2$thrsh_x, -->
<!--    C_thresh_12_3$thrsh_x), -->
<!--    "2 MT Cap" = c(  -->
<!--    C_thresh_13_1$thrsh_x, -->
<!--    C_thresh_13_2$thrsh_x, -->
<!--    C_thresh_13_3$thrsh_x)) -->
<!--  # get mean and var for tipping points: -->
<!--  mean(as.numeric(unlist(tmp))) -->
<!--  sd(as.numeric(unlist(tmp))) -->
<!--  # double check: -->
<!--  threshIN     <- C_thresh_13_2 -->
<!--  thrsh2_all   <- intersect(threshIN$signif2,threshIN$ix_pks) -->
<!--  df2_qnt      <- threshIN$df2_qnt -->
<!--  df2_qnt$tmp[(thrsh2_all[which( abs(df2_qnt$smoothed_mn[thrsh2_all])  ==  -->
<!--                                   max(abs(df2_qnt$smoothed_mn[thrsh2_all])) ) ] ) ] -->
<!--  df2_qnt$tmp[(thrsh2_all[which( abs(df2_qnt$smoothed_dwn[thrsh2_all]) ==  -->
<!--                                   max(abs(df2_qnt$smoothed_dwn[thrsh2_all])) )])  ] -->
<!--  df2_qnt$tmp[(thrsh2_all[which( abs(df2_qnt$smoothed_up[thrsh2_all])  ==  -->
<!--                                   max(abs(df2_qnt$smoothed_up[thrsh2_all]))  )])  ] -->
<!-- ``` -->
