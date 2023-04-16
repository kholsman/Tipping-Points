#'-------------------------------------
#'  plot_threshold
#'-------------------------------------
#' Get threshold for gam relationship based on Large et al. 2014
#' 
#' https://github.com/kholsman/EBM_Holsman_NatComm/blob/master/R/sub_fun/threshold.R
#' From Holsman etal 2020 
#' Holsman, K. K., A. Haynie, A. Hollowed, J.C.P. Reum, K. Aydin, A. J. Hermann, W. Cheng, 
#' A. Faig, J. N. Ianelli, K. Kearney, A. E. Punt, 2020.Ecosystem-based fisheries
#'  management forestalls climate-driven collapse, Nature Communications.
#' 
#'
#' @param thresh thresh is a threshold() object
#' @export 
#' @examples 

plot_threshold <- function(thresh){
   
  plot1<- ggplot(rbind(
    thresh$hat_qnt%>%select(driver = tmp,up=smoothed_up, mn=smoothed_mn, 
                     dwn=smoothed_dwn,sig)%>%mutate(method="a) smoothed gam (s(x))"),
    thresh$df1_qnt%>%select(driver = tmp,up=smoothed_up, mn=smoothed_mn, 
                     dwn=smoothed_dwn,sig)%>%mutate(method="b) First Deriv (s'(x)"),
    thresh$df2_qnt%>%select(driver = tmp,up=smoothed_up, mn=smoothed_mn,
                     dwn=smoothed_dwn,sig)%>%mutate(method="c) Second Deriv (s''(x)")))+
    geom_ribbon(aes(x=driver, ymin=dwn, ymax=up,fill=method))+
    facet_grid(method~.,scales="free_y")+
    geom_hline(yintercept=0,color="white")+
    scale_fill_viridis_d(begin = .8, end=.1)+
    theme_minimal()
  
  plot1 + geom_mark_rect(aes(x=driver, y=up,fill = sig,label = "sig. range"),color=FALSE)+
    geom_vline (xintercept = thresh$df2_qnt$tmp[thrsh2], color = "red") +
    theme(legend.position="none")+ ylab("")
  
  
}