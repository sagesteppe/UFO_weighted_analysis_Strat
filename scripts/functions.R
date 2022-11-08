plotWeigher <- function(x, pts){
  
  # this function moves an Original AIM sample design through the process of re-weighing
  # the acre points based on which base points were sampled or not. It utilizes the
  # 'adjwgt' function from spsurvey just for official book keeping. It mostly works as 
  # a wrapper to reduce the number of lines in a document dedicated to dealing with
  # multiple weighings. 
  
  pts_base <- filter(wsAcec_pts, str_detect(Panel, 'OverSample', negate = T))
  
  aim_sites <- pts_base %>%
    mutate(Plot.Status = if_else(Plot.Status == 'sampled', T, F)) %>% 
    pull(Plot.Status, stratum)
  aim_wgtcat <- pull(pts_base, stratum)
  
  aim_wgt <- wsAcec_OriginalWeights %>% # acreage and proportion give the same results. 
    select(PropTarget, Stratum) %>% 
    left_join(., pts_base %>% select(stratum), by = c('Stratum' = 'stratum')) %>% 
    mutate(Stratum = factor(Stratum, levels = (unique(aim_wgtcat))))  %>% 
    arrange(Stratum) %>% 
    pull(PropTarget, Stratum)
  
  aim_framesize <- pull(wsAcec_OriginalWeights, DesiredSS, Stratum)
  
  aim_sites <- aim_sites[order(names(aim_sites))] 
  aim_wgt <- aim_wgt[order(names(aim_wgt))] 
  aim_wgtcat <- aim_wgtcat[order(aim_wgtcat)]
  aim_framesize <- aim_framesize[match(unique(aim_wgtcat),names(aim_framesize))]
  
  aim_wgt <- aim_wgt[!is.na(names(aim_wgt))]
  
  res <- adjwgt(aim_wgt, aim_wgtcat, framesize = aim_framesize, sites = aim_sites) 
  names(res) <- aim_wgtcat
  
  res <- ifelse(res > 1, 1/res, res)
  
  
  # RES NOT SORTING CORRECTLY... LET'S RUN FULL MATCHES ON ALL THE DATA GOING INTO THE FUNCTION
  
  
  newWghts <- data.frame(
    'Stratum' = names(res),
    'Weight' = res) %>% 
    group_by(Stratum) %>% 
    filter(Weight < Inf) %>% 
    add_count(name = 'PlotsSampled') %>% 
    distinct(.keep_all = T) %>% 
    mutate(WghtPerPlot = Weight / PlotsSampled)
  
  newWghts1 <- data.frame(
    'Stratum' = names(res),
    'Weight' = res) %>% 
    group_by(Stratum, Weight) %>%
    filter(Weight == Inf) %>% 
    add_count(name = 'PlotsRejected')  %>% 
    distinct(.keep_all = T) %>% 
    ungroup %>% 
    select(-Weight)
  
  newWghts <- left_join(newWghts, newWghts1) %>% 
    left_join(., OriginalWeights %>% 
                select(TotalAcres = Acres, Rejected) ) %>% 
    arrange(-Weight) %>%  # this works to catch any
    group_by(Stratum) %>% # values which may be introduced
    slice_head(n = 1) %>%  # by an empty second data.frame
    mutate(WgtAcres = WghtPerPlot * TotalAcres, 
           AreaInference = TotalAcres * Weight)  %>% 
    select(Stratum, TotalAcres, AreaInference, WgtAcres, 
           PlotsSampled, PlotsRejected, PropInference = Weight, WghtPerPlot) 
  
  
  return(newWghts)
  
}
