
#' calculate acres weights of an orginal plot design
#' 
#' this function takes the intended weights of a sample design and calculates
#' the acre weights for each plot in each panel. If the number of plots is 
#' exceeded by the number of panels, than the proportion of the startum to which 
#' inference may be made is returned. This is basically a way to avoid having
#' to copy over the 
#' 
#' @param sample_area a vector data file displaying the extent of the target frame
#' @param strat_raster a categorical raster containing stratification variables 
#' @param pt_data a the intial aim sample points

originalWeights <- function(sample_area, strat_raster, pt_data){
  
  focal_pts <- pts[unlist(st_intersects(sample_area, st_transform(pt_data, st_crs(sample_area)))),]
  
  # identify the cover of each stratum in the focal area
  areaSpat <- vect(sample_area) 
  focal_areas <- terra::expanse(areaSpat, unit = 'ha') * 2.47105 # conversion to acres
  TotalFocalArea <- sum(focal_areas)
  focal_areas <- bind_cols(sample_area, Total_Area =  focal_areas, TotalFocalArea = TotalFocalArea)
  
  areaSpat <- project(areaSpat, crs(strat_raster))
  
  raster_cells <- extract(strat_raster, areaSpat) %>% 
    group_by(ID, BPS_CODE) %>% 
    count() %>% 
    ungroup(BPS_CODE) %>% 
    mutate(PropArea = n/sum(n))  %>% 
    left_join(., lookup_table, by = c('BPS_CODE' = 'RasterValue'))
  
  rm(areaSpat, TotalFocalArea)
  
  focal_areas <- left_join(focal_areas, raster_cells) %>% 
    mutate(Area = Total_Area * PropArea)  %>% 
    select(any_of(c('ID', 'NAME', 'TYPE', 'Total_Area', 'Code', 'PropArea', 'Area', 'TotalFocalArea')))
  
  area_summary <- focal_areas %>% 
    ungroup() %>% 
    group_by(Code) %>% 
    mutate(TotalArea = sum(Area)) %>% 
    ungroup() %>% 
    distinct(Code, .keep_all = T) %>% 
    select(Stratum = Code, TotalArea, PropArea, TotalFocalArea) %>%
    mutate(PropArea = TotalArea/sum(TotalArea)) %>% 
    st_drop_geometry()
  
  base_target_pts <- focal_pts %>% 
    filter(str_detect(Panel, 'OverSample', negate = T))
  
  # need to calculate the desired sites per stratum as function of plot weight. 
  area_summary <- area_summary %>% 
    left_join(., deSS, by = 'Stratum') %>%  # desired ss total_area 
    mutate(DesiredSS = round(DesiredSS * (nrow(base_target_pts)/255))) %>% 
    drop_na()
  
  pt_draw <- focal_pts %>% 
    group_by(stratum, Plot.Status) %>% 
    count() %>% 
    st_drop_geometry() %>% 
    pivot_wider(id_cols = stratum, names_from = Plot.Status, values_from = n, 
                values_fill = 0) %>% 
    rowwise() %>% 
    mutate(total = sum(across(not_sampled:sampled)), .before = 'not_sampled')
  
  OriginalWeights <- left_join(area_summary, pt_draw,
                               by = c('Stratum' = 'stratum')) %>% 
    select(any_of(c(Stratum, Total = total, NotSampled = not_sampled, Rejected = rejected, 
           Sampled = sampled, DesiredSS, PropArea, PropTarget, Acres =  TotalArea))) %>% 
    mutate(ApproxStWgt =  if_else(DesiredSS >= 5, (Acres/DesiredSS) * 5, (DesiredSS/5) * Acres))
  
  obs <- list(focal_pts, OriginalWeights)
  return(obs)
  
}


#' calculate realized weight acres of aim plots
#' 
#' this function moves an Original AIM sample design through the process of re-weighing
#' the acre points based on which base points were sampled or not. It utilizes the
#' 'adjwgt' function from spsurvey just for official book keeping. It mostly works as 
#' a wrapper to reduce the number of lines in a document dedicated to dealing with
#' multiple weighings in a sample design. 
#' 
#' @param OrigWeights a dataframe of the output of the originalWeigher function
#' @param pts an sf tibble of all drawn (at least) base points for the sample 
#' design with plot fates in a column ('sampled', 'rejected').

plotWeigher <- function(OrigWeights, pts){
  
  pts_base <- filter(pts, str_detect(Panel, 'OverSample', negate = T))
  
  aim_sites <- pts_base %>%
    mutate(Plot.Status = if_else(Plot.Status == 'sampled', T, F)) %>% 
    pull(Plot.Status, stratum)
  aim_wgtcat <- pull(pts_base, stratum)
  
  aim_wgt <- OrigWeights %>% # acreage and proportion give the same results. 
    select(PropTarget, Stratum) %>% 
    left_join(., pts_base %>% select(stratum), by = c('Stratum' = 'stratum')) %>% 
    mutate(Stratum = factor(Stratum, levels = (unique(aim_wgtcat))))  %>% 
    arrange(Stratum) %>% 
    pull(PropTarget, Stratum)
  
  aim_framesize <- pull(OrigWeights, DesiredSS, Stratum)
  
  aim_sites <- aim_sites[order(names(aim_sites))] 
  aim_wgt <- aim_wgt[order(names(aim_wgt))] 
  aim_wgtcat <- aim_wgtcat[order(aim_wgtcat)]
  aim_framesize <- aim_framesize[match(unique(aim_wgtcat),names(aim_framesize))]
  aim_wgt <- aim_wgt[!is.na(names(aim_wgt))]
  
  res <- adjwgt(aim_wgt, aim_wgtcat, framesize = aim_framesize, sites = aim_sites) 
  names(res) <- aim_wgtcat
  res <- ifelse(res > 1, 1/res, res)
  
  newWghts <- data.frame(
    'Stratum' = names(res),
    'Weight' = res) %>% 
    group_by(Stratum) %>% 
    filter(Weight > 0) %>% 
    add_count(name = 'PlotsSampled') %>% 
    distinct(.keep_all = T) %>% 
    mutate(WghtPerPlot = Weight / PlotsSampled)
  
  newWghts1 <- data.frame(
    'Stratum' = names(res),
    'Weight' = res) %>% 
    group_by(Stratum, Weight) %>%
    filter(Weight == 0) %>% 
    add_count(name = 'PlotsRejected')  %>% 
    distinct(.keep_all = T) %>% 
    ungroup %>% 
    select(-Weight)
  
  newWghts <- left_join(newWghts, newWghts1) %>% 
    left_join(., OrigWeights %>% 
                select(TotalAcres = Acres, Rejected, Stratum) ) %>% 
    arrange(-Weight) %>%  # this works to catch any
    group_by(Stratum) %>% # values which may be introduced
    slice_head(n = 1) %>%  # by an empty second data.frame
    mutate(WgtAcres = WghtPerPlot * TotalAcres, 
           AreaInference = TotalAcres * Weight)  %>% 
    select(Stratum, TotalAcres, AreaInference, WgtAcres, 
           PlotsSampled, PlotsRejected, PropInference = Weight, WghtPerPlot)  %>% 
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
  
  return(newWghts)
  
}
