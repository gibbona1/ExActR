library(shiny)

extentMat <- reactive({
  if(any(sapply(mapIds(), plot_wait)))
    return(NULL)
  
  res_l <- list()
  
  for(id in mapIds()[-1]){
    df1 <- sfs[[paste0(id - 1)]] %>% st_cast("POLYGON") %>% st_make_valid()
    df2 <- sfs[[paste0(id)]] %>% st_cast("POLYGON") %>% st_make_valid()
    
    code_grps <- codeGroups()
    
    st_intersection_faster <- function(x,y, ...){
      #faster replacement for st_intersection(x, y,...)
      ##https://github.com/r-spatial/sf/issues/801
      
      #y_subset <- s2::s2_intersects_matrix(x,y) %>% unlist() %>% {y[.,]}
      y_subset <-
        st_intersects(x, y) %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        {y[.,]}
      
      st_intersection(x$geometry, y_subset$geometry, ...)
    }
    
    cross_area <- function(grp1, grp2) {
      df1_sub <- filter(df1, (df1[[input[[sprintf("map%s_sel_col", id-1)]]]] %>% code_lookup) == grp1)
      df2_sub <- filter(df2, (df2[[input[[sprintf("map%s_sel_col", id)]]]] %>% code_lookup) == grp2)
      #st_intersection_faster(df1_sub, df2_sub) %>% clean_sum()
      st_intersection(df1_sub, df2_sub) %>% clean_sum()
    }
    
    cross_area2 <- function(i) {
      tryCatch(
        expr <- {
          grp1 <- comb_df[i,1]
          grp2 <- comb_df[i,2]
          print(paste0(i,"/", nrow(comb_df), ": ", grp1, "-", grp2))
          df1_sub <- filter(df1, (df1[[input[[sprintf("map%s_sel_col", id-1)]]]] %>% code_lookup) == grp1)
          df2_sub <- filter(df2, (df2[[input[[sprintf("map%s_sel_col", id)]]]] %>% code_lookup) == grp2)
          if(object.size(df1_sub) > 10^6 & object.size(df2_sub) > 10^6)
            return(NA)
          return(st_intersection_faster(df1_sub, df2_sub) %>% clean_sum())
        },
        error = function(e) {
          message("Timeout. Skipping.")
          return(NA)
        },
        TimeoutException = function(ex){
          message("Timeout Exception. Skipping.")
          return(NA)
        })
      #st_intersection(df1_sub, df2_sub) %>% clean_sum()
    }
    
    cross_area3 <- function(i) {
      grp1 <- comb_df[i,1]
      grp2 <- comb_df[i,2]
      print(paste0(i,"/", nrow(comb_df), ": ", grp1, "-", grp2))
      df1_sub <- filter(df1, (df1[[input[[sprintf("map%s_sel_col", id-1)]]]] %>% code_lookup) == grp1) %>% st_make_valid()
      df2_sub <- filter(df2, (df2[[input[[sprintf("map%s_sel_col", id)]]]] %>% code_lookup) == grp2) %>% st_make_valid()
      crop <- st_crop(df1_sub ,df2_sub) %>% st_make_valid()
      notvalid <- which(s2::s2_is_valid_detail(crop)==FALSE)
      if(length(notvalid) > 0){crop <- crop[-notvalid,]}
      crop %>% st_intersection_faster(df2_sub) %>% st_make_valid() %>% clean_sum()
      #st_intersection(df1_sub, df2_sub) %>% clean_sum()
    }
    
    cross_area4 <- function(g1, g2){
      cat(g2$CODE_06[1], "-", sep = "")
      crop <- st_crop(g1,g2) %>% st_make_valid()
      notvalid <- which(s2::s2_is_valid_detail(crop)==FALSE)
      if(length(notvalid) > 0){crop <- crop[-notvalid,]}
      crop %>% st_intersection_faster(g2) %>% st_make_valid() %>% clean_sum()
      #st_intersection_faster(g1, g2) %>% 
      #  #st_make_valid() %>% 
      #  clean_sum() %>%
      #  lazy_unlist()
    }
    
    gc()
    res <- lapply(split(df1, df1$CODE_00), function(g1) {
      cat("\n", g1$CODE_00[1], ": (", object.size(g1), ")\n\t", sep = "")
      sapply(split(df2, df2$CODE_06), function(g2) cross_area4(g1, g2) %>% lazy_unlist())
    }) 
    
    cross_mat <- do.call(rbind, res)
    #cross_mat <- do.call(rbind, lapply(code_grps, function(grp1) {
    #  sapply(code_grps, function(grp2) {
    #    lazy_unlist(cross_area(grp1,grp2))
    #  })})
    #)
    #gc()
    #comb_df <- expand.grid(grp1 = code_grps, grp2 = code_grps)
    #cross   <- sapply(1:nrow(comb_df), function(i) {
    #  #tryCatch({res <- withTimeout({ lazy_unlist(cross_area3(i))}, timeout = 10, onTimeout = "silent")
    #  withTimeout({res <- lazy_unlist(cross_area2(i))}, timeout = 10, onTimeout = "silent", substitute = FALSE)
    #  if(is.null(res))
    #    return(NA)
    #  return(res)
    #})
    
    rownames(cross_mat) <- colnames(cross_mat) <- code_grps
    
    cross_mat <- cross_mat / 10^4
    
    cross_df  <- as.data.frame(cross_mat)
    
    cross_df$openings      <- rowSums(cross_df)
    cross_df["closings", ] <- colSums(cross_df)
    
    res_l[[paste0(id)]] <- cross_df
  }
  return(res_l)
})