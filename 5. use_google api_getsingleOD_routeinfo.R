
#################################################################################################

## 1. 透過google api ( route(from, to)、獲得各od的旅次資訊如route(路徑)及step(節點)data，目的為其資料可透過ggmap畫出各OD路徑圖
## 2. 目前遭遇到的困難為，因google api應用流向限制( this can be due to Google gating api calls)，解決方案可參考：
##    http://social.gseosem.com/r-mapping-multiple-routes-using-ggmap/

#################################################################################################

##     example

        
        from <- "houson, texas"
        to <- "waco, texas"
        route_df <- route(from, to, structure = "route")
        qmap("college station, texas", zoom = 8) +geom_path(aes(x = lon, y = lat),  
                                                            colour = "red", 
                                                            size = 3,
                                                            data = route_df, 
                                                            lineend = "round")
        
        ## Not run:
        (legs_df <- route("houston","galveston"))
        legs2route(legs_df)
        (legs_df <- route(
          "marrs mclean science, baylor university",
          "220 south 3rd street, waco, tx 76701", # ninfa"s
          alternatives = TRUE))
        legs2route(legs_df)
        
        from <- "houson, texas"
        to <- "waco, texas"
        legs_df <- route(from, to)
        qmap("college station, texas", zoom = 8) +
          geom_segment(
            aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
            colour = "red", size = 1.5, data = legs_df
          )
        # notice boxy ends
        qmap("college station, texas", zoom = 8) +
          geom_leg(
            aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
            colour = "red", size = 1.5, data = legs_df
          )
        # notice overshooting ends
        route_df <- legs2route(legs_df)
        qmap("college station, texas", zoom = 8) +
          geom_path(
            aes(x = lon, y = lat),
            colour = "red", size = 1.5, data = route_df, lineend = "round"
          )
        ## End(Not run)
        
        
        ## Not run: # to cut down on check time
        
        from <- "houson, texas"
        to <- "waco, texas"
        route_df <- route(from, to, structure = "route")
        qmap("college station, texas", zoom = 8) +
          geom_path(
            aes(x = lon, y = lat), colour = "red", size = 1.5,
            data = route_df, lineend = "round"
          )
        qmap("college station, texas", zoom = 6) +
          geom_path(
            aes(x = lon, y = lat), colour = "red", size = 1.5,
            data = route_df, lineend = "round"
          )
        
        routeQueryCheck()
        
        (legs_df <- route(
          "marrs mclean science, baylor university",
          "220 south 3rd street, waco, tx 76701", # ninfa"s
          alternatives = TRUE))
        qmap("424 clay avenue, waco, tx", zoom = 15, maprange = TRUE, maptype = "hybrid",
             base_layer = ggplot(aes(x = startLon, y = startLat), data = legs_df)) +
          geom_leg(
            aes(x = startLon, y = startLat, xend = endLon, yend = endLat, colour = route),
            alpha = 3/4, size = 2, data = legs_df
          ) +
          scale_x_continuous(breaks = pretty(c(-97.1325,-97.119),4), lim = c(-97.1325,-97.119)) +
          facet_wrap(~ route) + theme_bw() +
          labs(x = "Longitude", y = "Latitude", colour = "Routes")
        ## End(Not run)

    #######利用google api (mapdist) 計算路徑長度/旅行時間資訊
        
        mapdist("waco, texas", "houston, texas")
        from <- c("houston, texas", "dallas")
        to <- "waco, texas"
        mapdist(from, to)
        mapdist(from, to, mode = "bicycling")
        mapdist(from, to, mode = "walking")
        from <- c("houston", "houston", "dallas")
        to <- c("waco, texas", "san antonio", "houston")
        mapdist(from, to)
        mapdist("the white house", "washington monument", mode = "walking")
        # geographic coordinates are accepted as well
        (wh <- as.numeric(geocode("the white house", source = "google")))
        (wm <- as.numeric(geocode("washington monument", source = "google")))
        mapdist(wh, wm, mode = "walking")
        mapdist("the white house", wm, mode = "walking")
        distQueryCheck()
        
     ######################

#####################################################################################################

##        start here~

centerpoint="24.97627, 121.27003"

for(i in 37:50){
  
  route_od<-joindb1[i,]
  
  from_lon<-route_od$o_lon
  from_lat<-route_od$o_lat
  
  to_lon<-route_od$d_lon
  to_lat<-route_od$d_lat 
  
  from<-paste(as.character(from_lat),as.character(from_lon),sep=",")
  to<-paste(as.character(to_lat),as.character(to_lon),sep=",")
  
  legs_df <-route(from, to)
  
  #Colors=grDevices::rainbow(13)
  
  path=geom_segment(
    aes(x = startLon, y = startLat, xend = endLon, yend = endLat),
    colour = "red", size = 1.5, data = legs_df
  )
  # lineend = "round")
  
  
  if(i==37){
    od_map=qmap(centerpoint, zoom = 11)+path
  }else{      
    (od_map=od_map+path)
    
    rm(path)
    rm(rout_od)
  }
  
}

od_map
rm(od_map)





