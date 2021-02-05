
function(input, output, session){
  
  ##leaflet interactive map
  output$map <- renderLeaflet({
    validate(
      need(input$lat, label = "Please input Latitude"),
      need(input$long, label = "Please input Longitude")
    )

    leaflet() %>%
      addTiles() %>% setView(lng = input$long, lat = input$lat, zoom = 7) %>% addMarkers(lng = input$long, lat = input$lat)
  })
  
  
  ##record coordinates by user click
  observe({
    p <-input$map_click
    if (is.null(p))
      return()
    else if (!is.null(p)) {
      leafletProxy("map") %>% clearMarkers() %>% 
        addMarkers(lng = p$lng, lat = p$lat) %>% setView(lng = p$lng, lat = p$lat, zoom = 7)
      updateNumericInput(session, "lat", value = round(p$lat, 3))
      updateNumericInput(session, "long", value= round(p$lng, 3))}
    })
  
  ##Control Nominal power by two input methods
  observe(
    if (input$calc == "TRUE"){
      output$mSTC <- renderText(paste("mSTC(W) = ", input$Vmn*input$Imn))
      NPP <- round(input$Vmn*input$Imn*input$Nms*input$Nmp*0.001, 2)
      output$NPP <- renderText(paste("NPP(kWp) = ", NPP))
    })

  ##Control Optimized Inclination Angle on the UI page
  output$angle <- renderUI(
    numericInput("beta", "Inclination angle (Optimum Angle is pre-filled)*", value = round(input$lat)-10, min = 0, max = 90))
  
  ## Start Modeling Session
  


  observeEvent(input$submit, {
    show_modal_spinner(
      spin = "cube-grid",
      color = "#00A08A"
    )
    ## Reorganize parameters
    lat <- input$lat
    long <- input$long
    
    ## Change input$Ki from charactor to Vector
    Ki <- as.numeric(unlist(strsplit(input$Ki,",")))
    
    ## Calculate Nameplate Energy (Kwp)
    if(input$calc == "TRUE"){
      nominal <- input$Vmn*input$Imn*input$Nms*input$Nmp*0.001}
    else if (input$calc == "FALSE"){
      nominal <- input$nominal}
    
    ## sc: scaling number
    ## p0: module plate power(Kwp)
    p0 <- input$Vmn*input$Imn*132/1000
    sc <- nominal/p0
    
    if (input$meteo == "NSRDB_TMY"){
      ## NSRDB PSM TMY Meteo Data
      m <- new_handle()
      handle_setopt(m, ssl_verifypeer = 0)
      
      url.nrel <- paste0('https://developer.nrel.gov/api/nsrdb/v2/solar/psm3-tmy-download.csv?api_key=8lnzqf01yYdcmDLN2EBb9RGgYRcIOmGrcVQ34YrX&wkt=POINT('
                         ,long,'%20',lat,
                         ')&attributes=dhi%2Cdni%2Cghi%2Cdew_point%2Cair_temperature%2Csurface_pressure%2Cwind_direction%2Cwind_speed%2Csurface_albedo&names=tmy&full_name=Jinghan%20Tian&email=jinghan.tian@duke.edu&affiliation=Duke&mailing_list=false&reason=test&utc=false')
      
      tmy <- read.table(curl(url.nrel, handle = m), skip = 2, header = TRUE, sep = ',', fill = TRUE, skipNul = TRUE,
                        colClasses = c("NULL", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                       "NULL", "numeric", "NULL", "NULL", "NULL"))
      
      names(tmy) <- c("Month", "Day", "Hour", "Minute", "DHI", "DNI", "G0", "Ta")
      
      idxLocal <- with(tmy, as.POSIXct(paste(paste(Month, Day, sep = "/"), paste(Hour, Minute, sep=':')), 
                                       format='%m/%d %H:%M'))
      
      ## Generate zoo -- TMY Meteo Data time-series
      NRELMeteo <- zoo(tmy[, c("G0", "DHI","DNI", "Ta")], idxLocal)
      Meteo <- NRELMeteo
    }
    
    else if (input$meteo == "PVGIS_TMY"){
      url <- paste0("https://re.jrc.ec.europa.eu/api/tmy?lat=",lat,"&lon=", long,"&outputformat=json")
      file <- fromJSON(file = url)
      data <- do.call(rbind, lapply(file$outputs$tmy_hourly, data.frame, stringsAsFactors = FALSE))
      data.df <- data[,c(4,5,6,2)]
      names(data.df) <- c("G0", "DNI", "DHI", "Ta")
      
      time <- seq(
        from=as.POSIXct("2020-1-1 0:00", tz="UTC"),
        to=as.POSIXct("2020-12-31 23:00", tz="UTC"),
        by="hour"
      )
      time <- time[-c(1417:1440)]
      local <- local2Solar(time, lon = long)
      
      PVGISMeteo <- zoo(data.df, local)
      
      Meteo <- window(PVGISMeteo, start = as.Date("2020-1-1"), end = as.Date("2020-12-31"))
    }
    
    ##Generate Meteo Monthly data)
    Meteo_Mon <- aggregate(Meteo, by=month, sum)
    Meteo_Mon$Ta <- round(Meteo_Mon$Ta/720, digits = 1)
    
    
    
    ## Calculate Energy Prod (ProdGCPV) based on TMY
    ##The solaR packge only support one inverter setting
    ##Calculate Energy Production of default Nominal Size (26.4Kwp)
    prodGC<- prodGCPV(lat, modeTrk = input$modTrek, modeRad = 'bdI',
                      sample = "hour",dataRad = Meteo, keep.night = TRUE,
                      beta = input$beta, betaLim = input$betaLim, alfa = input$alfa,
                      alb = input$alb, iS = input$iS,
                      module = list(Vocn = input$Vocn, Iscn = input$Iscn,
                                    Vmn = input$Vmn, Imn = input$Imn,
                                    Ncs = input$Ncs, Ncp = input$Ncp,
                                    CoefVT = input$CoefVT, TONC = input$TONC),
                      inverter = list(Ki = Ki, Pinv = input$Pinv,
                                      Vmin = input$Vmin, Vmax = input$Vmax,
                                      Gumb = input$Gumb),
                      effSys = list(ModQual = input$ModQual, ModDistp = input$ModDisp,
                                    OhmDC = input$OhmDC, OhmAC = input$OhmAC,
                                    MPP = input$MPP))
    
    
    prodGC_D <- as.zooD(prodGC, complete = TRUE)
    prodGC_M <- as.zooM(prodGC)
    prodGC_I <- replace_na(as.zooI(prodGC)[,"Pac"], 0)
    prodGC_I <- prodGC_I*sc/1000
    
    ## Balances and Main Results
    summary <- aggregate(prodGC_D[,c("G0d", "Gd", "Gefd", "Eac", "Edc", "Yf")], by=month, sum)
    summary <- as.data.frame(summary)
    summary[,c("G0d", "Gd", "Gefd", "Eac", "Edc")] <- summary[,c("G0d", "Gd", "Gefd", "Eac", "Edc")]/1000
    area <- nominal*100/input$effSTC
    summary$Tamb <- Meteo_Mon$Ta
    summary$Eac <- summary$Eac*sc
    summary$Edc <- summary$Edc*sc
    summary$EffInv <- summary$Eac*100/summary$Edc
    summary$EffArrR <- summary$Edc*100/(summary$Gd*area)
    summary$EffSysR <- summary$Eac*100/(summary$Gd*area)
    summary$Ya <-summary$Edc/nominal
    summary$Yr <- summary$Gd
    summary$PR <- summary$Yf*100/summary$Yr
    summary$CUF <- summary$Yf/7.2
    
    summary$Month <- factor(month.abb, levels = c(month.abb, "YEAR"))
    summary <- subset(summary, select = c(15, 1:3, 7, 5, 4, 8:12, 6, 13, 14))
    
    YEAR <- list("YEAR", sum(summary$G0d),sum(summary$Gd),sum(summary$Gefd),mean(summary$Tamb),
                 sum(summary$Edc), sum(summary$Eac), mean(summary$EffInv), mean(summary$EffArrR),
                 mean(summary$EffSysR), sum(summary$Ya), sum(summary$Yr), sum(summary$Yf),
                 mean(summary$PR), mean(summary$CUF))
    sumTable <- rbind(summary, YEAR)
    names(summary) <- c("Month", "GlobHor", "GlobInc",
                        "GlobEff","Temp",
                        "Edc_Array", "Eac_Grid",
                        "Inv_Eff", "Arr_Eff",
                        "Sys_Eff", "Ya","Yr", 
                        "Yf", "PR","CUF")
    sumTable <- format(sumTable, digits = 4, big.mark = ",", small.mark = ".", big.interval = 3)
    names(sumTable) <- c("Month", "GlobHor(kW/m2)", "GlobInc(kW/m2)",
                         "GlobEff(kW/m2)","Temp",
                         "Edc_Array(kWh)", "Eac_Grid(kWh)",
                         "Inv_Eff(%)", "Arr_Eff(%)",
                         "Sys_Eff(%)", "Ya","Yr", 
                         "Yf", "PR(%)","CUF(%)")
    
    output$sum <- renderTable(sumTable, striped = TRUE, hover = TRUE)
    
    ## Grid Connected Solar System Specifications
    
    a <- c("Type", "Latitude", "Longitude","Meteo Database", "Nominal Power(Kwp ac)", "Area/m2", "System Loss(%)", "Mode Trek"," Inclination Angle*")
    b <- c("Grid-Connected", input$lat, input$long, input$meteo, nominal, area, input$effSTC, input$modTrek, input$beta)
    
    specs.df <- data.frame(a, b)
    output$specs <- renderTable(specs.df, colnames = FALSE, bordered = FALSE, striped = FALSE)
    
    ## Balances and Main Results Plot
    output$pvOutput <- renderPlot({
      ggplot() + geom_col(data = summary, mapping=aes_string(x="Month", y=input$baseplot), fill = "#00A08A") + theme_minimal() +
        theme(axis.text = element_text(size = 12, family = 'serif'),
              axis.title = element_blank())
    })
    
    ## Normalized Energy Plot
    Norm <- summary[, c("Month","Yf", "Ya", "Yr")]
    Norm$Lc <- Norm$Yr - Norm$Ya
    Norm$Ls <- Norm$Ya - Norm$Yf
    Norm.df <- reshape2::melt(Norm[,c("Month","Lc", "Ls", "Yf")], id.vars = "Month", measure.vars = c("Lc","Ls","Yf"), variable.name = "Yield")
    Norm.df$value <- Norm.df$value/30
    
    output$NormEnergy <- renderPlot({
      ggplot() + geom_col(data = Norm.df, mapping=aes(x=Month, y=value, fill = Yield)) + ylab("Normalized Energy(kwh/kwp/day)") + 
        theme_minimal() + theme(axis.text = element_text(size = 12, family = 'serif', face= "bold"),
                                axis.title.y = element_text(size = 14, family = 'serif', face= "bold", color="black"),
                                axis.title.x = element_blank())
    })
    
    
    
    ## Time Seris Data plot
    
    ## Render Sunpath
    SolD <- fSolD(lat, BTd = fBTd(mode = "prom"))
    SolI <- fSolI(SolD, sample = "10 min", keep.night = FALSE)
    
    col = c("darkgreen","forestgreen", "green","gold","tan1", "orange","tomato", "red",
            "purple", "royalblue","mediumblue","midnightblue")
    names(col) = c(1:12)
    mon = month.abb
    
    output$sunPath <- renderPlot({
      xyplot(r2d(AlS) ~ r2d(AzS), groups = month, data = SolI, type = "l",
             xlab = expression(psi[s]), ylab = expression(gamma[s]), 
             col = col,
             panel=panel.superpose,
             panel.groups=function(x,y, group.number,...){
               panel.xyplot(x,y,...)
               idx <- round(length(x)/2 + 1)
               panel.text(x[idx], y[idx], mon[group.number], pos = 3,
                          col = col[group.number], offset = 0.2, cex = 0.8)})
    })
    
    ## TMYMeteo Plot
    
    ts <- Meteo[,c("G0","DNI","DHI")]
    ts$Pac <- prodGC_I
    meteo <- data.frame(Date = index(ts), ts)
    
    output$TMYMeteo <- renderPlotly({
      
      fig <- plot_ly(meteo, x = ~Date)
      fig <- fig%>% add_lines(y = ~ G0, name = "GHI(G0)", yaxis = "y",
                              line = list(color = 'rgb(255, 127, 14)', width = 2))
      fig <- fig%>% add_lines(y = ~ DNI, name = "DNI", yaxis = "y",
                              line = list(color = 'rgb(31, 119, 180)', width = 2))
      fig <- fig%>% add_lines(y = ~ DHI, name = "DHI",yaxis = "y",
                              line = list(color = 'rgb(44, 160, 44)', width = 2))
      fig <- fig%>% add_lines(y = ~ Pac, name = "Pac", yaxis = "y2",
                              line = list(color = 'rgb(214, 39, 40)', width = 2))
      
      fig <- fig %>% layout(
        yaxis = list(title = "Irradiation(W/m2)"),
        yaxis2 = list(overlaying = "y",
                      side = "right",
                      title = "Pac(kw)",
                      showgrid = FALSE),
        legend = list(orientation = "h",   # show entries horizontally
                      xanchor = "center",  # use center of legend as anchor
                      x = 0.5,
                      y = -0.8,
                      yanchor = "bottom"),
        margin = list(l = 50, r = 50, b = 50, t =50, pad = 4),
        xaxis = list(
          title = '',
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 day",
                step = "day",
                stepmode = "todate"),
              list(
                count = 7,
                label = "1 week",
                step = "day",
                stepmode = "todate"),
              list(
                count = 1,
                label = "1 month",
                step = "month",
                stepmode = "backward"),
              list(step = "all"))),
          rangeslider = list(type = "day", thickness = 0.2))
          )
      return(fig)
    })
    
    ## Irradiation Plot
    
    ## pvwatts result from NREL api
    
    list1 <- list(0,1,2,3,4)
    names(list1) <- c("fixed", "fixed_roof", "horiz", "horiz_bt", "two")
    array_type = list1[[input$modTrek]]
    azimuth = input$alfa + 180
    
    if (input$modTrek == "fixed"){
      tilt= input$beta}
    else tilt = 0
    
    pvwatts_url <- paste0("https://developer.nrel.gov/api/pvwatts/v6.json?api_key=8lnzqf01yYdcmDLN2EBb9RGgYRcIOmGrcVQ34YrX&lat=",lat,
                          "&lon=",long,"&system_capacity=",nominal,"&azimuth=",azimuth,"&tilt=",tilt,
                          "&array_type=", array_type,"&module_type=",1,"&losses=",input$effSTC)
    
    pvwatts <- fromJSON(file = pvwatts_url)
    poa_monthly <- pvwatts$outputs["poa_monthly"]
    dc_monthly <- pvwatts$outputs["dc_monthly"]
    ac_monthly <- pvwatts$outputs["ac_monthly"]
    pvwatts.df <- data.frame(month.abb, poa_monthly, dc_monthly, ac_monthly)
    pvwatts.df$Model = "PVWatts"
    
    solaR.df <- summary[,c(1,3,6,7)]
    solaR.df$Model <- "solaR"
    names(pvwatts.df) <- names(solaR.df)
    compare.df <- rbind(solaR.df, pvwatts.df)
    names(compare.df) <- c("Month","POA_GlobInc", "Edc_Array", "Eac_Grid", "Model")
    
    compTable <- data.frame(
      c("Annual POA-GlobInc Irradiance(kWh/m2)", "Annual Edc at Array(kWh)",
        "Annual Eac to Grid(kWh)", "Capacity Factor(%)"),
      c(sumTable[13,3], sumTable[13,6],
        sumTable[13,7], sumTable[13,15]),
      c(sum(pvwatts$outputs$poa_monthly), sum(pvwatts$outputs$dc_monthly),
        pvwatts$outputs$ac_annual, pvwatts$outputs$capacity_factor))
    names(compTable) <- c("Item", "solaR", "PVWatts")
    
    compTable <- format(compTable, digits = 4, big.mark = ",", small.mark = ".", big.interval = 3)
    
    output$compareTable <- renderTable(compTable, striped = FALSE)
    
    output$plot_compare <- renderPlot({
      ggplot() + geom_col(data = compare.df, mapping=aes_string(x="Month", y=input$item_pvwatts, fill = "Model"), position = position_dodge()) + 
        ylab("Comparison") + theme_minimal() +
        theme(axis.text = element_text(size = 12, family = 'serif'),
              axis.title = element_blank())
    })
    remove_modal_spinner()
  })
      
 
  observeEvent(input$solaR, {
    
    if(input$submit == 0 & input$solaR == 'result'){
      showModal(modalDialog(
        title = "Important Message",
        'Please Hit "Submit Parameters" Button on the First Pane to Start Silumation"'
      ))}
    })
  
  observeEvent(input$solaR, {
      show_modal_spinner(
        spin = "cube-grid",
        color = "#00A08A"
      )
      
      ## Download Site data
      h <- new_handle()
      handle_setopt(h, ssl_verifypeer = 0)
      
      site.df <- read.table(curl("https://m.lkeportal.com/publicsolarbatch/BS_2020.csv", handle = h), header = TRUE, sep = ",",
                            colClasses = c("character", "NULL", "NULL", "NULL", "NULL", "NULL","numeric", "NULL", "NULL", "numeric","NULL", "numeric"))
      
      
      
    
      
      names(site.df) <- c("Date", "Kw", "G0", "Ta")
      
      site.df$Date <- as.POSIXct(strptime(site.df$Date, format='%Y-%m-%d %H:%M:%S'))
      
      
      site.hour <- aggregate(site.df, by = list(format(site.df$Date,format='%Y-%m-%d %H')), mean)
      site.hour<-site.hour[,c(-1)]
      site.zoo <- read.zoo(site.hour, index.column = 1)
      
      ## Generate Site Map
      output$map_site <- renderLeaflet({
        leaflet() %>%
          addTiles() %>% setView(lng = -84.43, lat = 37.46, zoom = 7) %>% addMarkers(lng = -84.43, lat = 37.46)})
      
      
      ##Generate Site Specs
      c <- c("Site Name", "Owner","Coordinates", "Nominal Power(Kwp AC)", "Area/m2", "Mode Trek"," Inclination Angle")
      d <- c("E.W. Brown Plant", "KU Energy", "37.36N, -84.43W", "10MWp", "unknown", "Fixed Tilt", "25")
      specs2 <- data.frame(c,d)
      names(specs2) <-NULL
      output$specs2 <- renderTable(specs2, colnames = FALSE, striped = FALSE)
      
      ## Calc Prod
      prod.site <-  prodGCPV(lat = 37.46, modeTrk = "fixed",  modeRad = 'bdI', sample = "hour",
                             dataRad = site.zoo, keep.night = TRUE, beta = 25,
                             alb = 0.2, iS = 1)
      
      solaR.month <- aggregate(as.zooD(prod.site), by=month, sum)
      solaR <- solaR.month$Yf*10
      solaR <- data.frame("Eac" = solaR, "Source" = "solaR") 
      solaR$Month <- month.abb[index(solaR)]
      
      site <- aggregate(site.zoo$Kw, by=month, sum, na.rm = TRUE)
      site <- data.frame("Eac" = site/1250, "Source" = "Site")
      site$Month <- month.abb[index(site)]
      
      site.month <- rbind(solaR, site)
      site.month$Month <- factor(site.month$Month, levels = month.abb)
      
      output$monthly <- renderPlot({
        ggplot() + geom_col(data = site.month, mapping=aes_string(x="Month", y="Eac", fill = "Source"), position = position_dodge()) + 
          ylab("Comparison") + theme_minimal() +
          theme(axis.text = element_text(size = 12, family = 'serif'),
                axis.title = element_blank(),
                legend.position="top")
      })
      
      
      output$box1 <- renderValueBox({
        valueBox(
          paste0(round(sum(site$Eac)/1000, digits = 2), " GWh ac"), 
          subtitle = tags$p("Totoal Energy Generation in 2020", style = "font-size: 120%;"), icon = icon("solar-panel"),
          color = "yellow")
      })
      
      output$box2 <- renderValueBox({
        valueBox(
          paste0(round(sum(site$Eac)*100/sum(solaR$Eac), digits = 1), " %"), 
          subtitle = tags$p("Compared with solaR simulation result", style = "font-size: 120%;"), 
          icon = icon("thumbs-up"),color = "green")
      })
      
      
      sol.data <- read.table("https://api.solcast.com.au/world_radiation/forecasts?latitude=37.46&longitude=-84.43&format=csv&api_key=2CGFoGuNBmHLQtZnLRhxsg2J_6nOAtQF",
                             colClasses = c("numeric","NULL", "NULL", "NULL","NULL","NULL","NULL","NULL","numeric",
                                            "NULL","NULL","NULL","character", "NULL"),
                             header = TRUE, sep = ",")
      
      
      # change UTC to local time zone
      sol.time <- as.POSIXct(sol.data$PeriodEnd, format = "%Y-%m-%dT%H:%M:%SZ", tz="UTC")
      sol.local <- local2Solar(sol.time, lon = -84.43)
      sol.local<- na.fill(sol.local, "extend")
      
      ## Generate zoo object for Energy prodication
      sol.data <- sol.data[,c(1,2)]
      names(sol.data) <- c("G0", "Ta")
      sol.data$Date <- sol.local
      
      ## remove possible duplicate zoo from the dataframe
      sol.data <- sol.data[!duplicated(sol.data$Date),]
      sol.meteo <- zoo(sol.data[, c(1,2)], sol.data$Date)

      
      ## Predication using Solcast and solaR
      ## obtain solcast meteo data
      site.window <- window(site.zoo, start = Sys.Date()-7, end = max(site.hour$Date))
      site.window <- data.frame(site.window, "Date" = index(site.window))
      
      final.meteo <- merge(site.window[,c(2,3,4)], sol.data, all.x = TRUE, all.y = TRUE, by= "Date")
      
      prod.sol <-  prodGCPV(lat = 37.46, modeTrk = "fixed", modeRad = 'bdI', sample = "30 min",
                            dataRad = sol.meteo, keep.night = TRUE, beta = 25,
                            alb = 0.2, iS = 1)
      
      prod.sol.day <- as.zooI(prod.sol)
      prod.sol.day.df <- data.frame("Kw" = prod.sol.day$Pac*10/26.5, "Date" =index(prod.sol.day))
      
      final.prod <- merge(site.window[,c(1,4)], prod.sol.day.df, all.x = TRUE, all.y = TRUE, by= "Date")
      
      output$predication <- renderPlotly({
        
        fig1 <- plot_ly(final.meteo, x = ~Date)
        fig1 <- fig1 %>% add_lines(y = ~ G0.x, name = "Field Test GHI(G0)", yaxis = "y",
                                   line = list(color = 'rgb(255, 127, 14)', width = 1))
        fig1 <- fig1 %>% add_lines(y = ~ G0.y, name = "Projected GHI(G0)", yaxis = "y",
                                   line = list(color = 'rgb(255, 127, 14)', width = 1, dash = 'dot'))
        
        fig1 <- fig1 %>% add_lines(y = ~ Ta.x, name = "Field Test Temperature",yaxis = "y2",
                                   line = list(color = 'rgb(44, 160, 44)', width = 1))
        fig1 <- fig1 %>% add_lines(y = ~ Ta.y, name = "Projected Temperature", yaxis = "y2",
                                   line = list(color = 'rgb(44, 160, 44)',width = 1, dash = 'dot'))
        
        fig1 <- fig1 %>% layout(
          xaxis = list(title = ""),
          yaxis = list(title = "W/m2"),
          yaxis2 = list(overlaying = "y",
                        zeroline = FALSE,
                        side = "right",
                        title = "Ta(Celcius)",
                        showgrid = FALSE),
          margin = list(l = 50, r = 50, b = 50, t =50, pad = 4),
          legend = list(orientation = "h",   # show entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        y = -0.4,
                        yanchor = "bottom"))
        
        fig2 <- plot_ly(final.prod, x = ~Date)
        fig2 <- fig2 %>% add_lines(y = ~Kw.x, name = "Field Test Pac",
                                   line = list(color = 'rgb(31, 119, 180)', width = 2))
        fig2 <- fig2 %>% add_lines(y = ~Kw.y, name = "Projected Pac",
                                   line = list(color = 'rgb(31, 119, 180)', width = 2, dash = 'dot'))
        fig2 <- fig2 %>% layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Pac/kw"),
          margin = list(l = 50, r = 50, b = 50, t =50, pad = 4),
          legend = list(orientation = "h",
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5,
                        y = -0.4,
                        yanchor = "bottom"))
        
        if (input$plot_type == "m") 
          return(fig1)
        else if (input$plot_type == "e") 
          return(fig2)
      })
      remove_modal_spinner()
    }, once = TRUE)
}
  

 