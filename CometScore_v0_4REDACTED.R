library(dplyr)
library(plotly)


#load("~\\Optimal\\CometScore\\httpDataRawOrigins.Rda")
#load("~\\Optimal\\CometScore\\cruxOrigins.Rda")
load(file="~\\Optimal\\CometScore\\groupDF.Rda")
load(file="~\\Optimal\\CometScore\\cruxOriginsData.Rda")
#Group 1: Low overall content size, few scripts
#Group 2: low number of scripts, high content size
#Group 3: Many Script, low overall content size
groupDF <- groupDF %>%
  mutate("url" = substr(url, 1, nchar(url)-1))

metricDF <- cruxOriginData %>% 
  filter(origin %in% groupDF$url)
rm(cruxOriginData)
gc()

urlDat <- metricDF %>%
  group_by(origin, country_code, device) %>%
  mutate_at(vars(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~mean(., na.rm=TRUE)) %>%
  distinct(origin, country_code, device, .keep_all = T)


holdPoints <- metricDF %>%
  left_join(groupDF, by = c("origin" = "url")) %>%
  group_by(country_code, Cluster, device)

medians <- holdPoints %>%
  mutate("p75_cls" = p75_cls*1000) %>%
  select(-origin) %>%
  #mutate("p75_fid" = ifelse(p75_fid == 0, mean(p75_fid), p75_fid)) %>%
  mutate(across(c(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~quantile(., 0.50, na.rm = TRUE))) %>%
  mutate(across(c(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~ifelse(. == 0, 2, .))) %>%
  distinct(country_code, Cluster, device, .keep_all = T) %>%
  ungroup()

tenths <- holdPoints %>%
  mutate("p75_cls" = p75_cls*1000) %>%
  select(-origin) %>%
  #mutate("p75_fid" = ifelse(p75_fid == 0, mean(p75_fid), p75_fid)) %>%
  mutate(across(c(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~quantile(., 0.10, na.rm = TRUE))) %>%
  mutate(across(c(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~ifelse(. == 0, 1, .))) %>%
  distinct(country_code, Cluster, device, .keep_all = T) %>%
  ungroup()


getMetricScores <- function(url, cluster, device_in, country, medians, tenths, holdpoints, compUrls = NULL){
  #Define Median
  med <- as.data.frame(medians %>% filter(Cluster == cluster & device == device_in & country_code == country))
  #Define 10th Percentile
  p10 <- as.data.frame(tenths %>% filter(Cluster == cluster, device == device_in, country_code == country))
  
  #Define Location
  mu <- med %>%
    select(-country_code, -device, -Cluster) %>%
    mutate(across(c(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~log(.)))
  
  #Define shape of curve
  log10s <- p10 %>%
    select(-country_code, -device, -Cluster) %>%
    mutate(across(c(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~log(.)))
    
  sigma <- abs((log10s - mu))/((2^(1/2)) * 0.9061938024368232)
  
  #Point of Diminishing Returns
  pdr <- exp(mu + (sigma/2)*((-3*sigma) - sqrt(4 + sigma^2)))
  
  histDat <- holdPoints %>%
    filter((country_code == country) & (device == device_in)) %>%
    group_by(origin) %>%
    mutate(across(c(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~mean(., na.rm = TRUE))) %>%
    ungroup() %>%
    select(-country_code, -device, -Cluster) %>%
    distinct()
  
  
  x <- holdPoints %>%
    ungroup() %>%
    filter(((origin == url) | (origin %in% compUrls)) & (country_code == country) & (device == device_in)) %>%
    select(-origin, -country_code, -device, -Cluster) %>%
    mutate(across(c(p75_fcp, p75_fid, p75_lcp, p75_ttfb, p75_cls), ~mean(., na.rm = TRUE))) %>%
    distinct()
  
  scoreList <- c()
  plotList <- c()
  columns <- c("First Contentful Paint", "First Input Delay", "Largest Contentful Paint","Time to First Byte", "Cumulative Layout Shift")
  
  for(c in 1:length(x)){
    mu_c <- mu[c][[1]][[1]]
    sigma_c <- sigma[c][[1]][[1]]
    
    #----------All in Cluster-----------
    allDat <- data.frame("metric" = c(histDat[c+1][[1]])) %>%
      mutate("scoreLog" = xx,
             "t" =  xxx,
             "Erf" = xxx
             "Cdf" = xxx,
             "Cdf" = xxx,
             "Origin" = histDat$origin
      )
      
    scoreList[[columns[c]]] <- c(allDat$Cdf)
    
    
    
    #------Line plot of Input Relative to CDF--------
    # allDat %>%
    #   plot_ly(x = ~metric , y = ~Cdf , type = 'scatter', mode = 'lines', line = list(shape = 'spline')) %>%
    #   layout(
    #     annotations = list(
    #       x = c(x[c][[1]][[1]], pdr[c][[1]][[1]]),
    #       y = c(cdf[length(cdf)-1], cdf[1]),
    #       text = c("Your Input", "Point of Diminishing<br>Return"),
    #       xref = "x",
    #       yref = "y",
    #       showarrow = TRUE,
    #       arrowhead = 7,
    #       ax = c(50, 100),
    #       ay = c(-40, -10)
    #     ),
    #     xaxis = list(
    #       title = "Timing (ms)",
    #       gridcolor = 'grey'
    #     ),
    #     yaxis = list(
    #       title = "CDF (Score)",
    #       gridcolor = 'grey'
    #     ),
    #     title = paste0("Log-Normal CDF: Relationship Between Score and Timing<br>", columns[c]),
    #     shapes = list(
    #       list(
    #         type="rect",
    #         xref="x",
    #         yref="y",
    #         x0=0,
    #         y0=0,
    #         x1=xmax,
    #         y1=0.5,
    #         fillcolor="#F07358",
    #         opacity=0.4,
    #         line_width=0,
    #         layer="below"
    #       ),
    #       list(
    #         type="rect",
    #         xref="x",
    #         yref="y",
    #         x0=0,
    #         y0=0.5,
    #         x1=xmax,
    #         y1=0.9,
    #         fillcolor="#F3BE16",
    #         opacity=0.35,
    #         line_width=0,
    #         layer="below"
    #       ),
    #       list(
    #         type="rect",
    #         xref="x",
    #         yref="y",
    #         x0=0,
    #         y0=0.9,
    #         x1=xmax,
    #         y1=1,
    #         fillcolor="#7DB809",
    #         opacity=0.35,
    #         line_width=0,
    #         layer="below"
    #       )
    #     )
    #   )
    
    
    #-----CDF Visualization Per Metric--------
    
    #Abramowitz and Stegun formula 7.1.26 (error < 1.5e-7)
    score <- unname(unlist(c(pdr[c], p10[c+2], med[c+2], x[c], x[c]+500)))
    scoreLog <- (log(score) - mu_c)/(sqrt(2)*sigma_c)
    t <- xxx
    Erf <- xxx
    #Log-Normal CDF
    cdf <- (0.5*(1-Erf))
  
    xmax = x[c][[1]][[1]]+500
    plotList[[c]] <- plot_ly(x = c(0,score), y = c(1,cdf), type = 'scatter', mode = 'lines', line = list(shape = 'spline')) %>%
      layout(
        annotations = list(
          x = c(x[c][[1]][[1]], pdr[c][[1]][[1]]),
          y = c(cdf[length(cdf)-1], cdf[1]),
          text = c("Your Input", "Point of Diminishing<br>Return"),
          xref = "x",
          yref = "y",
          showarrow = TRUE,
          arrowhead = 7,
          ax = c(50, 100),
          ay = c(-40, -10)
        ),
        xaxis = list(
          title = "Timing (ms)",
          showgrid = FALSE
          #gridcolor = 'grey'
        ),
        yaxis = list(
          title = "CDF (Score)",
          showgrid = FALSE
          #gridcolor = 'grey'
        ),
        title = paste0("Log-Normal CDF: Relationship Between Score and Timing<br>", columns[c]),
        shapes = list(
          list(
            type="rect",
            xref="x",
            yref="y",
            x0=0,
            y0=0,
            x1=xmax,
            y1=0.5,
            fillcolor="#F07358",
            opacity=0.4,
            line_width=0,
            layer="below"
          ),
          list(
            type="rect",
            xref="x",
            yref="y",
            x0=0,
            y0=0.5,
            x1=xmax,
            y1=0.9,
            fillcolor="#F3BE16",
            opacity=0.35,
            line_width=0,
            layer="below"
          ),
          list(
            type="rect",
            xref="x",
            yref="y",
            x0=0,
            y0=0.9,
            x1=xmax,
            y1=1,
            fillcolor="#7DB809",
            opacity=0.35,
            line_width=0,
            layer="below"
          )
        )
      )
  }
  
  scoreDF <- as.data.frame(do.call("cbind", scoreList)) %>%
    mutate("Origin" = histDat$origin) %>%
    mutate(across(c(-Origin), ~ifelse(is.na(.), median(., na.rm = T), .))) %>%
    mutate("Score" = round(((`First Contentful Paint`*0.1)+(`First Input Delay`*0.3)+
                             (`Largest Contentful Paint`*0.25)+(`Time to First Byte`*0.20)+
                             (`Cumulative Layout Shift`*.15))*100, 0)) %>%
    group_by(Score) %>%
    mutate("Count" = length(Score)) %>%
    ungroup()
  
  annoDF <- scoreDF %>% 
    group_by(Score) %>%
    mutate("Count" = length(Origin)) %>%
    filter(Origin == url | Origin %in% compUrls) %>%
    mutate("Origin" = ifelse(Origin == url, "Your Score", Origin),
           "Origin" = gsub("https://", "", Origin),
           "ax" = ifelse(Score > 80, 10, -10),
           "ay" = -70
           ) %>%
    ungroup()
  
  annoList <- list(
    x = c(annoDF$Score),
    y = c(annoDF$Count),
    text = c(annoDF$Origin),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 7,
    ax = annoDF$ax,
    ay = annoDF$ay
  )
  
  
  scoreHist <- scoreDF %>%
    plot_ly(x = ~Score, type = 'histogram', nbinsx = 100,
    marker = list(color = "#A4A6A6",
                  line = list(color = "#A4A6A6",
                              width = 2))) %>% 
    layout(title = "Score Distribution",
           yaxis = list(title = "Count", zeroline = FALSE, showgrid = FALSE),
           xaxis = list(title = "Score", zeroline = FALSE),
           annotations = annoList,
           shapes = list(
             list(
               type="rect",
               xref="x",
               yref="y",
               x0=0,
               y0=0,
               x1=50,
               y1=max(scoreDF$Count),
               fillcolor="#F07358",
               opacity=0.4,
               line_width=0,
               layer="below"
             ),
             list(
               type="rect",
               xref="x",
               yref="y",
               x0=50,
               y0=0,
               x1=90,
               y1=max(scoreDF$Count),
               fillcolor="#F3BE16",
               opacity=0.35,
               line_width=0,
               layer="below"
             ),
             list(
               type="rect",
               xref="x",
               yref="y",
               x0=90,
               y0=0,
               x1=100,
               y1=max(scoreDF$Count),
               fillcolor="#7DB809",
               opacity=0.35,
               line_width=0,
               layer="below"
             )
           )
    )

  
  percentile <- round(ecdf(scoreDF$Score)(scoreDF$Score[which(scoreDF$Origin == url)])*100,0)
  return(list("cdfPlots" = plotList, "scoreHist" = scoreHist, "Percentile" = percentile, "Score" = scoreDF$Score[which(scoreDF$Origin == url)]))
 
  }

scoreStuff <- getMetricScores(url = holdPoints$origin[20], cluster = 1, device_in = 'desktop', country = 'us', compUrls = c(holdPoints$origin[1:3]),
                              tenths = tenths, holdpoints = holdpoints, medians = medians)
