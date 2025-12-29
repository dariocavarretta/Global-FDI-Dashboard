#### output of all user selections (conditional)



### libraries loaded 

library(dplyr)
library(scales)
library(shiny)
library(highcharter)
library(tidyr)
library(shinycssloaders)
library(purrr)


### functions and fixed color palettes for legend etc to accomodate many colors (overrun highcrater base limited palette)



## allow adjustment based on condition "either a or b" or chained for multiple conditions

`%||%` <- function(a, b) if (!is.null(a)) a else b


FDI_PALETTE_25_LIGHT <- c(
  "#1f4e79", "#b88a1e", "#7a4fa8", "#4c8cff", "#f7b538",
  "#b984db", "#008080", "#c0504d", "#9bbb59", "#8064a2",
  "#4bacc6", "#f79646", "#00b0f0", "#7030a0", "#ff6666",
  "#66cc66", "#6666ff", "#cc66ff", "#ffcc66", "#66cccc",
  "#cc9966", "#9999ff", "#ff99cc", "#99ccff", "#ccff99"
)

FDI_PALETTE_25_DARK <- c(
  "#4c8cff", "#f7b538", "#b984db", "#7aa6ff", "#ffd46b",
  "#d5a6f5", "#4db6ac", "#ef9a9a", "#a5d6a7", "#9575cd",
  "#4dd0e1", "#ffb74d", "#4fc3f7", "#ab47bc", "#ff8a80",
  "#81c784", "#7986cb", "#ce93d8", "#ffe082", "#80deea",
  "#d7ccc8", "#9fa8da", "#f48fb1", "#90caf9", "#c5e1a5"
)


FDI_MAP_PALETTE_120 <- c(
  "#4c8cff","#f7b538","#b984db","#4db6ac","#ef9a9a","#ffd46b","#81c784",
  "#9575cd","#ba68c8","#4dd0e1","#ff8a65","#aed581","#7986cb",
  colorRampPalette(c("#006699","#33ccff"))(20),
  colorRampPalette(c("#993333","#ff6666"))(20),
  colorRampPalette(c("#669933","#ccff99"))(20),
  colorRampPalette(c("#663399","#cc99ff"))(20),
  colorRampPalette(c("#aa8800","#ffdd55"))(20)
)



bar_palette <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
  "#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
  "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
  "#6A3D9A", "#FDBF6F", "#CAB2D6", "#B2DF8A", "#FB9A99",
  "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
  "#E6AB02", "#A6761D", "#A6CEE3", "#B2DF8A", "#FB9A99",
  "#FDBF6F", "#CAB2D6", "#FFFF99", "#1F78B4", "#33A02C"
)




#### fixed map tools

worldgeojson <- jsonlite::fromJSON(readr::read_rds("worldgeojson.rds"), simplifyVector = FALSE)


## map rendering 

render_map <- function(df, metric_label, mapdata, text_col, border_col, null_col) {
  
  vals <- df$value
  q <- quantile(vals, probs = c(0, 0.15, 0.30, 0.50, 0.70, 0.85, 1), na.rm = TRUE)
  
  stop_colors <- c('#6d3040', '#cd624f', '#dabc41', 
                   '#e9d584', '#bcd285', '#84A76D', '#4c7d55')
  
  n <- length(stop_colors)
  stops <- lapply(0:(n-1), function(i) {
    list(i/(n-1), stop_colors[i+1])
  })
  
  map_series <- df %>%
    transmute(
      iso3  = iso3,
      value = value
    )
  
  highchart(type = "map") %>%
    hc_add_series(
      mapData = mapdata,
      data = map_series,
      joinBy = c("iso-a3", "iso3"),
      name = metric_label,
      borderColor = border_col,
      nullColor = null_col,
      tooltip = list(pointFormat = "{point.name}: <b>{point.value}</b>")
    ) %>%
    hc_colorAxis(
      stops = stops,
      min = q[1],
      max = q[7]
    ) %>%
    hc_title(
      text = metric_label,
      style = list(color = text_col, fontSize = "18px")
    ) %>%
    hc_chart(backgroundColor="transparent") %>%
    hc_legend(itemStyle=list(color=text_col))
}



##### country selection map for univariate analysis (not used currently)

 # render_single_country_map <- function(selected_iso, mapdata,
 #                                       fill_col = "#b30000",
 #                                       border_col = "#fff") {
 # 
 #   hc_data <- lapply(mapdata$features, function(f) {
 #     iso <- f$properties$`iso-a3`
 #     list(
 #       `iso-a3` = iso,
 #       color    = ifelse(iso == selected_iso, fill_col, "rgba(128,128,128,0.15)")
 #     )
 #   })
 # 
 #   highchart(type = "map") %>%
 #    hc_add_series(
 #       mapData = mapdata,
 #       data = hc_data,
 #       joinBy = "iso-a3",
 #       borderColor = border_col,
 #       tooltip = list(pointFormat = "{point.name}"),
 #       showInLegend = FALSE,
 #       enableMouseTracking = TRUE,
 #       name = ""
 #     ) %>%
 #     hc_colorAxis(
 #      enabled = FALSE,
 #      showInLegend = FALSE
 #     ) %>%
 #     hc_legend(enabled = FALSE) %>%
 #     hc_chart(backgroundColor = "transparent")
 # }
 # 
 # render_bilateral_country_map <- function(
 #    iso_from,
 #    iso_to,
 #    mapdata,
 #    fill_col_from = "#b30000",   
 #    fill_col_to   = "#1f77b4",  
 #    border_col    = "#fff"
 # ) {
 #   
 #   hc_data <- lapply(mapdata$features, function(f) {
 #     
 #     iso <- f$properties$`iso-a3`
 #     
 #     color <- if (iso == iso_from) {
 #       fill_col_from
 #     } else if (iso == iso_to) {
 #       fill_col_to
 #     } else {
 #       "rgba(128,128,128,0.15)"
 #     }
 #     
 #     list(
 #       `iso-a3` = iso,
 #       color    = color
 #     )
 #   })
 #   
 #   highchart(type = "map") %>%
 #     hc_add_series(
 #       mapData = mapdata,
 #       data = hc_data,
 #       joinBy = "iso-a3",
 #       borderColor = border_col,
 #       tooltip = list(pointFormat = "{point.name}"),
 #       showInLegend = FALSE,
 #       enableMouseTracking = TRUE,
 #       name = ""
 #     ) %>%
 #     hc_colorAxis(
 #       enabled = FALSE,
 #       showInLegend = FALSE
 #     ) %>%
 #     hc_legend(enabled = FALSE) %>%
 #     hc_chart(backgroundColor = "transparent")
 # }
 # 



#### aggregates (blocks) definitions

AGGREGATE_DEFINITIONS <- list(
  "EU"        = "All 27 EU Member States",
  "NAFTA"     = "United States, Mexico, Canada",
  "UK"        = "United Kingdom",
  "ASEAN"     = "All 10 Southeast Asian economies",
  "MERCOSUR"  = "Brasil, Argentina, Paraguay, Uruguay, Venezuela, Bolivia",
  "AFR"       = "All 54 African Countries",
  "CIS"       = "10 CIS economies (post-Soviet region)",
  "BRICS"     = "Brazil, Russia, India, China, South Africa"
)




## max year displayed (to update here only once if needed)

MAX_YEAR <- 2025








###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################



##### UI + SERVERS





############################################################################################################################################


##################### GLOBAL TRENDS : GENERAL ANALYSIS 





### UI MODULE 1: general analysis: total global flows


totalFDI_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    tags$style(HTML("
      .shiny-input-container {
        position: relative !important;
        z-index: 2000 !important;
      }
      .selectize-dropdown,
      .selectize-dropdown-content,
      .selectize-control {
        z-index: 3000 !important;
      }
    ")),
    
    div(
      class = "info-banner",
      style = "margin-top:25px; margin-bottom:15px;",
      HTML(
        "Visualize Total Greenfield FDI Trends between 2003 – 2025 in terms of capital investment, 
         number of projects, and jobs created.<br>
         Use the controls below to filter timeframe, metric, trend type and frequency 
         (monthly, quarterly or annual)."
      )
    ),

    
    ### status submenu
        
    div(
      class = "status-options",
      div(class = "status-option active", `data-status` = "Announced", "Announced FDI"),
      div(class = "status-option",        `data-status` = "Opened",    "Opened FDI"),
      div(class = "status-option",        `data-status` = "Total",     "Total FDI")
    ),
    
    # kpi boxes with summary info
    div(
      class = "kpi-container",
      div(class = "kpi-card kpi-blue",
          h3("FDI Value"),
          div(class = "kpi-number", textOutput(ns("total_value")))
      ),
      div(class = "kpi-card kpi-orange",
          h3("Jobs Created"),
          div(class = "kpi-number", textOutput(ns("total_jobs")))
      ),
      div(class = "kpi-card kpi-green",
          h3("Number of Projects"),
          div(class = "kpi-number", textOutput(ns("total_projects")))
      )
    ),
    

    # Control panel of main time series with all options

    div(
      style="
        display:flex;
        margin-top:75px;
        margin-left:20px;
        border:1px solid var(--border);
        border-radius:12px;
        background:rgba(255,255,255,0.02);
      ",
      
      div(
        style="
          width:240px;
          padding:18px;
          border-right:1px solid var(--border);
          background:rgba(255,255,255,0.04);
          display:flex;
          flex-direction:column;
        ",
        
        div(class="left-menu-title", "Display Options"),
        
        # Metric
        div(
          class="control-group",
          style="margin-top:15px;",
          div("Metric:", style="font-size:13px;color:var(--text-muted);margin-bottom:6px;"),
          radioButtons(
            ns("metric"), NULL,
            choices = c(
              "Volume"             = "value",
              "Number of Projects" = "n_projects",
              "Jobs Created"       = "jobs"
            ),
            selected = "value"
          )
        ),
        
        # Mode
        div(
          class="control-group",
          style="margin-top:20px;",
          div("Mode:", style="font-size:13px;color:var(--text-muted);margin-bottom:6px;"),
          radioButtons(
            ns("graph_mode"), NULL,
            choices = c(
              "Frequency"  = "frequency",
              "Cumulative" = "cumulative"
            ),
            selected = "frequency"
          )
        ),
        
        # Frequency
        conditionalPanel(
          condition = sprintf("input['%s'] == 'frequency'", ns("graph_mode")),
          div(
            class="control-group",
            style="margin-top:20px;",
            div("Frequency:", style="font-size:13px;color:var(--text-muted);margin-bottom:6px;"),
            radioButtons(
              ns("freq_detail"), NULL,
              choices = c(
                "Monthly"   = "monthly",
                "Quarterly" = "quarterly",
                "Yearly"    = "yearly"
              ),
              selected = "monthly"
            )
          )
        ),
        
        # Timeframe
        div(
          class="control-group",
          style="margin-top:20px;",
          div("Timeframe:", style="font-size:13px;color:var(--text-muted);margin-bottom:6px;"),
          selectInput(
            ns("timeframe"), NULL,
            choices = c("All", "Custom"),
            selected = "All"
          )
        ),
        
        # Custom years if selected
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Custom'", ns("timeframe")),
          div(
            style="display:flex; gap:10px; margin-top:10px; font-size:13px;",
            div(
              style="width:70px;",
              div("Start Year:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
              selectInput(ns("start_year"), NULL, choices = NULL)
            ),
            div(
              style="width:70px;",
              div("End Year:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
              selectInput(ns("end_year"), NULL, choices = NULL)
            )
          )
        )
      ),
      
      # chart output
      
      div(
        style="
          flex-grow:1;
          padding:10px 20px;
          display:flex;
          align-items:center;
          justify-content:center;
        ",
        highchartOutput(ns("fdi_chart"), height="430px")
      )
    ),
    

    # secondary section: 4 graphs containing other measures of global trends

    div(
      class = "info-banner",
      style = "margin-top:45px; margin-bottom:35px;",
      HTML(
        "Explore a range of structural and performance indicators for Greenfield FDI.<br>
         Hover to inspect values or export charts directly."
      )
    ),
    
    div(
      style="
        width:95%;
        display:flex;
        justify-content:center;
        margin-left:30px;
        flex-direction:column;
      ",
      
      div(
        style="
          display:flex;
          gap:50px;
          margin-bottom:40px;
        ",
        
        # Efficiency chart
        
        div(
          style="flex:1; border:1px solid var(--text-main); padding:12px;",
          div(
            style="display:flex; justify-content:space-between; align-items:center; margin-bottom:10px;",
            div("Greenfield FDI Efficiency (Jobs per $1B)",
                style="font-size:16px; font-weight:600;"),
            selectInput(
              ns("eff_status"), NULL,
              choices = c("Total", "Announced", "Opened"),
              selected = "Opened",
              width = "150px",
              selectize = FALSE
            )
          ),
          highchartOutput(ns("eff_chart"), height="400px")
        ),
        
        # Volatility chart
        
        div(
          style="flex:1; border:1px solid var(--text-main); padding:12px;",
          div("Greenfield FDI Volatility – Global flows",
              style="font-size:16px; font-weight:600; margin-bottom:10px;"),
          highchartOutput(ns("volatility_chart"), height="435px")
        )
      ),
      
      div(
        style="
          display:flex;
          gap:50px;
          margin-bottom:30px;
        ",
        
        # Conversion ratio of opened/announced project chart
        
        div(
          style="flex:1; border:1px solid var(--text-main); padding:12px;",
          div("FDI Conversion Ratio (Opened / Announced)",
              style="font-size:16px; font-weight:600; margin-bottom:30px;"),
          highchartOutput(ns("conversion_chart"), height="400px")
        ),
        
        # Distribution of project size chart
        
        div(
          style="flex:1; border:1px solid var(--text-main); padding:12px;",
          div(
            style="display:flex; justify-content:space-between; align-items:center; margin-bottom:30px;",
            div("Distribution of FDI Projects by Size (%)",
                style="font-size:16px; font-weight:600;"),
            selectInput(
              ns("dist_status"), NULL,
              choices = c("Total", "Announced", "Opened"),
              selected = "Opened",
              width="150px",
              selectize = FALSE
            )
          ),
          highchartOutput(ns("dist_chart"), height="400px")
        )
      )
    )
  )
}






### SERVER MODULE 1: general analysis: total global flows



totalFDI_server <- function(id, fdi_data, fdi_monthly, fdi_quarterly, fdi_yearly, conversion, efficiency, volatility, dist){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    

    ### KPI filtering based purely on status

    
    filtered_status <- reactive({
      status_sel <- input$status %||% "Announced"
      
      if (status_sel == "Announced") {
        fdi_data %>% filter(status_group == "Announced")
      } else if (status_sel == "Opened") {
        fdi_data %>% filter(status_group == "Opened")
      } else {
        fdi_data
      }
    })
    
    output$total_value <- renderText({
      df <- filtered_status()
      total_val <- sum(df$value, na.rm=TRUE) / 1000
      paste0("$", scales::comma(total_val), " B")
    })
    
    output$total_jobs <- renderText({
      df <- filtered_status()
      scales::comma(sum(df$jobs, na.rm=TRUE))
    })
    
    output$total_projects <- renderText({
      df <- filtered_status()
      scales::comma(sum(df$n_projects, na.rm=TRUE))
    })
    
    

    ### value scale helper (for value) --> yearly in Billions, monthly or quarterly in Millions

    
    value_scaling <- reactive({
      if (input$metric != "value") return(1)
      
      freq <- input$freq_detail %||% "yearly"
      
      if (freq == "yearly") return(1000)  
      return(1)                           
    })
    
    # Unit label for axis of chart
    
    value_unit <- reactive({
      if (input$metric != "value") return("")
      
      freq <- input$freq_detail %||% "yearly"
      
      if (freq == "yearly") return("USD Billion")
      return("USD Million")
    })
    
    
    ### frequency filtering based on input + custom years
    
    observe({
      req(input$graph_mode == "frequency")
      freq <- input$freq_detail %||% "yearly"
      
      df <- switch(freq,
                   "monthly"   = fdi_monthly,
                   "quarterly" = fdi_quarterly,
                   "yearly"    = fdi_yearly,
                   fdi_yearly)
      
      req(df)
      if (!"year" %in% names(df)) return()
      
      yrs <- sort(unique(as.integer(df$year)))
      yrs <- yrs[!is.na(yrs)]
      yrs_chr <- as.character(yrs)
      
      updateSelectInput(session, "start_year",
                        choices = yrs_chr, selected = yrs_chr[1])
      updateSelectInput(session, "end_year",
                        choices = yrs_chr, selected = tail(yrs_chr, 1))
    })
    
    
    
     # dataset selected based on inputs
    
    ts_selected <- reactive({
      mode  <- input$graph_mode %||% "frequency"
      freq  <- input$freq_detail %||% "yearly"
      tf    <- input$timeframe %||% "All"
      
      df <- switch(mode,
                   "frequency" = switch(freq,
                                        "monthly"   = fdi_monthly,
                                        "quarterly" = fdi_quarterly,
                                        "yearly"    = fdi_yearly,
                                        fdi_yearly),
                   "cumulative" = fdi_yearly,
                   fdi_yearly)
      
      req(df)
      if (!("year" %in% names(df))) {
        return(df)
      }
      df <- df %>% mutate(year = as.integer(year))
      
      # year range clamping
      
      clamp_range <- function(start, end, df) {
        miny <- min(df$year, na.rm = TRUE)
        maxy <- max(df$year, na.rm = TRUE)
        start <- max(miny, start)
        end   <- min(maxy, end)
        list(start = start, end = end)
      }
      
      
      if (tf == "Custom") {
        if (is.null(input$start_year) || is.null(input$end_year) ||
            input$start_year == "" || input$end_year == "") {
          return(df)
        }
        
        start <- suppressWarnings(as.integer(input$start_year))
        end   <- suppressWarnings(as.integer(input$end_year))
        
        if (is.na(start) || is.na(end)) {
          return(df)
        }
        
        if (start > end) { tmp <- start; start <- end; end <- tmp }
        
        r <- clamp_range(start, end, df)
        start <- r$start; end <- r$end
        
        df <- df %>% filter(year >= start, year <= end)
        return(df)
      }
      
      df
    })
    
  
    
    ### pivot dataset for highcharter time series output + adjustments
    
    ts_wide <- reactive({
      df <- ts_selected()
      req(nrow(df) > 0)
      
      # take metric input
      
      metric <- input$metric
      
      # filter dataset based on input, status, period
      
      df <- df %>%
        group_by(period, sort_index, status_group) %>%
        summarise(metric_value = sum(.data[[metric]], na.rm=TRUE),
                  .groups="drop") %>%
        tidyr::complete(period, status_group, fill=list(metric_value=0)) %>%
        arrange(sort_index)
      
      ## pivot 
      
      wide <- df %>%
        pivot_wider(
          names_from = status_group,
          values_from = metric_value,
          values_fill = 0
        ) %>%
        mutate(
          Announced = Announced %||% 0,
          Opened    = Opened %||% 0,
          Total     = Announced + Opened
        )
      
      # apply value scaling function
      
      scale <- value_scaling()
      if (input$metric == "value") {
        wide <- wide %>%
          mutate(
            Announced = Announced / scale,
            Opened    = Opened    / scale,
            Total     = Total     / scale
          )
      }
      
      # user selectes cumulative instead of period
      
      if (input$graph_mode == "cumulative") {
        wide <- wide %>%
          arrange(sort_index) %>%
          mutate(
            Announced = cumsum(Announced),
            Opened    = cumsum(Opened),
            Total     = cumsum(Total)
          )
      }
      
      wide
    })
    
    

    ### Now that dataset is ready: main time series chart

    
    output$fdi_chart <- renderHighchart({
      df <- ts_wide()
      req(nrow(df) > 0)
      
      
      # theme switch colors
      
      theme <- input$theme_mode %||% "dark"
      
      text_col   <- if (theme == "light") "#1e1f21" else "#ffffff"
      grid_col   <- if (theme == "light") "#c8c8c8" else "#c8c8c8"
      col_ann    <- if (theme == "light") "#1f4e79" else "#4c8cff"
      col_act    <- if (theme == "light") "#b88a1e" else "#f7b538"
      col_tot    <- if (theme == "light") "#7a4fa8" else "#b984db"
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      # metric labels used for graph titles, axis titles etc

      metric_label <- switch(
        input$metric,
        "value"      = paste("Volume (", value_unit(), ")", sep=""),
        "jobs"       = "Jobs Created",
        "n_projects" = "Number of Projects"
      )
      
      ## other switches
      
      metric <- input$metric %||% "value"
      freq   <- input$freq_detail %||% "yearly"
      
      #highchart
      
      highchart() %>%
        hc_chart(backgroundColor= bg_col) %>%
        hc_title(
          text=paste("Global Greenfield FDI ", metric_label, "over time"),
          style=list(color=text_col, fontSize="16px", fontWeight="600")
        ) %>%
        
        hc_xAxis(
          categories=df$period,
          labels=list(style=list(color=text_col))
        ) %>%
        
        hc_yAxis(
          title = list(
            text  = metric_label,
            style = list(color = text_col)
          ),
          labels = list(
            style = list(color = text_col)
          ),
          gridLineColor = grid_col
        ) %>%
        
        hc_add_series(name="Announced", data=df$Announced, type="line", color=col_ann) %>%
        hc_add_series(name="Opened",    data=df$Opened,    type="line", color=col_act) %>%
        hc_add_series(name="Total",     data=df$Total,     type="line", color=col_tot) %>%
        
        
        hc_tooltip(
          shared = TRUE,
          valueDecimals = if (metric == "value") 2 else 0,
          valueSuffix = if (metric == "value") {
            if (freq == "yearly") " B" else " M"
          } else ""
        ) %>%
        
        hc_plotOptions(series=list(lineWidth=3)) %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_exporting(enabled=TRUE)
    })
    
    

    ### efficieny bar chart
    
    
    ### dataset reactive on input

    
    eff_filtered <- reactive({
      st <- input$eff_status
      df <- efficiency %>% filter(status_group == st)
      df
    })
    
    # graph rendering
    
    output$eff_chart <- renderHighchart({
      df <- eff_filtered()
      req(nrow(df) > 0)
      
      theme <- input$theme_mode %||% "dark"
      
      text_col <- if (theme=="light") "#1e1f21" else "#ffffff"
      grid_col <- if (theme=="light") "#c8c8c8" else "#c8c8c8"
      bar_col  <- if (theme=="light") "#4c8cff" else "#7aa6ff"
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      highchart() %>%
        hc_chart(type="column", backgroundColor=bg_col) %>%
        hc_xAxis(categories=df$year,
                 labels=list(style=list(color=text_col))) %>%
        hc_yAxis(labels=list(style=list(color=text_col)),
                 gridLineColor=grid_col) %>%
        hc_add_series(name=paste("Efficiency —", input$eff_status),
                      data=round(df$efficiency,1),
                      color=bar_col) %>%
        hc_tooltip(shared=TRUE, valueSuffix=" jobs per $1B") %>%
        hc_plotOptions(column=list(borderWidth=0)) %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_exporting(enabled=TRUE)
    })
    
    

    ### volatility chart 

    
    output$volatility_chart <- renderHighchart({
      df <- volatility
      req(nrow(df) > 0)
      
      theme <- input$theme_mode %||% "dark"
      text_col <- if (theme=="light") "#1e1f21" else "#ffffff"
      grid_col <- if (theme=="light") "#c8c8c8" else "#c8c8c8"
      
      col_ann <- if (theme=="light") "#1f4e79" else "#4c8cff"
      col_act <- if (theme=="light") "#b88a1e" else "#f7b538"
      col_tot <- if (theme=="light") "#7a4fa8" else "#b984db"
      
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      df_ann <- df %>% filter(status_group=="Announced")
      df_act <- df %>% filter(status_group=="Opened")
      df_tot <- df %>% filter(status_group=="Total")
      
      highchart() %>%
        hc_chart(type="line", backgroundColor= bg_col) %>%
        hc_title(text="Global Greenfield FDI Volatility (YoY %)",
                 style=list(color=text_col)) %>%
        hc_xAxis(categories=df_tot$year,
                 labels=list(style=list(color=text_col))) %>%
        hc_yAxis(labels=list(style=list(color=text_col)),
                 gridLineColor=grid_col) %>%
        hc_add_series(name="Announced", data=df_ann$vol_pct, color=col_ann) %>%
        hc_add_series(name="Opened",    data=df_act$vol_pct, color=col_act) %>%
        hc_add_series(name="Total",     data=df_tot$vol_pct, color=col_tot) %>%
        hc_tooltip(shared=TRUE, valueSuffix=" %") %>%
        hc_plotOptions(series=list(lineWidth=3)) %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_exporting(enabled=TRUE)
    })
    
    

    ### conversion ratio chart

    
    output$conversion_chart <- renderHighchart({
      df <- conversion
      req(nrow(df)>0)
      
      theme <- input$theme_mode %||% "dark"
      text_col <- if (theme=="light") "#1e1f21" else "#ffffff"
      grid_col <- if (theme=="light") "#c8c8c8" else "#c8c8c8"
      line_col <- if (theme=="light") "#4c8cff" else "#7aa6ff"
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      highchart() %>%
        hc_chart(type="line", backgroundColor= bg_col) %>%
        hc_xAxis(categories=df$year,
                 labels=list(style=list(color=text_col))) %>%
        hc_yAxis(labels=list(style=list(color=text_col)),
                 gridLineColor=grid_col) %>%
        hc_add_series(name="Conversion Ratio",
                      data=round(df$conversion_ratio,3),
                      color=line_col) %>%
        hc_tooltip(shared=TRUE, valueSuffix=" ratio") %>%
        hc_plotOptions(series=list(lineWidth=3)) %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_exporting(enabled=TRUE)
    })
    
    
    ### project size distribution chart (stacked area)

    
    dist_filtered <- reactive({
      st <- input$dist_status
      dist %>% filter(status_group == st)
    })
    
    output$dist_chart <- renderHighchart({
      df <- dist_filtered()
      req(nrow(df) > 0)
      
      # Force bucket order (hopefully... constant fight with highcharter order)
      df$bucket <- factor(df$bucket,
                          levels = c("Small (<20M)",
                                     "Medium (20–100M)",
                                     "Large (100–500M)",
                                     "Mega (>500M)"))
      
      theme <- input$theme_mode %||% "dark"
      text_col <- if (theme=="light") "#1e1f21" else "#ffffff"
      grid_col <- if (theme=="light") "#c8c8c8" else "#c8c8c8"
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      cols <- c(
        "Small (<20M)"      = if (theme=="light") "#4c8cff" else "#7aa6ff",
        "Medium (20–100M)"  = if (theme=="light") "#1f4e79" else "#4c8cff",
        "Large (100–500M)"  = if (theme=="light") "#b88a1e" else "#f7b538",
        "Mega (>500M)"      = if (theme=="light") "#7a4fa8" else "#b984db"
      )
      
      df_wide <- df %>%
        tidyr::pivot_wider(
          id_cols = c(year),
          names_from = bucket,
          values_from = share,
          values_fill = 0
        ) %>%
        arrange(year)
      
      hc <- highchart() %>%
        hc_chart(type="area", backgroundColor= bg_col) %>%
        hc_xAxis(categories=df_wide$year,
                 labels=list(style=list(color=text_col))) %>%
        hc_yAxis(max=100,
                 labels=list(style=list(color=text_col)),
                 gridLineColor=grid_col) %>%
        hc_plotOptions(area=list(stacking="percent",
                                 lineWidth=0,
                                 marker=list(enabled=FALSE))) %>%
        hc_tooltip(shared=TRUE, valueSuffix=" %") %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_exporting(enabled=TRUE)
      
      bucket_order <- c("Small (<20M)",
                        "Medium (20–100M)",
                        "Large (100–500M)",
                        "Mega (>500M)")
      
      for (b in bucket_order) {
        hc <- hc %>% hc_add_series(
          name = b,
          data = round(df_wide[[b]], 1),
          color = cols[[b]]
        )
      }
      
      hc
    })
    
    
  })
}






#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################





# UI MODULE 2:  FDI trends: country level


totalFDI_country_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    
    
    div(class = "info-banner",
        HTML(
        "Explore Greenfield FDI Inflows, Outflows, total Flows and net Flows for different countries. Use the interactive menu panels and dropdown menus on the charts to filter in great detail<br>
        On first load, allow a few seconds for all charts to render. Filters from the first menu panel apply to both inflow and outflow charts simultaneously")),
    
    
    #### menu filtering for inflow and outflow charts: top countries by inflow or outflow
    
    
    div(
      style = "display:flex; justify-content:center; width:100%;",
      
      div(
        class = "country-control-panel",
        style = "
          display:flex;
          flex-direction:row;
          align-items:flex-start;
          justify-content:space-between;
          gap:40px;
          padding:15px 20px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
          margin-bottom:30px;
          margin-top:40px;
          width:90%;
        ",
        
        div(
          style = "flex:1;",
          div(class = 'left-menu-title', 'Select Metric'),
          
          tags$style(sprintf(" #%s .shiny-options-group label { 
                    font-size: 11px !important; color: var(--text-main); 
                  } "
          , ns("metric"))),
          radioButtons(
            inputId = ns("metric"),
            label = NULL,
            choices = c(
              "Volume" = "inflow_value",
              "Jobs Created"       = "inflow_job",
              "Number of Projects" = "inflow_n"
            ),
            inline = TRUE,
            selected = "inflow_value"
          ),
          
          div(class = "left-menu-title", style="margin-top:15px;", "Select Status"),

          tags$style(sprintf(" #%s .shiny-options-group label { 
                    font-size: 11px !important; color: var(--text-main); 
                  } "
                             , ns("status"))),
          radioButtons(
            inputId = ns("status"),
            label = NULL,
            choices = c(
              "Announced" = "Announced",
              "Opened" = "Opened",
              "Total" = "Total"
            ),
            inline = TRUE,
            selected = "Opened"
          )
        ),
        
        div(
          style = "flex:1;",
          div(class = "left-menu-title", "Countries to Display"),
          sliderInput(
            inputId = ns("top_n"),
            label = NULL,
            min = 5,
            max = 25,
            value = 10,
            width = "100%"
          )
        ),
        
        div(
          style = "flex:1;",
          div(class = "left-menu-title", "Timeframe"),
          sliderInput(
            inputId = ns("year_range"),
            label = NULL,
            min = 2003,
            max = MAX_YEAR,
            value = c(2003, MAX_YEAR),
            width = "100%"
          )
        )
      )
    ),
    
    
    ### two charts in the same row 
    
    div(class="two-chart-row",
        
        div(class="chart-box",
            div(style="display:flex; justify-content:space-between; align-items:center;",
                selectInput(ns("inflow_view"), NULL,
                            choices = c("Bar"="bar","Trend"="trend"),
                            width="120px")
            ),
            withSpinner(highchartOutput(ns("inflow_chart"), height="450px"), type = 6, color = "#b984db")
        ),
        
        div(class="chart-box",
            div(style="display:flex; justify-content:space-between; align-items:center;",
                selectInput(ns("outflow_view"), NULL,
                            choices = c("Bar"="bar","Trend"="trend"),
                            width="120px")
            ),
            withSpinner(highchartOutput(ns("outflow_chart"), height="450px"), type = 6, color = "#b984db")
        )
    ),
    
    
  ### section for world map tool: total or net FDI for all countries  
    
  
    div(class = "info-banner",
        HTML(
        "The following tool allows to analyze further FDI flows for all countries in the world. You can select between total flows (inflows + outflows) or net flows (inflows - outflows)<br>
        You can use detailed filtering from the left-side panel. When a new filter is applied, allow a few seconds for the World Map to render")),
  
  # menu selection / filtering
    
    div(
      style="
    display:flex;
    margin-top:35px;
    margin-left:20px;
    border:2px solid var(--text-main);
    border-radius:12px;
    background:rgba(255,255,255,0.02);
    overflow:hidden;
    margin-bottom:100px;
  ",
      
     
      div(
        style="
      width:230px;
      padding:18px;
      border-right:1px solid var(--border);
      background:rgba(255,255,255,0.04);
      display:flex;
      flex-direction:column;
      font-size:13px;
    ",
        
        div(class="left-menu-title", "Map Options"),
        
        div(
          class="control-group",
          style="margin-top:15px;",
          div("Metric:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
          
          tags$style(sprintf("#%s .shiny-options-group label { font-size:12px !important; }",
                             ns("map_metric"))),
          
          selectInput(
            ns("map_metric"),
            NULL,
            choices = c(
              "Volume" = "flow_value",
              "Jobs Created"       = "flow_job",
              "Number of Projects" = "flow_n"
            ),
            width="100%"
          )
        ),
        
        div(
          class="control-group",
          style="margin-top:20px;",
          div("Flow Type:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
          
          tags$style(sprintf("#%s .shiny-options-group label { font-size:12px !important; }",
                             ns("map_flow_type"))),
          
          selectInput(
            ns("map_flow_type"),
            NULL,
            choices = c(
              "Total Flow (In + Out)" = "flow",
              "Net Flow (In − Out)"   = "netflow"
            ),
            width="100%"
          )
        ),
        
        div(
          class="control-group",
          style="margin-top:20px;",
          div("Status:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
          
          tags$style(sprintf("#%s .shiny-options-group label { font-size:12px !important; }",
                             ns("map_status"))),
          
          selectInput(
            ns("map_status"),
            NULL,
            choices = c("Announced","Opened","Total"),
            selected = "Opened",
            width = "100%"
          )
        ),
        
        div(
          class="control-group",
          style="margin-top:20px;",
          div("Timeframe:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
          
          sliderInput(
            ns("map_year_range"),
            label = NULL,
            min = 2003,
            max = MAX_YEAR,
            value = c(2003, MAX_YEAR),
            width = "100%"
          )
          
        )
      ),
      
      
      ### output: world map
      
     
      div(
        style="
      flex-grow:1;
      padding:10px 20px;
      display:flex;
      flex-direction:column;
      justify-content:flex-start;
      height:750px;
      margin-bottom:150px;
    ",
        
        h4("World Map of Total Greenfield FDI Flows", class="chart-title", style="margin-bottom:15px;"),
        
        div(
          style="flex-grow:1;",
          withSpinner(highchartOutput(ns("flow_map"), height="750px"), type = 6, color = "#b984db")
        )
      )
    )
    
  )
}



######## SERVER

totalFDI_country_server <- function(id, fdi_country_inflow, fdi_country_outflow, fdi_country_flow) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    
  #### filters 
    
    filter_years <- function(df) {
      df %>% filter(year >= input$year_range[1],
                    year <= input$year_range[2])
    }
    
    filter_status <- function(df) {
      if (input$status == "Total") {
        df %>%
          filter(status %in% c("Announced", "Opened")) %>%
          group_by(across(-c(inflow_value, inflow_job, inflow_n, status))) %>%
          summarise(
            inflow_value = sum(inflow_value),
            inflow_job   = sum(inflow_job),
            inflow_n     = sum(inflow_n),
            .groups = "drop"
          )
      } else {
        df %>% filter(status == input$status)
      }
    }
    
    
    scale_factor <- reactive({
      if (input$metric == "inflow_value") 1000 else 1
    })
    
    metric_label <- reactive({
      switch(
        input$metric,
        inflow_value = "Volume (USD Billion)",
        inflow_job   = "Jobs Created",
        inflow_n     = "Number of Projects"
      )
    })
    
    observeEvent(fdi_country_inflow, {
      yrs <- sort(unique(fdi_country_inflow$year))
      updateSliderInput(
        session,
        "year_range",
        min = min(yrs),
        max = max(yrs),
        value = c(min(yrs), max(yrs))
      )
    })
    
    
    inflow_metric <- reactive({
      input$metric
    })
    
    outflow_metric <- reactive({
      sub("^inflow_", "outflow_", input$metric)
    })
    
    
    # reactive filtered datasets for highhcarter
    
    inflow_filtered <- reactive({
      fdi_country_inflow %>%
        filter_years() %>%
        filter_status() %>%
        group_by(dest_country) %>%
        summarise(value = sum(.data[[input$metric]], na.rm=TRUE) / scale_factor()) %>%
        arrange(desc(value)) %>%
        slice(1:input$top_n)
    })
    
    outflow_filtered <- reactive({
      fdi_country_outflow %>%
        filter_years() %>%
        filter_status() %>%
        group_by(src_country) %>%
        summarise(
          value = sum(.data[[outflow_metric()]], na.rm = TRUE) / scale_factor(),
          .groups = "drop"
        ) %>%
        arrange(desc(value)) %>%
        slice(1:input$top_n)
    })
    
    
    top_inflow_countries  <- reactive(inflow_filtered()$dest_country)
    top_outflow_countries <- reactive(outflow_filtered()$src_country)
    
    
    inflow_trend <- reactive({
      fdi_country_inflow %>%
        filter_years() %>%
        filter_status() %>%
        filter(dest_country %in% top_inflow_countries()) %>%
        group_by(year, dest_country) %>%
        summarise(value = sum(.data[[input$metric]], na.rm=TRUE) / scale_factor(),
                  .groups="drop")
    })
    
    
    outflow_trend <- reactive({
      fdi_country_outflow %>%
        filter_years() %>%
        filter_status() %>%
        filter(src_country %in% top_outflow_countries()) %>%
        group_by(year, src_country) %>%
        summarise(
          value = sum(.data[[outflow_metric()]], na.rm = TRUE) / scale_factor(),
          .groups = "drop"
        )
    })
    
    

    # highchart outputs based on filters and selected chart type 
    
    # inflow output
    
    output$inflow_chart <- renderHighchart({
      
      theme <- input$theme_mode %||% "dark"
      text_col  <- if (theme == "light") "#1e1f21" else "#ffffff"
      grid_col  <- if (theme == "light") "#c8c8c8" else "#c8c8c8"
      palette   <- if (theme == "light") FDI_PALETTE_25_LIGHT else FDI_PALETTE_25_DARK
      
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      decimals <- if (input$metric == "inflow_value") 2 else 0
      suffix   <- if (input$metric == "inflow_value") " B" else ""
      
      
      ## bar mode
      
      if (input$inflow_view == "bar") {
        
        df <- inflow_filtered()
        
        hc <- highchart() %>%
          hc_title(
            text = paste0("Top ", input$top_n, " Countries by ", input$status, " Greenfield FDI Inflow ", metric_label()),
            style = list(color=text_col, fontSize="16px", fontWeight="600")
          )%>%
          hc_chart(type="column", backgroundColor=bg_col) %>%
          hc_colors(palette) %>%
          
          hc_xAxis(
            categories=df$dest_country,
            labels=list(style=list(color=text_col)),
            lineColor=text_col,
            tickColor=text_col
          ) %>%
          
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=text_col)),
            labels=list(style=list(color=text_col)),
            gridLineColor=grid_col
          ) %>%
          
          hc_add_series(
            name=paste("Inflow —", input$status),
            data=df$value,
            colorByPoint=TRUE
          )%>%
          hc_exporting(enabled=TRUE)
        
        
      } else {
        
       #  trend mode
        
        df <- inflow_trend()
        years <- sort(unique(df$year))
        
        hc <- highchart() %>%
          hc_title(
            text = paste0("Top ", input$top_n, " Countries by ", input$status, " Greenfield FDI Inflow ", metric_label()),
            style = list(color=text_col, fontSize="16px", fontWeight="600")
          )%>%
          hc_chart(type="line", backgroundColor=bg_col) %>%
          hc_colors(palette) %>%
          hc_xAxis(
            categories=years,
            labels=list(style=list(color=text_col))
          ) %>%
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=text_col)),
            labels=list(style=list(color=text_col)),
            gridLineColor=grid_col
          )
        
        for (cty in unique(df$dest_country)) {
          series_df <- df %>% filter(dest_country == cty) %>% arrange(year)
          hc <- hc %>% hc_add_series(name = cty, data = series_df$value, type="line")
        }
      }
      
      hc %>%
        hc_tooltip(shared=TRUE, valueDecimals=decimals, valueSuffix=suffix) %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_exporting(enabled=TRUE)
    })
    
    
    # outflow: same type of output
    
    output$outflow_chart <- renderHighchart({
      
      theme <- input$theme_mode %||% "dark"
      text_col  <- if (theme == "light") "#1e1f21" else "#ffffff"
      grid_col  <- if (theme == "light") "#c8c8c8" else "#c8c8c8"
      palette   <- if (theme == "light") FDI_PALETTE_25_LIGHT else FDI_PALETTE_25_DARK
      
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      decimals <- if (input$metric == "inflow_value") 2 else 0
      suffix   <- if (input$metric == "inflow_value") " B" else ""
      
      
      if (input$outflow_view == "bar") {
        
        df <- outflow_filtered()
        
        hc <- highchart() %>%
          hc_title(
            text = paste0("Top ", input$top_n, " Countries by ", input$status, " Greenfield FDI Outflow ", metric_label()),
            style = list(color=text_col, fontSize="16px", fontWeight="600")
          )%>%
          hc_chart(type="column", backgroundColor= bg_col) %>%
          hc_colors(palette) %>%
          
          hc_xAxis(
            categories=df$src_country,
            labels=list(style=list(color=text_col)),
            lineColor=text_col,
            tickColor=text_col
          ) %>%
          
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=text_col)),
            labels=list(style=list(color=text_col)),
            gridLineColor=grid_col
          ) %>%
          
          hc_add_series(
            name=paste("Outflow —", input$status),
            data=df$value,
            colorByPoint=TRUE
          )%>%
          hc_exporting(enabled=TRUE)
        
        
      } else {
        
        df <- outflow_trend()
        years <- sort(unique(df$year))
        
        hc <- highchart() %>%
          hc_title(
            text = paste0("Top ", input$top_n, " Countries by ", input$status, " Greenfield FDI Outflow ", metric_label()),
            style = list(color=text_col, fontSize="16px", fontWeight="600")
          )%>%
          hc_chart(type="line", backgroundColor=bg_col) %>%
          hc_colors(palette) %>%
          hc_xAxis(
            categories=years,
            labels=list(style=list(color=text_col))
          ) %>%
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=text_col)),
            labels=list(style=list(color=text_col)),
            gridLineColor=grid_col
          )
        
        for (cty in unique(df$src_country)) {
          series_df <- df %>% filter(src_country == cty) %>% arrange(year)
          hc <- hc %>% hc_add_series(name = cty, data = series_df$value, type="line")
        }
      }
      
      hc %>%
        hc_tooltip(shared=TRUE, valueDecimals=decimals, valueSuffix=suffix) %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_exporting(enabled=TRUE)
    })
    
    
    
    ## data filtered for map
    
    map_filtered <- reactive({
      req(fdi_country_flow)
      
      df <- fdi_country_flow %>%
        filter(
          year >= input$map_year_range[1],
          year <= input$map_year_range[2]
        )
      
      # Status 
      if (input$map_status == "Total") {
        df <- df %>%
          filter(status %in% c("Announced", "Opened")) %>%
          group_by(country, iso3) %>%
          summarise(
            flow_value    = sum(flow_value, na.rm=TRUE),
            flow_job      = sum(flow_job, na.rm=TRUE),
            flow_n        = sum(flow_n, na.rm=TRUE),
            netflow_value = sum(netflow_value, na.rm=TRUE),
            netflow_job   = sum(netflow_job, na.rm=TRUE),
            netflow_n     = sum(netflow_n, na.rm=TRUE),
            .groups="drop"
          )
      } else {
        df <- df %>%
          filter(status == input$map_status) %>%
          group_by(country, iso3) %>%
          summarise(
            flow_value    = sum(flow_value, na.rm=TRUE),
            flow_job      = sum(flow_job, na.rm=TRUE),
            flow_n        = sum(flow_n, na.rm=TRUE),
            netflow_value = sum(netflow_value, na.rm=TRUE),
            netflow_job   = sum(netflow_job, na.rm=TRUE),
            netflow_n     = sum(netflow_n, na.rm=TRUE),
            .groups="drop"
          )
      }
      
      df
    })
    
    observeEvent(fdi_country_flow, {
      yrs <- sort(unique(fdi_country_flow$year))
      updateSliderInput(
        session,
        "map_year_range",
        min = min(yrs),
        max = max(yrs),
        value = c(min(yrs), max(yrs))
      )
    })
    
    ### map data with iso3 mapping for worldgeojson
    
    map_data <- reactive({
      df <- map_filtered()
      req(nrow(df) > 0)
      
      metric <- input$map_metric           
      flowtype <- input$map_flow_type      
      
      var_name <- if (flowtype == "flow") {
        metric                         
      } else {
        paste0("net", metric)         
      }
      
      df %>%
        transmute(
          iso3  = iso3,
          value = .data[[var_name]]
        )
    })
    
    
    ## map output 
    
    output$flow_map <- renderHighchart({
      
      df <- map_data()
      req(nrow(df) > 0)
      
      theme <- input$theme_mode %||% "dark"
      
      text_col   <- if (theme=="light") "#1e1f21" else "#ffffff"
      border_col <- if (theme=="light") "#d0d0d0" else "#444444"
      null_col   <- if (theme=="light") "#eeeeee" else "#444444"
      
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      metric_label <- switch(
        input$map_metric,
        flow_value = "Volume",
        flow_job   = "Number of Jobs",
        flow_n     = "Number of Projects"
      )
      
      metric_label <- if (input$map_flow_type == "flow") {
        paste("Total", metric_label, "(In + Out)")
      } else {
        paste("Net", metric_label, "(In − Out)")
      }
      
      metric_label <- paste0("Greenfield FDI by ", input$map_status," ", metric_label)
      
      mapdata <- worldgeojson
      
      map_series <- data.frame(
        iso3  = unname(as.character(df$iso3)),
        value = unname(as.numeric(df$value))
      )
      
      vals <- map_series$value
      q <- as.numeric(quantile(vals, probs = c(0, .15, .30, .50, .70, .85, 1), na.rm=TRUE))
      
      stops <- list(
        list(0.00, '#6d3040'),
        list(0.15, '#cd624f'),
        list(0.30, '#dabc41'),
        list(0.50, '#e9d584'),
        list(0.70, '#bcd285'),
        list(0.85, '#84A76D'),
        list(1.00, '#4c7d55')
      )

      is_money <- input$map_metric == "flow_value"
      
      highchart(type = "map") %>%
        hc_add_series(
          mapData = mapdata,
          data = map_series,
          joinBy = c("iso-a3", "iso3"),
          name = metric_label,
          borderColor = border_col,
          nullColor = null_col,
          tooltip = list(
            pointFormatter = JS(
              sprintf(
                "function() {
         let v = this.value;
         return this.name + ': <b>' + %s + '</b>';
       }",
                if (is_money)
                  "Highcharts.numberFormat(v / 1000, 2) + ' B'"
                else
                  "v"
              )
            )
          )
          
        )%>%
        hc_colorAxis(
          min = q[1],
          max = q[7],
          stops = stops,
          labels = list(
            style = list(color = text_col),
            formatter = JS(
              if (is_money)
                "function() { return Highcharts.numberFormat(this.value / 1000, 0) + ' B'; }"
              else
                "function() { return this.value; }"
            )
          ),
          
          legend = list(
            layout = "horizontal",
            width = 600,        
            height = 12,
            align = "center",
            floating = FALSE
          )
        )%>%
        hc_mapNavigation(enabled = TRUE) %>%    
        hc_chart(
          zooming = list(
            mouseWheel = TRUE,                 
            singleTouch = TRUE
          )
        )%>%
        hc_title(
          text = metric_label,
          style=list(color=text_col, fontSize="18px")
        ) %>%
        hc_chart(backgroundColor= bg_col) %>%
        hc_legend(itemStyle=list(color=text_col)) %>%
        hc_exporting(enabled=TRUE)
    })
    
  })
}

    




















#################################################################################################################
#################################################################################################################
#################################################################################################################






# UI MODULE 3: aggregate/block analysis 




totalFDI_aggregates_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "info-banner",
      HTML(
        "Explore Greenfield FDI flows by regional aggregates. This page contains definitions for each aggregate, data on their summary flows and sectoral trends <br>
        Countries belonging to multiple Aggregates have been kept in all aggregates.
        New entries in BRICS have not been included for now as they all happened from 2024 onwards."
        )
    ),
    
    div(
      class = "aggregate-kpi-grid",
      
      lapply(names(AGGREGATE_DEFINITIONS), function(a) {
        div(
          class = paste("aggregate-kpi-card", tolower(a)),  
          div(class="aggregate-kpi-title", a),
          div(class="aggregate-kpi-desc", AGGREGATE_DEFINITIONS[[a]])
        )
      })
    ),
    
   # control panel
   
    div(
      style = "display:flex; justify-content:center; width:100%;",
      div(
        class = "country-control-panel",
        style = "
          display:flex;
          flex-direction:row;
          justify-content:space-between;
          gap:40px;
          padding:15px 20px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
          margin-top:40px;
          width:90%;
        ",
        
        div(
          style="flex:1;",
          div(class="left-menu-title", "Select Metric"),
          radioButtons(
            ns("metric"),
            NULL,
            choices = c(
              "Volume" = "flow_value",
              "Jobs Created"       = "flow_job",
              "Number of Projects"           = "flow_n"
            ),
            inline = TRUE,
            selected = "flow_value"
          ),
          
          div(class="left-menu-title", style="margin-top:15px;", "Select Status"),
          radioButtons(
            ns("status"),
            NULL,
            choices = c("Announced","Opened","Total"),
            inline = TRUE,
            selected = "Opened"
          )
        ),
        

        
        div(
          style="flex:1;",
          div(class="left-menu-title", "Timeframe"),
          sliderInput(
            ns("year_range"),
            NULL,
            min = 2003, max = MAX_YEAR,
            value = c(2003, MAX_YEAR),
            width="100%"
          )
        )
      )
    ),
    
    div(
      class = "two-chart-row",
      
      # output for inflow chart ad outflow chart (exactly as in "country" tab)
      
      div(class="chart-box",
          div(style="display:flex; justify-content:space-between; align-items:center;",
              selectInput(ns("agg_inflow_view"), NULL,
                          choices = c("Bar"="bar", "Pie"="pie", "Trend"="trend"),
                          width="120px")
          ),
          withSpinner(
            highchartOutput(ns("agg_inflow_chart"), height="450px"),
            type = 6, color = "#b984db"
          )
      ),
      
      div(class="chart-box",
          div(style="display:flex; justify-content:space-between; align-items:center;",
              selectInput(ns("agg_outflow_view"), NULL,
                          choices = c("Bar"="bar", "Pie" = "pie", "Trend"="trend"),
                          width="120px")
          ),
          withSpinner(
            highchartOutput(ns("agg_outflow_chart"), height="450px"),
            type = 6, color = "#b984db"
          )
      )
    ),
    
    div(
        class = "info-banner",
        HTML(
          "The following Sankey Tool allows you to explore in detail the sectoral path of Aggregates´ FDI. For each aggregate, you are able to
          follow how FDI flows decompose among sector, subsector and activity subgroups*. Use the left side menu to filter more in detail for flow type, metric and status <br>
          *(sector labelling follows standard fDi Markets classification)"
        )),
    
    

    # SANKEY section
   
   ## menu selection
   
    div(
      style="
    display:flex;
    margin-top:60px;
    border:2px solid var(--text-main);
    border-radius:12px;
    background:rgba(255,255,255,0.02);
    overflow:hidden;
    width:90%;
    margin-bottom:100px;
    margin-left:100px;
  ",
      
      div(
        style="
      width:230px;
      padding:18px;
      border-right:1px solid var(--border);
      background:rgba(255,255,255,0.04);
      display:flex;
      flex-direction:column;
      font-size:13px;
    ",
        
        div(class="left-menu-title", "Sankey Options"),
        
        div(
          style="margin-top:15px;",
          div("Metric:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
          selectInput(
            ns("sankey_metric"), NULL,
            choices = c(
              "Volume" = "flow_value",
              "Jobs Created"       = "flow_job",
              "Number of Projects" = "flow_n"
            ),
            width="100%"
          )
        ),
        
        div(
          style="margin-top:20px;",
          div("Status:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
          selectInput(
            ns("sankey_status"), NULL,
            choices = c("Opened","Announced","Total"),
            width="100%"
          )
        ),
        
        div(
          style="margin-top:20px;",
          div("Flow Type:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
        selectInput(
          ns("sankey_flow_type"), NULL,
          choices = c(
            "Inflow" = "inflow",
            "Outflow" = "outflow",
            "Total Flow (In + Out)" = "flow",
            "Net Flow (In − Out)"   = "netflow"
          ),
          width = "100%"
        )
        ),
        
        div(
          style="margin-top:20px;",
          div("Timeframe:", style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
          sliderInput(
            ns("sankey_year_range"), NULL,
            min = 2003,
            max = MAX_YEAR,
            value = c(2003, MAX_YEAR),
            width="100%"
          )
        )
        ),
        
      
      # RIGHT PANEL: sankey output
      div(
        style="flex-grow:1; padding:10px 20px;",
        h4("FDI Flows – Aggregates → Sectors → Subsectors",
           class="chart-title", style="margin-bottom:15px;"),
        withSpinner(
          highchartOutput(ns("agg_sankey"), height="750px"),
          type=6, color="#b984db"
        )
      )
    )
  )
    
}


# SERVER MODULE 3: aggregate/block analysis 


totalFDI_aggregates_server <- function(id, aggregates_all_flows, agg_sankey) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    AGGREGATE_COLORS <- list(
      "EU"       = "#4682B4",
      "NAFTA"    = "#DC783C",
      "BRICS"    = "#C81E1E",
      "UK"       = "#963296",
      "ASEAN"    = "#FFC832",
      "MERCOSUR" = "#1EB478",
      "AFR"      = "#008000",
      "CIS"      = "#646496"
    )
    

    # usual filter helpers

    filter_years <- function(df) {
      df %>% filter(year >= input$year_range[1],
                    year <= input$year_range[2])
    }
    
    filter_status <- function(df) {
      if (input$status == "Total") {
        df %>%
          filter(status %in% c("Announced", "Opened")) %>%
          group_by(across(-c(
            inflow_value, inflow_job, inflow_n,
            outflow_value, outflow_job, outflow_n,
            flow_value, flow_job, flow_n,
            netflow_value, netflow_job, netflow_n,
            status
          ))) %>%
          summarise(
            inflow_value = sum(inflow_value),
            inflow_job   = sum(inflow_job),
            inflow_n     = sum(inflow_n),
            outflow_value = sum(outflow_value),
            outflow_job   = sum(outflow_job),
            outflow_n     = sum(outflow_n),
            flow_value    = sum(flow_value),
            flow_job      = sum(flow_job),
            flow_n        = sum(flow_n),
            netflow_value = sum(netflow_value),
            netflow_job   = sum(netflow_job),
            netflow_n     = sum(netflow_n),
            .groups = "drop"
          )
      } else {
        df %>% filter(status == input$status)
      }
    }
    

    # metric scaling and labelling

    
    scale_factor <- reactive({
      if (input$metric %in% c("inflow_value","outflow_value","flow_value","netflow_value")) 1000 else 1
    })
    
    metric_label <- reactive({
      switch(
        input$metric,
        inflow_value = "Volume (USD Billion)",
        outflow_value = "Volume (USD Billion)",
        flow_value = "Volume (USD Billion)",
        netflow_value = "Net Volume (USD Billion)",
        
        inflow_job = "Jobs Created",
        outflow_job = "Jobs Created",
        flow_job   = "Jobs Created",
        netflow_job = "Net Jobs",
        
        inflow_n = "Number of Projects",
        outflow_n = "Number of Projects",
        flow_n    = "Number of Projects",
        netflow_n = "Net Projects"
      )
    })
    

    ### outflow and inflow reactive datasets for highcharter

    
    #INFLOW
    agg_inflow_filtered <- reactive({
      aggregates_all_flows %>%
        filter_years() %>%
        filter_status() %>%
        group_by(aggregate) %>%
        summarise(
          value = sum(inflow_value, na.rm = TRUE) / scale_factor(),
          .groups = "drop"
        ) %>%
        arrange(desc(value))
    })
    
    agg_inflow_trend <- reactive({
      aggregates_all_flows %>%
        filter_years() %>%
        filter_status() %>%
        group_by(year, aggregate) %>%
        summarise(
          value = sum(inflow_value, na.rm = TRUE) / scale_factor(),
          .groups = "drop"
        )
    })
    
    # OUTFLOW
    agg_outflow_filtered <- reactive({
      aggregates_all_flows %>%
        filter_years() %>%
        filter_status() %>%
        group_by(aggregate) %>%
        summarise(
          value = sum(outflow_value, na.rm = TRUE) / scale_factor(),
          .groups = "drop"
        ) %>%
        arrange(desc(value))
    })
    
    agg_outflow_trend <- reactive({
      aggregates_all_flows %>%
        filter_years() %>%
        filter_status() %>%
        group_by(year, aggregate) %>%
        summarise(
          value = sum(outflow_value, na.rm = TRUE) / scale_factor(),
          .groups = "drop"
        )
    })
    
    

    # INFLOW CHART
    
    output$agg_inflow_chart <- renderHighchart({
      
      theme <- input$theme_mode %||% "dark"
      text_col  <- if (theme=="light") "#1e1f21" else "#ffffff"
      grid_col  <- if (theme=="light") "#c8c8c8" else "#c8c8c8"
      
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      decimals <- if (input$metric %in% c("inflow_value","outflow_value","flow_value","netflow_value")) 2 else 0
      suffix   <- if (input$metric %in% c("inflow_value","outflow_value","flow_value","netflow_value")) " B" else ""
      
      view <- input$agg_inflow_view
      
      if (view == "bar") {
        
        df <- agg_inflow_filtered()
        
        hc <- highchart() %>%
          hc_title(text = paste0(input$status, " Greenfield FDI Inflow ", metric_label()),
                   style = list(color=text_col)) %>%
          hc_chart(type="column", backgroundColor= bg_col) %>%
          hc_xAxis(categories=df$aggregate,
                   labels=list(style=list(color=text_col)),
                   lineColor=text_col, tickColor=text_col) %>%
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=text_col)),
            labels=list(style=list(color=text_col)),
            gridLineColor=grid_col
          ) %>%
          hc_add_series(
            name="Inflow",
            data = purrr::map2(
              df$aggregate, df$value,
              ~ list(name=.x, y=.y, color = AGGREGATE_COLORS[[.x]] %||% "#A0A7BA")
            )
          ) %>%
          hc_exporting(enabled=TRUE)
        
      } else if (view == "pie") {
        
        df <- agg_inflow_filtered()
        
        hc <- highchart() %>%
          hc_chart(type="pie", backgroundColor= bg_col) %>%
          hc_title(
            text = paste0(input$status, " Greenfield FDI Inflow ", metric_label(), " - Share"),
            style = list(color=text_col)
          ) %>%
          hc_plotOptions(
            pie = list(
              allowPointSelect = TRUE,
              cursor = "pointer",
              dataLabels = list(
                enabled = TRUE,
                format = "<b>{point.name}</b>: {point.percentage:.1f}%",
                style = list(
                  color = text_col,
                  fontSize = "12px",
                  textOutline = "none"
                )
              )
            )
          ) %>%
          hc_add_series(
            name = "Share",
            data = purrr::map2(
              df$aggregate, df$value,
              ~ list(
                name  = .x,
                y     = .y,
                color = AGGREGATE_COLORS[[.x]] %||% "#A0A7BA"
              )
            )
          ) %>%
          hc_exporting(enabled=TRUE)
        
      } else {
        
        df <- agg_inflow_trend()
        years <- sort(unique(df$year))
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor= bg_col) %>%
          hc_title(text=paste0(input$status, " Greenfield FDI Inflow ", metric_label(), " - Trend" ),
                   style=list(color=text_col)) %>%
          hc_xAxis(categories=years,
                   labels=list(style=list(color=text_col))) %>%
          hc_yAxis(title=list(text=metric_label(), style=list(color=text_col)),
                   labels=list(style=list(color=text_col)),
                   gridLineColor=grid_col) %>%
          hc_exporting(enabled=TRUE)
        
        for (agg in unique(df$aggregate)) {
          series_df <- df %>% filter(aggregate == agg) %>% arrange(year)
          hc <- hc %>% hc_add_series(
            name = agg,
            data = series_df$value
          )
        }
        
      }
      
      
      hc <- hc %>%
        hc_tooltip(shared=TRUE, valueDecimals=decimals, valueSuffix=suffix) %>%
        hc_legend(
          itemStyle = list(color = text_col),
          itemHoverStyle = list(color = text_col),
          itemHiddenStyle = list(color = grid_col),
          title = list(style = list(color = text_col))
        ) %>%
        hc_exporting(enabled = TRUE)
    })
    
    

    # OUTFLOW CHART

    
    output$agg_outflow_chart <- renderHighchart({
      
      theme <- input$theme_mode %||% "dark"
      text_col  <- if (theme=="light") "#1e1f21" else "#ffffff"
      grid_col  <- if (theme=="light") "#c8c8c8" else "#c8c8c8"
      
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      decimals <- if (input$metric %in% c("outflow_value","inflow_value","flow_value","netflow_value")) 2 else 0
      suffix   <- if (input$metric %in% c("outflow_value","inflow_value","flow_value","netflow_value")) " B" else ""
      
      view <- input$agg_outflow_view
      
      if (view == "bar") {
        
        df <- agg_outflow_filtered()
        
        hc <- highchart() %>%
          hc_title(text = paste0(input$status, " Greenfield FDI Outflow ", metric_label()),
                   style=list(color=text_col)) %>%
          hc_chart(type="column", backgroundColor= bg_col) %>%
          hc_xAxis(categories=df$aggregate,
                   labels=list(style=list(color=text_col)),
                   lineColor=text_col, tickColor=text_col) %>%
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=text_col)),
            labels=list(style=list(color=text_col)),
            gridLineColor=grid_col
          ) %>%
          hc_add_series(
            name="Outflow",
            data = purrr::map2(
              df$aggregate, df$value,
              ~ list(name=.x, y=.y, color = AGGREGATE_COLORS[[.x]] %||% "#A0A7BA")
            )
          ) %>%
          hc_exporting(enabled=TRUE)
        
      } else if (view == "pie") {
        
        df <- agg_outflow_filtered()
        
        hc <- highchart() %>%
          hc_chart(type="pie", backgroundColor= bg_col) %>%
          hc_title(
            text = paste0(input$status, " Greenfield FDI outflow ", metric_label(), " - Share"),
            style = list(color=text_col)
          ) %>%
          hc_plotOptions(
            pie = list(
              allowPointSelect = TRUE,
              cursor = "pointer",
              dataLabels = list(
                enabled = TRUE,
                format = "<b>{point.name}</b>: {point.percentage:.1f}%",
                style = list(
                  color = text_col,
                  fontSize = "12px",
                  textOutline = "none"
                )
              )
            )
          ) %>%
          hc_add_series(
            name = "Share",
            data = purrr::map2(
              df$aggregate, df$value,
              ~ list(
                name  = .x,
                y     = .y,
                color = AGGREGATE_COLORS[[.x]] %||% "#A0A7BA"
              )
            )
          ) %>%
          hc_exporting(enabled=TRUE)
        
      } else {
        
        df <- agg_outflow_trend()
        years <- sort(unique(df$year))
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor= bg_col) %>%
          hc_title(text=paste0(input$status, " Greenfield FDI Outflow ", metric_label(), " - Trend" ),
                   style=list(color=text_col)) %>%
          hc_xAxis(categories=years,
                   labels=list(style=list(color=text_col))) %>%
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=text_col)),
            labels=list(style=list(color=text_col)),
            gridLineColor=grid_col
          ) %>%
          hc_exporting(enabled=TRUE)
        
        for (agg in unique(df$aggregate)) {
          series_df <- df %>% filter(aggregate == agg) %>% arrange(year)
          hc <- hc %>% hc_add_series(
            name = agg,
            data = series_df$value
          )
        }
        
      }
      
      hc <- hc %>%
        hc_tooltip(shared=TRUE, valueDecimals=decimals, valueSuffix=suffix) %>%
        hc_legend(
          itemStyle = list(color = text_col),
          itemHoverStyle = list(color = text_col),
          itemHiddenStyle = list(color = grid_col),
          title = list(style = list(color = text_col))
        ) 
      })
    
    
    
    
    ###### SANKEY section
    
    
    ### filtering and helpers
    
    observeEvent(agg_sankey, {
      req(agg_sankey)
      
      yrs <- sort(unique(agg_sankey$year))
      if (length(yrs) == 0) return()
      
      updateSliderInput(
        session,
        "sankey_year_range",
        min = min(yrs),
        max = max(yrs),
        value = c(min(yrs), max(yrs))
      )
    })
    
    sankey_df <- reactive({
      req(agg_sankey)
      
      df0 <- agg_sankey %>%
        filter(year >= input$sankey_year_range[1],
               year <= input$sankey_year_range[2],
               status == input$sankey_status)
      
      # Building of directional tidy datasets (one row per aggregate/sector/subsector/activity) for sankey
      inflow_df <- df0 %>%
        select(aggregate, year, status, Sector, Subsector, Activity,
               inflow_value, inflow_job, inflow_n) %>%
        rename(
          value_value = inflow_value,
          value_job   = inflow_job,
          value_n     = inflow_n
        ) %>%
        mutate(direction = "inflow")
      
      outflow_df <- df0 %>%
        select(aggregate, year, status, Sector, Subsector, Activity,
               outflow_value, outflow_job, outflow_n) %>%
        rename(
          value_value = outflow_value,
          value_job   = outflow_job,
          value_n     = outflow_n
        ) %>%
        mutate(direction = "outflow")
      
      # Combined flow 
      flow_df <- bind_rows(inflow_df, outflow_df) %>% mutate(direction = "flow")
      
      # Netflow: inflow - outflow 
      net_df <- df0 %>%
        select(aggregate, year, status, Sector, Subsector, Activity,
               inflow_value, outflow_value, inflow_job, outflow_job, inflow_n, outflow_n) %>%
        group_by(aggregate, year, status, Sector, Subsector, Activity) %>%
        summarise(
          value_value = sum(inflow_value, na.rm = TRUE) - sum(outflow_value, na.rm = TRUE),
          value_job   = sum(inflow_job,   na.rm = TRUE) - sum(outflow_job,   na.rm = TRUE),
          value_n     = sum(inflow_n,     na.rm = TRUE) - sum(outflow_n,     na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(direction = "netflow")
      
      #  dataset based on flow_type
      chosen <- switch(
        input$sankey_flow_type,
        inflow  = inflow_df,
        outflow = outflow_df,
        flow    = flow_df,
        netflow = net_df,
        # fallback case:
        flow_df
      )
      
      metric_input <- input$sankey_metric
      
      
      
      
      suffix_map <- list(value = "value_value", job = "value_job", n = "value_n")
      if (grepl("_", metric_input)) {
        suffix <- sub(".*_", "", metric_input)
        value_col <- suffix_map[[suffix]] %||% "value_value"
      } else {
        value_col <- suffix_map[[metric_input]] %||% "value_value"
      }
      
      df <- chosen %>%
        mutate(value = .data[[value_col]]) %>%
        filter(!is.na(value) & value > 0)
      
      top_n_sectors     <- 10
      top_n_subsectors  <- 10
      top_n_activities  <- 10
      
      top_sectors <- df %>%
        group_by(Sector) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = top_n_sectors) %>%
        pull(Sector)
      
      df <- df %>% filter(Sector %in% top_sectors)
      
      top_subsectors <- df %>%
        group_by(Subsector) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = top_n_subsectors) %>%
        pull(Subsector)
      
      df <- df %>% filter(Subsector %in% top_subsectors)
      
      top_activities <- df %>%
        group_by(Activity) %>%
        summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total)) %>%
        slice_head(n = top_n_activities) %>%
        pull(Activity)
      
      df <- df %>% filter(Activity %in% top_activities)
      
      # final safety: we drop missing strings to fix issues 
      
      df <- df %>% filter(!is.na(aggregate) & aggregate != "",
                          !is.na(Sector)    & Sector    != "",
                          !is.na(Subsector) & Subsector != "",
                          !is.na(Activity)  & Activity  != "")
      
      df
    })
    
      #  corrected sankey plot 
    output$agg_sankey <- renderHighchart({
      df <- sankey_df()
      req(nrow(df) > 0)
      
      theme <- input$theme_mode %||% "dark"
      text_col <- if (theme == "light") "#1e1f21" else "#ffffff"
      
      bg_col     <- if (theme == "light") "#f2f4f7" else "#2a2b2d"
      
      
      
      metric_label <- reactive({
        switch(input$sankey_metric,
               "flow_value"="Capital Investment (USD Million)",
               "flow_job"="Jobs Created",
               "flow_n"="Projects")
      })
      
      # build links for sankey nodes
      link_agg_sec <- df %>%
        group_by(aggregate, Sector) %>%
        summarise(weight = sum(value, na.rm = TRUE), .groups = "drop") %>%
        transmute(from = as.character(aggregate), to = as.character(Sector), weight = weight)
      
      link_sec_sub <- df %>%
        group_by(Sector, Subsector) %>%
        summarise(weight = sum(value, na.rm = TRUE), .groups = "drop") %>%
        transmute(from = as.character(Sector), to = as.character(Subsector), weight = weight)
      
      link_sub_act <- df %>%
        group_by(Subsector, Activity) %>%
        summarise(weight = sum(value, na.rm = TRUE), .groups = "drop") %>%
        transmute(from = as.character(Subsector), to = as.character(Activity), weight = weight)
      
      links <- bind_rows(link_agg_sec, link_sec_sub, link_sub_act)
      
      # nodes ( order preserved but  unique)
      nodes <- tibble(name = unique(c(links$from, links$to)))
      
      
      tooltip_bg <- if ((input$theme_mode %||% "dark") == "dark") "#000000" else "#ffffff"
      tooltip_txt <- if ((input$theme_mode %||% "dark") == "dark") "#ffffff" else "#1e1f21"
      
      
      #  sankey highcharter
      highchart() %>%
        hc_chart(type = "sankey", backgroundColor = bg_col) %>%
        hc_add_series(
          type = "sankey",
          keys = c("from", "to", "weight"),
          data = list_parse(links),
          nodes = list_parse(nodes),
          name = "FDI flows",
          linkOpacity = 0.55,
          nodeWidth = 36,
          minLinkWidth = 6
        ) %>%
        hc_plotOptions(
          sankey = list(
            nodePadding = 22,
            curveFactor = 0.45,
            dataLabels = list(
              enabled = TRUE,
              color = text_col,
              style = list(fontSize = "12px", fontWeight = "600", textOutline = "none")
            )
          )
        ) %>%
        hc_title(
          text = paste0(
            "Greenfield FDI Sankey — ", input$sankey_status, " — ",
              metric_label(), " ",
             switch(input$sankey_flow_type, inflow="Inflow", outflow="Outflow", flow="Total", netflow="Net")
          ),
          style = list(color = text_col, fontSize = "16px")
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = tooltip_bg,
          borderColor     = tooltip_bg,
          style = list(
            color = tooltip_txt,
            fontSize = "12px"
          ),
          pointFormatter = JS("
    function () {

      // LINK tooltip
      if (this.from && this.to) {
        return '<b>' + this.from + ' → ' + this.to + '</b><br>' +
               Highcharts.numberFormat(this.weight, 0);
      }

      // NODE tooltip
      if (this.name && this.sum !== undefined) {
        return '<b>' + this.name + '</b><br>' +
               Highcharts.numberFormat(this.sum, 0);
      }

      return null;
    }
  ")
        )%>%
        hc_exporting(enabled = TRUE)
    })
    
    
  })
}













########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################






# UI MODULE 4: sector totals




totalFDI_sector_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "info-banner",
      style = "margin-bottom: 25px;",
      HTML(
        "Explore Greenfield FDI flows across Sectors, Subsectors, and Activities following fDi Markets´ internal classification.<br>
        Explore the taxonomy of each label and use the menu panel below to filter sectoral flows in great detail" 
      )
    ),
    
    # KPI 
    div(
      class = "kpi-container",
      
      div(class = "kpi-card kpi-blue",
          h3("Sectors"),
          div(class = "kpi-number-static", "37 Sectors organized to fit FDI project types")
      ),
      
      div(class = "kpi-card kpi-orange",
          h3("Subsectors"),
          div(class = "kpi-number-static", "269 categories for more granularity")
      ),
      
      div(class = "kpi-card kpi-green",
          h3("Activities"),
          div(class = "kpi-number-static", "Type of investment project (NOT an Industry)")
      )
    ),
    

    # CONTROL PANEL (always the same now with sector level selection)

    div(
      style = "display:flex; justify-content:center; width:100%;",
      div(
        class = "country-control-panel",
        style = "
          display:flex;
          flex-direction:row;
          justify-content:space-between;
          gap:40px;
          padding:15px 20px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
          margin-top:40px;
          width:90%;
        ",
        
        div(
          style="flex:1;",
          div(class="left-menu-title", "Select Level"),
          selectInput(ns("level"), NULL,
                      choices = c("Sector","Subsector","Activity"),
                      width="100%"),
          
          div(class="left-menu-title", style="margin-top:15px;", "Top N Items"),
          sliderInput(ns("top_n"), NULL,
                      min = 3, max = 20, value = 10, step = 1,
                      width="100%")
        ),
        
        div(
          style="flex:1;",
          div(class="left-menu-title", "Select Metric"),
          radioButtons(ns("metric"),
                       NULL,
                       choices = c("Capital Investment"="flow_value",
                                   "Jobs Created"="flow_job",
                                   "Projects"="flow_n"),
                       inline = TRUE,
                       selected = "flow_value"),
          
          div(class="left-menu-title", style="margin-top:15px;", "Select Status"),
          radioButtons(ns("status"),
                       NULL,
                       choices = c("Announced","Opened","Total"),
                       inline = TRUE,
                       selected = "Opened")
        ),
        
        div(
          style="flex:1;",
          div(class="left-menu-title", "Timeframe"),
          sliderInput(ns("year_range"), NULL,
                      min = 2003, max = MAX_YEAR,
                      value = c(2003, MAX_YEAR),
                      width="100%")
        )
      )
    ),
    
    # Two chart row as before
    
    
    div(
      class = "two-chart-row",
      div(
        class="chart-box",
        style="width:50%;",
        div(style="display:flex; justify-content:flex-end;",
            selectInput(ns("chart_view"), NULL,
                        choices = c("Bar"="bar", "Pie"="pie", "Trend"="trend"),
                        width="120px")),
        withSpinner(
          highchartOutput(ns("sector_chart"), height="450px"),
          type = 6, color = "#b984db"
        )
      ),
      
      div(
        class="chart-box",
        style="width:50%; margin-left:20px;",
        withSpinner(
          highchartOutput(ns("sector_sankey"), height="450px"),
          type = 6, color = "#b984db"
        )
      )
    ),
    
   
    
    
    ### donut pie graph tool section: in depth sector composition
    
    div(
      class = "info-banner",
      style = "margin-top:40px; margin-bottom:25px;",
      HTML("The following 'Donut' tool allows you to explore more in detail how each Sector´s FDI flows are distributed across Activities, Countries, Projects of different sizes, and so on. Use the left side panel to filter for Metric, Status and Timeframe. Click on a sector from the donut to explore further metrics. Filters apply both to the Donut as well as to some of the specific sectoral charts")
    ),
    
    div(
      id = ns("pie_panel_container"),
      style="
    display:flex;
    margin-top:20px;
    margin-left:20px;
    border:2px solid var(--text-main);
    border-radius:12px;
    background:rgba(255,255,255,0.02);
    overflow:hidden;
    margin-bottom:60px;
  ",
      
      div(
        style="
      width:230px;
      padding:18px;
      border-right:1px solid var(--border);
      background:rgba(255,255,255,0.04);
      display:flex;
      flex-direction:column;
      font-size:13px;
    ",
        
        div(class="left-menu-title","Pie Options"),
        
        div(style="margin-top:18px;",
            div("Status:",style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
            selectInput(ns("pie_status"),NULL,
                        choices=c("Announced","Opened","Total"),
                        selected="Opened", width="100%")
        ),
        
        div(style="margin-top:20px;",
            div("Metric:",style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
            selectInput(ns("pie_metric"),NULL,
                        choices=c("Volume"="flow_value",
                                  "Jobs Created"="flow_job",
                                  "Projects"="flow_n"),
                        width="100%")
        ),
        
        div(style="margin-top:20px;",
            div("Timeframe:",style="font-size:12px;color:var(--text-muted);margin-bottom:6px;"),
            sliderInput(ns("pie_years"),NULL,
                        min=2003,max=MAX_YEAR,
                        value=c(2003,MAX_YEAR),
                        width="100%")
        ),
        
        
        

        # back button from drilldown 

        conditionalPanel(
          condition = sprintf("input['%s'] != null", ns("sector_clicked")),
          div(
            style = "margin-top:40px;",
            actionButton(
              ns("pie_back"),
              "← Back to all sectors",
              class = "btn btn-secondary",
              style = "
            width:100%;
            font-size:14px;
            background-color:#5552;
            color:var(--text-main);
            border:1px solid var(--border);
          "
            )
          )
        )
      ),
      

      # right panel: donut chart and eventually drilldown dadshboard

      div(
        style="
      flex-grow:1;
      padding:10px 20px;
      display:flex;
      flex-direction:column;
      justify-content:flex-start;
      min-height:80vh;
    ",
        
     #PIE (conditional on when no drilldown)
        conditionalPanel(
          condition = sprintf("input['%s'] == null", ns("sector_clicked")),
          withSpinner(
            highchartOutput(ns("sector_big_pie"), height="80vh"),
            type = 6, color = "#b984db"
          )
        ),
        
        # drilldown (conditional on user clicking on sector)
     
        conditionalPanel(
          condition = sprintf("input['%s'] != null", ns("sector_clicked")),
          
          div(
            style="display:grid; grid-template-columns:1fr 1fr; grid-gap:25px;",
            
            # Chart 1 - Activity Distribution
            div(
              style="background:transparent;",
              withSpinner(
                highchartOutput(ns("drill_activity"), height="420px"),
                type = 6, color = "#b984db"
              )
            ),
            
            # Chart 2 - Project Size Distribution
            div(
              style="background:transparent;",
              withSpinner(
                highchartOutput(ns("drill_size"), height="420px"),
                type = 6, color = "#b984db"
              )
            ),
            
            # Chart 3 - Conversion Ratio Over Time
            div(
              style="background:transparent;",
              withSpinner(
                highchartOutput(ns("drill_conversion"), height="420px"),
                type = 6, color = "#b984db"
              )
            ),
            
            # Chart 4 - Top Countries (flow type selection)
            div(
              style="background:transparent; position:relative;",
              
              div(
                style="
                  position:absolute;
                  top:-30px;
                  right:0px;
                  z-index:10;
                  font-size:12px;
                  width:140px;
                 ",
                selectInput(
                  ns("pie_flowtype"),
                  label = NULL,
                  choices = c(
                    "Inflow"      = "inflow",
                    "Outflow"     = "outflow",
                    "Total flows" = "total"
                  ),
                  selected = "inflow",
                  width = "100%"
                )
              ),
              
              withSpinner(
                highchartOutput(ns("drill_countries"), height="420px"),
                type = 6, color = "#b984db"
              )
            )
          )
      )
    )
  
    )
)
}





# SERVER MODULE 4: sector totals


totalFDI_sector_server <- function(
    id,
    sector_df,
    subsector_df,
    activity_df,
    sec_act_df,
    fdi
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## usual reactive and filtering now with sector levels as well
    
    
    data_level <- reactive({
      switch(input$level,
             "Sector"    = sector_df,
             "Subsector" = subsector_df,
             "Activity"  = activity_df)
    })
    
    
    level_label <- reactive({
      switch(input$level,
             "Sector"    = "Sectors",
             "Subsector" = "Subsectors",
             "Activity"  = "Activities"
      )
    })
    
    level_var <- reactive(rlang::sym(input$level))
    
    filter_years <- function(df) {
      df %>% filter(
        year >= input$year_range[1],
        year <= input$year_range[2]
      )
    }
    
    filter_status <- function(df) {
      lv <- level_var()
      if (input$status == "Total") {
        df %>%
          filter(status %in% c("Announced","Opened")) %>%
          group_by(year, !!lv) %>%
          summarise(
            status="Total",
            flow_value=sum(flow_value),
            flow_job=sum(flow_job),
            flow_n=sum(flow_n),
            .groups="drop"
          )
      } else {
        df %>% filter(status == input$status)
      }
    }
    
    metric_label <- reactive({
      switch(input$metric,
             "flow_value"="Volume (USDM)",
             "flow_job"="Jobs Created",
             "flow_n"="Projects")
    })
    
    metric_title <- reactive({
      switch(input$metric,
             "flow_value"="Volume ",
             "flow_job"="Jobs Created ",
             "flow_n"="Projects ")
    })
    
    data_filtered <- reactive({
      data_level() %>% filter_years() %>% filter_status()
    })
    
    top_vals <- reactive({
      lv <- level_var()
      data_filtered() %>%
        group_by(!!lv) %>%
        summarise(value = sum(.data[[input$metric]], na.rm=TRUE), .groups="drop") %>%
        arrange(desc(value)) %>%
        slice_head(n=input$top_n)
    })
    
    theme_vals <- reactive({
      theme <- input$theme_mode %||% "dark"
      if (theme=="light") list(text="#1e1f21", grid="#c8c8c8", bg_col ="#f2f4f7")
      else                list(text="#ffffff", grid="#c8c8c8", bg_col = "#2a2b2d")
      
      
    })
    
    palette_vals <- reactive({
      theme <- input$theme_mode %||% "dark"
      if (theme=="light") FDI_PALETTE_25_LIGHT else FDI_PALETTE_25_DARK
    })
    
    #render highcharter for main charts
    
    output$sector_chart <- renderHighchart({
      
      
      df_small <- top_vals()
      if (nrow(df_small) == 0) {
        return(highchart() %>% hc_title(text = "No Data"))
      }
      
      df_all <- data_filtered()
      lv_name <- input$level
      view <- input$chart_view
      theme <- theme_vals()
      pal <- palette_vals()
      
  
      common_tooltip <- function(hc) {
        hc %>%
          hc_tooltip(
            useHTML = TRUE,
            backgroundColor = theme$bg,
            borderColor     = theme$bg,
            style = list(
              color = theme$text,
              fontSize = "12px"
            ),
            formatter = JS("
        function () {
          return '<b>Flow:</b> ' +
                 Highcharts.numberFormat(this.y, 2);
        }
      ")
          )
      }
      
      

      # BAR

      if (view == "bar") {
        return(
          highchart() %>%
            hc_chart(type = "column", backgroundColor = theme$bg_col) %>%
            hc_colors(pal) %>%
            hc_title(
              text = paste(
                "Top",input$top_n, "Greenfield FDI ",  level_label(),
                " by ", input$status, metric_title()
              ),
              style = list(color = theme$text)
            ) %>%
            hc_xAxis(
              categories = df_small[[lv_name]],
              labels = list(style = list(color = theme$text))
            ) %>%
            hc_yAxis(
              title = list(text = metric_label(), style = list(color = theme$text)),
              labels = list(style = list(color = theme$text)),
              gridLineColor = theme$grid
            ) %>%
            hc_exporting(enabled=TRUE)%>%
            hc_add_series(
              data = df_small$value,
              colorByPoint = TRUE,
              showInLegend = FALSE,
              name = NULL
            ) %>%
            common_tooltip
        )
      }
      

      # PIE

      if (view == "pie") {
        return(
          highchart() %>%
            hc_chart(type = "pie", backgroundColor = theme$bg_col) %>%
            hc_colors(pal) %>%
            hc_title(
              text = paste(
                "Share - Top",input$top_n, "Greenfield FDI ",  level_label(),
                " by ", input$status, metric_title()
              ),
              style = list(color = theme$text)
            ) %>%
            hc_plotOptions(
              pie = list(
                allowPointSelect = TRUE,
                cursor = "pointer",
                dataLabels = list(
                  enabled = TRUE,
                  format = "<b>{point.name}</b>: {point.percentage:.1f}%",
                  style = list(color = theme$text, textOutline = "none")
                )
              )
            ) %>%
            hc_exporting(enabled=TRUE)%>%
            hc_add_series(
              data = purrr::map2(
                df_small[[lv_name]], df_small$value,
                ~ list(name = .x, y = .y)
              ),
              colorByPoint = TRUE,
              name = NULL
            ) %>%
            common_tooltip
        )
      }
      

      # TREND
      
      years <- sort(unique(df_all$year))
      
      hc <- highchart() %>%
        hc_chart(type = "line", backgroundColor = theme$bg_col) %>%
        hc_colors(pal) %>%
        hc_title(
          text = paste(
            "Trend - Top", input$top_n, "Greenfield FDI ",  level_label(),
            " by ", input$status, metric_title()
          ),
          style = list(color = theme$text)
        ) %>%
        hc_xAxis(
          categories = years,
          labels = list(style = list(color = theme$text))
        ) %>%
        hc_yAxis(
          title = list(text = metric_label(), style = list(color = theme$text)),
          labels = list(style = list(color = theme$text)),
          gridLineColor = theme$grid
        ) %>%
        hc_legend(
          itemStyle = list(color = theme$text),
          itemHoverStyle = list(color = theme$text),
          itemHiddenStyle = list(color = theme$text)
        ) %>%
        common_tooltip %>%
        hc_exporting(enabled=TRUE)
      
      i <- 1
      for (x in df_small[[lv_name]]) {
        df_series <- df_all %>%
          filter(.data[[lv_name]] == x) %>%
          group_by(year) %>%
          summarise(value = sum(.data[[input$metric]]), .groups = "drop")
        
        hc <- hc %>%
          hc_add_series(
            name = x,
            data = df_series$value,
            color = pal[i]
          )
        i <- i + 1
      }
      
      hc
    })
    
    ###################################################

    #                         SANKEY section

    sec_act_filtered <- reactive({
      req(sec_act_df)
      sec_act_df %>%
        filter(year>=input$year_range[1],
               year<=input$year_range[2],
               status==input$status)
    })
    
    sankey_df <- reactive({
      df <- sec_act_filtered()
      metric_col <- input$metric
      
      top_sectors <- df %>%
        group_by(Sector) %>%
        summarise(total=sum(.data[[metric_col]],na.rm=TRUE),.groups="drop") %>%
        arrange(desc(total)) %>% slice_head(n=input$top_n) %>% pull(Sector)
      
      df <- df %>% filter(Sector %in% top_sectors)
      
      top_activities <- df %>%
        group_by(Activity) %>%
        summarise(total=sum(.data[[metric_col]],na.rm=TRUE),.groups="drop") %>%
        arrange(desc(total)) %>% slice_head(n=input$top_n) %>% pull(Activity)
      
      df <- df %>% filter(Activity %in% top_activities)
      
      df %>% group_by(Sector,Activity) %>%
        summarise(value=sum(.data[[metric_col]],na.rm=TRUE),.groups="drop")
      })
    
    output$sector_sankey <- renderHighchart({
      df <- sankey_df()
      req(nrow(df) > 0)
      
      theme <- theme_vals()
      
      # Explicit tooltip colors !!! 
      tooltip_bg   <- if (input$theme_mode %||% "dark" == "dark") "#000000" else "#ffffff"
      tooltip_text <- if (input$theme_mode %||% "dark" == "dark") "#ffffff" else "#1e1f21"
      
      bg_col <- if (input$theme_mode %||% "dark" == "dark") "#2a2b2d" else "#f2f4f7"
      
      
      links <- df %>% transmute(from = Sector, to = Activity, weight = value)
      nodes <- tibble(name = unique(c(links$from, links$to)))
      
      highchart() %>%
        hc_chart(type = "sankey", backgroundColor = bg_col) %>%
        hc_title(
          text = "Sankey Chart: Sector -> Activity Flows",
          style = list(color = theme$text)
        )%>%
        hc_add_series(
          type  = "sankey",
          data  = list_parse(links),
          nodes = list_parse(nodes),
          keys  = c("from", "to", "weight")
        ) %>%
        hc_plotOptions(
          sankey = list(
            nodePadding = 16,
            dataLabels = list(
              enabled = TRUE,
              color = theme$text,
              style = list(textOutline = "none")
            )
          )
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = tooltip_bg,
          borderColor     = tooltip_bg,
          style = list(
            color = tooltip_text,
            fontSize = "12px"
          ),
          pointFormatter = JS("
        function () {
          // LINKS
          if (this.from && this.to) {
            return '<b>' + this.from + ' → ' + this.to + '</b><br>' +
                   Highcharts.numberFormat(Math.round(this.weight), 0);
          }
          // NODES
          if (this.name && this.sum !== undefined) {
            return '<b>' + this.name + '</b><br>' +
                   Highcharts.numberFormat(Math.round(this.sum), 0);
          }
          return null;
        }
      ")
        ) %>%
        hc_exporting(enabled=TRUE)
    })
    
    
   
    #####################################################
    
    ###big donut section
    
    pie_filtered <- reactive({
      df <- sec_act_df %>%
        filter(
          year >= input$pie_years[1],
          year <= input$pie_years[2]
        )
      
      if (input$pie_status != "Total") {
        df <- df %>% filter(status == input$pie_status)
      } else {
        df <- df %>% filter(status %in% c("Announced","Opened"))
      }
      
      df %>%
        group_by(Sector) %>%
        summarise(value = sum(.data[[input$pie_metric]], na.rm = TRUE), .groups="drop") %>%
        arrange(desc(value))
    })
    
    # ---------- SELECTED SECTOR STATE ----------
    selected_sector <- reactiveVal(NULL)
    
    
    observeEvent(input$sector_clicked, {
      selected_sector(input$sector_clicked)
    })
    
    
    
    observeEvent(input$pie_back, {
      selected_sector(NULL)   
      session$sendCustomMessage(
        "resetSectorClickedGlobal",
        list(
          id = ns("sector_clicked"),
          chartId = ns("sector_big_pie")
        )
      )
    })
    
    
    

    pie_metric_label <- reactive({
      switch(input$pie_metric,
             "flow_value" = "Capital Investment (USD M)",
             "flow_job"   = "Jobs Created",
             "flow_n"     = "Projects")
    })
    
    
    hide_series_name <- list(
      showInLegend = FALSE,
      name = NULL
    )
    
    
    
    # HELPER: map pie_status -> fdi status_group values 
    drill_status_vals <- reactive({
     
      if (input$pie_status == "Total") return(c("Announced","Opened"))
      if (input$pie_status == "Opened") return("Opened")
      if (input$pie_status == "Announced") return("Announced")
      # fallback
      c("Announced","Opened")
    })
    
    # Pie donut with drill event that sets shiny input 
    
    output$sector_big_pie <- renderHighchart({
      df <- pie_filtered()
      if (nrow(df) == 0) return(highchart() %>% hc_title(text = "No Data"))
      
      theme <- theme_vals()
      pal   <- palette_vals()
      
      # blank drilldown placeholders (necessary)
      drill_series <- df %>%
        mutate(drilldown = Sector) %>%
        purrr::pmap(function(Sector, value, drilldown) {
          list(
            id = Sector,
            type = "column",
            name = paste("Details for", Sector),
            data = list(),
            colorByPoint = TRUE
          )
        })
      
      drill_js_down <- JS(sprintf(
        "function(e){ if(e.point && e.point.name){ Shiny.setInputValue('%s', e.point.name, {priority: 'event'}); }}",
        session$ns("sector_clicked")
      ))
      drill_js_up <- JS(sprintf(
        "function(e){ Shiny.setInputValue('%s', null, {priority: 'event'}); }",
        session$ns("sector_clicked")
      ))
      
      highchart() %>%
        hc_chart(type = "pie",
                 backgroundColor = theme$bg_col,
                 events = list(
                   drilldown = drill_js_down,
                   drillup = drill_js_up
                 )) %>%
        hc_colors(pal) %>%
        hc_title(text = "FDI Flows by Sector : in-Depth Analysis", style = list(color = theme$text)) %>%
        hc_subtitle(text = " Click on any Sector from the Donut Pie Chart to explore in-depth details", style = list(color = theme$text))%>%
        hc_plotOptions(
          pie = list(
            innerSize = "35%",
            allowPointSelect = TRUE,
            cursor = "pointer",
            borderColor = theme$grid,
            dataLabels = list(
              enabled = TRUE,
              distance = 10,
              color = theme$text,
              connectorColor = theme$text,
              style = list(color = theme$text, textOutline = "none"),
              formatter = JS("
            function() {
              return '<b>' + this.point.name + '</b>: ' +
                     Highcharts.numberFormat(this.percentage,1) + '%';
            }
          ")
            )
          ),
          series = list(
            dataLabels = list(
              style = list(color = theme$text, textOutline = "none")
            )
          )
        ) %>%
        hc_drilldown(
          series = drill_series,
          activeDataLabelStyle = list(color = theme$text, textDecoration = "none"),
          activeAxisLabelStyle = list(color = theme$text)
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = if (input$theme_mode %||% "dark" == "dark") "#000000" else "#ffffff",
          borderColor     = if (input$theme_mode %||% "dark" == "dark") "#000000" else "#d0d0d0",
          style = list(
            color = if (input$theme_mode %||% "dark" == "dark") "#ffffff" else "#1e1f21",
            fontSize = "12px"
          ),
          formatter = JS("
    function() {
      return '<b>' + this.point.name + '</b><br>' +
             Highcharts.numberFormat(this.point.y, 0) +
             ' (' + Highcharts.numberFormat(this.point.percentage, 1) + '%)';
    }
  ")
        )%>%
        hc_add_series(
          name = "FDI Value",
          data = purrr::map2(df$Sector, df$value, ~list(name = .x, y = .y, drilldown = .x)),
          colorByPoint = TRUE
        ) %>%
        hc_exporting(enabled=TRUE)
    })
    
    
    fdi_metric_col <- reactive({
      switch(input$pie_metric,
             "flow_value" = "value",
             "flow_job"   = "jobs",
             "flow_n"     = "n_projects")
    })
    
    #  REACTIVE: fdi rows filtered by selected sector + timeframe + status 
    fdi_sector_filtered <- reactive({
      req(selected_sector())
      
      sv <- selected_sector()
      yrs <- input$pie_years
      status_vals <- drill_status_vals()
      metric_col <- fdi_metric_col()
      
      f <- fdi %>%
        filter(
          Sector == sv,
          year >= yrs[1],
          year <= yrs[2],
          status_group %in% status_vals
        ) %>%
        select(all_of(c("src_country","dest_country","Sector","Subsector","Activity","year","status_group","n_projects","value","jobs")))
      
      f
    })
    
    
    # DRILL 1: activity distribution
    output$drill_activity <- renderHighchart({
      req(selected_sector())
      metric_col <- fdi_metric_col()
      
      df <- fdi_sector_filtered() %>%
        group_by(Activity) %>%
        summarise(value = sum(.data[[metric_col]], na.rm = TRUE), .groups = "drop") %>%
        arrange(value)
      
      theme <- theme_vals()
      pal   <- palette_vals()
      
      tooltip_bg   <- if (input$theme_mode %||% "dark" == "dark") "#000000" else "#ffffff"
      tooltip_text <- if (input$theme_mode %||% "dark" == "dark") "#ffffff" else "#1e1f21"
      
      highchart() %>%
        hc_chart(type = "bar", backgroundColor = theme$bg_col) %>%
        hc_colors(pal) %>%
        hc_title(
          text = paste0("Activity distribution — ", selected_sector()),
          style = list(color = theme$text)
        ) %>%
        hc_xAxis(
          type = "category",
          categories = df$Activity,
          labels = list(style = list(color = theme$text))
        ) %>%
        hc_yAxis(
          title = list(text = pie_metric_label(), style = list(color = theme$text)),
          labels = list(style = list(color = theme$text))
        ) %>%
        
        hc_plotOptions(
          series = list(
            dataLabels = list(enabled = FALSE)
          )
        ) %>%
        
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = tooltip_bg,
          borderColor     = tooltip_bg,
          style = list(color = tooltip_text),
          formatter = JS("
        function () {
          return '<b>' + this.point.category + '</b><br>' +
                 Highcharts.numberFormat(this.point.y, 2);
        }
      ")
        ) %>%
        
        hc_add_series(
          data = df$value,
          colorByPoint = TRUE,
          showInLegend = FALSE,
          name = NULL
        ) %>%
        hc_exporting(enabled=TRUE)
    })
    
    
    # DRILL 2: project size 
    
    output$drill_size <- renderHighchart({
      req(selected_sector())
      
      df <- fdi_sector_filtered() %>%
        mutate(size_bucket = case_when(
          value < 20 ~ "Small (<20M)",
          value >= 20 & value < 100 ~ "Medium (20–100M)",
          value >= 100 & value < 500 ~ "Large (100–500M)",
          value >= 500 ~ "Mega (>500M)",
          TRUE ~ "Unknown"
        )) %>%
        group_by(year, size_bucket) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
      
      yrs <- sort(unique(df$year))
      buckets <- unique(df$size_bucket)
      
      theme <- theme_vals()
      pal   <- palette_vals()
      
      tooltip_bg   <- if (input$theme_mode %||% "dark" == "dark") "#000000" else "#ffffff"
      tooltip_text <- if (input$theme_mode %||% "dark" == "dark") "#ffffff" else "#1e1f21"
      
      hc <- highchart() %>%
        hc_chart(type = "area", backgroundColor = theme$bg_col) %>%
        hc_colors(pal) %>%
        hc_title(
          text = paste0("Project size distribution — ", selected_sector()),
          style = list(color = theme$text)
        ) %>%
        hc_xAxis(categories = yrs, labels = list(style = list(color = theme$text))) %>%
        hc_yAxis(
          title = list(text = "Total Capital (USD M)", style = list(color = theme$text)),
          labels = list(style = list(color = theme$text))
        ) %>%
        hc_plotOptions(area = list(stacking = "normal")) %>%
        hc_legend(
          itemStyle = list(color = theme$text),
          itemHoverStyle = list(color = theme$text),
          itemHiddenStyle = list(color = theme$text)
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = tooltip_bg,
          borderColor     = tooltip_bg,
          style = list(color = tooltip_text),
          formatter = JS("
        function () {
          return '<b>' + this.series.name + '</b><br>' +
                 'Year: <b>' + this.x + '</b><br>' +
                 'Flow: <b>' + Highcharts.numberFormat(this.y, 2) + '</b>';
        }
      ")
        ) %>%
        hc_exporting(enabled=TRUE)
      
      for (bucket in buckets) {
        vals <- df %>%
          filter(size_bucket == bucket) %>%
          right_join(tibble(year = yrs), by = "year") %>%
          arrange(year) %>%
          mutate(value = replace_na(value, 0)) %>%
          pull(value)
        
        hc <- hc %>% hc_add_series(
          name = bucket,
          data = vals
        )
      }
      
      hc
    })
    
    
    # DRILL 3: conversion ratio
    
    output$drill_conversion <- renderHighchart({
      req(selected_sector())
      
      yrs <- input$pie_years
      
      df_conv <- fdi %>%
        filter(
          Sector == selected_sector(),
          year >= yrs[1],
          year <= yrs[2]
        ) %>%
        group_by(year, status_group) %>%
        summarise(nproj = sum(n_projects, na.rm = TRUE), .groups="drop") %>%
        tidyr::pivot_wider(
          names_from = status_group,
          values_from = nproj,
          values_fill = 0
        ) %>%
        mutate(ratio = ifelse(Announced == 0, NA_real_, Opened / Announced)) %>%
        arrange(year)
      
      theme <- theme_vals()
      pal   <- palette_vals()
      
      tooltip_bg   <- if (input$theme_mode %||% "dark" == "dark") "#000000" else "#ffffff"
      tooltip_text <- if (input$theme_mode %||% "dark" == "dark") "#ffffff" else "#1e1f21"
      
      highchart() %>%
        hc_chart(type = "line", backgroundColor =theme$bg_col) %>%
        hc_colors(pal) %>%
        hc_title(
          text = paste0("Conversion (Opened / Announced) — ", selected_sector()),
          style = list(color = theme$text)
        ) %>%
        hc_xAxis(
          categories = df_conv$year,
          labels = list(style = list(color = theme$text))
        ) %>%
        hc_yAxis(
          title = list(text = "Opened / Announced", style = list(color = theme$text)),
          labels = list(style = list(color = theme$text))
        ) %>%
        hc_add_series(
          name = "Conversion ratio",
          data = df_conv$ratio
        ) %>%
        hc_legend(
          itemStyle = list(color = theme$text),
          itemHoverStyle = list(color = theme$text),
          itemHiddenStyle = list(color = theme$text)
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = tooltip_bg,
          borderColor     = tooltip_bg,
          style = list(color = tooltip_text),
          formatter = JS("
        function () {
          return '<b>Year:</b> ' + this.x + '<br>' +
                 '<b>Conversion ratio:</b> ' +
                 Highcharts.numberFormat(this.y, 2);
        }
      ")
    ) %>%
        hc_exporting(enabled=TRUE)
    })
    
    # DRILL 4: top countries based on flow type
    
    output$drill_countries <- renderHighchart({
      req(selected_sector())
      
      yrs <- input$pie_years
      metric_col <- fdi_metric_col()
      status_vals <- drill_status_vals()
      flowtype <- input$pie_flowtype %||% "inflow"
      
      f <- fdi %>%
        filter(
          Sector == selected_sector(),
          year >= yrs[1],
          year <= yrs[2],
          status_group %in% status_vals
        )
      
      dfc <- switch(
        flowtype,
        "inflow"  = f %>%
          group_by(src_country) %>%
          summarise(value = sum(.data[[metric_col]]), .groups="drop") %>%
          rename(country = src_country),
        
        "outflow" = f %>%
          group_by(dest_country) %>%
          summarise(value = sum(.data[[metric_col]]), .groups="drop") %>%
          rename(country = dest_country),
        
        "total"   = bind_rows(
          f %>% transmute(country = src_country, value = .data[[metric_col]]),
          f %>% transmute(country = dest_country, value = .data[[metric_col]])
        ) %>%
          group_by(country) %>%
          summarise(value = sum(value), .groups="drop")
      )
      
      dfc <- dfc %>%
        arrange(desc(value)) %>%
        slice_head(n = 10) %>%
        arrange(value)
      
      theme <- theme_vals()
      pal   <- palette_vals()
      
      tooltip_bg   <- if (input$theme_mode %||% "dark" == "dark") "#000000" else "#ffffff"
      tooltip_text <- if (input$theme_mode %||% "dark" == "dark") "#ffffff" else "#1e1f21"
      
      highchart() %>%
        hc_chart(type = "bar", backgroundColor = theme$bg_col) %>%
        hc_colors(pal) %>%
        hc_title(
          text = paste0(
            "Top countries — ",
            selected_sector(),
            " (", tools::toTitleCase(flowtype), ")"
          ),
          style = list(color = theme$text)
        ) %>%
        hc_xAxis(
          type = "category",
          categories = dfc$country,
          labels = list(style = list(color = theme$text))
        ) %>%
        hc_yAxis(
          title = list(text = pie_metric_label(), style = list(color = theme$text)),
          labels = list(style = list(color = theme$text))
        ) %>%
        
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = tooltip_bg,
          borderColor     = tooltip_bg,
          style = list(color = tooltip_text),
          formatter = JS("
        function () {
          return '<b>' + this.point.category + '</b><br>' +
                 'Flow: <b>' +
                 Highcharts.numberFormat(this.point.y, 2) +
                 '</b>';
        }
      ")
        ) %>%
        
        hc_add_series(
          data = dfc$value,
          colorByPoint = TRUE,
          name = NULL,
          showInLegend = FALSE
        ) %>%
        hc_exporting(enabled=TRUE)
    })
    
    
  })
}



















#######################################################################################################################################################
########################################################################################################################################################



########################################################################################################################################################
######################### UNILATERAL ANALYSIS ##########################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################


### UI Module 1: country selection


unilateral_selection_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "info-banner",
      style = "margin-top:25px; margin-bottom:15px;",
      HTML("
        Select a country to explore unilateral Greenfield FDI inflows, outflows,
        net flows and structural indicators.<br>
        The selected country will automatically update all unilateral charts.
      ")
    ),
    
    # ### selection menu
    div(
      style = "
        display:flex;
        justify-content:center;
        margin-top:40px;
        margin-bottom:70px;
      ",
      
      div(
        style = "
          width:450px;
          padding:25px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
        ",
        
        div(
          class = 'left-menu-title',
          style = 'font-size:20px; text-align:center; margin-bottom:20px;',
          'Select Country for Unilateral Analysis'
        ),
        
        selectizeInput(
          inputId = ns("country"),
          label = NULL,
          choices = NULL,
          width = "100%",
          options = list(
            placeholder = "Choose a country",
            maxOptions  = 200
          )
        )
      )
    ),
    
    
    ## print selected country 
    
    div(
      style = "
        display:flex;
        justify-content:center;
        margin-top:40px;
        margin-bottom:580px;
      ",
      
      div(
        class = "kpi-card kpi-blue",
        style = "width:360px; text-align:center;",
        h3("Selected Country"),
        div(class = "kpi-number", textOutput(ns("country_name"))),
        div(class = "kpi-number-static", textOutput(ns("country_iso")))
      )
    )
  )
}

######################### server of module 1

country_selection_server <- function(id, unilateral_list, selected_country) {
  
  moduleServer(id, function(input, output, session) {
    

    ### choices with US and CHN on top
    
    country_list <- unilateral_list %>%
      mutate(
        label = paste0(country, " - ", iso3),
        order = case_when(
          iso3 == "USA" ~ 1,
          iso3 == "CHN" ~ 2,
          TRUE ~ 3
        )
      ) %>%
      arrange(order, country)
    
    choices <- setNames(country_list$iso3, country_list$label)
    
    # populating selectize of choices
    
    session$onFlushed(function() {
      updateSelectizeInput(
        session,
        "country",
        choices  = choices,
        selected = isolate(selected_country())
      )
    }, once = TRUE)
    
    observeEvent(input$country, {
      req(input$country)
      selected_country(input$country)
    })
    
    country_name <- function(iso) {
      nm <- unilateral_list %>%
        filter(iso3 == iso) %>%
        pull(country)
      ifelse(length(nm) == 0, iso, nm)
    }
    
    
    delayed_country <- reactive({
      req(selected_country())
      
      Sys.sleep(2)   # busy spinner: 2 seconds after selection to prevent any crashing from quick tab switching
      
      list(
        name = country_name(selected_country()),
        iso  = selected_country()
      )
    })
    
    # output of selected country in KPI format
    
    output$country_name <- renderText({
      delayed_country()$name
    })
    
    output$country_iso <- renderText({
      delayed_country()$iso
    })
    
    return(list(
      selected_country = reactive(selected_country())
    ))
  })
}








##############################################################################################
##############################################################################################


# UI MODULE 2: inflow analysis 

unilateral_inflow_UI <- function(id) {
  ns <- NS(id)
  
  ## group choices when available 
  
  group_choices <- c(
    "EU-group"     = "EU",
    "NAFTA-group"  = "NAFTA",
    "ASEAN-group"  = "ASEAN",
    "BRICS-group"  = "BRICS"
    )
  
  
  tagList(

    
    div(
      class = "info-banner",
      style = "margin-bottom: 25px;",
      HTML("
        Unilateral Analysis of FDI Inflows to the selected country.<br>
        Explore raw numbers, trends, partner-country contributions,
        and sector–country dynamics shaping investment inflows.
      ")
    ),
    
    

    # card to show which country has been selected

    div(
      class = "kpi-container",
      div(
        class = "kpi-card kpi-blue",
        h3("Selected Country"),
        div(class = "kpi-number", textOutput(ns("kpi_country_name")))
      )
    ),
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin-top:35px;
    margin-bottom:35px;
    margin-left:20px;
 
     "
    ),
    
    
    

    # SECTION 1 — country inflows

    
    div(
      id = ns("country_inflow_section"),
      style = "
        display:flex;
        flex-direction:row;
        gap:20px;
        margin-top:70px;
        margin-bottom:100px;
        
      ",
      
     # menu
      div(
        style = "
          width:260px;
          padding:18px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
          display:flex;
          flex-direction:column;
          font-size:12px;
          margin-left:50px;
        ",
        
        div(class="left-menu-title", "Country Inflows — Options",
            style="font-size:14px; margin-bottom:10px;"),
        
        div("Top N Countries:", style="margin-top:10px; margin-bottom:5px;"),
        sliderInput(ns("top_n_country"), NULL,
                    min = 3, max = 40, value = 10, step = 1,
                    width="100%"),
        
        div("Metric:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("metric_country"), NULL,
          choices = c("Capital Investment"="flow_value",
                      "Jobs Created"="flow_job",
                      "Projects"="flow_n"),
          inline = FALSE,
          selected = "flow_value",
          width="100%"
        ),
        
        div("Status:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("status_country"), NULL,
          choices = c("Announced","Opened","Total"),
          inline = FALSE,
          selected = "Opened"
        ),
        
        div("Country Groups:", style="margin-top:15px; margin-bottom:5px;"),
        checkboxGroupInput(
          ns("country_groups"),
          label = NULL,
          choices  = group_choices,
          selected = "EU",
          inline   = FALSE
        ),
        
        
        div("Timeframe:", style="margin-top:15px; margin-bottom:5px;"),
        sliderInput(ns("year_range_country"), NULL,
                    min = 2003, max = MAX_YEAR,
                    value = c(2003, MAX_YEAR),
                    width="100%"),
      ),
      
      
      ### dropdown choice of chart type
      
     
      div(
        style = "
    flex-grow:1;
    padding:10px;
    background:transparent;
    position:relative;        
  ",
        
        div(
          style="
      position:absolute;
      top:-20px;
      right:10px;
      z-index:10;
      width:130px;
    ",
          selectInput(
            ns("chart_view_country"),
            label = NULL,
            choices = c(
              "Bar"        = "bar",
              "Pie"        = "pie",
              "Trend"      = "trend",
              "Cumulative" = "cumulative"
            ),
            width = "130px"
          )
        ),
        
        
        ### chart
        
        withSpinner(
          highchartOutput(ns("country_inflow_chart"), height="460px"),
          type = 6, color = "#b984db"
        )
      )
      
    ),
    
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin-top:35px;
    margin-bottom:35px;
    margin-left:20px;
  "
    ),
    
    

    # SECTION 2 — inflows by sector

    
    div(
      id = ns("sector_inflow_section"),
      style = "
        display:flex;
        flex-direction:row;
        gap:20px;
        margin-top:60px;
        margin-bottom:60px;
      ",
      
      
      # menu selection
    
      div(
        style = "
          width:260px;
          padding:18px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
          display:flex;
          flex-direction:column;
          font-size:12px;
          margin-left:50px;

        ",
        
        div(class="left-menu-title", "Sectoral Inflows — Options",
            style="font-size:14px; margin-bottom:10px;"),
        
        div("Level:", style="margin-top:10px; margin-bottom:5px;"),
        selectInput(ns("sector_level"), NULL,
                    choices = c("Sector","Subsector","Activity"),
                    width="100%"),
        
        div("Top N Items:", style="margin-top:15px; margin-bottom:5px;"),
        sliderInput(ns("top_n_sector"), NULL,
                    min = 3, max = 40, value = 10, step = 1,
                    width="100%"),
        
        div("Metric:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("metric_sector"), NULL,
          choices = c("Capital Investment"="flow_value",
                      "Jobs Created"="flow_job",
                      "Projects"="flow_n"),
          inline = FALSE,
          selected = "flow_value"
        ),
        
        div("Status:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("status_sector"), NULL,
          choices = c("Announced","Opened","Total"),
          inline = FALSE,
          selected = "Opened"
        ),
        
        div("Timeframe:", style="margin-top:15px; margin-bottom:5px;"),
        sliderInput(ns("year_range_sector"), NULL,
                    min = 2003, max = MAX_YEAR,
                    value = c(2003, MAX_YEAR),
                    width="100%")
      ),
      
      
    
      div(
        style = "
    flex-grow:1;
    padding:10px;
    background:transparent;
    position:relative;
  ",
        ### dropdown choice of chart type
        div(
          style="
      position:absolute;
      top:-20px;
      right:10px;
      z-index:10;
      width:130px;
    ",
          selectInput(
            ns("chart_view_sector"),
            label = NULL,
            choices = c(
              "Bar"        = "bar",
              "Pie"        = "pie",
              "Trend"      = "trend",
              "Cumulative" = "cumulative"
            ),
            width = "130px"
          )
        ),
        
        # chart output
        withSpinner(
          highchartOutput(ns("sector_inflow_chart"), height="460px"),
          type = 6, color = "#b984db"
        )
      )
      
    ),
    
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin-top:35px;
    margin-bottom:35px;
    margin-left:20px;
  "
    ),
    

    # 3rd section: sankey flows

    div(
      id = ns("sankey_inflow_section"),
      style = "
    margin-top:60px;
    margin-bottom:60px;
    padding-left:50px;
    padding-right:50px;
  ",
      
      h3(
        "Investment Pathway — Countries → Sectors → Activities ", inline = TRUE),
      

      #   control bar (horizontal)

      div(
        style = "
      width:100%;
      display:flex;
      flex-direction:row;
      align-items:flex-end;
      gap:25px;
      padding:15px;
      margin-top:25px;
      margin-bottom:30px;
      border:2px solid var(--text-main);
      border-radius:12px;
      background:rgba(255,255,255,0.03);
    ",
        
        div(
          style="flex:1;",
          div("Metric:", style="margin-bottom:5px; font-weight:600;"),
          radioButtons(
            ns("sankey_metric"), NULL,
            choices = c(
              "Capital Investment" = "flow_value",
              "Jobs Created"       = "flow_job",
              "Projects"           = "flow_n"
            ),
            inline = TRUE,
            selected = "flow_value"
          )
        ),
        
        div(
          style="flex:1;",
          div("Status:", style="margin-bottom:5px; font-weight:600;"),
          radioButtons(
            ns("sankey_status"), NULL,
            choices = c("Announced", "Opened", "Total"),
            inline = TRUE,
            selected = "Opened"
          )
        ),
        
        div(
          style="flex:1.3;",
          div("Country Groups:", style="margin-bottom:5px; font-weight:600;"),
          tags$div(
            style="
      display:grid;
      grid-template-columns: repeat(2, max-content);
      column-gap: 20px;
      row-gap: 6px;
    ",
            checkboxGroupInput(
              ns("sankey_country_groups"),
              label = NULL,
              choices  = group_choices,
              selected = "EU"
            )
          )
        ),
        
        
        
        div(
          style="flex:1;",
          div("Timeframe:", style="margin-bottom:5px; font-weight:600;"),
          sliderInput(
            ns("sankey_year_range"), NULL,
            min = 2003, max = MAX_YEAR,
            value = c(2003, MAX_YEAR),
            width = "100%"
          )
        )
      ),
      
      # Sankey Chart
      withSpinner(
        highchartOutput(ns("sankey_inflow_chart"), height="600px"),
        type = 6, color = "#b984db"
      )
    )
    
  ) 
}


#########################################################################################

#SERVER MODULE 2: inflows of selected country

unilateral_inflow_server <- function(id, selected_country, fdi, unilateral_list) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_country_name <- reactive({
      req(selected_country())
      iso <- selected_country()
      
      nm <- unilateral_list %>%
        filter(iso3 == iso) %>%
        pull(country)
      
      if (length(nm) == 0) return(iso) 
      nm
      
      
    })
    
    output$kpi_country_name <- renderText(selected_country_name())
    
    

    theme <- reactive(input$theme_mode %||% "dark")
    
    text_col <- reactive(if (theme() == "light") "#1e1f21" else "#ffffff")
    grid_col <- reactive(if (theme() == "light") "#c8c8c8" else "#2a2b2d")
    bg_col   <-  reactive(if (theme() == "light") "#f2f4f7" else "#232427")
    
    
    map_country_group <- function(iso3_ref) {
      dplyr::case_when(
        iso3_ref %in% c(
          "FRA","DEU","ITA","ESP","POL","NLD","BEL","SWE","AUT","DNK","FIN",
          "PRT","CZE","GRC","HUN","IRL","ROU","SVK","SVN","HRV","BGR",
          "EST","LVA","LTU","CYP","LUX","MLT"
        ) ~ "European Union",
        
        iso3_ref %in% c("CAN","MEX","USA") ~ "NAFTA",
        
        iso3_ref %in% c("BRA","RUS","IND","CHN","ZAF") ~ "BRICS",
        
        iso3_ref %in% c(
          "IDN","MYS","PHL","SGP","THA","VNM",
          "BRN","KHM","LAO","MMR"
        ) ~ "ASEAN",
        
        TRUE ~ NA_character_
      )
    }
    
    
    
    apply_country_groups <- function(df, selected_groups) {
      
      if (is.null(selected_groups) || length(selected_groups) == 0) {
        return(df)
      }
      
      group_map <- c(
        EU     = "European Union",
        NAFTA = "NAFTA",
        ASEAN = "ASEAN",
        BRICS = "BRICS"
      )
      
      active_groups <- unname(group_map[selected_groups])
      
      df %>%
        mutate(
          country_group = map_country_group(iso3_ref),
          src_country = ifelse(
            !is.na(country_group) & country_group %in% active_groups,
            country_group,
            src_country
          )
        ) %>%
        select(-country_group)
    }
    
    
    
    # BASE INFLOW DATA
    
    inflow_df <- reactive({
      req(selected_country())
      fdi %>% filter(iso3_cp == selected_country())
    })
    
    # utilities
    
    filter_years <- function(df, yr) df %>% filter(year >= yr[1], year <= yr[2])
    
    filter_status <- function(df, status_choice) {
      
      grouping_vars <- c(
        setdiff(names(df), c("value", "jobs", "n_projects", "status"))
      )
      
      if (status_choice == "Total") {
        
        df %>%
          filter(status %in% c("Announced", "Opened")) %>%
          group_by(across(all_of(grouping_vars))) %>%
          summarise(
            value = sum(value, na.rm=TRUE),
            jobs  = sum(jobs, na.rm=TRUE),
            n_projects = sum(n_projects, na.rm=TRUE),
            .groups = "drop"
          )
        
      } else {
        
        df %>% filter(status == status_choice)
        
      }
    }
    
    
    metric_var <- reactive({
      switch(input$metric_country,
             flow_value="value", flow_job="jobs", flow_n="n_projects")
    })
    
    metric_label <- reactive({
      switch(input$metric_country,
             flow_value = "Volume (USD Million)",
             flow_job   = "Jobs Created",
             flow_n     = "Number of Projects")
    })
    
    
    metric_name <- reactive({
      switch(input$metric_country,
             flow_value = "Volume",
             flow_job   = "Jobs Created",
             flow_n     = "Number of Projects")
    })
    
    

    # COUNTRY INFLOW DATA

    country_inflow_data <- reactive({
      inflow_df() %>%
        filter_years(input$year_range_country) %>%
        filter_status(input$status_country) %>%
        apply_country_groups(input$country_groups) %>%
        group_by(src_country) %>%
        summarise(value = sum(.data[[metric_var()]]), .groups="drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = input$top_n_country)
    })
    
    
    country_inflow_trend <- reactive({
      inflow_df() %>%
        filter_years(input$year_range_country) %>%
        filter_status(input$status_country) %>%
        apply_country_groups(input$country_groups) %>%
        group_by(year, src_country) %>%
        summarise(value = sum(.data[[metric_var()]]), .groups="drop")
    })
    
    
    country_inflow_cumulative <- reactive({
      country_inflow_trend() %>%
        group_by(src_country) %>%
        arrange(year) %>%
        mutate(cum_value = cumsum(value))
    })
    
    
    
    expand_country_years <- function(trend_df, top_ct, years) {
      
      full_grid <- expand.grid(
        year = years,
        src_country = top_ct,
        stringsAsFactors = FALSE
      )
      
      merged <- full_grid %>%
        left_join(trend_df, by = c("year","src_country"))
      
      merged$value[is.na(merged$value)] <- 0
      
      merged
    }
    
    expand_country_years_cumulative <- function(trend_df, top_ct, years) {
      
      expanded <- expand_country_years(trend_df, top_ct, years)
      
      expanded %>%
        arrange(src_country, year) %>%
        group_by(src_country) %>%
        mutate(cum_value = cumsum(value)) %>%
        ungroup()
    }
    
    

    # HIGHCHART OUTPUT

    output$country_inflow_chart <- renderHighchart({
      
      df <- country_inflow_data()
      req(nrow(df) > 0)
      
      view <- input$chart_view_country
      txt <- text_col(); grd <- grid_col()
      
      country_for_title <- {
        nm <- selected_country_name()
        
        prefix_countries <- c(
          "United States",
          "United Kingdom",
          "Netherlands",
          "Philippines",
          "United Arab Emirates"
        )
        
        if (nm %in% prefix_countries) {
          paste("the", nm)
        } else {
          nm
        }
      }
      
      
     ## bar chart
      if (view == "bar") {
        
        df$color <- bar_palette[seq_len(nrow(df))]
        
        hc <- highchart() %>%
          hc_chart(type="column", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0(
              "Top ", input$top_n_country,
              " Sources by Greenfield FDI ", metric_name(),
              " into ", country_for_title,
              " (", input$status_country, " Projects)"
            ),
            style=list(color=txt)
          ) %>%
          hc_xAxis(
            categories = df$src_country,
            labels=list(style=list(color=txt))
          ) %>%
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=txt)),
            labels=list(style=list(color=txt)),
            gridLineColor=grd
          ) %>%
          hc_add_series(
            type = "column",
            name = NULL,
            showInLegend = FALSE,
            data = purrr::map2(df$value, df$color, ~ list(y = .x, color = .y))
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.category + '</b><br>' +
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
      }
      
      # PIE 
      
      
      else if (view == "pie") {
        
        hc <- highchart() %>%
          hc_chart(type = "pie", backgroundColor = bg_col()) %>%
          hc_title(
            text = paste0(
              "Share of Top ", input$top_n_country,
              " Sources by Greenfield FDI ", metric_name(),
              " into ", country_for_title,
              " (", input$status_country, " Projects)"
            ),
            style = list(color = txt)
          ) %>%
          hc_plotOptions(
            pie = list(
              allowPointSelect = TRUE,
              cursor = "pointer",
              dataLabels = list(
                enabled = TRUE,
                format = "<b>{point.name}</b>: {point.percentage:.1f}%",
                style = list(
                  color = txt,
                  fontSize = "12px",
                  textOutline = "none"
                )
              )
            )
          ) %>%
          hc_add_series(
            type = "pie",
            data = purrr::map2(
              df$src_country, df$value,
              ~ list(name = .x, y = .y)
            )
          )%>%
          hc_tooltip(
            pointFormatter = JS("
        function () {
          return 'Flow: <b>' +
                 Highcharts.numberFormat(this.y, 2) +
                 '</b>';
        }
      ")
          ) %>%
          hc_exporting(enabled=TRUE)
      }
      
      
     # trend chart
      
      
      else if (view == "trend") {
        
        raw_tr <- country_inflow_trend()
        
        years <- seq(min(raw_tr$year), max(raw_tr$year))
        top_ct <- df$src_country
        
        tr <- expand_country_years(raw_tr, top_ct, years)
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0(
              "Top ", input$top_n_country,
              " Sources by Greenfield FDI ", metric_name(),
              " into ", country_for_title,
              " (", input$status_country, " Projects)"
            ),
            style=list(color=txt)) %>%
          hc_xAxis(categories=years, labels=list(style=list(color=txt))) %>%
          hc_yAxis(title=list(text=metric_label(), style=list(color=txt)),
                   labels=list(style=list(color=txt)),
                   gridLineColor=grd) %>%
          hc_legend(
            itemStyle=list(color=txt),
            itemHoverStyle=list(color=txt),
            itemHiddenStyle=list(color=grd)
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.series.name + '</b><br>' +
             'Year: ' + this.category + '<br>' +
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
        
        for (ct in top_ct) {
          series_df <- tr %>% filter(src_country == ct) %>% arrange(year)
          hc <- hc %>% hc_add_series(name=ct, data=series_df$value)
        }
      }
      
     # cumulative chart
      
      else if (view == "cumulative") {
        
        raw_tr <- country_inflow_trend()
        
        years <- seq(min(raw_tr$year), max(raw_tr$year))
        top_ct <- df$src_country
        
        tr <- expand_country_years_cumulative(raw_tr, top_ct, years)
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor=bg_col()) %>%
          hc_title(text = paste0(
            "Top ", input$top_n_country,
            " Sources by Greenfield FDI ", metric_name(),
            " into ", country_for_title,
            " (", input$status_country, " Projects)"
          ),
          style=list(color=txt)) %>%
          hc_xAxis(categories=years, labels=list(style=list(color=txt))) %>%
          hc_yAxis(title=list(text=metric_label(), style=list(color=txt)),
                   labels=list(style=list(color=txt)),
                   gridLineColor=grd) %>%
          hc_legend(
            itemStyle=list(color=txt),
            itemHoverStyle=list(color=txt),
            itemHiddenStyle=list(color=grd)
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.series.name + '</b><br>' +
             'Year: ' + this.category + '<br>' +
             'Cumulative flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
        
        for (ct in top_ct) {
          series_df <- tr %>% filter(src_country == ct) %>% arrange(year)
          hc <- hc %>% hc_add_series(name=ct, data=series_df$cum_value)
        }
      }
      
      
      hc
    })
    
    
    
    
    
    
    #### sector section (more or less same as country)
    
    
    sector_var <- reactive({
      switch(input$sector_level,
             "Sector"    = "Sector",
             "Subsector" = "Subsector",
             "Activity"  = "Activity")
    })
    
    sector_label <- reactive({
      switch(input$sector_level,
             "Sector"    = "Sectors",
             "Subsector" = "Subsectors",
             "Activity"  = "Activities")
    })
    
    metric_var_sector <- reactive({
      switch(input$metric_sector,
             flow_value = "value",
             flow_job   = "jobs",
             flow_n     = "n_projects")
    })
    
    metric_label_sector <- reactive({
      switch(input$metric_sector,
             flow_value = "Volume (USD Million)",
             flow_job   = "Jobs Created",
             flow_n     = "Number of Projects")
    })
    
    metric_name_sector <- reactive({
      switch(input$metric_sector,
             flow_value = "Volume",
             flow_job   = "Jobs Created",
             flow_n     = "Number of Projects")
    })
    
    
    sector_inflow_data <- reactive({
      inflow_df() %>%
        filter_years(input$year_range_sector) %>%
        filter_status(input$status_sector) %>%
        group_by(.data[[sector_var()]]) %>%
        summarise(value = sum(.data[[metric_var_sector()]], na.rm = TRUE), .groups="drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = input$top_n_sector)
    })
    
    sector_inflow_trend <- reactive({
      inflow_df() %>%
        filter_years(input$year_range_sector) %>%
        filter_status(input$status_sector) %>%
        group_by(year, !!sym(sector_var())) %>%
        summarise(value = sum(.data[[metric_var_sector()]], na.rm = TRUE), .groups="drop")
    })
    
    sector_inflow_cumulative <- reactive({
      sector_inflow_trend() %>%
        group_by(!!sym(sector_var())) %>%
        arrange(year) %>%
        mutate(cum_value = cumsum(value))
    })
    
    
    
    expand_sector_years <- function(trend_df, items, years, col_name) {
      
      full_grid <- expand.grid(
        year = years,
        item = items,
        stringsAsFactors = FALSE
      )
      
      merged <- full_grid %>%
        left_join(
          trend_df %>% rename(item = all_of(col_name)),
          by = c("year", "item")
        )
      
      merged$value[is.na(merged$value)] <- 0
      merged
    }
    
    expand_sector_years_cumulative <- function(trend_df, items, years, col_name) {
      expand_sector_years(trend_df, items, years, col_name) %>%
        arrange(item, year) %>%
        group_by(item) %>%
        mutate(cum_value = cumsum(value)) %>%
        ungroup()
    }
    
    
    
    output$sector_inflow_chart <- renderHighchart({
      
      df <- sector_inflow_data()
      req(nrow(df) > 0)
      
      view <- input$chart_view_sector
      txt <- text_col(); grd <- grid_col()
      var <- sector_var()
      
      country_for_title <- {
        nm <- selected_country_name()
        
        prefix_countries <- c(
          "United States",
          "United Kingdom",
          "Netherlands",
          "Philippines",
          "United Arab Emirates"
        )
        
        if (nm %in% prefix_countries) {
          paste("the", nm)
        } else {
          nm
        }
      }
      
      
      ### bar chart
      
      if (view == "bar") {
        
        df$color <- bar_palette[seq_len(nrow(df))]
        
        hc <- highchart() %>%
          hc_chart(type = "column", backgroundColor = bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_sector, " ",  sector_label(), " in " , country_for_title, " by Greenfield FDI Inflow ", metric_name_sector(), " (", input$status_sector, " Projects)"),
            style = list(color = txt)
          ) %>%
          hc_xAxis(
            categories = df[[var]],
            labels = list(style = list(color = txt))
          ) %>%
          hc_yAxis(
            title = list(text = metric_label_sector(), style = list(color = txt)),
            labels = list(style = list(color = txt)),
            gridLineColor = grd
          ) %>%
          hc_add_series(
            type = "column",
            name = NULL,
            showInLegend = FALSE,
            data = purrr::map2(
              df$value,
              df$color,
              ~ list(y = .x, color = .y)
            )
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.category + '</b><br>' +
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
      }
      
      
      ## pie chart
      
      
      else if (view == "pie") {
        
        hc <- highchart() %>%
          hc_chart(type = "pie", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Share of Top ", input$top_n_sector, " ",  sector_label(), " in " , country_for_title, " by Greenfield FDI Inflow ", metric_name_sector(), " (", input$status_sector, " Projects)"),
            style=list(color=txt)
          ) %>%
          hc_plotOptions(
            pie = list(
              allowPointSelect = TRUE,
              cursor = "pointer",
              dataLabels = list(
                enabled = TRUE,
                format = "<b>{point.name}</b>: {point.percentage:.1f}%",
                style = list(color = txt, textOutline="none")
              )
            )
          ) %>%
          hc_add_series(
            data = purrr::map2(df[[var]], df$value, ~list(name=.x, y=.y))
          )%>%
          hc_tooltip(
            pointFormatter = JS("
        function () {
          return 'Flow: <b>' +
                 Highcharts.numberFormat(this.y, 2) +
                 '</b>';
        }
      ")
          ) %>%
          hc_exporting(enabled=TRUE)
      }
      
      
      ### trend chart
      
      else if (view == "trend") {
        
        raw_tr <- sector_inflow_trend()
        
        years <- seq(min(raw_tr$year), max(raw_tr$year))
        items <- df[[var]]
        
        tr <- expand_sector_years(raw_tr, items, years, var)
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_sector, " ",  sector_label(), " in " , country_for_title, " by Greenfield FDI Inflow ", metric_name_sector(), " (", input$status_sector, " Projects)"),
            style=list(color=txt)
          ) %>%
          hc_xAxis(categories=years, labels=list(style=list(color=txt))) %>%
          hc_yAxis(
            title=list(text=metric_label_sector(), style=list(color=txt)),
            labels=list(style=list(color=txt)),
            gridLineColor=grd
          ) %>%
          hc_legend(
            itemStyle=list(color=txt),
            itemHoverStyle=list(color=txt),
            itemHiddenStyle=list(color=grd)
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.series.name + '</b><br>' +
             'Year: ' + this.category + '<br>' +
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
        
        for (it in items) {
          series_df <- tr %>% filter(item == it) %>% arrange(year)
          hc <- hc %>% hc_add_series(name=it, data=series_df$value)
        }
      }
      

      # cumulative chart
      
      else if (view == "cumulative") {
        
        raw_tr <- sector_inflow_trend()
        
        years <- seq(min(raw_tr$year), max(raw_tr$year))
        items <- df[[var]]
        
        tr <- expand_sector_years_cumulative(raw_tr, items, years, var)
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_sector, " ",  sector_label(), " in " , country_for_title, " by Greenfield FDI Inflow ", metric_name_sector(), " (", input$status_sector, " Projects)"),
            style=list(color=txt)
          ) %>%
          hc_xAxis(categories=years, labels=list(style=list(color=txt))) %>%
          hc_yAxis(
            title=list(text=metric_label_sector(), style=list(color=txt)),
            labels=list(style=list(color=txt)),
            gridLineColor=grd
          ) %>%
          hc_legend(
            itemStyle=list(color=txt),
            itemHoverStyle=list(color=txt),
            itemHiddenStyle=list(color=grd)
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.series.name + '</b><br>' +
             'Year: ' + this.category + '<br>' +
             'Cumulative flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
        
        for (it in items) {
          series_df <- tr %>% filter(item == it) %>% arrange(year)
          hc <- hc %>% hc_add_series(name=it, data=series_df$cum_value)
        }
      }
      
      hc
    })
    
    
    
    
    #### sankey (commented)
    
    
    
    sankey_df <- reactive({
      req(selected_country())
      
      df <- inflow_df() %>%
        filter_years(input$sankey_year_range) %>%
        filter_status(input$sankey_status) %>%
        apply_country_groups(input$sankey_country_groups) %>%
        mutate(
          metric = .data[[ switch(
            input$sankey_metric,
            flow_value="value",
            flow_job="jobs",
            flow_n="n_projects"
          ) ]]
        ) %>%
        filter(metric > 0)
      
      
      # TOP 10 source countries 
      top_ct <- df %>%
        group_by(src_country) %>%
        summarise(v = sum(metric), .groups="drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 10) %>%
        pull(src_country)
      
      df <- df %>% filter(src_country %in% top_ct)
      
      # TOP 10 Sectors 
      top_sector <- df %>%
        group_by(Sector) %>%
        summarise(v = sum(metric), .groups="drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 10) %>%
        pull(Sector)
      
      df <- df %>% filter(Sector %in% top_sector)
      
      #TOP 10 Activities 
      top_act <- df %>%
        group_by(Activity) %>%
        summarise(v = sum(metric), .groups="drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 10) %>%
        pull(Activity)
      
      df <- df %>% filter(Activity %in% top_act)
      
      df
    })
    
    
    
    output$sankey_inflow_chart <- renderHighchart({
      df <- sankey_df()
      req(nrow(df) > 0)
      
      final_country <- selected_country_name()
      txt <- text_col()
      
      # 1 → 2
      link_ct_sector <- df %>%
        group_by(src_country, Sector) %>%
        summarise(weight = sum(metric), .groups="drop") %>%
        transmute(from = src_country, to = Sector, weight)
      
      # 2 → 3
      link_sector_act <- df %>%
        group_by(Sector, Activity) %>%
        summarise(weight = sum(metric), .groups="drop") %>%
        transmute(from = Sector, to = Activity, weight)
      
      # 3 → 4 
      link_act_final <- df %>%
        group_by(Activity) %>%
        summarise(weight = sum(metric), .groups="drop") %>%
        transmute(from = Activity, to = final_country, weight)
      
      links <- bind_rows(link_ct_sector, link_sector_act, link_act_final)
      
      nodes <- tibble(name = unique(c(links$from, links$to)))
      
      highchart() %>%
        hc_chart(type = "sankey", backgroundColor = bg_col()) %>%
        hc_add_series(
          type = "sankey",
          data = list_parse(links),
          nodes = list_parse(nodes),
          name = "FDI Pathway",
          linkOpacity = 0.55,
          nodeWidth = 25
        ) %>%
        hc_title(
          text = paste(
            "FDI Inflow Pathway — Sources → Sectors → Activities →", final_country
          ),
          style = list(color = txt)
        ) %>%
        hc_plotOptions(
          sankey = list(
            dataLabels = list(
              enabled = TRUE,
              color = txt,
              style = list(fontSize = "12px", textOutline = "none")
            )
          )
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = if (theme() == "dark") "#000000" else "#ffffff",
          borderColor     = if (theme() == "dark") "#000000" else "#d0d0d0",
          style = list(
            color    = if (theme() == "dark") "#ffffff" else "#1e1f21",
            fontSize = "12px"
          ),
          pointFormatter = JS("
    function () {

      // ----- LINKS -----
      if (this.from && this.to) {
        return '<b>' + this.from + ' → ' + this.to + '</b><br>' +
               '<b>' + Highcharts.numberFormat(Math.round(this.weight), 0) + '</b>';
      }

      // ----- NODES -----
      if (this.name && this.sum !== undefined) {
        return '<b>' + this.name + '</b><br>' +
               '<b>' + Highcharts.numberFormat(Math.round(this.sum), 0) + '</b>';
      }

      return null;
    }
  ")
        ) %>%
        hc_exporting(enabled=TRUE)
      
    })
    
    
    
    
    
    
  })
}








########################################################################################################
##### outflow tab (same structure as inflow)


### UI MODULE 3: outflows

unilateral_outflow_UI <- function(id) {
  ns <- NS(id)
  
  group_choices <- c(
    "EU-group"     = "EU",
    "NAFTA-group"  = "NAFTA",
    "ASEAN-group"  = "ASEAN",
    "BRICS-group"  = "BRICS"
  )
  
  tagList(

    

    div(
      class = "info-banner",
      style = "margin-bottom: 25px;",
      HTML("
        Unilateral Analysis of FDI Outflows from the selected country.<br>
        Explore raw numbers, trends, partner-country contributions,
        and sector–country dynamics shaping investment inflows.
      ")
    ),
    
    

    # KPI CARD of selected country

    div(
      class = "kpi-container",
      div(
        class = "kpi-card kpi-blue",
        h3("Selected Country"),
        div(class = "kpi-number", textOutput(ns("kpi_country_name")))
      )
    ),
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin-top:35px;
    margin-bottom:35px;
    margin-left:20px;
 
     "
    ),
    
  
    
    div(
      id = ns("country_outflow_section"),
      style = "
        display:flex;
        flex-direction:row;
        gap:20px;
        margin-top:70px;
        margin-bottom:100px;
        
      ",
      


      div(
        style = "
          width:260px;
          padding:18px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
          display:flex;
          flex-direction:column;
          font-size:12px;
          margin-left:50px;
        ",
        
        div(class="left-menu-title", "Country Outflows — Options",
            style="font-size:14px; margin-bottom:10px;"),
        
        div("Top N Countries:", style="margin-top:10px; margin-bottom:5px;"),
        sliderInput(ns("top_n_country"), NULL,
                    min = 3, max = 40, value = 10, step = 1,
                    width="100%"),
        
        div("Metric:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("metric_country"), NULL,
          choices = c("Capital Investment"="flow_value",
                      "Jobs Created"="flow_job",
                      "Projects"="flow_n"),
          inline = FALSE,
          selected = "flow_value",
          width="100%"
        ),
        
        div("Status:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("status_country"), NULL,
          choices = c("Announced","Opened","Total"),
          inline = FALSE,
          selected = "Opened"
        ),
        
        div("Country Groups:", style="margin-top:15px; margin-bottom:5px;"),
        checkboxGroupInput(
          ns("country_groups"),
          label = NULL,
          choices  = group_choices,
          selected = "EU",
          inline   = FALSE
        ),
        
        
        div("Timeframe:", style="margin-top:15px; margin-bottom:5px;"),
        sliderInput(ns("year_range_country"), NULL,
                    min = 2003, max = MAX_YEAR,
                    value = c(2003, MAX_YEAR),
                    width="100%"),
      ),
      
      
      div(
        style = "
    flex-grow:1;
    padding:10px;
    background:transparent;
    position:relative;        
  ",
        
        div(
          style="
      position:absolute;
      top:-20px;
      right:10px;
      z-index:10;
      width:130px;
    ",
          selectInput(
            ns("chart_view_country"),
            label = NULL,
            choices = c(
              "Bar"        = "bar",
              "Pie"        = "pie",
              "Trend"      = "trend",
              "Cumulative" = "cumulative"
            ),
            width = "130px"
          )
        ),
        
        withSpinner(
          highchartOutput(ns("country_outflow_chart"), height="460px"),
          type = 6, color = "#b984db"
        )
      )
      
    ),
    
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin-top:35px;
    margin-bottom:35px;
    margin-left:20px;
  "
    ),
    
    
   
    
    div(
      id = ns("sector_outflow_section"),
      style = "
        display:flex;
        flex-direction:row;
        gap:20px;
        margin-top:60px;
        margin-bottom:60px;
      ",
      
    
      div(
        style = "
          width:260px;
          padding:18px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
          display:flex;
          flex-direction:column;
          font-size:12px;
          margin-left:50px;

        ",
        
        div(class="left-menu-title", "Sectoral Outflows — Options",
            style="font-size:14px; margin-bottom:10px;"),
        
        div("Level:", style="margin-top:10px; margin-bottom:5px;"),
        selectInput(ns("sector_level"), NULL,
                    choices = c("Sector","Subsector","Activity"),
                    width="100%"),
        
        div("Top N Items:", style="margin-top:15px; margin-bottom:5px;"),
        sliderInput(ns("top_n_sector"), NULL,
                    min = 3, max = 40, value = 10, step = 1,
                    width="100%"),
        
        div("Metric:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("metric_sector"), NULL,
          choices = c("Capital Investment"="flow_value",
                      "Jobs Created"="flow_job",
                      "Projects"="flow_n"),
          inline = FALSE,
          selected = "flow_value"
        ),
        
        div("Status:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("status_sector"), NULL,
          choices = c("Announced","Opened","Total"),
          inline = FALSE,
          selected = "Opened"
        ),
        
      
        
        div("Timeframe:", style="margin-top:15px; margin-bottom:5px;"),
        sliderInput(ns("year_range_sector"), NULL,
                    min = 2003, max = MAX_YEAR,
                    value = c(2003, MAX_YEAR),
                    width="100%")
      ),
      
      

      div(
        style = "
    flex-grow:1;
    padding:10px;
    background:transparent;
    position:relative;
  ",
        
        div(
          style="
      position:absolute;
      top:-20px;
      right:10px;
      z-index:10;
      width:130px;
    ",
          selectInput(
            ns("chart_view_sector"),
            label = NULL,
            choices = c(
              "Bar"        = "bar",
              "Pie"        = "pie",
              "Trend"      = "trend",
              "Cumulative" = "cumulative"
            ),
            width = "130px"
          )
        ),
        
        withSpinner(
          highchartOutput(ns("sector_outflow_chart"), height="460px"),
          type = 6, color = "#b984db"
        )
      )
      
    ),
    
    
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin-top:35px;
    margin-bottom:35px;
    margin-left:20px;
  "
    ),
    
    
    div(
      id = ns("sankey_outflow_section"),
      style = "
    margin-top:60px;
    margin-bottom:60px;
    padding-left:50px;
    padding-right:50px;
  ",
      
      
      h3(
        "Investment Pathway — Activities → Sectors → Countries ", inline = TRUE),
    
    div(
      style = "
          width:100%;
          display:flex;
          flex-direction:row;
          align-items:flex-end;
          gap:25px;
          padding:15px;
          margin-top:25px;
          margin-bottom:30px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
        ",
      
      div(
        style="flex:1;",
        div("Metric:", style="margin-bottom:5px; font-weight:600;"),
        radioButtons(
          ns("sankey_metric_out"), NULL,
          choices = c(
            "Capital Investment" = "flow_value",
            "Jobs Created"       = "flow_job",
            "Projects"           = "flow_n"
          ),
          inline = TRUE,
          selected = "flow_value"
        )
      ),
      
      div(
        style="flex:1;",
        div("Status:", style="margin-bottom:5px; font-weight:600;"),
        radioButtons(
          ns("sankey_status_out"), NULL,
          choices = c("Announced", "Opened", "Total"),
          inline = TRUE,
          selected = "Opened"
        )
      ),
      
      div(
        style="flex:1.3;",
        div("Country Groups:", style="margin-bottom:5px; font-weight:600;"),
        tags$div(
          style="
      display:grid;
      grid-template-columns: repeat(2, max-content);
      column-gap: 20px;
      row-gap: 6px;
    ",
          checkboxGroupInput(
            ns("sankey_country_groups"),
            label = NULL,
            choices  = group_choices,
            selected = "EU"
          )
        )
      ),
      
      
      div(
        style="flex:1;",
        div("Timeframe:", style="margin-bottom:5px; font-weight:600;"),
        sliderInput(
          ns("sankey_year_range_out"), NULL,
          min = 2003, max = MAX_YEAR,
          value = c(2003, MAX_YEAR),
          width = "100%"
        )
      )
    ),
    
    # Sankey chart
    withSpinner(
      highchartOutput(ns("sankey_outflow_chart"), height="600px"),
      type = 6, color = "#b984db"
    )
  )
 )
    
   
}






### SERVER MODULE 3: outflows 

##############################################################################################
unilateral_outflow_server <- function(id, selected_country, fdi, unilateral_list) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    selected_country_name <- reactive({
      req(selected_country())
      iso <- selected_country()
      
      nm <- unilateral_list %>%
        filter(iso3 == iso) %>%
        pull(country)
      
      if (length(nm) == 0) return(iso) 
      nm
    })
    
    # KPI: Full country name
    output$kpi_country_name <- renderText(selected_country_name())
    
    
    # THEME 
    theme <- reactive(input$theme_mode %||% "dark")
    
    text_col <- reactive(if (theme() == "light") "#1e1f21" else "#ffffff")
    grid_col <- reactive(if (theme() == "light") "#c8c8c8" else "#2a2b2d")
    bg_col   <-  reactive(if (theme() == "light") "#f2f4f7" else "#232427")
    
    
    map_country_group <- function(iso3_cp) {
      case_when(
        iso3_cp %in% c(
          "FRA","DEU","ITA","ESP","POL","NLD","BEL","SWE","AUT","DNK","FIN",
          "PRT","CZE","GRC","HUN","IRL","ROU","SVK","SVN","HRV","BGR",
          "EST","LVA","LTU","CYP","LUX","MLT"
        ) ~ "European Union",
        
        iso3_cp %in% c("CAN","MEX","USA") ~ "NAFTA",
        
        iso3_cp %in% c("BRA","RUS","IND","CHN","ZAF") ~ "BRICS",
        
        iso3_cp %in% c(
          "IDN","MYS","PHL","SGP","THA","VNM",
          "BRN","KHM","LAO","MMR"
        ) ~ "ASEAN",
        
        TRUE ~ NA_character_
      )
    }
    
    
    
    apply_country_groups <- function(df, selected_groups) {
      
      if (is.null(selected_groups) || length(selected_groups) == 0) {
        return(df)
      }
      
      group_map <- c(
        EU     = "European Union",
        NAFTA = "NAFTA",
        ASEAN = "ASEAN",
        BRICS = "BRICS"
      )
      
      active_groups <- unname(group_map[selected_groups])
      
      df %>%
        mutate(
          country_group = map_country_group(iso3_cp),
          dest_country = ifelse(
            !is.na(country_group) & country_group %in% active_groups,
            country_group,
            dest_country
          )
        ) %>%
        select(-country_group)
    }
    
    
    
    # BASE OUTFLOW DATA: keep inflow name for copy paste

    outflow_df <- reactive({
      req(selected_country())
      fdi %>% filter(iso3_ref == selected_country())
    })
    
    # utilities
    filter_years <- function(df, yr) df %>% filter(year >= yr[1], year <= yr[2])
    
    filter_status <- function(df, status_choice) {
      
      grouping_vars <- c(
        setdiff(names(df), c("value", "jobs", "n_projects", "status"))
      )
      
      if (status_choice == "Total") {
        
        df %>%
          filter(status %in% c("Announced", "Opened")) %>%
          group_by(across(all_of(grouping_vars))) %>%
          summarise(
            value = sum(value, na.rm=TRUE),
            jobs  = sum(jobs, na.rm=TRUE),
            n_projects = sum(n_projects, na.rm=TRUE),
            .groups = "drop"
          )
        
      } else {
        
        df %>% filter(status == status_choice)
        
      }
    }
    
    metric_var <- reactive({
      switch(input$metric_country,
             flow_value="value", flow_job="jobs", flow_n="n_projects")
    })
    
    metric_label <- reactive({
      switch(input$metric_country,
             flow_value = "Volume (USD Million)",
             flow_job   = "Jobs Created",
             flow_n     = "Number of Projects")
    })
    
    
    metric_name <- reactive({
      switch(input$metric_country,
             flow_value = "Volume",
             flow_job   = "Jobs Created",
             flow_n     = "Number of Projects")
    })
    
    
    # COUNTRY OUTFLOW DATA : keep inflow name for copy paste

    country_outflow_data <- reactive({
      outflow_df() %>%
        filter_years(input$year_range_country) %>%
        filter_status(input$status_country) %>%
        apply_country_groups(input$country_groups) %>%   
        group_by(dest_country) %>%                                     
        summarise(value = sum(.data[[metric_var()]]), .groups="drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = input$top_n_country)
    })
    
    
    country_outflow_trend <- reactive({
      outflow_df() %>%
        filter_years(input$year_range_country) %>%
        filter_status(input$status_country) %>%
        apply_country_groups(input$country_groups) %>%   # ✅
        group_by(year, dest_country) %>%
        summarise(value = sum(.data[[metric_var()]]), .groups="drop")
    })
    
    
    country_outflow_cumulative <- reactive({
      country_outflow_trend() %>%
        group_by(dest_country) %>%
        arrange(year) %>%
        mutate(cum_value = cumsum(value))
    })
    
    
    ### add NAs as 0 for trend and cumulative
    
    expand_country_years <- function(trend_df, top_ct, years) {
      
      full_grid <- expand.grid(
        year = years,
        dest_country = top_ct,
        stringsAsFactors = FALSE
      )
      
      merged <- full_grid %>%
        left_join(trend_df, by = c("year","dest_country"))
      
      merged$value[is.na(merged$value)] <- 0
      
      merged
    }
    
    expand_country_years_cumulative <- function(trend_df, top_ct, years) {
      
      expanded <- expand_country_years(trend_df, top_ct, years)
      
      expanded %>%
        arrange(dest_country, year) %>%
        group_by(dest_country) %>%
        mutate(cum_value = cumsum(value)) %>%
        ungroup()
    }
    
    
    # HIGHCHART OUTPUT : same names
    
    

    output$country_outflow_chart <- renderHighchart({
      
      country_for_title <- {
        nm <- selected_country_name()
        
        prefix_countries <- c(
          "United States",
          "United Kingdom",
          "Netherlands",
          "Philippines",
          "United Arab Emirates"
        )
        
        if (nm %in% prefix_countries) {
          paste("the", nm)
        } else {
          nm
        }
      }
      
      df <- country_outflow_data()
      req(nrow(df) > 0)
      
      view <- input$chart_view_country
      txt <- text_col(); grd <- grid_col()
      
      #  bar
      
      
      if (view == "bar") {
        
        df$color <- bar_palette[seq_len(nrow(df))]
        
        hc <- highchart() %>%
          hc_chart(type="column", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_country," Destinations of Greenfield FDI Outlows from ", country_for_title, " by ", metric_name(), " (", input$status_country, " Projects)"),
            style=list(color=txt)
          ) %>%
          hc_xAxis(
            categories = df$dest_country,
            labels=list(style=list(color=txt))
          ) %>%
          hc_yAxis(
            title=list(text=metric_label(), style=list(color=txt)),
            labels=list(style=list(color=txt)),
            gridLineColor=grd
          ) %>%
          hc_add_series(
            type = "column",
            name = NULL,
            showInLegend = FALSE,
            data = purrr::map2(df$value, df$color, ~ list(y = .x, color = .y))
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.category + '</b><br>' +
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
      }
      
    #   pie
      
      
      
      else if (view == "pie") {
        
        hc <- highchart() %>%
          hc_chart(type = "pie", backgroundColor = bg_col()) %>%
          hc_title(
            text = paste0("Share of Top ", input$top_n_country," Destinations of Greenfield FDI Outlows from ", country_for_title, " by ", metric_name(), " (", input$status_country, " Projects)"),
            style = list(color = txt)
          ) %>%
          hc_plotOptions(
            pie = list(
              allowPointSelect = TRUE,
              cursor = "pointer",
              dataLabels = list(
                enabled = TRUE,
                format = "<b>{point.name}</b>: {point.percentage:.1f}%",
                style = list(
                  color = txt,
                  fontSize = "12px",
                  textOutline = "none"
                )
              )
            )
          ) %>%
          hc_add_series(
            type = "pie",
            data = purrr::map2(
              df$dest_country, df$value,
              ~ list(name = .x, y = .y)
            )
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return 
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
      }
      
 
      
      
      ### trend
      
           
       else if (view == "trend") {
        
        raw_tr <- country_outflow_trend()
        
        years <- seq(min(raw_tr$year), max(raw_tr$year))
        top_ct <- df$dest_country
        
        tr <- expand_country_years(raw_tr, top_ct, years)
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_country," Destinations of Greenfield FDI Outlows from ", country_for_title, " by ", metric_name(), " (", input$status_country, " Projects)"),
            style=list(color=txt)) %>%
          hc_xAxis(categories=years, labels=list(style=list(color=txt))) %>%
          hc_yAxis(title=list(text=metric_label(), style=list(color=txt)),
                   labels=list(style=list(color=txt)),
                   gridLineColor=grd) %>%
          hc_legend(
            itemStyle=list(color=txt),
            itemHoverStyle=list(color=txt),
            itemHiddenStyle=list(color=grd)
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.series.name + '</b><br>' +
             'Year: ' + this.category + '<br>' +
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
        
        for (ct in top_ct) {
          series_df <- tr %>% filter(dest_country == ct) %>% arrange(year)
          hc <- hc %>% hc_add_series(name=ct, data=series_df$value)
        }
      }
      

     ### cumulative      
      
      else if (view == "cumulative") {
        
        raw_tr <- country_outflow_trend()
        
        years <- seq(min(raw_tr$year), max(raw_tr$year))
        top_ct <- df$dest_country
        
        tr <- expand_country_years_cumulative(raw_tr, top_ct, years)
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_country," Destinations of Greenfield FDI Outlows from ", country_for_title, " by ", metric_name(), " (", input$status_country, " Projects)"),
            style=list(color=txt)) %>%
          hc_xAxis(categories=years, labels=list(style=list(color=txt))) %>%
          hc_yAxis(title=list(text=metric_label(), style=list(color=txt)),
                   labels=list(style=list(color=txt)),
                   gridLineColor=grd) %>%
          hc_legend(
            itemStyle=list(color=txt),
            itemHoverStyle=list(color=txt),
            itemHiddenStyle=list(color=grd)
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.series.name + '</b><br>' +
             'Year: ' + this.category + '<br>' +
             'Cumulative flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
        
        for (ct in top_ct) {
          series_df <- tr %>% filter(dest_country == ct) %>% arrange(year)
          hc <- hc %>% hc_add_series(name=ct, data=series_df$cum_value)
        }
      }
      
      
      hc
    })
    
    
    
    
    
    
    #### sector
    
    
    sector_var <- reactive({
      switch(input$sector_level,
             "Sector"    = "Sector",
             "Subsector" = "Subsector",
             "Activity"  = "Activity")
    })
    
    sector_label <- reactive({
      switch(input$sector_level,
             "Sector"    = "Sectors",
             "Subsector" = "Subsectors",
             "Activity"  = "Activities
             ")
    })
    
    metric_var_sector <- reactive({
      switch(input$metric_sector,
             flow_value = "value",
             flow_job   = "jobs",
             flow_n     = "n_projects")
    })
    
    metric_label_sector <- reactive({
      switch(input$metric_sector,
             flow_value = "Volume (USD Million)",
             flow_job   = "Jobs Created",
             flow_n     = "Number of Projects")
    })
    
    
    metric_name_sector <- reactive({
      switch(input$metric_sector,
             flow_value = "Volume",
             flow_job   = "Jobs Created",
             flow_n     = "Number of Projects")
    })
    
    
    
    sector_outflow_data <- reactive({
      outflow_df() %>%
        filter_years(input$year_range_sector) %>%
        filter_status(input$status_sector) %>%
        group_by(.data[[sector_var()]]) %>%
        summarise(value = sum(.data[[metric_var_sector()]], na.rm = TRUE), .groups="drop") %>%
        arrange(desc(value)) %>%
        slice_head(n = input$top_n_sector)
    })
    
    sector_outflow_trend <- reactive({
      outflow_df() %>%
        filter_years(input$year_range_sector) %>%
        filter_status(input$status_sector) %>%
        group_by(year, !!sym(sector_var())) %>%
        summarise(value = sum(.data[[metric_var_sector()]], na.rm = TRUE), .groups="drop")
    })
    
    sector_outflow_cumulative <- reactive({
      sector_outflow_trend() %>%
        group_by(!!sym(sector_var())) %>%
        arrange(year) %>%
        mutate(cum_value = cumsum(value))
    })
    
    
    
    expand_sector_years <- function(trend_df, items, years, col_name) {
      
      full_grid <- expand.grid(
        year = years,
        item = items,
        stringsAsFactors = FALSE
      )
      
      merged <- full_grid %>%
        left_join(
          trend_df %>% rename(item = all_of(col_name)),
          by = c("year", "item")
        )
      
      merged$value[is.na(merged$value)] <- 0
      merged
    }
    
    expand_sector_years_cumulative <- function(trend_df, items, years, col_name) {
      expand_sector_years(trend_df, items, years, col_name) %>%
        arrange(item, year) %>%
        group_by(item) %>%
        mutate(cum_value = cumsum(value)) %>%
        ungroup()
    }
    
    
    
    output$sector_outflow_chart <- renderHighchart({
      
      country_for_title <- {
        nm <- selected_country_name()
        
        prefix_countries <- c(
          "United States",
          "United Kingdom",
          "Netherlands",
          "Philippines",
          "United Arab Emirates"
        )
        
        if (nm %in% prefix_countries) {
          paste("the", nm)
        } else {
          nm
        }
      }
      
      
      df <- sector_outflow_data()
      req(nrow(df) > 0)
      
      view <- input$chart_view_sector
      txt <- text_col(); grd <- grid_col()
      var <- sector_var()
      
      
      
      
      # bar
      
      
      
      
      if (view == "bar") {
        
        df$color <- bar_palette[seq_len(nrow(df))]
        
        hc <- highchart() %>%
          hc_chart(type = "column", backgroundColor = bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_sector,  " Greenfield FDI Outflow ", sector_label(), " from ", country_for_title, " by ", metric_name_sector(), " (", input$status_sector, " Projects)"),
            style = list(color = txt)
          ) %>%
          hc_xAxis(
            categories = df[[var]],
            labels = list(style = list(color = txt))
          ) %>%
          hc_yAxis(
            title = list(text = metric_label_sector(), style = list(color = txt)),
            labels = list(style = list(color = txt)),
            gridLineColor = grd
          ) %>%
          hc_add_series(
            type = "column",
            name = NULL,
            showInLegend = FALSE,
            data = purrr::map2(
              df$value,
              df$color,
              ~ list(y = .x, color = .y)
            )
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.category + '</b><br>' +
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
      }
      
      
      ### pie
      
      else if (view == "pie") {
        
        hc <- highchart() %>%
          hc_chart(type = "pie", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Share of Top ", input$top_n_sector,  " Greenfield FDI Outflow ", sector_label(), " from ", country_for_title, " by ", metric_name_sector(), " (", input$status_sector, " Projects)"),
            style=list(color=txt)
          ) %>%
          hc_plotOptions(
            pie = list(
              allowPointSelect = TRUE,
              cursor = "pointer",
              dataLabels = list(
                enabled = TRUE,
                format = "<b>{point.name}</b>: {point.percentage:.1f}%",
                style = list(color = txt, textOutline="none")
              )
            )
          ) %>%
          hc_add_series(
            data = purrr::map2(df[[var]], df$value, ~list(name=.x, y=.y))
          )%>%
          hc_tooltip(
            pointFormatter = JS("
        function () {
          return 'Flow: <b>' +
                 Highcharts.numberFormat(this.y, 2) +
                 '</b>';
        }
      ")
          ) %>%
          hc_exporting(enabled=TRUE)
      }
      
      
      ### trend
      
      
      else if (view == "trend") {
        
        raw_tr <- sector_outflow_trend()
        
        years <- seq(min(raw_tr$year), max(raw_tr$year))
        items <- df[[var]]
        
        tr <- expand_sector_years(raw_tr, items, years, var)
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_sector,  " Greenfield FDI Outflow ", sector_label(), " from ", country_for_title, " by ", metric_name_sector(), " (", input$status_sector, " Projects)"),
            style=list(color=txt)
          ) %>%
          hc_xAxis(categories=years, labels=list(style=list(color=txt))) %>%
          hc_yAxis(
            title=list(text=metric_label_sector(), style=list(color=txt)),
            labels=list(style=list(color=txt)),
            gridLineColor=grd
          ) %>%
          hc_legend(
            itemStyle=list(color=txt),
            itemHoverStyle=list(color=txt),
            itemHiddenStyle=list(color=grd)
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.series.name + '</b><br>' +
             'Year: ' + this.category + '<br>' +
             'Flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
        
        for (it in items) {
          series_df <- tr %>% filter(item == it) %>% arrange(year)
          hc <- hc %>% hc_add_series(name=it, data=series_df$value)
        }
      }
      
      
      
      
      #    cumulative
      
      
      
      else if (view == "cumulative") {
        
        raw_tr <- sector_outflow_trend()
        
        years <- seq(min(raw_tr$year), max(raw_tr$year))
        items <- df[[var]]
        
        tr <- expand_sector_years_cumulative(raw_tr, items, years, var)
        
        hc <- highchart() %>%
          hc_chart(type="line", backgroundColor=bg_col()) %>%
          hc_title(
            text = paste0("Top ", input$top_n_sector,  " Greenfield FDI Outflow ", sector_label(), " from ", country_for_title, " by ", metric_name_sector(), " (", input$status_sector, " Projects)"),
            style=list(color=txt)
          ) %>%
          hc_xAxis(categories=years, labels=list(style=list(color=txt))) %>%
          hc_yAxis(
            title=list(text=metric_label_sector(), style=list(color=txt)),
            labels=list(style=list(color=txt)),
            gridLineColor=grd
          ) %>%
          hc_legend(
            itemStyle=list(color=txt),
            itemHoverStyle=list(color=txt),
            itemHiddenStyle=list(color=grd)
          )%>%
          hc_tooltip(
            pointFormatter = JS("
    function () {
      return '<b>' + this.series.name + '</b><br>' +
             'Year: ' + this.category + '<br>' +
             'Cumulative flow: <b>' +
             Highcharts.numberFormat(this.y, 2) +
             '</b>';
    }
  ")
          ) %>%
          hc_exporting(enabled=TRUE)
        
        
        for (it in items) {
          series_df <- tr %>% filter(item == it) %>% arrange(year)
          hc <- hc %>% hc_add_series(name=it, data=series_df$cum_value)
        }
      }
      
      hc
    })
    
    
    ####### sankey tool (commented)
    
    
    
    metric_var_sankey <- reactive({
      switch(input$sankey_metric_out,
             flow_value = "value",
             flow_job   = "jobs",
             flow_n     = "n_projects")
    })
    
    # Preparing base DF
    sankey_out_base <- reactive({
      req(selected_country())
      fdi %>% filter(iso3_ref == selected_country())
    })
    
    # Status filtering (same as above)
    filter_status_sankey <- function(df, status_choice) {
      
      grouping_vars <- setdiff(names(df), c("value","jobs","n_projects","status"))
      
      if (status_choice == "Total") {
        df %>%
          filter(status %in% c("Announced","Opened")) %>%
          group_by(across(all_of(grouping_vars))) %>%
          summarise(
            value      = sum(value, na.rm = TRUE),
            jobs       = sum(jobs, na.rm = TRUE),
            n_projects = sum(n_projects, na.rm = TRUE),
            .groups = "drop"
          )
      } else {
        df %>% filter(status == status_choice)
      }
    }
    
    #  sankey DF
    sankey_out_df <- reactive({
      
      df <- sankey_out_base() %>%
        filter(year >= input$sankey_year_range_out[1],
               year <= input$sankey_year_range_out[2]) %>%
        filter_status_sankey(input$sankey_status_out) %>%
        apply_country_groups(input$sankey_country_groups)
      
        
      
      df <- df %>% mutate(value = .data[[metric_var_sankey()]])
      df <- df %>% filter(!is.na(value) & value > 0)
      
      # TOP 10 FILTERS 
      top_activities <- df %>%
        group_by(Activity) %>%
        summarise(v = sum(value), .groups="drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 10) %>%
        pull(Activity)
      
      df <- df %>% filter(Activity %in% top_activities)
      
      top_sectors <- df %>%
        group_by(Sector) %>%
        summarise(v = sum(value), .groups="drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 10) %>%
        pull(Sector)
      
      df <- df %>% filter(Sector %in% top_sectors)
      
      top_dest <- df %>%
        group_by(dest_country) %>%
        summarise(v = sum(value), .groups="drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 10) %>%
        pull(dest_country)
      
      df <- df %>% filter(dest_country %in% top_dest)
      
      df
    })
    
    # Render Sankey
    output$sankey_outflow_chart <- renderHighchart({
      df <- sankey_out_df()
      req(nrow(df) > 0)
      
      theme <- input$theme_mode %||% "dark"
      text_col <- if (theme == "light") "#1e1f21" else "#ffffff"
      selected_cty <- selected_country_name()
      
      
      #  BUILD LINK TABLES
      
      
      # Country → Activity
      link_cty_act <- df %>%
        group_by(Activity) %>%
        summarise(weight = sum(value), .groups="drop") %>%
        mutate(from = selected_cty, to = Activity)
      
      # Activity → Sector
      link_act_sec <- df %>%
        group_by(Activity, Sector) %>%
        summarise(weight = sum(value), .groups="drop") %>%
        rename(from = Activity, to = Sector)
      
      # Sector → Destination Country
      link_sec_cty <- df %>%
        group_by(Sector, dest_country) %>%
        summarise(weight = sum(value), .groups="drop") %>%
        rename(from = Sector, to = dest_country)
      
      links <- bind_rows(link_cty_act, link_act_sec, link_sec_cty)
      
      nodes <- tibble(name = unique(c(links$from, links$to)))
      

      #  PLOT
      
      highchart() %>%
        hc_chart(type="sankey", backgroundColor=bg_col()) %>%
        hc_add_series(
          type="sankey",
          keys=c("from","to","weight"),
          data=list_parse(links),
          nodes=list_parse(nodes),
          name="FDI Outflows",
          linkOpacity=0.55,
          nodeWidth=36
        ) %>%
        hc_plotOptions(
          sankey=list(
            nodePadding=22,
            curveFactor=0.45,
            dataLabels=list(
              enabled=TRUE,
              color=text_col,
              style=list(fontSize="12px", fontWeight="600", textOutline="none")
            )
          )
        ) %>%
        hc_title(
          text = paste0(
            "FDI Outflow Pathway — ", selected_cty,
            " → Activity → Sector → Destination Countries"
          ),
          style=list(color=text_col, fontSize="16px")
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = if (theme() == "dark") "#000000" else "#ffffff",
          borderColor     = if (theme() == "dark") "#000000" else "#d0d0d0",
          style = list(
            color    = if (theme() == "dark") "#ffffff" else "#1e1f21",
            fontSize = "12px"
          ),
          pointFormatter = JS("
    function () {

      // ----- LINKS -----
      if (this.from && this.to) {
        return '<b>' + this.from + ' → ' + this.to + '</b><br>' +
               '<b>' + Highcharts.numberFormat(Math.round(this.weight), 0) + '</b>';
      }

      // ----- NODES -----
      if (this.name && this.sum !== undefined) {
        return '<b>' + this.name + '</b><br>' +
               '<b>' + Highcharts.numberFormat(Math.round(this.sum), 0) + '</b>';
      }

      return null;
    }
  ")
        ) %>%
        hc_exporting(enabled=TRUE)
      
      
    })
    
    
    
  })
}





#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################
#########################################################################################################################################################








################################### total and net flows 




unilateral_total_net_map_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    


    div(
      class = "info-banner",
      style = "margin-bottom:25px;
      margin-top:35px;",
      HTML("
      IMPORTANT! loading might not work smoothly all the time, however always allow for up to 10 seconds for the charts to render, as they will always render<br>
       This section provides a comprehensive view of the selected country’s Greenfield FDI dynamics.<br>
       The upper chart presents a detailed time-series analysis of inflows, outflows, total and net flows, with flexible controls over metrics, frequency, and aggregation.<br>
       The lower map offers a global overview of bilateral FDI relationships, highlighting total and net flows between the selected country and its partner economies.
     ")
    ),
    

    # KPI CARD

    div(
      class = "kpi-container",
      div(
        class = "kpi-card kpi-blue",
        h3("Selected Country"),
        div(class="kpi-number", textOutput(ns("kpi_country_name")))
      )
    ),
    
    div(
      style="
        width:100%;
        border-bottom:2px solid var(--text-main);
        opacity:0.35;
        margin:35px 0 50px 20px;
      "
    ),
    
    

    # TIME SERIES – FDI FLOWS

    
    div(
      style="margin:40px 50px 60px 50px;",
      
     
      
      div(
        style="
      display:flex;
      flex-direction:row;
      gap:20px;
    ",
        
        div(
          style="
        width:300px;
        padding:18px;
        border:2px solid var(--text-main);
        border-radius:12px;
        background:rgba(255,255,255,0.03);
        font-size:12px;
      ",
          
          div(
            class="left-menu-title",
            "FDI Flows - Options",
            style="font-size:14px; margin-bottom:10px;"
          ),  
          
          div("Metric:", style="margin-bottom:5px;"),
          radioButtons(
            ns("ts_metric"),
            NULL,
            choices = c(
              "Capital Investment" = "value",
              "Jobs Created"       = "jobs",
              "Projects"           = "n_projects"
            ),
            selected = "value"
          ),
          
          div("Flow Type:", style="margin-top:15px; margin-bottom:5px;"),
          radioButtons(
            ns("ts_flow"),
            NULL,
            choices = c(
              "Inflows"  = "inflow",
              "Outflows" = "outflow",
              "Total"    = "total",
              "Net (In − Out)" = "net"
            ),
            selected = "total"
          ),
          
          div("Frequency:", style="margin-top:15px; margin-bottom:5px;"),
          radioButtons(
            ns("ts_freq"),
            NULL,
            choices = c(
              "Monthly"   = "month",
              "Quarterly" = "quarter",
              "Annual"    = "year"
            ),
            selected = "year"
          ),
          
          div("Plot Type:", style="margin-top:15px; margin-bottom:5px;"),
          radioButtons(
            ns("ts_plot_type"),
            NULL,
            choices = c(
              "Period Values" = "level",
              "Cumulative"    = "cum"
            ),
            selected = "level"
          ),
          
          div("Timeframe:", style="margin-top:15px; margin-bottom:5px;"),
          sliderInput(
            ns("ts_year_range"),
            NULL,
            min = 2003, max = MAX_YEAR,
            value = c(2003, MAX_YEAR),
            width = "100%"
          )
        ),
        
        div(
          style="flex-grow:1;",
          withSpinner(
            highchartOutput(ns("flows_ts_chart"), height="520px"),
            type=6, color="#b984db"
          )
        )
      )
    ),
    
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin-top:45px;
    margin-bottom:35px;
    margin-left:40px;
  "
    ),
    

    # MAP SECTION

    div(
      style="
        display:flex;
        flex-direction:row;
        gap:20px;
        margin-bottom:80px;
      ",
      
      div(
        style="
          width:260px;
          padding:18px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
          display:flex;
          flex-direction:column;
          font-size:12px;
          margin-left:50px;
        ",
        
        div(class="left-menu-title",
            "World Map — Options",
            style="font-size:14px; margin-bottom:10px;"),
        
        div("Flow Type:", style="margin-top:10px; margin-bottom:5px;"),
        radioButtons(
          ns("flow_type"),
          NULL,
          choices = c(
            "Total Flow" = "total",
            "Net Flow (In − Out)" = "net"
          ),
          selected = "net"
        ),
        
        div("Metric:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("metric"),
          NULL,
          choices = c(
            "Capital Investment"="flow_value",
            "Jobs Created"="flow_job",
            "Projects"="flow_n"
          ),
          selected="flow_value"
        ),
        
        div("Status:", style="margin-top:15px; margin-bottom:5px;"),
        radioButtons(
          ns("status"),
          NULL,
          choices=c("Announced","Opened","Total"),
          selected="Opened"
        ),
        
        div("Timeframe:", style="margin-top:15px; margin-bottom:5px;"),
        sliderInput(
          ns("year_range"),
          NULL,
          min=2003, max=2025,
          value=c(2003,2025),
          width="100%"
        )
      ),
      
      div(
        style="flex-grow:1; padding:10px;",
        withSpinner(
          highchartOutput(ns("world_map"), height="620px"),
          type=6, color="#b984db"
        )
      )
    )
  )
}





unilateral_total_net_map_server <- function(
    id,
    selected_country,
    fdi,
    unilateral_list
) {
  
  moduleServer(id, function(input, output, session) {
    
    
    selected_country_name <- reactive({
      req(selected_country())
      nm <- unilateral_list %>%
        dplyr::filter(iso3 == selected_country()) %>%
        dplyr::pull(country)
      ifelse(length(nm) == 0, selected_country(), nm)
    })
    
    output$kpi_country_name <- renderText(selected_country_name())
    
    
   
    theme <- reactive(input$theme_mode %||% "dark")
    txt_col <- reactive(if (theme() == "light") "#1e1f21" else "#ffffff")
    bg_col <- reactive(if (theme() == "light") "#f2f4f7" else "#232427")
    
    
    

    # METRIC HANDLING

    
    metric_var <- reactive({
      switch(input$metric,
             flow_value = "value",
             flow_job   = "jobs",
             flow_n     = "n_projects")
    })
    
    metric_label <- reactive({
      switch(input$metric,
             flow_value = "volume (in millions)",
             flow_job   = "jobs",
             flow_n     = "projects")
    })
    
    

    # FILTER HELPERS

    
    filter_status <- function(df, status_choice) {
      grp <- setdiff(names(df), c("value", "jobs", "n_projects", "status"))
      if (status_choice == "Total") {
        df %>%
          dplyr::filter(status %in% c("Announced", "Opened")) %>%
          dplyr::group_by(dplyr::across(all_of(grp))) %>%
          dplyr::summarise(
            value      = sum(value,      na.rm = TRUE),
            jobs       = sum(jobs,       na.rm = TRUE),
            n_projects = sum(n_projects, na.rm = TRUE),
            .groups    = "drop"
          )
      } else {
        df %>% dplyr::filter(status == status_choice)
      }
    }
    
    

    # FLOW COMPUTATION

    
    flow_map_df <- reactive({
      
      req(selected_country())
      
      df <- fdi %>%
        dplyr::filter(
          year >= input$year_range[1],
          year <= input$year_range[2]
        ) %>%
        filter_status(input$status)
      
      df <- df %>% dplyr::mutate(val = .data[[metric_var()]])
      
      #  OUTFLOWS: selected_country -> partner 
      out_df <- df %>%
        dplyr::filter(iso3_ref == selected_country()) %>%
        dplyr::group_by(iso3_cp) %>%
        dplyr::summarise(out = sum(val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(dest_country = iso3_cp)
      
      #  INFLOWS: partner -> selected_country
      in_df <- df %>%
        dplyr::filter(iso3_cp == selected_country()) %>%
        dplyr::group_by(iso3_ref) %>%
        dplyr::summarise(inf = sum(val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::rename(dest_country = iso3_ref)
      
      # Merging
      
      m <- dplyr::full_join(out_df, in_df, by = "dest_country") %>%
        dplyr::mutate(
          out   = tidyr::replace_na(out, 0),
          inf   = tidyr::replace_na(inf, 0),
          total = inf + out,
          net   = inf - out
        )
      
      m$value <- if (input$flow_type == "net") m$net else m$total
      
      m
    })
    
    

    # RENDER MAP

    output$world_map <- renderHighchart({
      
      df <- flow_map_df()
      req(nrow(df) > 0)
      
      # values for quantile-based scaling
      vals <- df$value
      vals <- vals[is.finite(vals)]
      if (length(vals) == 0) vals <- c(0, 1)
      
      q <- as.numeric(
        quantile(
          vals,
          probs = c(0, .15, .30, .50, .70, .85, 1),
          na.rm = TRUE
        )
      )
      
      vmin <- q[1]
      vmax <- q[7]
      if (vmin == vmax) vmax <- vmin + 1
      
      
      legend_title <- if (input$metric == "flow_value") {
        if (input$flow_type == "net") {
          "Net Volume (in millions)"
        } else {
          "Total Volume (in millions)"
        }
      } else {
        if (input$flow_type == "net") {
          paste("Net", metric_label())
        } else {
          paste("Total", metric_label())
        }
      }
      
      
      highchart(type = "map") %>%
        hc_chart(backgroundColor = bg_col()) %>%
        
        hc_add_series_map(
          worldgeojson,
          df,
          joinBy = c("iso-a3", "dest_country"),
          value  = "value",
          name   = paste(
            if (input$flow_type == "net") "Net" else "Total",
            metric_label(),
            "with",
            selected_country_name()
          )
        ) %>%
        
        
      hc_colorAxis(
        min = vmin,
        max = vmax,
        title = list(
          text = legend_title,
          style = list(
            color = txt_col(),
            fontSize = "12px",
            fontWeight = "600"
          )
        ),
        labels = list(
          style = list(
            color = txt_col(),
            fontSize = "11px"
          )
        ),
        stops = list(
          list(0.00, '#6d3040'),
          list(0.15, '#cd624f'),
          list(0.30, '#dabc41'),
          list(0.50, '#e9d584'),
          list(0.70, '#bcd285'),
          list(0.85, '#84A76D'),
          list(1.00, '#4c7d55')
        )
      ) %>%
        
      
      hc_legend(
        enabled = TRUE,
        itemStyle = list(
          color = txt_col(),
          fontSize = "12px"
        ),
        itemHoverStyle = list(color = txt_col()),
        title = list(
          style = list(
            color = txt_col(),
            fontSize = "12px",
            fontWeight = "600"
          )
        )
      ) %>%
        
        hc_title(
          text = paste(
            "Global",
            if (input$flow_type == "net") "net" else "total",
            "FDI flows relative to",
            selected_country_name(), "by", metric_label()
          ),
          style = list(color = txt_col())
        ) %>%
        
        hc_tooltip(
          useHTML = TRUE,
          backgroundColor = if (theme() == "dark") "#000000" else "#ffffff",
          borderColor     = if (theme() == "dark") "#000000" else "#d0d0d0",
          style = list(
            color      = if (theme() == "dark") "#ffffff" else "#1e1f21",
            fontSize   = "12px"
          ),
          pointFormat = paste0(
            "<b>{point.name}</b><br>",
            "Value: <b>{point.value:,.0f}</b>"
          )
        )%>%
        hc_mapNavigation(
          enabled = TRUE,           
          enableMouseWheelZoom = TRUE 
        )%>%
        hc_caption( useHTML = TRUE,
          text = "Note: Net flows refer to selected country´s net flow with the partner country on the map, meaning Selected Country´s inflow from partner country minus Selected Country´s outflow to partner country <br>
          Total Flow refers to the sum of inflows and outfloes between the Selected Country and the country displayed on the map",
          style = list(
            color      = if (theme() == "dark") "#ffffff" else "#1e1f21"
          )) %>%
        hc_exporting(enabled=TRUE)
    })
    
    
    
    
    #### first chart
    
    
    
    
    ts_df <- reactive({
      
      req(selected_country())
      
      df <- fdi %>%
        dplyr::filter(
          year >= input$ts_year_range[1],
          year <= input$ts_year_range[2],
          status %in% c("Opened", "Announced")
        ) %>%
        dplyr::mutate(
          val = .data[[input$ts_metric]],
          period = dplyr::case_when(
            input$ts_freq == "year"    ~ as.character(year),
            input$ts_freq == "quarter" ~ paste0(year, " Q", lubridate::quarter(ym)),
            input$ts_freq == "month"   ~ as.character(ym)
          )
        )
      
      # INFLOWS BY STATUS 
      inflow <- df %>%
        dplyr::filter(iso3_cp == selected_country()) %>%
        dplyr::group_by(period, status) %>%
        dplyr::summarise(val = sum(val, na.rm = TRUE), .groups="drop")
      
      #  OUTFLOWS BY STATUS 
      outflow <- df %>%
        dplyr::filter(iso3_ref == selected_country()) %>%
        dplyr::group_by(period, status) %>%
        dplyr::summarise(val = sum(val, na.rm = TRUE), .groups="drop")
      
      #  MERGE FLOWS 
      flows <- dplyr::full_join(
        inflow %>% dplyr::rename(inflow = val),
        outflow %>% dplyr::rename(outflow = val),
        by = c("period", "status")
      ) %>%
        dplyr::mutate(
          inflow  = tidyr::replace_na(inflow, 0),
          outflow = tidyr::replace_na(outflow, 0),
          total   = inflow + outflow,
          net     = inflow - outflow
        )
      
      # SELECT FLOW TYPE 
      flows$value <- dplyr::case_when(
        input$ts_flow == "inflow"  ~ flows$inflow,
        input$ts_flow == "outflow" ~ flows$outflow,
        input$ts_flow == "total"   ~ flows$total,
        TRUE                       ~ flows$net
      )
      
      #  TOTAL STATUS
      total_status <- flows %>%
        dplyr::group_by(period) %>%
        dplyr::summarise(
          value  = sum(value, na.rm = TRUE),
          status = "Total",
          .groups = "drop"
        )
      
      final <- dplyr::bind_rows(
        flows %>% dplyr::select(period, value, status),
        total_status
      )
      
      #  CUMULATIVE 
      
      
      all_periods <- sort(unique(final$period))
      all_status  <- c("Opened", "Announced", "Total")
      
      final <- tidyr::expand_grid(
        period = all_periods,
        status = all_status
      ) %>%
        dplyr::left_join(final, by = c("period", "status")) %>%
        dplyr::mutate(value = tidyr::replace_na(value, 0))
      
      if (input$ts_plot_type == "cum") {
        final <- final %>%
          dplyr::group_by(status) %>%
          dplyr::arrange(period) %>%
          dplyr::mutate(value = cumsum(value)) %>%
          dplyr::ungroup()
      }
      
      final
      
    })
    
    
    
    
    output$flows_ts_chart <- renderHighchart({
      
      df <- ts_df()
      req(nrow(df) > 0)
      
      flow_lbl <- switch(
        input$ts_flow,
        inflow  = "inflow",
        outflow = "outflow",
        total   = "total flow",
        net     = "net flow"
      )
      
      metric_lbl <- switch(
        input$ts_metric,
        value = "volume",
        jobs = "jobs",
        n_projects = "projects"
      )
      
      freq_lbl <- switch(
        input$ts_freq,
        month   = "Monthly",
        quarter = "Quarterly",
        year    = "Annual"
      )
      
      highchart() %>%
        hc_chart(
          type = "line",
          backgroundColor = bg_col()
        ) %>%
        hc_xAxis(
          categories = sort(unique(df$period)),
          labels = list(style=list(color=txt_col()))
        ) %>%
        hc_yAxis(
          title = list(
            text = paste(
              flow_lbl,
              switch(
                input$ts_metric,
                value       = "volume (Million USD)",
                jobs        = "Jobs",
                n_projects  = "Projects"
              )
            ),
            style = list(color=txt_col())
          ),
          labels = list(style=list(color=txt_col()))
        ) %>%
        hc_add_series_list(
          list(
            list(
              name = "Opened",
              data = df %>% dplyr::filter(status=="Opened") %>% dplyr::pull(value),
              color = "lightblue"
            ),
            list(
              name = "Announced",
              data = df %>% dplyr::filter(status=="Announced") %>% dplyr::pull(value),
              color = "orange"
            ),
            list(
              name = "Total",
              data = df %>% dplyr::filter(status=="Total") %>% dplyr::pull(value),
              dashStyle = "Dash",
              color = "lightgreen"
            )
          )
        )%>%
        hc_title(
          text = paste(
            selected_country_name(),
            "- Greenfield FDI",
            flow_lbl,
            metric_lbl,
            "over time"
          ),
          style = list(color = txt_col())
        ) %>%
        hc_tooltip(
          shared = TRUE,
          valueDecimals = if (input$ts_metric == "value") 2 else 0
        )%>%
      hc_legend(itemStyle = list(color = txt_col())) %>%
        hc_exporting(enabled = TRUE)
    })
    
    
    
    
  })
}



################################################################################################################################
##################################################################################################################################












################### bilateral


bilateral_selection_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "info-banner",
      style = "margin-top:25px; margin-bottom:15px;",
      HTML("
        Select two countries to explore bilateral Greenfield FDI flows.<br>
        Directional flows, sectoral patterns, and investment pathways
        will update automatically based on the selected pair.
      ")
    ),
    
    # SELECTION PANEL 
    div(
      style = "
        display:flex;
        justify-content:center;
        gap:40px;
        margin-top:40px;
        margin-bottom:100px;
      ",
      
      # COUNTRY A

      div(
        style = "
          width:420px;
          padding:25px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
        ",
        
        div(
          class = 'left-menu-title',
          style = 'font-size:20px; text-align:center; margin-bottom:20px;',
          'First Country'
        ),
        
        selectizeInput(
          inputId = ns("country_from"),
          label = NULL,
          choices = NULL,
          width = "100%",
          options = list(
            placeholder = "Choose first country",
            maxOptions  = 200
          )
        )
      ),
      
      # COUNTRY B
      div(
        style = "
          width:420px;
          padding:25px;
          border:2px solid var(--text-main);
          border-radius:12px;
          background:rgba(255,255,255,0.03);
        ",
        
        div(
          class = 'left-menu-title',
          style = 'font-size:20px; text-align:center; margin-bottom:20px;',
          'Second Country'
        ),
        
        selectizeInput(
          inputId = ns("country_to"),
          label = NULL,
          choices = NULL,
          width = "100%",
          options = list(
            placeholder = "Choose second country",
            maxOptions  = 200
          )
        )
        
      )
      
    ),
    
    
    ### OUTPUT SLEECTED COUNTRIES
    
    div(
      style = "
    display:flex;
    justify-content:center;
    gap:40px;
    margin-top:40px;
    margin-bottom:580px;
  ",
      
      div(
        class = "kpi-card kpi-blue",
        style = "width:320px; text-align:center;",
        h3("First Country"),
        div(class = "kpi-number", textOutput(ns("from_country_name"))),
        div(style="opacity:0.7;", textOutput(ns("from_country_iso")))
      ),
      
      div(
        class = "kpi-card kpi-red",
        style = "width:320px; text-align:center;",
        h3("Second Country"),
        div(class = "kpi-number", textOutput(ns("to_country_name"))),
        div(style="opacity:0.7;", textOutput(ns("to_country_iso")))
      )
    )
  )
}



###### server 





bilateral_selection_server <- function(
    id,
    country_list,
    selected_from,
    selected_to
) {
  
  moduleServer(id, function(input, output, session) {
    
    # helper
    country_name <- function(iso) {
      nm <- country_list %>% filter(iso3 == iso) %>% pull(country)
      ifelse(length(nm) == 0, iso, nm)
    }
    
    # choices ordered
    countries <- country_list %>%
      mutate(
        label = paste0(country, " - ", iso3),
        order = case_when(
          iso3 == "USA" ~ 1,
          iso3 == "CHN" ~ 2,
          TRUE ~ 3
        )
      ) %>%
      arrange(order, country)
    
    base_choices <- setNames(countries$iso3, countries$label)
    
    # initial population
    session$onFlushed(function() {
      
      updateSelectizeInput(
        session, "country_from",
        choices  = base_choices,
        selected = "USA"
      )
      
      updateSelectizeInput(
        session, "country_to",
        choices  = base_choices[base_choices != "USA"],
        selected = "CHN"
      )
      
      selected_from("USA")
      selected_to("CHN")
      
    }, once = TRUE)
    
    
    
    ### next: to avoid selecting the same country in both country A and B
    
    # FROM → update TO
    
    observeEvent(input$country_from, {
      req(input$country_from)
      
      if (identical(input$country_from, selected_from())) return()
      
      selected_from(input$country_from)
      
      updateSelectizeInput(
        session,
        "country_to",
        choices  = base_choices[base_choices != input$country_from],
        selected = if (input$country_to == input$country_from) NULL else input$country_to
      )
      
    }, ignoreInit = TRUE)
    
    
    # TO → update FROM
    
    observeEvent(input$country_to, {
      req(input$country_to)
      
      if (identical(input$country_to, selected_to())) return()
      
      selected_to(input$country_to)
      
      updateSelectizeInput(
        session,
        "country_from",
        choices  = base_choices[base_choices != input$country_to],
        selected = if (input$country_from == input$country_to) NULL else input$country_from
      )
      
    }, ignoreInit = TRUE)
    

    # delayed KPI rendering: again 2 seconds of time

    delayed_render <- reactive({
      req(selected_from(), selected_to())
      Sys.sleep(2)
      
      list(
        from_name = country_name(selected_from()),
        from_iso  = selected_from(),
        to_name   = country_name(selected_to()),
        to_iso    = selected_to()
      )
    })
    
    output$from_country_name <- renderText(delayed_render()$from_name)
    output$from_country_iso  <- renderText(delayed_render()$from_iso)
    output$to_country_name   <- renderText(delayed_render()$to_name)
    output$to_country_iso    <- renderText(delayed_render()$to_iso)
    
    return(list(
      country_from = reactive(selected_from()),
      country_to   = reactive(selected_to())
    ))
  })
}






########################################################################################################################

#BILATERAL UI for FLOWS 




bilateral_flows_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    div(
      style = "margin-bottom:25px;
      margin-top:35px;",
      class = "info-banner",
      HTML("
        Bilateral Greenfield FDI analysis between the selected country pair.<br>
        Charts display both Announced and Opened project flows in both directions, total flows as well as net flows.
      ")
    ),
    
    div(
      class = "kpi-container",
      style = "margin-bottom:50px;",
      div(class = "kpi-card kpi-blue",
          h3("First Country"),
          div(class = "kpi-number", textOutput(ns("country_from_name"))),
          div(class = "kpi-number-static", textOutput(ns("country_from_iso")))
      ),
      
      div(class = "kpi-card kpi-orange",
          h3("Second Country"),
          div(class = "kpi-number", textOutput(ns("country_to_name"))),
          div(class = "kpi-number-static", textOutput(ns("country_to_iso")))
      )
    ),
    
    div(
      style = "
    margin:30px 0 10px 30px;
    font-size:14px;
    font-weight:600;
    opacity:0.8;
    ",
      "Total Greenfield FDI across the selected country pair"
    ),
    
    
    div(
      class = "status-options",
      div(class="status-option active", `data-status`="Announced", "Announced FDI"),
      div(class="status-option",        `data-status`="Opened",    "Opened FDI")
    ),
    
    div(
      class="kpi-container",
      div(class="kpi-card kpi-red",
          h3("FDI Value"),
          div(class="kpi-number", textOutput(ns("kpi_value")))
      ),
      div(class="kpi-card kpi-orange",
          h3("Jobs Created"),
          div(class="kpi-number", textOutput(ns("kpi_jobs")))
      ),
      div(class="kpi-card kpi-green",
          h3("Projects"),
          div(class="kpi-number", textOutput(ns("kpi_projects")))
      )
    ),
    
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin:35px 0 35px 20px;
  "
    ),
    
    
    div(
      style = "
    display:flex;
    gap:30px;
    margin:40px 0 30px 50px;
    padding:18px;
    border:2px solid var(--border);
    border-radius:12px;
    background:rgba(255,255,255,0.03);
    ",
      
      div(
        style="width:220px;",
        div("Metric", class="left-menu-title"),
        radioButtons(ns("metric"), NULL,
                     choices=c("Value"="value","Jobs"="jobs","Projects"="n_projects"),
                     selected="value")
      ),
      div(
        style="width:220px;",
        div("Mode", class="left-menu-title"),
        radioButtons(ns("mode"), NULL,
                     choices=c("Period values"="flow","Cumulative"="cumulative"),
                     selected="flow")
      ),
      div(
        style="width:220px;",
        div("Frequency", class="left-menu-title"),
        radioButtons(ns("frequency"), NULL,
                     choices=c("Monthly"="monthly",
                               "Quarterly"="quarterly",
                               "Yearly"="yearly"),
                     selected="yearly")
      ),
      div(
        style="width:240px;",
        div("Timeframe (Years)", class="left-menu-title"),
        sliderInput(
          ns("year_range"),
          NULL,
          min = 2003,
          max = MAX_YEAR,
          value = c(2003, MAX_YEAR),
          sep = "",
          width = "100%"
        )
      )
      
    ),
    
    div(
      class="two-chart-row",
      div(class="chart-box",
          div(class="chart-title", textOutput(ns("title_ab"))),
          highchartOutput(ns("chart_ab"), height="420px")),
      div(class="chart-box",
          div(class="chart-title", textOutput(ns("title_ba"))),
          highchartOutput(ns("chart_ba"), height="420px"))
    ),
    
    div(
      style = "
    width:100%;
    border-bottom:2px solid var(--text-main);
    opacity:0.35;
    margin:45px 0 35px 20px;
  "
    ),
    
    div(
      class = "two-chart-row",
      
      div(
        class = "chart-box",
        div(class = "chart-title", textOutput(ns("title_total"))),
        highchartOutput(ns("chart_total"), height = "420px")
      ),
      
      div(
        class = "chart-box",
        div(class = "chart-title", textOutput(ns("title_net"))),
        highchartOutput(ns("chart_net"), height = "420px")
      )
    )
    
  )
}



bilateral_flow_server <- function(
    id,
    selected_country_from,
    selected_country_to,
    unilateral_list,
    data,          
    data_month     
) {
  
  moduleServer(id, function(input, output, session) {
    
    
    
    country_name <- function(iso) {
      nm <- unilateral_list %>% filter(iso3 == iso) %>% pull(country)
      ifelse(length(nm) == 0, iso, nm)
    }
    
    output$country_from_name <- renderText(country_name(selected_country_from()))
    output$country_from_iso  <- renderText(selected_country_from())
    output$country_to_name   <- renderText(country_name(selected_country_to()))
    output$country_to_iso    <- renderText(selected_country_to())
    

    kpi_df <- reactive({
      req(selected_country_from(), selected_country_to())
      
      data %>%
        filter(
          status == (input$status %||% "Announced"),
          (
            (iso3_cp == selected_country_from() & iso3_ref == selected_country_to()) |
              (iso3_cp == selected_country_to()   & iso3_ref == selected_country_from())
          )
        )
    })
    
    output$kpi_value <- renderText({
      paste0("$", scales::comma(sum(kpi_df()$value)/1000), " B")
    })
    
    output$kpi_jobs <- renderText({
      scales::comma(sum(kpi_df()$jobs))
    })
    
    output$kpi_projects <- renderText({
      scales::comma(sum(kpi_df()$n_projects))
    })
    
   
    chart_base <- reactive({
      req(selected_country_from(), selected_country_to())
      
      data_month %>%
        filter(
          year >= input$year_range[1],
          year <= input$year_range[2],
          status %in% c("Announced","Opened"),
          (
            (iso3_cp == selected_country_from() & iso3_ref == selected_country_to()) |
              (iso3_cp == selected_country_to()   & iso3_ref == selected_country_from())
          )
        )
    })
    
    
    aggregate_ts <- function(df, src, dest) {
      
      df <- df %>%
        filter(iso3_cp == src, iso3_ref == dest)
      
      df <- switch(
        input$frequency,
        "monthly" = df %>%
          mutate(period = paste(year, sprintf("%02d", month), sep="-")),
        
        "quarterly" = df %>%
          mutate(period = paste0(year, "-Q", ceiling(month / 3))),
        
        "yearly" = df %>%
          mutate(period = as.character(year))
      )
      
      df_sum <- df %>%
        group_by(period, status) %>%
        summarise(
          value      = sum(value, na.rm = TRUE),
          jobs       = sum(jobs, na.rm = TRUE),
          n_projects = sum(n_projects, na.rm = TRUE),
          .groups = "drop"
        )
      
      all_periods <- sort(unique(df$period))
      all_status  <- c("Announced", "Opened")
      
      df_complete <- df_sum %>%
        tidyr::complete(
          period = all_periods,
          status = all_status,
          fill = list(
            value = 0,
            jobs = 0,
            n_projects = 0
          )
        ) %>%
        arrange(period)
      
      if (input$mode == "cumulative") {
        df_complete <- df_complete %>%
          group_by(status) %>%
          mutate(across(c(value, jobs, n_projects), cumsum)) %>%
          ungroup()
      }
      
      df_complete
    }
    
    
    ts_ab <- reactive({
      aggregate_ts(chart_base(),
                   selected_country_from(),
                   selected_country_to())
    })
    
    ts_ba <- reactive({
      aggregate_ts(chart_base(),
                   selected_country_to(),
                   selected_country_from())
    })
    
    
    
    to_wide <- function(df) {
      df %>%
        tidyr::pivot_wider(
          names_from  = status,
          values_from = c(value, jobs, n_projects),
          values_fill = 0
        )
    }
    
    
    ts_total <- reactive({
      
      df_ab <- ts_ab()
      df_ba <- ts_ba()
      
      df <- df_ab %>%
        rename_with(~ paste0(.x, "_ab"), c(value, jobs, n_projects)) %>%
        left_join(
          df_ba %>%
            rename_with(~ paste0(.x, "_ba"), c(value, jobs, n_projects)),
          by = c("period", "status")
        ) %>%
        mutate(
          value      = dplyr::coalesce(value_ab, 0) + dplyr::coalesce(value_ba, 0),
          jobs       = dplyr::coalesce(jobs_ab, 0)  + dplyr::coalesce(jobs_ba, 0),
          n_projects = dplyr::coalesce(n_projects_ab, 0) +
            dplyr::coalesce(n_projects_ba, 0)
        ) %>%
        select(period, status, value, jobs, n_projects)
      
      tidyr::complete(
        df,
        period,
        status,
        fill = list(value = 0, jobs = 0, n_projects = 0)
      ) %>%
        arrange(period)
    })
    
    
    
    
    ts_net <- reactive({
      
      df_ab <- ts_ab()
      df_ba <- ts_ba()
      
      df <- df_ab %>%
        rename_with(~ paste0(.x, "_ab"), c(value, jobs, n_projects)) %>%
        left_join(
          df_ba %>%
            rename_with(~ paste0(.x, "_ba"), c(value, jobs, n_projects)),
          by = c("period", "status")
        ) %>%
        mutate(
          value      = dplyr::coalesce(value_ab, 0) - dplyr::coalesce(value_ba, 0),
          jobs       = dplyr::coalesce(jobs_ab, 0)  - dplyr::coalesce(jobs_ba, 0),
          n_projects = dplyr::coalesce(n_projects_ab, 0) -
            dplyr::coalesce(n_projects_ba, 0)
        ) %>%
        select(period, status, value, jobs, n_projects)
      
      tidyr::complete(
        df,
        period,
        status,
        fill = list(value = 0, jobs = 0, n_projects = 0)
      ) %>%
        arrange(period)
    })
    
    
    
    
    
    metric_label <- reactive({
      switch(input$metric,
             value      = "Volume (USD Millions)",
             jobs       = "Jobs Created",
             n_projects = "Number of Projects")
    })
    
    
    
    

    
    country_name <- function(iso) {
      nm <- unilateral_list %>%
        filter(iso3 == iso) %>%
        pull(country)
      ifelse(length(nm) == 0, iso, nm)
    }
    
    
    output$title_ab <- renderText({
      paste0(
        "Greenfield FDI flows from ",
        country_name(selected_country_from()),
        " to ",
        country_name(selected_country_to()),
        " by ",
        metric_label()
      )
    })
    
    output$title_ba <- renderText({
      paste0(
        "Greenfield FDI flows from ",
        country_name(selected_country_to()),
        " to ",
        country_name(selected_country_from()),
        " by ",
        metric_label()
      )
    })
    
    output$title_total <- renderText({
      paste0(
        "Total Greenfield FDI flows between ",
        country_name(selected_country_from()),
        " and ",
        country_name(selected_country_to()),
        " by ",
        metric_label()
      )
    })
    
    output$title_net <- renderText({
      paste0(
        "Net Greenfield FDI flows (",
        country_name(selected_country_from()),
        " flows to ",
        country_name(selected_country_to()),
        " minus ",
        country_name(selected_country_to()),
        " flows to ",
        country_name(selected_country_from()),
        ") by ",
        metric_label()
      )
    })
    
    
   
    
    
    theme <- reactive(input$theme_mode %||% "dark")
    
    text_col <- reactive(if (theme() == "light") "#1e1f21" else "#ffffff")
    grid_col <- reactive(if (theme() == "light") "#c8c8c8" else "#c8c8c8")
    bg_col   <- reactive(if (theme() == "light") "#f2f4f7" else "#2a2b2d")
    
    col_ann  <- reactive(if (theme() == "light") "#1f4e79" else "#4c8cff")
    col_op   <- reactive(if (theme() == "light") "#b88a1e" else "#f7b538")
    
    make_chart <- function(df) {
      metric <- input$metric
      
      value_fmt <- if (metric == "value") 2 else 0
      
      highchart() %>%
        hc_chart(type="line", backgroundColor = bg_col()) %>%
        hc_xAxis(
          categories = unique(df$period),
          labels = list(style = list(color = text_col()))
        ) %>%
        hc_yAxis(
          title = list(
            text  = metric_label(),
            style = list(color = text_col())
          ),
          labels = list(style = list(color = text_col())),
          gridLineColor = grid_col()
        )%>%
        hc_add_series(
          name  = "Announced",
          data  = df %>% filter(status=="Announced") %>% pull(metric),
          color = col_ann()
        ) %>%
        hc_add_series(
          name  = "Opened",
          data  = df %>% filter(status=="Opened") %>% pull(metric),
          color = col_op()
        ) %>%
        hc_tooltip(
          shared = TRUE,
          valueDecimals = value_fmt
        ) %>%       
        hc_legend(itemStyle = list(color = text_col()))%>%
        hc_exporting(enabled = TRUE)
    }
    
    
    output$chart_ab <- renderHighchart({
      req(nrow(ts_ab()) > 0)
      make_chart(ts_ab())
    })
    
    output$chart_ba <- renderHighchart({
      req(nrow(ts_ba()) > 0)
      make_chart(ts_ba())
    })
    
    
    make_dual_chart <- function(df) {
      metric <- input$metric
      
      value_fmt <- if (metric == "value") 2 else 0
      
      highchart() %>%
        hc_chart(type = "line", backgroundColor = bg_col()) %>%
        hc_xAxis(
          categories = unique(df$period),
          labels = list(style = list(color = text_col()))
        ) %>%
        hc_yAxis(
          title = list(
            text  = metric_label(),
            style = list(color = text_col())
          ),
          labels = list(style = list(color = text_col())),
          gridLineColor = grid_col()
        ) %>%
        hc_add_series(
          name  = "Announced",
          data  = df %>% filter(status == "Announced") %>% pull(metric),
          color = col_ann()
        ) %>%
        hc_add_series(
          name  = "Opened",
          data  = df %>% filter(status == "Opened") %>% pull(metric),
          color = col_op()
        ) %>%
        hc_tooltip(
          shared = TRUE,
          valueDecimals = value_fmt
        ) %>%
        hc_legend(itemStyle = list(color = text_col()))%>%
        hc_exporting(enabled = TRUE)
    }
    
    
    output$chart_total <- renderHighchart({
      req(nrow(ts_total()) > 0)
      make_dual_chart(ts_total())
    })
    
    output$chart_net <- renderHighchart({
      req(nrow(ts_net()) > 0)
      make_dual_chart(ts_net())
    })
    
    
    
  })
}




##### bilateral UI for SECTORS





bilateral_sectors_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    div(
      class = "info-banner",
      style = "margin-bottom:25px;
      margin-top:35px;",
      HTML("
        Bilateral sectoral composition of Greenfield FDI flows.<br>
        Compare sector, subsector, or activity patterns across directions between the two selected countries.
      ")
    ),
    
    div(
      class = "kpi-container",
      
      div(class = "kpi-card kpi-blue",
          h3("First Country"),
          div(class = "kpi-number", textOutput(ns("country_from_name"))),
          div(class = "kpi-number-static", textOutput(ns("country_from_iso")))
      ),
      
      div(class = "kpi-card kpi-orange",
          h3("Second Country"),
          div(class = "kpi-number", textOutput(ns("country_to_name"))),
          div(class = "kpi-number-static", textOutput(ns("country_to_iso")))
      )
    ),
    
    div(
      style="
        width:100%;
        border-bottom:2px solid var(--text-main);
        opacity:0.35;
        margin:35px 0 35px 20px;
      "
    ),
    
    div(
      style="
        display:flex;
        gap:30px;
        margin:40px 0 30px 50px;
        padding:18px;
        border:2px solid var(--border);
        border-radius:12px;
        background:rgba(255,255,255,0.03);
      ",
      
      div(
        style="width:160px;",
        div("Status", class="left-menu-title"),
        radioButtons(
          ns("status"), NULL,
          choices = c("Announced","Opened","Total"),
          selected = "Opened"
        )
      ),
      
      div(
        style="width:200px;",
        div("Level", class="left-menu-title"),
        selectInput(
          ns("level"), NULL,
          choices = c("Sector","Subsector","Activity"),
          selected = "Sector"
        )
      ),
      
      div(
        style="width:200px;",
        div("Metric", class="left-menu-title"),
        radioButtons(
          ns("metric"), NULL,
          choices = c(
            "Volume"   = "value",
            "Jobs"     = "jobs",
            "Projects" = "n_projects"
          ),
          selected = "value"
        )
      ),
      
      div(
        style="width:160px;",
        div("Top N", class="left-menu-title"),
        sliderInput(
          ns("top_n"), NULL,
          min = 3, max = 30, value = 10
        )
      ),
      
      div(
        style="width:240px;",
        div("Timeframe (Years)", class="left-menu-title"),
        sliderInput(
          ns("year_range"), NULL,
          min = 2003, max = MAX_YEAR,
          value = c(2003, MAX_YEAR),
          sep = ""
        )
      )
    ),
    
    div(
      class = "two-chart-row",
      
      # A → B
      div(
        class = "chart-box",
        style = "position:relative;",
        
        div(
          style="position:absolute; top:-18px; right:10px; width:140px; z-index:10;",
          selectInput(
            ns("view_ab"), NULL,
            choices = c("Bar"="bar","Pie"="pie","Trend"="trend","Cumulative"="cumulative"),
            width = "140px"
          )
        ),
        
        div(class="chart-title", textOutput(ns("title_ab"))),
        highchartOutput(ns("chart_ab"), height="420px")
      ),
      
      # B → A
      div(
        class = "chart-box",
        style = "position:relative;",
        
        div(
          style="position:absolute; top:-18px; right:10px; width:140px; z-index:10;",
          selectInput(
            ns("view_ba"), NULL,
            choices = c("Bar"="bar","Pie"="pie","Trend"="trend","Cumulative"="cumulative"),
            width = "140px"
          )
        ),
        
        div(class="chart-title", textOutput(ns("title_ba"))),
        highchartOutput(ns("chart_ba"), height="420px")
      )
    ),
    
    
    div(
      style="
        width:100%;
        border-bottom:2px solid var(--text-main);
        opacity:0.35;
        margin:35px 0 35px 20px;
      "
    ),
    
    
    
    div(
      class = "info-banner",
      style = "margin-bottom:25px;
      margin-top:35px;",
      HTML("
        Sankey Tools allow you to decompose flows between the two countries in terms of sector, subsector, and activitiy.<br>
        The following sankey tools are also filtered from the same top menu and offer a more in-depth decomposition of sectoral flows compared to the first two charts.
      ")
    ),
    
    
    div(
      class = "two-chart-row",
      style = "width:90%;",
      
      div(
        class = "chart-box",
        style = "width:90%;",
        
        div(class = "chart-title", textOutput(ns("title_sankey_ab"))),
        highchartOutput(ns("sankey_ab"), height = "620px")
      )
    ),
    
   
    div(
      class = "two-chart-row",
      style = "width:90%;",
      
      div(
        class = "chart-box",
        style = "width:90%;",
        
        div(class = "chart-title", textOutput(ns("title_sankey_ba"))),
        highchartOutput(ns("sankey_ba"), height = "620px")
      )
    )
  )
}







bilateral_sectors_server <- function(
    id,
    selected_country_from,
    selected_country_to,
    fdi,
    unilateral_list
) {
  
  moduleServer(id, function(input, output, session) {
    
    theme <- reactive(input$theme_mode %||% "dark")
    
    text_col <- reactive(if (theme() == "light") "#1e1f21" else "#ffffff")
    grid_col <- reactive(if (theme() == "light") "#c8c8c8" else "#c8c8c8")
    bg_col   <- reactive(if (theme() == "light") "#f2f4f7" else "#2a2b2d")
    
    country_name <- function(iso) {
      nm <- unilateral_list %>% filter(iso3 == iso) %>% pull(country)
      ifelse(length(nm) == 0, iso, nm)
    }
    
    output$country_from_name <- renderText(country_name(selected_country_from()))
    output$country_from_iso  <- renderText(selected_country_from())
    output$country_to_name   <- renderText(country_name(selected_country_to()))
    output$country_to_iso    <- renderText(selected_country_to())
    
    base_df <- reactive({
      req(selected_country_from(), selected_country_to())
      
      df <- fdi %>%
        filter(
          year >= input$year_range[1],
          year <= input$year_range[2],
          (
            (iso3_cp == selected_country_from() & iso3_ref == selected_country_to()) |
              (iso3_cp == selected_country_to()   & iso3_ref == selected_country_from())
          )
        )
      
      if (input$status != "Total") {
        df <- df %>% filter(status == input$status)
      }
      
      df
    })
    
    level_var <- reactive({
      switch(input$level,
             Sector    = "Sector",
             Subsector = "Subsector",
             Activity  = "Activity")
    })
    
    metric_var <- reactive(input$metric)
    
    metric_label <- reactive({
      switch(input$metric,
             value = "Volume (USD Millions)",
             jobs  = "Jobs Created",
             n_projects = "Number of Projects")
    })
    
    sector_ts <- function(src, dest) {
      
      df <- base_df() %>%
        filter(iso3_cp == src, iso3_ref == dest) %>%
        group_by(year, item = .data[[level_var()]]) %>%
        summarise(
          value      = sum(value, na.rm=TRUE),
          jobs       = sum(jobs, na.rm=TRUE),
          n_projects = sum(n_projects, na.rm=TRUE),
          .groups="drop"
        )
      
      top_items <- df %>%
        group_by(item) %>%
        summarise(v = sum(.data[[metric_var()]]), .groups="drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = input$top_n) %>%
        pull(item)
      
      df %>% filter(item %in% top_items)
    }
    
    df_ab <- reactive(sector_ts(selected_country_from(), selected_country_to()))
    df_ba <- reactive(sector_ts(selected_country_to(), selected_country_from()))
    
    make_title <- function(from, to) {
      paste0(
        "Top ", input$top_n, " Greenfield FDI ",
        input$level, " flows from ",
        country_name(from), " to ", country_name(to),
        " (", input$status, ", ", metric_label(), ")"
      )
    }
    
    output$title_ab <- renderText(make_title(selected_country_from(), selected_country_to()))
    output$title_ba <- renderText(make_title(selected_country_to(), selected_country_from()))
    
    tooltip_formatter <- reactive({
      if (metric_var() == "value") {
        JS("
          function () {
            return '<b>' + this.point.name + '</b><br>' +
                   Highcharts.numberFormat(this.y, 2);
          }
        ")
      } else {
        JS("
          function () {
            return '<b>' + this.point.name + '</b><br>' +
                   Highcharts.numberFormat(this.y, 0);
          }
        ")
      }
    })
    
    render_sector_chart <- function(df, view) {
      
      req(nrow(df) > 0)
      
      metric <- metric_var()
      
      value_decimals <- if (metric == "value") 2 else 0
      
      
      if (view %in% c("bar","pie")) {
        
        plot_df <- df %>%
          group_by(item) %>%
          summarise(val = sum(.data[[metric]]), .groups="drop") %>%
          arrange(desc(val)) %>%
          slice_head(n = input$top_n)
        
        colors <- bar_palette[seq_len(nrow(plot_df))]
        
        hc <- highchart() %>%
          hc_chart(
            type = ifelse(view == "bar","column","pie"),
            backgroundColor = bg_col()
          ) %>%
          hc_tooltip(pointFormatter = tooltip_formatter())
        
        if (view == "bar") {
          
          colors <- bar_palette[seq_len(nrow(plot_df))]
          
          hc %>%
            hc_chart(type = "column", backgroundColor = bg_col()) %>%
            
            hc_xAxis(
              categories = plot_df$item,
              labels = list(style = list(color = text_col()))
            ) %>%
            
            hc_yAxis(
              title = list(text = metric_label(), style = list(color = text_col())),
              labels = list(style = list(color = text_col())),
              gridLineColor = grid_col()
            ) %>%
            
            hc_add_series(
              name = NULL,
              showInLegend = FALSE,
              data = purrr::map2(
                plot_df$val,
                colors,
                ~ list(y = .x, color = .y)
              )
            ) %>%
            
            hc_tooltip(
              useHTML = TRUE,
              backgroundColor = if (theme() == "dark") "#000000" else "#ffffff",
              borderColor     = if (theme() == "dark") "#000000" else "#d0d0d0",
              style = list(color = text_col()),
              pointFormatter = JS(sprintf(
                "function () {
           return '<b>' + this.category + '</b><br>' +
                  '<b>' + Highcharts.numberFormat(this.y, %d) + '</b>';
         }",
                value_decimals
              ))
            )%>%
            hc_exporting(enabled = TRUE)
          
        } else if (view == "pie") {
          
          colors <- bar_palette[seq_len(nrow(plot_df))]
          
          hc %>%
            hc_chart(type = "pie", backgroundColor = bg_col()) %>%
            
            hc_plotOptions(
              pie = list(
                allowPointSelect = TRUE,
                cursor = "pointer",
                colors = colors,
                dataLabels = list(
                  enabled = TRUE,
                  format = "<b>{point.name}</b>: {point.percentage:.1f}%",
                  style = list(
                    color = text_col(),
                    fontSize = "12px",
                    textOutline = "none"
                  )
                )
              )
            ) %>%
            
            hc_add_series(
              type = "pie",
              name = metric_label(),
              data = purrr::map2(
                plot_df$item,
                plot_df$val,
                ~ list(name = .x, y = .y)
              )
            ) %>%
            
            hc_tooltip(
              useHTML = TRUE,
              backgroundColor = if (theme() == "dark") "#000000" else "#ffffff",
              borderColor     = if (theme() == "dark") "#000000" else "#d0d0d0",
              style = list(color = text_col()),
              pointFormatter = JS(sprintf(
                "function () {
           return '<b>' + this.point.name + '</b><br>' +
                  'Share: <b>' + Highcharts.numberFormat(this.percentage, 1) + '%%</b><br>' +
                  'Value: <b>' + Highcharts.numberFormat(this.y, %d) + '</b>';
         }",
                value_decimals
              ))
            )%>%
            hc_exporting(enabled = TRUE)
        }
        
        
      } else {
        
        years <- sort(unique(df$year))
        items <- unique(df$item)
        
        full_grid <- expand.grid(
          year = years,
          item = items,
          stringsAsFactors = FALSE
        )
        
        plot_df <- full_grid %>%
          left_join(
            df %>%
              select(year, item, !!metric_var()),
            by = c("year", "item")
          ) %>%
          mutate(
            val = replace_na(.data[[metric_var()]], 0)
          ) %>%
          arrange(item, year) %>%
          group_by(item) %>%
          {
            if (view == "cumulative") {
              mutate(., val = cumsum(val))
            } else {
              .
            }
          } %>%
          ungroup()
        
        hc <- highchart() %>%
          hc_chart(type = "line", backgroundColor = bg_col()) %>%
          
          hc_xAxis(
            categories = years,
            labels = list(style = list(color = text_col()))
          ) %>%
          
          hc_yAxis(
            title = list(
              text = metric_label(),
              style = list(color = text_col())
            ),
            labels = list(style = list(color = text_col())),
            gridLineColor = grid_col()
          ) %>%
          
          hc_legend(
            enabled = TRUE,
            itemStyle = list(color = text_col()),
            itemHoverStyle = list(color = text_col()),
            itemHiddenStyle = list(color = grid_col())
          ) %>%
          
          hc_tooltip(
            shared = TRUE,
            valueDecimals = if (input$metric == "value") 2 else 0
          )%>%
          hc_exporting(enabled = TRUE)
        
        
        for (i in seq_along(unique(plot_df$item))) {
          it <- unique(plot_df$item)[i]
          hc <- hc %>%
            hc_add_series(
              name = it,
              data = plot_df %>% filter(item==it) %>% pull(val),
              color = bar_palette[i]
            )
        }
        
        hc
      }
    }
    
    output$chart_ab <- renderHighchart(render_sector_chart(df_ab(), input$view_ab))
    output$chart_ba <- renderHighchart(render_sector_chart(df_ba(), input$view_ba))
    
    
    
    output$title_sankey_ab <- renderText({
      paste0(
        "Greenfield FDI value chain from ",
        country_name(selected_country_from()),
        " to ",
        country_name(selected_country_to()),
        " by ",
        input$status, " ", metric_label()
      )
    })
    
    output$title_sankey_ba <- renderText({
      paste0(
        "Greenfield FDI value chain from ",
        country_name(selected_country_to()),
        " to ",
        country_name(selected_country_from()),
        " by ",
        input$status, " ", metric_label()
      )
    })
    
    
    
    build_sankey_data <- function(src, dest) {
      
      metric <- metric_var()
      
      df <- base_df() %>%
        filter(iso3_cp == src, iso3_ref == dest) %>%
        mutate(
          value = .data[[metric]],
          value = ifelse(is.na(value), 0, value)
        )
      
      top_sector <- df %>%
        group_by(Sector) %>%
        summarise(v = sum(value), .groups = "drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 9)
      
      df <- df %>% filter(Sector %in% top_sector$Sector)
      
      top_sub <- df %>%
        group_by(Sector, Subsector) %>%
        summarise(v = sum(value), .groups = "drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 9)
      
      df <- df %>%
        semi_join(top_sub, by = c("Sector", "Subsector"))
      
      top_act <- df %>%
        group_by(Subsector, Activity) %>%
        summarise(v = sum(value), .groups = "drop") %>%
        arrange(desc(v)) %>%
        slice_head(n = 9)
      
      df <- df %>%
        semi_join(top_act, by = c("Subsector", "Activity"))
      
      links <- bind_rows(
        
        # country → sector
        df %>%
          group_by(Sector) %>%
          summarise(weight = sum(value), .groups="drop") %>%
          transmute(
            from = country_name(src),
            to   = Sector,
            weight
          ),
        
        # sector → subsector
        df %>%
          group_by(Sector, Subsector) %>%
          summarise(weight = sum(value), .groups="drop") %>%
          transmute(
            from = Sector,
            to   = Subsector,
            weight
          ),
        
        # subsector → activity
        df %>%
          group_by(Subsector, Activity) %>%
          summarise(weight = sum(value), .groups="drop") %>%
          transmute(
            from = Subsector,
            to   = Activity,
            weight
          ),
        
        # activity → country
        df %>%
          group_by(Activity) %>%
          summarise(weight = sum(value), .groups="drop") %>%
          transmute(
            from = Activity,
            to   = country_name(dest),
            weight
          )
      )
      
      links
    }
    
    
    render_sankey <- function(links) {
      
      highchart() %>%
        hc_chart(
          type = "sankey",
          backgroundColor = bg_col()
        ) %>%
        hc_subtitle(
          text = "Origin -> Sector -> Subsector -> Activity -> Destination",
          style = list(color = text_col())
        )%>%
        hc_add_series(
          type = "sankey",
          data = links,
          keys = c("from", "to", "weight"),
          name = "FDI Flows"
        ) %>%
        hc_tooltip(
          pointFormat = paste0(
            "{point.fromNode.name} → {point.toNode.name}<br>",
            "<b>{point.weight:.2f}</b>"
          )
        ) %>%
        hc_legend(
          enabled = FALSE
        )%>%
        hc_exporting(enabled = TRUE)
    }
    
    
    
    output$sankey_ab <- renderHighchart({
      links <- build_sankey_data(
        selected_country_from(),
        selected_country_to()
      )
      req(nrow(links) > 0)
      render_sankey(links)
    })
    
    output$sankey_ba <- renderHighchart({
      links <- build_sankey_data(
        selected_country_to(),
        selected_country_from()
      )
      req(nrow(links) > 0)
      render_sankey(links)
    })
    
    
  })
}










#####################################################################################################################################







########################## ABOUT TAB: UI ONLY







about_UI <- function() {
  tagList(
    tags$div(
      class = "info-banner",
      style = "margin-top:40px; margin-bottom:100px;",
      "About this dashboard"
    ),
    
    tags$div(
      style = "
        max-width: 900px;
        margin: 0 auto;
        line-height: 1.6;
        font-size: 15px;
      ",
      
      tags$h3("Greenfield FDI Global Dashboard — Public Demo Version"),
      
      tags$p(
        "This dashboard is a public demonstration and structural replica of an internal ",
        "Greenfield Foreign Direct Investment (FDI) dashboard. It has been created for ",
        "illustrative and technical demonstration purposes only and does not contain ",
        "any confidential or proprietary data."
      ),
      
      tags$p(
        "All data displayed in this application are ",
        tags$strong("synthetic, randomly generated, and non-confidential"),
        ". They are designed solely to mimic the structure, dimensions, and functionality ",
        "of the original internal dashboard and should not be interpreted as real ",
        "investment figures or used for analytical, policy, or decision-making purposes."
      ),
      
      tags$h4("Data coverage (synthetic)"),
      tags$ul(
        tags$li("Time period: 2003–2025"),
        tags$li("Frequency: annual, quarterly, and monthly (where available)"),
        tags$li("Statuses: Announced, Opened, and Total FDI"),
        tags$li("Metrics: Volume (Millions/Billions), Jobs Created, Number of Projects")
      ),
      
      tags$h4("Main components"),
      tags$ul(
        tags$li("Global trends: countries, aggregates, and sectors"),
        tags$li("Country-level unilateral inflow, outflow, total flow, and net flow analysis"),
        tags$li("Aggregates comparisons: flows and sector trends"),
        tags$li("Bilateral country analysis: flows and sectors"),
        tags$li("Geopolitical event module: work in progress")
      ),
      
      tags$h4("Usage and limitations"),
      tags$p(
        "This public version is intended exclusively to demonstrate the layout, ",
        "interactive features, and analytical structure of the dashboard. ",
        "It does not reflect real-world FDI developments and must not be used ",
        "for empirical analysis or policy evaluation."
      ),
      
      tags$h4("Background"),
      tags$p(
        "The original internal dashboard was developed to analyze greenfield FDI ",
        "inflows and outflows using confidential data sources. This public replica ",
        "extends the geographical scope to all countries while replacing the ",
        "underlying data with fully synthetic equivalents."
      ),
      
      tags$hr(style = "margin-top: 20px; margin-bottom: 60px;"),
      
      tags$p(
        tags$strong("Version:"), " 1.0.2 (Public Demo)", tags$br(),
        tags$strong("Maintainer:"), " International Economics Department (IE)", tags$br(),
        tags$strong("Status:"), " Public demonstration version (synthetic data only)"
      )
    )
  )
}


