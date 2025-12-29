library(shiny)
library(shinyjs)
library(dplyr)
library(shinybusy)

#setwd("C:/Users/Dario/Desktop/global dashboard")

# datasets to be loaded in and sourcing

fdi           <- readRDS("data/fdi_basic.rds")
fdi_monthly   <- readRDS("data/fdi_monthly.rds")
fdi_quarterly <- readRDS("data/fdi_quarterly.rds")
fdi_yearly    <- readRDS("data/fdi_yearly.rds")
efficiency <- readRDS("data/efficiency_yearly.rds")
volatility_yearly   <- readRDS("data/volatility_yearly.rds")
conversion_yearly <- readRDS("data/conversion_yearly.rds")
project_size_dist_yearly <- readRDS("data/project_size_dist_yearly.rds")


fdi_country_inflow <- readRDS("data/fdi_country_inflow.rds")  
fdi_country_outflow <- readRDS("data/fdi_country_outflow.rds")
fdi_country_flow <- readRDS("data/fdi_country_flow.rds")


aggregates_all_flows <- readRDS("data/aggregates_all_flows.rds")
agg_sankey           <- readRDS("data/agg_sankey.rds")


sector_df <- readRDS("data/sector_df.rds")
subsector_df <- readRDS("data/subsector_df.rds")
activity_df <- readRDS("data/activity_df.rds")
sec_act_df <- readRDS("data/sec_act_df.rds")

unilateral_list <- readRDS("data/country_menu.rds")


fdi_only <- readRDS("data/fdi_only_flows.rds")
fdi_only_month <- readRDS("data/fdi_only_flows_month.rds")


source("output.R")


# styled UI

ui <- htmlTemplate(
  "index.html",
  screen = uiOutput("screen"),
  busy = add_busy_spinner(
    spin = "cube-grid",
    color = "#b984db",
    position = "full-page",
    timeout = 200
  )
)


# SERVER including all sub - servers

server <- function(input, output, session) {
  
  totalFDI_server(
    id = "total_fdi_global",
    fdi_data   = fdi,
    fdi_monthly = fdi_monthly,
    fdi_quarterly = fdi_quarterly,
    fdi_yearly = fdi_yearly,
    conversion = conversion_yearly,
    efficiency = efficiency,
    volatility = volatility_yearly,
    dist       = project_size_dist_yearly
  )
  
  totalFDI_country_server(
    id = "total_fdi_country",
    fdi_country_inflow  = fdi_country_inflow,
    fdi_country_outflow = fdi_country_outflow,
    fdi_country_flow = fdi_country_flow
    )
  
  totalFDI_aggregates_server(
    id = "total_fdi_aggregates",
    aggregates_all_flows = aggregates_all_flows,
    agg_sankey = agg_sankey
  )
  
  totalFDI_sector_server(
    id           = "total_fdi_sector",
    sector_df    = sector_df,
    subsector_df = subsector_df,
    activity_df  = activity_df,
    sec_act_df = sec_act_df,
    fdi=fdi
    )
  

  # reactive state for unilateral and bilateral selections
  
  unilateral_state <- reactiveVal(NULL)
  bilateral_state <- reactiveVal(NULL)
  
  
  ## unilateral selection
  
  selected_country <- reactiveVal(NULL)
  

  observeEvent(list(input$active_tab, input$global_option), {
    
    tab <- input$active_tab %||% "general"
    opt <- input$global_option %||% if (tab %in% c("unilateral", "bilateral")) "selection" else "total"
    
    # unilateral  
    if (tab == "unilateral" && opt == "selection") {
      unilateral_state(
        
        country_selection_server(
          id = "country_selection",
          unilateral_list = unilateral_list,
          selected_country = selected_country
        )
      )
    }
    
    # bilateral  
    if (tab == "bilateral" && opt == "selection") {
      bilateral_state(
        bilateral_selection_server(
          id = "bilateral_selection",
          country_list = unilateral_list,
          selected_from = selected_country_from,
          selected_to   = selected_country_to
        )
      )
    }
    
  })
  
  
  unilateral_inflow_server(
    id = "unilateral_inflow",
    selected_country = selected_country,
    fdi = fdi,
    unilateral_list = unilateral_list
  )
  
  unilateral_outflow_server(
    id = "unilateral_outflow",
    selected_country = selected_country,
    fdi = fdi,
    unilateral_list = unilateral_list
  )
  
  unilateral_total_net_map_server(
    id = "unilateral_flow",
    selected_country = selected_country,
    fdi = fdi,
    unilateral_list = unilateral_list
  )
  
  # selection for bilateral
  
  selected_country_from <- reactiveVal(NULL)
  selected_country_to   <- reactiveVal(NULL)
  
  
  bilateral_selection_server(
    id = "bilateral_selection",
    country_list = unilateral_list,
    selected_from = selected_country_from,
    selected_to   = selected_country_to
  )
  
  
  bilateral_flow_server(
    id = "bilateral_flows",
    selected_country_from = selected_country_from,
    selected_country_to = selected_country_to,
    unilateral_list = unilateral_list,
    data = fdi_only,
    data_month = fdi_only_month
  )
  
  
  bilateral_sectors_server(
    id = "bilateral_sectors",
    selected_country_from = selected_country_from,
    selected_country_to = selected_country_to,
    fdi = fdi,
    unilateral_list = unilateral_list
  )
  
  
  # main right panel screen --> output
  
    output$screen <- renderUI({
      tab <- input$active_tab %||% "general"
      
      tagList(
        if (tab == "general") {
          div(class = "top-options",
              div(class = "top-option active", `data-opt` = "total",      "Total"),
              div(class = "top-option",        `data-opt` = "country",    "Country"),
              div(class = "top-option",        `data-opt` = "aggregates", "Aggregates"),
              div(class = "top-option",        `data-opt` = "sector",     "Sector")
          )
        },
        
        if (tab == "unilateral") {
          div(class = "top-options",
              div(class = "top-option active", `data-opt` = "selection",  "Country Selection ->"),
              div(class = "top-option",        `data-opt` = "inflow",     "Inflows"),
              div(class = "top-option",        `data-opt` = "outflow",    "Outflows"),
              div(class = "top-option",        `data-opt` = "net_total",  "Net and Total Flows")
          )
        },
        
        if (tab == "bilateral") {
          div(class = "top-options",
              div(class = "top-option active", `data-opt` = "selection", "Country Selection ->"),
              div(class = "top-option",        `data-opt` = "flow",      "Flows"),
              div(class = "top-option",        `data-opt` = "sectors",      "Sectors")
              
          )
        },
     
        uiOutput("dynamic_content")
      )
    })
    
    
    ## dynamic output that switches based on selection
  
  output$dynamic_content <- renderUI({
    
    tab  <- input$active_tab
    opt  <- input$global_option
    
    if (is.null(tab)) tab <- "general"
    
    if (is.null(opt)) {
      if (tab %in% c("unilateral", "bilateral")) {
        opt <- "selection"
      } else {
        opt <- "total"
      }
    }
    
    
    
    switch(
      tab,
      
      "general" = switch(
        opt,
        
        "total" = div(
          totalFDI_UI("total_fdi_global")
        ),
        
        "country"    = div(
          totalFDI_country_UI("total_fdi_country")
        ),
        
        "aggregates" = div(
          totalFDI_aggregates_UI("total_fdi_aggregates")
        ),
        
        "sector"     = div(
          totalFDI_sector_UI("total_fdi_sector")
        )
        
        ),
      
      "unilateral" = switch (
        opt, 
        
        "selection" = div(
          unilateral_selection_UI("country_selection")
          
        ),
        
        "inflow" = div(
          unilateral_inflow_UI("unilateral_inflow")
        ),
        
        "outflow" = div(
          unilateral_outflow_UI("unilateral_outflow")
        ),
        
        "net_total" = div(
          unilateral_total_net_map_UI("unilateral_flow")
        )
      ),
      
      "bilateral" = switch(
        opt,
        
        "selection" = div(
          bilateral_selection_UI("bilateral_selection")
        ),
        
        "flow" = div(
          bilateral_flows_UI("bilateral_flows")
          
        ),
        
        "sectors" = div(
          bilateral_sectors_UI("bilateral_sectors")
        )
      ),
      
      
      
      
      "about" = about_UI()
      
    )
  })
}

# run app

shinyApp(ui, server)




