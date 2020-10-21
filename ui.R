
shinyUI (
    fluidPage(
        
        useShinyjs(), 
        
        titlePanel ("Supermarket example using RStudio, Shiny Apps, GitHub, and Heroku"),
    
        sidebarLayout (
            
            sidebarPanel (
            
                width = 2,
                
                wellPanel (
                    
                    selectInput (
                        "SelectCustomer", 
                        label    = "Customer:",
                        choices  = c ("", unique(myData$pe_name) ),
                        selected = NULL,
                        multiple = TRUE
                    ),
                    
                    selectInput (
                        "SelectSupermarket", 
                        label    = "Supermarket:",
                        choices  = c ("", unique(myData$s_name) ),
                        selected = NULL,
                        multiple = TRUE
                    ),
                    
                    selectInput (
                        "SelectProduct", 
                        label    = "Product:",
                        choices  = c ("", unique(myData$pr_name) ),
                        selected = NULL,
                        multiple = TRUE
                    ),
                    
                    actionButton ("btnResetFilters", "Reset filters")
                ),
                
                wellPanel (
                    h5 ( paste0 ("All expenses: ",      sum(myData$ssb_item_price),  " Euro" ) ),
                    h5 ( textOutput("mySumText") )
                ),
                
                wellPanel (
                    h5 ("Optimized for 1920x1080")
                )
                
            ),
            
            mainPanel (
                width=10,
                fluidRow(
                    column ( width=6, plotOutput ( "myPlots", height=900 ) ),
                    column ( 
                        width=5, 
                        plotOutput ( "myPlotTimePerson",  height=500 ),
                        div ( dataTableOutput ( "myDataTable"), style="font-size:70%" )
                    )
                )
            )
            
        )
    )
)
