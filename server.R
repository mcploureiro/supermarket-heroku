
shinyServer (function(input, output) {

    ##########################################################################################################################
    # reactive content
    
    myDataFiltered <- reactive ({
        if ( length ( input$SelectCustomer    ) > 0 ) { myData <- myData %>% filter ( pe_name %in% input$SelectCustomer    ) }
        if ( length ( input$SelectSupermarket ) > 0 ) { myData <- myData %>% filter ( s_name  %in% input$SelectSupermarket ) }
        if ( length ( input$SelectProduct     ) > 0 ) { myData <- myData %>% filter ( pr_name %in% input$SelectProduct     ) }
        return ( myData ) 
    })
    
    mySum <- reactive ({
        mySum <- myDataFiltered () %>% summarise ( mySum = sum (ssb_item_price) ) %>% pull ()
        return ( mySum ) 
    })
    
    mySmartFit <- reactive ({
        return ( input$mySmartFit )
    })
    
    observeEvent(input$btnResetFilters, {
        shinyjs::reset ("SelectCustomer")
        shinyjs::reset ("SelectSupermarket") 
        shinyjs::reset ("SelectProduct") 
    })
    
    ##########################################################################################################################
    # output functions
    
    output$mySum <- renderText({
        return ( mySum () )
    })
    
    output$mySumText <- renderText({
        return ( paste0 ( "Filtered expenses: ", mySum() , " Euro" ) )
    })
    
    output$myPlots <- renderPlot ({
    
        mySum_byPerson <- 
            myData %>%
            select ( pe_name ) %>%
            distinct () %>%
            left_join (
                myDataFiltered () %>% group_by ( pe_name ) %>% summarise ( mySum = sum (ssb_item_price) ),
                by = "pe_name"
            )
        myPlot1 <-
            ggplot ( data=mySum_byPerson ) +
            geom_col ( aes(x=mySum,y=pe_name,fill=pe_name) ) +
            geom_text ( aes(x=mySum,y=pe_name,label=mySum), hjust=0 ) +
            theme_minimal () +
            ggtitle ("Expenses by person") +
            ylab ("") +
            xlab ("SUM of expenses (Euro)") +
            theme ( plot.margin=unit(c(1,2,1,0),"cm"), legend.position="none" ) +
            scale_fill_manual ( values=myFill )

        mySum_bySupermaketPerson <- 
            myData %>%
            select ( s_name, pe_name ) %>%
            distinct () %>%
            left_join (
                myDataFiltered () %>% group_by ( s_name, pe_name ) %>% summarise ( mySum = sum (ssb_item_price) ),
                by = c("s_name","pe_name")
            )
        myPlot2 <-
            ggplot ( data=mySum_bySupermaketPerson ) +
            geom_col ( aes(x=mySum,y=s_name,fill=pe_name), position="stack" ) +
            theme_minimal () +
            ggtitle ("Expenses by supermaket and person") +
            ylab ("") +
            xlab ("SUM of expenses (Euro)") +
            theme ( plot.margin=unit(c(1,2,1,0),"cm"), legend.position="none" ) +
            scale_fill_manual ( values=myFill )
        
        mySum_byProductPerson <- 
            myData %>%
            select ( pr_name, pe_name ) %>%
            distinct () %>%
            left_join (
                myDataFiltered () %>% group_by ( pr_name, pe_name ) %>% summarise ( mySum = sum (ssb_item_price) ),
                by = c("pr_name","pe_name")
            )
        myPlot3 <-
            ggplot ( data=mySum_byProductPerson, aes(x=mySum,y=pr_name,fill=pe_name) ) +
            geom_col () +
            theme_minimal () +
            ggtitle ("Expenses by product and person") +
            ylab ( "" ) +
            xlab ( "SUM of expenses (Euro)" ) +
            theme ( plot.margin=unit(c(1,2,1,0),"cm"), legend.position="none" ) +
            scale_fill_manual ( values=myFill )
            
        myPlot1 <- myPlot1 + scale_x_continuous ( limits=c(0,mySum()), expand=c(0,0) )
        myPlot2 <- myPlot2 + scale_x_continuous ( limits=c(0,mySum()), expand=c(0,0) )
        myPlot3 <- myPlot3 + scale_x_continuous ( limits=c(0,mySum()), expand=c(0,0) )
        
        g <- rbind ( ggplotGrob(myPlot1), ggplotGrob(myPlot2), ggplotGrob(myPlot3) )
        grid::grid.draw ( g )
        
    })
    
    output$myPlotTimePerson <- renderPlot ({
        mySum_byDayPerson <- 
            myDataFiltered () %>% 
            group_by ( ss_date, pe_name ) %>% 
            summarise ( mySum = sum (ssb_item_price) )
        myExpansion <<- 
            data.frame ( ss_date = seq.Date ( as.Date(min(myData$ss_date)), as.Date(max(myData$ss_date)), by="day" ) ) %>%
            left_join ( mySum_byDayPerson %>% mutate ( ss_date = as.Date(ss_date) ), by="ss_date" ) %>%
            within ( mySum[is.na(mySum)] <- 0 ) 
        myPlot4 <-
            ggplot ( data=myExpansion, aes(x=ss_date,y=mySum,fill=pe_name) ) +
            geom_col () +
            theme_minimal () +
            ggtitle ("Expenses by date and person") +
            xlab ("Date of expense") +
            ylab ("SUM of expenses (Euro)") +
            theme (plot.margin=unit(c(1,0,3,0),"cm") ) +
            scale_fill_manual ( values=myFill, name="", na.translate=FALSE )
        myPlot4 <- myPlot4 + scale_y_continuous ( limits=c(0,mySum()), expand=c(0,0) )
        return ( myPlot4 )
    })
    
    output$myDataTable <- renderDataTable ({
        DT::datatable (
            myDataFiltered() %>% select (Person=pe_name, Supermaket=s_name, Product=pr_name, ExpenseDate=ss_date, ExpensePrice=ssb_item_price ) ,
            rownames     = FALSE,
            extensions   = c("Buttons"),
            options      = list (
                lengthMenu = c(5,10,50,100), 
                pageLength = 10, 
                dom        = "lBrtip", #"lfBrtip"
                buttons    = c("copy","excel")
            ) 
        ) 
    })
    
})
