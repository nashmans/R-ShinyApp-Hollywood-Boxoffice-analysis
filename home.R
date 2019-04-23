output$pageStub <- renderUI(tagList(
  fluidRow(
    column(10,
           HTML(
             "<h4> Welcome to the home page of <b> Hollywood Economics </b> .</h4>"
           ),
           
           
           HTML(
                "<p>*<i>Click the 'Box Office' page for Box Office Revenue insights</p>",
                "<p>  Here you can interact with two plots to study :</p>",
                
                "<p>   _a) Box office Trend by Major Studios vs. Minor over the past 50 yrs</p>",
                "<p>   _b) Biggest players and their market share in U.S. Domestic & Interna'l Markets</p>",
                
                
                "<p>*Click on the 'Trend' page for the latest trend in spending power among hollywood Majors</p>"
                
             
                )
           
    )
