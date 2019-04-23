output$pageStub <- renderUI(tagList(
  fluidRow(
    column(5,
           HTML("<p>This is the home page of my workspace on Economcis of Hollywood.</p>",
                
                "<p>The second page provide you with insights Box Office Revenue.",
                " Here you can interact with two plots to study :",
                "a) Revenue Trend by Major Studios vs. Minor over the past 50 yrs",
                "b) Biggest players and market share in U.S. Domestic & Interna'l Markets",
                
                
                "<p>The third link goes to a page that doesn't exist to demonstrate",
                "error handling for bad URLs.</p>")
    )
  )
))