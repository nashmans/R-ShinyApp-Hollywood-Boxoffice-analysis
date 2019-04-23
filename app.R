#
### This program "moves" the ui into the server, thereby allowing the use of
###    the URL "search" protocol to create multi-page web site/app. The advantage is that
###    each page can have its own file (to keep things organized) and only one file is
###    loaded into Shiny Server at a time, limiting memory usage.

### Note, the URL "path" protocol currently doesn't work, because Shiny Server currently
###    returns a "not found" error rather than passing the path to the session.

library(shiny)

# put a message in console or server log; note this happens only when the app is started!
cat("uiStub application started...\n")

ui = uiOutput("uiStub")                                # single-output stub ui

server = function(input, output, session) {
    cat("Session started.\n")                               # this prints when a session starts
    onSessionEnded(function() {cat("Session ended.\n\n")})  # this prints when a session ends
    
    # build menu; same on all pages
    output$uiStub = renderUI(tagList(             # a single-output stub ui basically lets you
        fluidPage(                                  #     move the ui into the server function
            fluidRow(
                column(12,
                       HTML("<h3><a href='?home'>Home</a> | ",
                            "<a href='?movies-main'>Box Office</a> |",
                            "<a href='?movies-two'>Trend:2010s</a>",
                            "</h3>")
                )
            ),
            uiOutput("pageStub")                     # loaded server code should render the
        )                                           #    rest of the page to this output$
    ))
    
    # load server code for page specified in URL
    validFiles = c("home.R",                             # valid files must be hardcoded here
                   "movies-main.R",
                   "movies-two.R")                     #    for security (use all lower-case
    #    names to prevent Unix case problems)
    fname = isolate(session$clientData$url_search)       # isolate() deals with reactive context
    if(nchar(fname)==0) { fname = "?home" }              # blank means home page
    fname = paste0(substr(fname, 2, nchar(fname)), ".R") # remove leading "?", add ".R"
    
    cat(paste0("Session filename: ", fname, ".\n"))      # print the URL for this session
    
    if(!fname %in% validFiles){                          # is that one of our files?
        output$pageStub <- renderUI(tagList(              # 404 if no file with that name
            fluidRow(
                column(5,
                       HTML("<h2>404 Not Found Error:</h2><p>That URL doesn't exist. Use the",
                            "menu above to navigate to the page you were looking for.</p>")
                )
            )
        ))
        return()    # to prevent a "file not found" error on the next line after a 404 error
    }
    source(fname, local=TRUE)                            # load and run server code for this page
}
# Run the application
shinyApp(ui = ui, server = server)
