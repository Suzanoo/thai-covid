ui <- dashboardPage(
  dashboardHeader(title = 'Covid-19 Pandemic'),
  dashboardSidebar(
    # uiOutput("userpanel"),
    sidebarMenu(style = "position: Scroll; overflow: visible;", id = "sidebarmenu",
                menuItem("World Pandemic", tabName = "iaa", icon = icon("th", lib = "font-awesome")),
                menuItem("Thailand Dashboard", tabName = "cso", icon = icon("tachometer-alt", lib = "font-awesome"),
                         badgeLabel = "new",
                         badgeColor = "green"),
                fluidRow(
                  column(12, offset = 2.5,
                         conditionalPanel(
                           "input.sidebarmenu === 'cso'",
                           # a. FILTERS
                           useShinyjs(),
                           div(id = "form",
                               tags$hr(),
                               selectInput("timeline", "Timeline",
                                           choices = list("Daily", "Weekly", "Monthly"),
                                           bookmarkButton(id = "bookmark1")),
                               selectInput("cases", "Cases",
                                           choices = list( "Total Cases", "Total Deaths", "New Cases", "New Deaths"),
                                           bookmarkButton(id = "bookmark2")),
                               uiOutput(outputId = 'date'),
                               actionButton("button1", "Render", icon = icon("redo")),
                               hr()
                               
                               # sliderInput("date", label = h3("Date"), min = min_date,
                               #             max = max_date, value = max_date)
                               # div(id = "control", style = "opacity: 0.95; z-index: 10;",
                               #     plotlyOutput("plot_xxx", height="250px", width="100%"),
                               #     h6('Source:'),
                               #     h6('Thailand Department of Disease Control:'),
                               #     h6('https://covid19.ddc.moph.go.th')
                               # )
                           )
                         )
                  )
                  
                )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "iaa",
              fluidRow(
                h2("WORLD PANDEMIC"),
              ),
              fluidRow(
                div(class='col-lg-3 col-md-6',
                    valueBoxOutput("world1",width = 12),
                ),
                div(class='col-lg-3 col-md-6',
                    valueBoxOutput("world2",width = 12),
                ),
                div(class='col-lg-3 col-md-6',
                    valueBoxOutput("world3",width = 12),
                ),
                div(class='col-lg-3 col-md-6',
                    valueBoxOutput("world4",width = 12),
                ),
              ),
              fluidRow(
                div(class='col-lg-12', style= 'background-color: grey;',
                    leafletOutput('render_world')
                )
              ),
              h5("Source from Johns Hopkins repo: https://github.com/CSSEGISandData/COVID-19"),
              h5("©2022 highwaynumber12@gmail.com")
              
              ## skip
              # fluidRow(offset = 2.5,
              #   column(12, offset = 2.5, style = "opacity: 0.75; z-index: 10;",
              #          id = "control2", class = "panel panel-default",## z-index modification
              #          plotlyOutput("plot_world", height="250px", width="100%"),
              #          radioButtons(
              #            inputId = "rb", label = "Cases", inline = TRUE,
              #            choiceNames = list("New Cases", "Total Cases", "New Deaths", "Total Deaths"),
              #            choiceValues = list("new_cases", "cases", "new_deaths", "deaths")
              #          )
              #   )
              # ),
              
      ),
      tabItem(tabName = "cso",
              fluidRow(
                h2("THAILAND REPORT"),
              ),
              fluidRow(
                div(class='col-lg-3 col-md-6',
                    valueBoxOutput("count1",width = 12),
                ),
                div(class='col-lg-3 col-md-6',
                    valueBoxOutput("count2",width = 12),
                ),
                div(class='col-lg-3 col-md-6',
                    valueBoxOutput("count3",width = 12),
                ),
                div(class='col-lg-3 col-md-6',
                    valueBoxOutput("count4",width = 12),
                ),
              ),
              fluidRow(
                div(class='col-lg-12', style= 'background-color: grey;',
                    leafletOutput('render_thai')
                )
              ),
              br(),
              br(),
              ## skip 
              # fluidRow(offset = 2.5,
              #          column(12, offset = 2.5, style = "opacity: 0.75; z-index: 10;",
              #                 id = "control", class = "panel panel-default",## z-index modification
              #                 plotlyOutput("plot_xxx", height="250px", width="100%"),
              #          )
              # ),
              
              h5("Source: Thailand Department of Disease Control: https://covid19.ddc.moph.go.th/"),
              h5("©2022 highwaynumber12@gmail.com")
      )
    )
  )
  # bs4DashFooter(
  
  # )
)
