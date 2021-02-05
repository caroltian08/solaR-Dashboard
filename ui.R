mytheme <- create_theme(
  theme = "default",

  # Custome ui background and text color
  bs_vars_global(
    body_bg = "#f6f6f6",
    text_color = "#000000",
    border_radius_base = 2,
    grid_columns = 12
  ),

  # Custome primary text color
  bs_vars_color(
    brand_primary = "#133b69",
    brand_info = "#000000"
  ),

  bs_vars_modal(
    md = "80%"
  ),

  # Custome Input text effect
  bs_vars_input(
    bg = "#f8f8f8",
    color = "#545454",
    border = "#ebebeb",
    border_radius = "5px",
    border_focus = "#99dbbd"
  ),

  # Custom navbar
  bs_vars_navbar(
    height = "32px",
    default_bg = "#00A08A",
    default_color = "#ffffff",
    default_border = "#00A08A",
    default_link_color = "#ffffff",
    default_link_active_bg = "#006052",
    default_link_active_color = "#ffffff"
  ),

  # tabPanel()
  bs_vars_tabs(
    border_color = "#d3d3d3",
    link_hover_border_color = "#d3d3d3",
    active_link_hover_bg = "#d3d3d3",
    active_link_hover_color = "#006052"
    ),

  # Custom default button
  bs_vars_button(
    font_weight = "bold",
    default_bg = "#e74c3c",
    default_border = "#e74c3c",
    default_color = "#FFF",
    border_radius_large = TRUE
  ),

  bs_vars_font(
    family_sans_serif = "'Roboto'",
    size_base = "14px",
    size_large = "16px",
    size_small = "8px"
  ),

  bs_vars_table(
    cell_padding = NULL,
    condensed_cell_padding = NULL,
    bg = "#eaeaea",
    bg_accent = "#dddddd",
    bg_active = "#dddddd",
    border_color = NULL),

  # wellPanel()
  bs_vars_wells(
    bg = "#ffffff",
    border = "#ffffff")
)

fluidPage(

  useShinydashboard(),

  tags$head(HTML("<title>solaR App</title>")),

  tags$style(
    HTML(
      "
        #NPP {font-family:Roboto;font-size: 14px;}
        #angle {font-family:Roboto;font-size: 14px;}
        #mSTC {font-family:Roboto;font-size: 14px;}
        #specs {font-family:Roboto;font-size: 14px;}
        #sum {font-family:Roboto;font-size: 12px;}
        #compareTable {font-family:Roboto;font-size: 14px;}
        #specs2 {font-family:Roboto;font-size: 14px;}
      "
    )
  ),


  navbarPage(title = h4("solaR App", style = "font-size: 20px; font-weight: bold; font-family:'Roboto'"), id = "solaR",
             use_theme(mytheme),

             selected = "setup",

             tabPanel(value = "setup",
                      title = h4("Modeling Setup",style = "font-size: 15px; font-family:'Roboto'"),
                      fluidPage(
                        fluidRow(

                          column(3,
                                 leafletOutput("map"),

                                 hr(),

                                 p("Instructions", align = "center",
                                   style = "font-size: 18px; font-weight: bold; font-family:'Roboto';color:#e74c3c"),
                                 p("This tool is based on solaR Package developed by", tags$em("Oscar Perpinan Lamigueiro"),
                                   tags$a(href="https://www.jstatsoft.org/article/view/v050i09", "link"),
                                   align = "left", style = "font-size: 15px; font-family:'Roboto';"),
                                 p("1. Input PV parameters to the panels on the right side of this page, all the request items are labeled by *",
                                   align = "left", style = "font-size: 15px; font-family:'Roboto'"),
                                 p('2. Select Meteo Data Source, and click "Sumbmit Parameters" button below to initiate simulation session',
                                   align = "left",style = "font-size: 15px; font-family:'Roboto'"),
                                 p('3. Click Modeling Result pane on the navigation bar to check modeling reults',
                                   align = "left", style = "font-size: 15px; font-family:'Roboto'"),
                                 p('4. Dashboar pane on the navigation bar is a Demo page for companies to monitor the performances thier solar site',
                                   align = "left", style = "font-size: 15px; font-family:'Roboto'"),

                                 hr(),

                                 wellPanel(
                                   selectInput("meteo", "Select Meteo Data Source",
                                               choices = c("NSRDB_TMY", "PVGIS_TMY"),
                                               selected = "NSRDB_TMY"),

                                   fluidRow(align = 'center',
                                     actionButton("submit", "Submit Parameters",
                                                  class = "btn-default")))
                          ),

                          column(3,
                                 wellPanel(
                                   p("Bacis Parameters", align = 'center',
                                     style = "font-size: 15px; color:#006052; font-weight:bold; font-family:'Roboto'"),
                                   numericInput("lat", "Latitude*", value=37.200, min=-90.0, max=90.0),
                                   numericInput("long", "Longitude*", value=-80.000, min=-180.0, max=180.0),
                                   checkboxInput("calc", "Generate Nameplate Peak Power-NPP(Kwp ac) using the poped box"),
                                   conditionalPanel(condition = "input.calc == 0",
                                                    numericInput("nominal", "Installed Nameplate Peak Power(KWp ac)*", value = 26)),
                                   conditionalPanel(condition = "input.calc == 1",
                                                    p("Nameplate Peak Power-NPP = mSTC * Nmp * Nms * 0.001",
                                                      style = "font-family:'Roboto';color:#006052;font-style:italic;font-size: 12px"),
                                                    verbatimTextOutput("NPP")),
                                   numericInput("effSTC", "Efficiency at STC (%)", value = 10),
                                   radioButtons("modTrek", "Module Mount Type*",
                                                choices = list("Fixed Mount" = "fixed",
                                                               "Tracker-one axis" = "horiz",
                                                               "Tracker-two axis" = "two"), selected = "fixed"),
                                   conditionalPanel(condition = "input.modTrek == 'fixed'",
                                                    uiOutput("angle"),
                                                    numericInput("alfa", "Azimuth angle", value = 0, min = 0, max = 90)),
                                   conditionalPanel(condition = "input.modTrek != 'fixed'",
                                                    numericInput('betaLim', label = "Maximum inclination angle", value = 90)),
                                   numericInput("iS", "Degree of Dirtiness(1:Cleanest)", value = 2, min = 1, max = 4),
                                   numericInput("alb", "Albedo Reflection Coefficient", value = 0.2, min = 0, max = 0.5))
                                 ),

                                 column(3,
                                        wellPanel(
                                          p("NPP-related Module Parameters", align = "center",
                                            style = "font-size: 15px; font-weight: bold;color:#006052; font-family:'Roboto'"),
                                          conditionalPanel(condition = "input.calc == 1",
                                                           numericInput("Vmn", "STC Maximum Power Point Voltage-Vmn (V)", value = 46.08),
                                                           numericInput("Imn", "STC Maximum Power Current-Imn (I)", value = 4.35),
                                                           p("Module STC Rating-mSTC = Vmn * Imn",
                                                             style = "font-family:'Roboto';color:#006052;font-style:italic;font-size: 12px"),
                                                           verbatimTextOutput("mSTC"),
                                                           numericInput("Nms", "Number of Modules in Series-Nms", value = 12),
                                                           numericInput("Nmp", "Number of Modules in parallel-Nmp", value = 11))
                                          ),

                                        wellPanel(
                                          checkboxInput("showModule",
                                                        label = p("Show Other Module Parameters",
                                                                  style = "font-size: 15px; font-weight: bold;color:#006052; font-family:'Roboto'")),
                                          conditionalPanel(condition = "input.showModule == 1",
                                                           numericInput('Vocn', "STC Open Circuit Voltage (V)", value = 57.6),
                                                           numericInput("Iscn", "STC Short Circuit Current (I)", value = 4.7),
                                                           numericInput("Ncs", "Number of Cells in Series", value = 96),
                                                           numericInput("Ncp", "Number of Cells in Parallel", value = 1),
                                                           numericInput("CoefVT", "Coefficient of Decrement of Cell Voltage with Temperature(V/c)",
                                                                        value = 0.0023),
                                                           numericInput("TONC", "Nominal Operation Cell Temperature (C)", value = 47))
                                          )
                                        ),

                                 column(3,
                                        wellPanel(
                                          checkboxInput("showGI",
                                                        label = p("Show Inverter and System Loss Parameters",
                                                                  style = "font-size: 15px; font-weight: bold;color:#006052; font-family:'Roboto'")),

                                          conditionalPanel(condition = "input.showGI == 1",
                                                           textInput("Ki", "Coefficients of the efficiency curve (Enter a Vector, Comma Delimited",
                                                                     value = "0.01, 0.025, 0.05"),
                                                           numericInput("Pinv", "Nominal Inverter Power (W)", value = 25000),
                                                           numericInput("Vmax", "Maximum Voltage of the MPP Range (V)", value = 750),
                                                           numericInput("Vmin", "Minimum Voltage of the MPP Range (V)", value = 300),
                                                           numericInput("Gumb", "Minimum Irradiance for the Inverter to Start (W/m2)", value = 20),
                                                           numericInput("ModQual", "Average Tolerance of the Modules (%)", value = 3),
                                                           numericInput("ModDisp", "Module Parameter Disperssion Losses (%)", value = 2),
                                                           numericInput("OhmDC", "Joule Losses due to the DC Wiring (%)", value = 1.5),
                                                           numericInput("OhmAC", "Joule Losses due to the AC wiring (%)", value = 1.5),
                                                           numericInput("MPP", "Average error of the MPP algorithm (%)", value = 1))
                                          )
                                        )
                          ),
                        hr(),
                        )
             ),

             tabPanel(value = "result",
                      title = h4("Modeling Result", style = "font-size: 16px; font-family:'Roboto'"),
                      fluidRow(
                        tabsetPanel(id = "Modeling_Result",
                                    tabPanel("Monthly Performances",
                                             br(),
                                             fluidRow(
                                               column(3, align = 'center',
                                                      wellPanel(
                                                                br(),
                                                                br(),
                                                                p("Specifications Summary", align = "center",
                                                                  style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                                                br(),
                                                                tableOutput("specs") %>% withSpinner(color="#00A08A"),
                                                                p("*Inclination Angle only valid when Mode Trek is Fixed",
                                                                  style = "font-family:'Roboto';font-style:italic; font-size: 14px")
                                                                )
                                                      ),

                                               column(4, align = 'center',
                                                      selectInput("baseplot",
                                                                  label = p("Select Base Plot type",align = "center",
                                                                            style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                                                  choices = list("Monthly Total Energy Output(kWh)" = "Eac_Grid",
                                                                                 "Monthly in-plane Irradiation(kW/m2)" = "GlobInc",
                                                                                 "Array Yield" = "Ya","Final Yield" = "Yf",
                                                                                 "Reference Yield" = "Yr","Performance Ratio(%)" = "PR",
                                                                                 "CUF(%)" = "CUF"),
                                                                  selected = "Eac"),
                                                      plotOutput("pvOutput") %>% withSpinner(color="#00A08A")),


                                               column(5,
                                                      br(),
                                                      p("Normalized Energy Plot", align = "center",
                                                      style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                                      br(),
                                                      br(),
                                                      plotOutput("NormEnergy") %>% withSpinner(color="#00A08A"))

                                               ),

                                            hr(),

                                            fluidRow(
                                              column(11, offset = 0.5, align = 'center',
                                                     p("Balances and Main Results", align = "center",
                                                       style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                                     tableOutput("sum") %>% withSpinner(color="#00A08A"))

                                            )
                                   ),

                                   tabPanel("Time Series Modeling",
                                            br(),
                                            fluidRow(
                                              column(4,
                                                     p("Sun Path Plot", align = "center",
                                                       style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                                     p("Azimuth and height solar angles during the 'average days'",
                                                       align = "center", style = "font-family:'Roboto';font-size: 13px"),

                                                     plotOutput("sunPath") %>% withSpinner(color="#00A08A")),
                                              column(8,
                                                     p("TMY Meteo and Energy Output Time Series Plot", align = "center",
                                                       style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                                     plotlyOutput("TMYMeteo") %>% withSpinner(color="#00A08A")))
                                   ),

                                   tabPanel("Comparison with PVWatts",
                                            br(),
                                            sidebarLayout(
                                              sidebarPanel(
                                                selectInput("item_pvwatts", "Selected Items from the Modeling Result (Monthly)",
                                                            choices = list("POA_GlobInc",
                                                                           "Edc_Array",
                                                                           "Eac_Grid"),
                                                            selected = "Eac_Grid"),
                                                tableOutput("compareTable") %>% withSpinner(color="#00A08A")

                                              ),
                                              mainPanel(
                                                p("Compare solaR Silumation result with PVWatts(NREL)", align = "center",
                                                  style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                                plotOutput("plot_compare") %>% withSpinner(color="#00A08A")
                                              )))
                        )
                      ),
                      hr()),

             tabPanel(value = "dashboard", title = h4("Site Dashboard", style = "font-size: 16px; font-family:'Roboto'"),

                      fluidPage(
                        fluidRow(
                          column(width = 3,
                                 p("E.W. Brown Solar Station Dashboard", align = "center",
                                   style = "font-size: 18px; font-weight: bold; color:#006052; font-family:'Roboto'"),
                                 leafletOutput("map_site")
                          ),


                          column(width = 3, offset=0.5,
                                 fluidRow(
                                   valueBoxOutput("box1", width = NULL),
                                   valueBoxOutput("box2", width = NULL)),

                                 fluidRow(align = 'center',
                                          tableOutput("specs2"))
                          ),

                          column(width = 5, offset=0.5,
                                 p("Montly Energy Output (MWh ac) and comparison with solaR Simulation ", align = "center",
                                   style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                 br(),
                                 plotOutput("monthly") %>% withSpinner(color="#00A08A"))
                        ),

                        hr(),

                        fluidRow(
                          column(4,
                                 p("Dashboard Introduction", align = "center",
                                   style = "font-size: 18px; font-weight: bold; color:#e74c3c; font-family:'Roboto'"),
                                 p("1. This Demo Dashboard is designed for companies to monitor the performances of their solar sites"),
                                 p("2. The Demo Site is E.W. Brown Station, real-time field data is achieved from",
                                   tags$a(href = "https://lge-ku.com/our-company/community/neighbor-neighbor/ew-brown-generating-station", "Company Website")),
                                 p("3. The Monthly Energy Output was calculated using field-record Meteo data (G0 and Ta)"),
                                 p("4. The Predication of Three-day-ahead Meteo data was obtained from",
                                   tags$a(href = "https://solcast.com/solar-data-api/", "Solcast"),
                                   "; the energy output predication is calculated using Solcast meteo data and solaR"),
                                 img(src = "Brown.jpg", height="100%", width="100%", align = 'center'),
                                 p("Photo of E.W. Brown Station", align = 'center',
                                   style = "font-size: 12px; font-family:'Roboto'")),


                          column(7,
                                 p("Three-day-ahead Meteo and Energy Output Predication", align = "center",
                                   style = "font-family:'Roboto';font-weight:bold;font-size: 15px"),
                                 selectInput('plot_type', label = "Choose Plot Type",
                                             choices = c("Site Meteo and Predication" = "m",
                                                         "Energy Output and Predcation" = "e"),
                                             selected = "m"),
                                 plotlyOutput("predication") %>% withSpinner(color="#00A08A"))
                        ),

                        hr(),
                        )
                      ),

             footer = p("-- Developed by Jinghan Tian", align = "center",
                        tags$a(herf = "https://www.linkedin.com/in/jinghan-tian/", "LinkedIn"),
                        " --",
                        style = "font-family:'Roboto';font-style:italic;font-size: 14px;")

             )

  )









