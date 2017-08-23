library(shiny)
library(googleVis)


shinyUI(navbarPage(
  
  theme = "bootstrap.css",
  title = strong("Modeling Default Probabilities by KMV"),
  windowTitle = "KMV",
  

  
  ### ~~~~~~~~~~~~~~~ ###
  ###  Tab1: Company  ###
  ### ~~~~~~~~~~~~~~~ ###
  
  tabPanel("Company",
           sidebarPanel(uiOutput("exchange"),
                        uiOutput("company")
                        ),
           mainPanel(fluidRow(column(width = 2, offset = 1,  em(h3("Name:"))),     column(width = 8, h3(textOutput("company_Name")))),
                     fluidRow(column(width = 2, offset = 1,  em(h3("Last Sale:"))),  column(width = 8, h3(textOutput("company_LastSale")))),
                     fluidRow(column(width = 2, offset = 1,  em(h3("Market Cap:"))), column(width = 8, h3(textOutput("company_MarketCap")))),
                     fluidRow(column(width = 2, offset = 1,  em(h3("IPO year:"))),   column(width = 8, h3(textOutput("company_IPOyear")))),
                     fluidRow(column(width = 2, offset = 1,  em(h3("Sector:"))),     column(width = 8, h3(textOutput("company_Sector")))),
                     fluidRow(column(width = 2, offset = 1,  em(h3("Industry:"))),   column(width = 8, h3(textOutput("company_Industry"))))
                     )
           ),
  
  
  ### ~~~~~~~~~~~~~~~~~~~~~~ ###
  ###  Tab2: Financial Data  ###
  ### ~~~~~~~~~~~~~~~~~~~~~~ ###
  
  navbarMenu("Financial Data",
             tabPanel("OHLC Price Data",
                      fluidRow(column(width = 3, offset = 0, wellPanel(strong(h2("OHLC Price Data")), uiOutput("radioButtons_OHLC"), downloadButton('downloadData_OHLC', 'Download'))),
                               column(width = 9, 
                                      conditionalPanel(condition = "input.radioButtons_OHLC == 'Chart'", plotOutput("chart_OHLC")), 
                                      conditionalPanel(condition = "input.radioButtons_OHLC == 'Data'", htmlOutput("table_OHLC"))
                                      )
                               )
                      ),
             "----",
             # "IS/BS/CF by Q/A",
             tabPanel("Income Statement", 
                      fluidRow(column(width = 3, offset = 0, wellPanel(strong(h2("Income Statement")), uiOutput("radioButtons_IS"), downloadButton('downloadData_IS', 'Download'))),
                               column(width = 9, (htmlOutput("table_IS")))
                               )
                      ),
             tabPanel("Balance Sheet",
                      fluidRow(column(width = 3, offset = 0, wellPanel(strong(h2("Balance Sheet")), uiOutput("radioButtons_BS"), downloadButton('downloadData_BS', 'Download'))),
                               column(width = 9, (htmlOutput("table_BS")))
                               )
                      ),
             tabPanel("Cash Flow Statement",
                      fluidRow(column(width = 3, offset = 0, wellPanel(strong(h2("Cash Flow Statement")), uiOutput("radioButtons_CF"), downloadButton('downloadData_CF', 'Download'))),
                               column(width = 9, (htmlOutput("table_CF")))
                               )
                      )
  ),
  
  
  ### ~~~~~~~~~~~~~~~~~~~ ###
  ###  Tab3: Methodology  ###
  ### ~~~~~~~~~~~~~~~~~~~ ###
  
  tabPanel("Methodology",
           navlistPanel(widths = c(2, 10), well = FALSE,
             tabPanel(strong("Step1"),
                      wellPanel(fluidRow(column(12, h3(strong("Estimation of the market value and volatility of the firm's asset."))))),
                      fluidRow(column(12, h5("Consider equity as a call option on the firm's assets with a strike price equal to the book value of the firm's liabilities."))),
                      fluidRow(column(12, h5("According the BSM model, asset value is described by the following process"))),
                      fluidRow(column(6, withMathJax('$$d_A=\\mu Ad_t+\\sigma_AAd_z$$'))),
                      fluidRow(column(6, h5("where"))),
                      fluidRow(column(6, offset=1, withMathJax("\\(A\\) and \\(d_A\\) are the firm's asset value and change in asset value"))),
                      fluidRow(column(6, offset=1, withMathJax("\\(\\mu\\) and \\(\\sigma_A\\) are the firm's asset value drift and volatility"))),
                      fluidRow(column(6, offset=1, withMathJax("\\(d_z\\) is a Wiener process"))),
                      fluidRow(column(6, h5("The solution to BSM equation gives the value of the call option as:"))),
                      fluidRow(column(6, offset=2, withMathJax("\\(E\\)\\(=\\)\\(A\\)\\(N\\)(\\(d_1\\))\\(-\\)\\(F\\)\\(e^{-rt}\\)\\(N\\)(\\(d_2\\))"))),
                      fluidRow(column(6, withMathJax("$$d_1=\\frac{\\ln(\\frac{A}{F})+(r+\\frac{\\sigma_A^2}{2})t}{\\sigma_A\\sqrt t}$$"))),
                      fluidRow(column(6, withMathJax("$$d_2=d_1-\\sigma_A\\sqrt t$$"))),
                      fluidRow(column(6, h5("where"))),
                      fluidRow(column(6, offset=1, withMathJax("\\(F\\) is the face value of the debt due at time \\(t\\)"))),
                      fluidRow(column(6, offset=1, withMathJax("\\(E\\) is the market value of equity"))),
                      fluidRow(column(6, h5("Applying Ito's lemma gives the following."))),
                      fluidRow(column(6, withMathJax("$$\\frac{\\sigma_E}{\\sigma_A}=\\frac{A}{E}N(d_1)$$"))),
                      fluidRow(column(12, h5("Then we can use the two equations above to solve for asset value and asset volatility"))),
                      fluidRow(column(12, h5("For the firm selected (based on the most recent data), "))),
                      fluidRow(column(12, h5(htmlOutput("total_common_shares_outstanding_approx")))),
                      fluidRow(column(12, h5(htmlOutput("default_point")))),
                      fluidRow(column(12, h5(htmlOutput("equity_volatility")))),
                      fluidRow(column(12, uiOutput("rf"))),
                      fluidRow(column(12, plotOutput("equity_vs_asset_plot"))),
                      fluidRow(column(12, h5(htmlOutput("asset_volatility")))),
                      fluidRow(column(12, helpText("Note: Default Point (replaced by 0 when missing) is calculated using Total Current Liabilities + 0.5 * Total Long Term Debt, which is assumed to mature in 1 year.")))
                      ),
             tabPanel(strong("Step2"),
                      wellPanel(fluidRow(column(12, h3(strong("Calculation of the Distance to Default (DD)."))))),
                      fluidRow(column(6, offset=1, withMathJax("$$DD = \\frac{\\ln(\\frac{A}{F}) + (r-\\frac{\\sigma_A^2}{2})}{\\sigma_A\\sqrt t}$$"))),
                      fluidRow(column(12, h5(htmlOutput("DD"))))
                      ),
             tabPanel(strong("Step3"),
                      wellPanel(fluidRow(column(12, h3(strong("Matching the Distnace to Default (DD) to actual probabilities of default.")))))
                      )
             )
           ),
  
  
  ### ~~~~~~~~~~~~~~~ ###
  ###  Tab4: Contact  ###
  ### ~~~~~~~~~~~~~~~ ###
  
  tabPanel("Contact", 
           fluidRow(column(12,
                           em("For any support or maintenance, please reach out to "),
                           a("Tongchuan Yu", target="_blank", href=print("https://mysite3.contact.aig.net/Person.aspx?accountname=R1-Core\\tyu")),
                           em("by"),
                           a("email", href="mailto:tongchuan.yu@aig.com?subject=KMV Tool Support Request"),
                           "."
                           ) 
                    ),
           br(),br(),
           fluidRow(column(12, imageOutput("aig_logo")))
           )

  
  
  
))
