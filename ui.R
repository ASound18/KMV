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
                      fluidRow(column(width = 3, offset = 0, wellPanel(strong(h2("OHLC Price Data")), uiOutput("radioButtons_OHLC"))),
                               column(width = 9, 
                                      conditionalPanel(condition = "input.radioButtons_OHLC == 'Chart'", plotOutput("chart_OHLC")), 
                                      conditionalPanel(condition = "input.radioButtons_OHLC == 'Data'", htmlOutput("table_OHLC"))
                                      )
                               )
                      ),
             "----",
             # "IS/BS/CF by Q/A",
             tabPanel("Income Statement", 
                      fluidRow(column(width = 3, offset = 0, wellPanel(strong(h2("Income Statement")), uiOutput("radioButtons_IS"))),
                               column(width = 9, (htmlOutput("table_IS")))
                               )
                      ),
             tabPanel("Balance Sheet",
                      fluidRow(column(width = 3, offset = 0, wellPanel(strong(h2("Balance Sheet")), uiOutput("radioButtons_BS"))),
                               column(width = 9, (htmlOutput("table_BS")))
                               )
                      ),
             tabPanel("Cash Flow Statement",
                      fluidRow(column(width = 3, offset = 0, wellPanel(strong(h2("Cash Flow Statement")), uiOutput("radioButtons_CF"))),
                               column(width = 9, (htmlOutput("table_CF")))
                               )
                      )
  ),
  
  
  ### ~~~~~~~~~~~~~~~~~~~ ###
  ###  Tab3: Methodology  ###
  ### ~~~~~~~~~~~~~~~~~~~ ###
  
  tabPanel("Methodology"),
  
  
  ### ~~~~~~~~~~~~~~~ ###
  ###  Tab4: Contact  ###
  ### ~~~~~~~~~~~~~~~ ###
  
  tabPanel("Contact", 
           em("For any support or maintenance, please reach out to "),
           a("Tongchuan Yu.", target="_blank", href=print("https://mysite3.contact.aig.net/Person.aspx?accountname=R1-Core\\tyu"))
           )

  
  
  
))
