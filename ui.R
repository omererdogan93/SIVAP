# https://rstudio.github.io/shinydashboard/get_started.html
library(shinydashboard)
library(DT)


dashboardPage(skin = "yellow",  title = "Sports International Veri Analizi",
  # dashboardHeader(title = tags$a(href='http://mycompanyishere.com', tags$img(src='logo.png'))),
  # dashboardHeader(title = "My Dashboard",
  #                 tags$li(a(href = 'http://shinyapps.company.com', icon("power-off"), title = "Back to Apps Home"), class = "dropdown"),
  #                 tags$li(a(href = 'http://www.company.com', img(src = 'company_logo.png', title = "Company Home", height = "30px"),
  #                           style = "padding-top:10px; padding-bottom:10px;"), class = "dropdown")
  #                 ),
  dashboardHeader(title=
                  tags$a(tags$img(src = 'si.png', title = "SI", height = "40px"),
                            style = "padding-top:5px; padding-bottom:5px;"),
                                  tags$li(a(img(src = 'bilkent.png', title = "Bilkent Üniversitesi", height = "44px"),
                                            style = "padding-top:1px; padding-bottom:0px;"), class = "dropdown")),
  
  dashboardSidebar(
    disable = FALSE,
    sidebarMenu(
      uiOutput("customerUI"),
      menuItem("Dönemler", tabName = "dönem", icon = icon("dashboard")),
      menuItem("Sözleşmeler", icon = icon("th"), tabName = "sözleşme"),
      # menuItem("Sözleşmeler", icon = icon("th"), tabName = "sözleşme",
      #           badgeLabel = "new", badgeColor = "green"),
      menuItem("Kullanım", icon = icon("th"), tabName = "kullanım"),
      menuItem("Arkadaşlar", icon = icon("th"), tabName = "arkadaş"),
      menuItem("Aktivite", icon = icon("th"), tabName = "aktivite")
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dönem",
              h2("Sözleşme Dönemleri"),
              fluidRow(width=12,
                       # column(2,
                       #        box(width = 12, uiOutput("customerUI")),
                       # ),
                       # column(10,
                       box(width=12, plotOutput("plotSözleşme", height="800px"))
                       # box(width = 12, plotOutput("plotSözleşme", height=350))
                       # )
              )
      ),
      
      tabItem(tabName = "sözleşme",
              h2("Sözleşmeler"),
              fluidRow(
                box(width=12, DTOutput("sözleşmeBilgisi"))
              )
      ),
      
      tabItem(tabName = "kullanım",
              h2("Kullanım"),
              fluidRow(
                box(plotOutput("plotKullanım")),
                box(DTOutput("kullanımİstTablosu")),
                box(width=12, DTOutput("kullanımTablosu"))
              )
      ),
      
      tabItem(tabName = "arkadaş",
              h2("Arkadaşlar"),
              fluidRow(
                box(DTOutput("arkadaşTablosu")),
                box(plotOutput("plotArkadaşBeraberKullanım")),
                box(width = 12, DTOutput("ArkadaşSözleşmeleriTablosu"))
              )
      ),

      tabItem(tabName = "aktivite",
              h2("Aktivite"),
              fluidRow(
                box(width=12, DTOutput("aktiviteTablosu"))
              )
      )
      
    )
  )
) 