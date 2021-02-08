#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
rsconnect::setAccountInfo(name='nrdata',
                          token='A26E9600E6A087E54571F3F1602F4F65',
                          secret='BVUeIznLpMf9RkkYoIJHQorkEnNA73sT9+5ka+0/')
library(shinydashboard)
library(shiny)
library(rsconnect)


ui <- dashboardPage(
    skin="blue",
    dashboardHeader(title = "Natural resource economic dashboard"),
    dashboardSidebar(sidebarMenu(
      menuItem("Employment", tabName = "employment", icon = icon("dashboard")),
      menuItem("GDP", tabName = "gdp", icon = icon("dashboard")),
      menuItem("Exports", tabName = "ex", icon = icon("dashboard")),
      menuItem("Rail shipments", tabName = "rail", icon = icon("dashboard")),
      menuItem("Stock prices", tabName = "stock", icon = icon("dashboard")),
      menuItem("Manufacturing", tabName = "mfg", icon = icon("dashboard"))
    )),
    dashboardBody(tabItems(
      # First tab content
      tabItem(tabName = "employment",
              h2("Employment data, natural resource sectors (Canada, various geographies)"),
              fluidRow(
            shinydashboard::box(highchartOutput("hcontainer"),
                width="12"),
            
            shinydashboard::box(highchartOutput("hcontainer1"),
                width="12")
          )
        ),
    # Second tab content
    tabItem(tabName = "gdp",
            h2("Gross domestic product, natural resource sectors (Canada)")
  )
)
)
)
server <- function(input, output) { 
  library(highcharter)
      output$hcontainer <- renderHighchart ({
        sna.naics<-read.csv("sna_naics.csv")
        sna.naics.df<-as.data.frame(sna.naics)
        names(sna.naics.df)[2]<-"Year"
        names(sna.naics.df)[5]<-"stats"
        names(sna.naics.df)[6]<-"naics"
        names(sna.naics.df)[13]<-"Jobs"
        sna.naics.321.df<-dplyr::filter(sna.naics.df,naics == "Wood product manufacturing [321]",stats == "Total number of jobs",GEO != "Canada",GEO != "Northwest Territories including Nunavut")
          hchart(sna.naics.321.df, "column",hcaes(x = Year, y = Jobs, group= GEO),stacking = "normal")  %>%
            hc_exporting(enabled = TRUE) %>% 
            hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                       shared = TRUE, borderWidth = 2) %>%
            hc_colors(c("#ffc0d1", "#fafafa","#ccd2dd", "#4a4a4a","#EFC000FF", "#91b8bd","#9ae5de", "#efe8d1","#0073C2FF", "#336666","#e3120b", "#8abbd0","#244747"))%>%
            hc_title(text="Wood product manufacturing",align="center") %>%
            hc_subtitle(text="Data Source: Labour statistics consistent with the System of National Accounts (SNA), STATCAN #36-10-0489-01",align="center") %>%
            hc_add_theme(hc_theme_elementary())
    })
    
    output$hcontainer1 <- renderHighchart ({
      sna.naics<-read.csv("sna_naics.csv")
      sna.naics.df<-as.data.frame(sna.naics)
      names(sna.naics.df)[2]<-"Year"
      names(sna.naics.df)[5]<-"stats"
      names(sna.naics.df)[6]<-"naics"
      names(sna.naics.df)[13]<-"Jobs"
      sna.naics.322.df<-dplyr::filter(sna.naics.df,naics == "Paper manufacturing [322]",stats == "Total number of jobs",GEO != "Canada",GEO != "Northwest Territories including Nunavut" )
        hchart(sna.naics.322.df, "column",hcaes(x = Year, y = Jobs, group= GEO),stacking = "normal")  %>%
            hc_exporting(enabled = TRUE) %>% 
            hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                       shared = TRUE, borderWidth = 2) %>%
            hc_colors(c("#ffc0d1", "#fafafa","#ccd2dd", "#4a4a4a","#EFC000FF", "#91b8bd","#9ae5de", "#efe8d1","#0073C2FF", "#336666","#e3120b", "#8abbd0","#244747"))%>%
            hc_title(text="Paper manufacturing",align="center") %>%
            hc_subtitle(text="Data Source: Labour statistics consistent with the System of National Accounts (SNA), STATCAN #36-10-0489-01",align="center") %>%
            hc_add_theme(hc_theme_elementary())
    })
 
}

# Run the application 
shinyApp(ui = ui, server = server)
