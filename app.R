#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shinydashboard)
require(shiny)
require(highcharter)



#Load data
sna.naics<-read.csv(file='Shiny app/applink/sna_naics.csv')
names(sna.naics)[2]<-"Year"
names(sna.naics)[5]<-"statistics"
names(sna.naics)[6]<-"naics"
names(sna.naics)[13]<-"Jobs"

sna.naics.322<-filter(sna.naics,naics == "Paper manufacturing [322]", statistics == "Total number of jobs",GEO != "Canada",GEO != "Northwest Territories including Nunavut" )
sna.naics.322.df<-as.data.frame(sna.naics.322)

sna.naics.321<-filter(sna.naics,naics == "Wood product manufacturing [321]",statistics== "Total number of jobs",GEO != "Canada",GEO != "Northwest Territories including Nunavut")
sna.naics.321.df<-as.data.frame(sna.naics.321)

ui <- dashboardPage(
    skin="blue",
    dashboardHeader(title = "Natural resource economic dashboard"),
    dashboardSidebar(),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(highchartOutput("hcontainer"),
                width="12"),
            
            box(highchartOutput("hcontainer1"),
                width="12")
        )
    )
)

server <- function(input, output) { 
    output$hcontainer <- renderHighchart ({
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
