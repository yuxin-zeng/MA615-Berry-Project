library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(dashboardthemes)
library(tidyverse)
library(magrittr)
library(kableExtra)

## read the data
berries=read_csv("berries.csv",col_names=T)

## look at number of unique values in each column
berries%>%summarize_all(n_distinct) -> aa

## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique value column names 
colnames(berries)[bb]

## list the 1-unique single values. Consider if they should be used for labels
single_values<-berries[1,bb]

## remove the 1-unique columns from the dataset
berries%<>%select(-all_of(bb))

##Only keep the State name
berries%<>%select(-4)

##Period="Year"
berries=berries%>%filter(Period=="YEAR")

#Write the dataset 
bberry=berries%>%filter(Commodity=="BLUEBERRIES")
rberry=berries%>%filter(Commodity=="RASPBERRIES")
sberry=berries%>%filter(Commodity=="STRAWBERRIES")



####################################################################BLUEBERRIES
bberry%<>%separate(`Data Item`, c("B","type", "meas", "what"), sep=",") 
bberry%<>%select(-B)

bberry%<>%separate(type,c("b1", "type", "b2", "lab1", "lab2")," ")
bberry%<>%select(-c(b1,b2)) 



## Domain Category
bberry %<>% separate(Domain, c("D_left", "D_right"), sep = ", ")

bberry %<>% separate(`Domain Category`, c("DC_left", "DC_right"), sep = ", ")

bberry %<>% separate(DC_left, c("DC_left_l", "DC_left_r"), sep = ": ")
bberry %<>% separate(DC_right, c("DC_right_l", "DC_right_r"), sep = ": ") 

bberry %<>% select(-DC_left_l) 
bberry %<>% select(-DC_right_l)

bberry %<>% mutate(label = paste(lab1,lab2)) 



## remove "Chemical" and joint the columns
bberry %<>% mutate(D_left = "CHEMICAL", D_left = "") 
bberry %<>% mutate(Chemical=paste(D_left, D_right)) 
bberry %<>% select(-c(D_left, D_right)) 

bberry %<>% select(Year, State, type, what, meas, label, DC_left_r, DC_right_r, Chemical, Value )
bberry[is.na(bberry)] <- " "

#We have entries in both the "what" and "meas" columns that begin  "MEASURED IN"
f1 <- function(a,b){
    if(a){
        return(b)
    }else{
        return("")
    }
}

f1_log <- c(FALSE, TRUE, TRUE)
f1_str <- c("one", "two", "three")
map2(f1_log, f1_str, f1)

index_meas <- str_detect(bberry$meas, "MEASURED IN")
bberry %<>% mutate(m_in_1 = unlist(map2(index_meas, bberry$meas, f1))) 
bberry %<>% mutate(meas = str_replace(bberry$meas, "MEASURED IN.*$", ""))

##do the same thing with the "what" column  
index_what <- str_detect(bberry$what, "MEASURED IN")
bberry %<>% mutate(m_in_2 = unlist(map2(index_what, bberry$what, f1))) 
bberry %<>% mutate(what = str_replace(bberry$what, "MEASURED IN.*$", ""))

bberry %<>% mutate(units = str_trim(paste(m_in_1, m_in_2))) 
bberry$units %>% unique()

index_units <- str_detect(bberry$what, "MEASURED IN")
bberry %<>% mutate(m_in_2 = unlist(map2(index_what, bberry$what, f1))) 
bberry %<>% mutate(what = str_replace(bberry$what, "MEASURED IN.*$", ""))


###################################################################clean it up 
bberry$what %>% unique()  ## rename Avg
bberry$meas %>% unique()  ## rename marketing
bberry$label %>% unique() ## rename harvest 
bberry$DC_left_r %>% unique() # rename chemical_family
tmp <- bberry$DC_right_r %>% unique() # rename materials --213
tmp <- bberry$Value %>% unique() # values
tmp <- bberry$units %>% unique() # Measures

bberry %<>% rename(Avg = what)
bberry %<>% rename(Marketing = meas, Harvest = label, Chem_family = DC_left_r, Materials = DC_right_r, Measures = units)


bberry %<>% mutate(production = str_trim(paste(Marketing, Harvest)))
bberry %<>% mutate(Chemical = str_trim(paste(Chem_family, Chemical)))


bberry %<>% select(Year, State, type, production, Avg, Measures, Materials, Chemical, Value)

##remove uneaningful value
bberry %<>% filter(Value != "(D)")
bberry%<>% filter(Value !=  "(NA)")


#eda
options(scipen = 200)
bberry$Value=as.numeric(gsub(",","",bberry$Value))
dim(bberry) 
summary(bberry)
p=qplot(x=Value,data=bberry,ylab = 'frequency')

#State
library(ggplot2)
p1 <- ggplot(bberry,aes(x=Year,y=Value,fill=State))+geom_bar(position="dodge",stat="identity")+xlab("year") + ylab("value") + labs(fill="State")

#type
p2 <- ggplot(bberry,aes(x=Year,y=Value,fill=type))+geom_bar(position="dodge",stat="identity")+xlab("year") + ylab("value") + labs(fill="type")

#production
p3 <- ggplot(bberry,aes(x=Year,y=Value,fill=production))+geom_bar(position="dodge",stat="identity")+xlab("year") + ylab("value") + labs(fill="production")



ui <- dashboardPage(
    dashboardHeader(title = "berries"),
    
    dashboardSidebar(  
        sidebarMenu(
            menuItem("data", tabName = "Plot1", icon = icon("dashboard")),
            menuItem("Plot", tabName = "Plot2", icon = icon("dashboard")))),
    dashboardBody(
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
        tabItems(
            tabItem(tabName = "Plot1",
                    DTOutput("table1")),
            tabItem(tabName = "Plot2",
                    box(plotOutput("plot1")),
                    box(plotOutput("plot2")),
                    box(plotOutput("plot3")),
                    box(plotOutput("plot4"))
                    
            ))
    )
)


server <- function(input, output) {
    output$plot1 <- renderPlot({
        p
    })
    output$table1 <-renderDT({
        datatable(bberry)
    })
    output$plot2 <- renderPlot({
        p1
    })
    output$plot3 <-renderPlot({
        p2
    })
    output$plot4 <-renderPlot({
        p3
    })
}
shinyApp(ui, server)
