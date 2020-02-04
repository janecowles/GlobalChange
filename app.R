library(shiny)
library(ggplot2)
library(grid)
library(RColorBrewer)
library(shinydashboard)

#### UI!


ui <- dashboardBody(

    tags$head(
      tags$style(
        "body{
    min-height: 611px;
    height: auto;
    max-width: 800px;
    margin: auto;
        }"
      )
    ),fluidPage(
    titlePanel(h1("How does warming impact productivity?",align="center"),windowTitle = "Global Change"),
  fluidRow(column(4,align="center",
    tags$i("Intervention:"),br(),
    tags$b("Warming via heat lamps"),style='margin-bottom:30px;border-right:1px solid; padding: 10px;color:grey'),
column(4,align="center",
    tags$i("Response of interest:"),br(),
    tags$b("Plant biomass production"),style='margin-bottom:30px;border-right:1px solid; padding: 10px;color:grey'),
column(4,align="center",
  tags$i("Existing gradient:"),br(),
    tags$b("Plant species diversity"),style='margin-bottom:30px;border:0px solid; padding: 10px;color:grey')),
  fluidRow(column(12,align="center",imageOutput("plotpic",height = "300px"))),
  # h6("Response of interest: Plant biomass production  //  Intervention: Warming via heat lamps  //  Existing gradient: Plant species diversity"),
  fluidRow(column(3,align="center",
    h3("Treatment selection"),
      checkboxGroupInput("checkGroup", h5("Global change Treatment"),
        choices = list("No warming" = 1, "Low warming" = 2,"High warming" = 3),selected = 1),
      radioButtons("picture", h5("Diversity"),
        choices = list("1 species"=1,"4 species"=4, "16 species"=16),selected = 1) ),
    column(9,align="center",plotOutput("distPlot"))),
  hr(),
  h4("The impact of warming depends on the context (i.e. plant community).",align="center"),
  h5("The most diverse plots show the largest growth response to warming.",align="center"),
  br(),
  fluidRow(
    column(3,
      h5("Here's why:",align="center"),
      # textOutput("divcomment")),
    span(textOutput("divcomment"), style="color:grey"),
      # br(),
    span(textOutput("warmingcomment"), style="color:black")),
      # p("Biomass x warming = amelioration of negative impacts.")),
    column(4,align="center", imageOutput("image")),
    column(5,align="center", plotOutput("DryPlot"))
    ),
    # span(textOutput("summary"),style="color:firebrick",align="center"),

  hr(),
  fluidRow(column(2,align="center",
    span(h6("NSF Funded Research at Cedar Creek Ecosystem Science Reserve"),style="color:black"),
      span(h6("Shiny app (c) JCD"),style="color:grey"),
),
    column(1,align="center",
      # span(h6("(c) JCD 2020"),style="color:grey"),
      ),
    column(6,align="center",
    helpText(a("Click here for related peer reviewed paper!", href="https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.13111", target="_blank")),
    h6("---"),
        helpText(a("[See code on Github]", href="https://github.com/janecowles/GlobalChange/blob/master/app.R", target="_blank")),
br()),
    
    column(1,align="center",
      # span(h6("(c) JCD 2020"),style="color:grey"),
      ),
    column(2,align="center",
    imageOutput("logo",height="50px")),
    ))
)
# Define server logic required to draw a histogram
server <- function(input, output) {
bac <- read.csv("data/BAC_df.csv")
  # bac <- data.frame(HeatNum=c(0,1,2),HeatOrd = c("Control","Low","High"),Diversity = c(1,4,16),Prductivity=c(4,6,9))
bac$HeatOrd <- factor(bac$HeatOrd,levels=c("Control","Low","High"))
    output$distPlot <- renderPlot({
        inpic <- as.numeric(input$picture)
        if(nrow(bac[bac$HeatNum%in%input$checkGroup,])==0){
       ggplot(bac,aes(Diversity,Productivity,group=factor(HeatOrd,levels=c("Control","Low","High")),color=factor(HeatOrd,levels=c("Control","Low","High"))))+geom_point(color="white")+scale_color_manual(name="warming",values=c("royalblue","orange","firebrick"),drop=F)+coord_cartesian(ylim=c(0,600),xlim=c(0,17))+theme_classic()+labs(x="Diversity (# of Species)",y="Plant Biomass Production")+annotate(geom="text",x=6,y=400,label="Choose at least one warming treatment!",color="firebrick",size=4)+theme(axis.title=element_text(size=14,face="bold"))
        }else
       ggplot(bac[bac$HeatNum%in%input$checkGroup,],aes(Diversity,Productivity,group=factor(HeatOrd,levels=c("Control","Low","High")),color=factor(HeatOrd,levels=c("Control","Low","High"))))+geom_rect(mapping=aes(xmin=inpic-1, xmax=inpic+1, ymin=-100, ymax=700), fill="gray80",color="grey", alpha=0.1)+geom_jitter(alpha=0.2,width=0.1)+geom_line(stat="summary",fun.y="mean",size=2)+scale_color_manual(name="warming",values=c("royalblue","orange","firebrick"),drop=F)+coord_cartesian(ylim=c(0,600),xlim=c(0,17))+theme_classic()+labs(x="Diversity (# of Species)",y="Plant Biomass Production") + theme(axis.title=element_text(size=14,face="bold"))
},width=500)
    output$logo <- renderImage({
      return(list(
        src = "images/cdr.jpg",
        contentType = "image/jpeg",
        alt = " "
      ))
    },deleteFile = FALSE)
        output$plotpic <- renderImage({
      return(list(
        src = "images/plotpic.jpg",
        contentType = "image/jpeg",
        alt = " "
      ))
    },deleteFile = FALSE)
          output$image <- renderImage({
    if (is.null(input$picture))
      return(NULL)

    if (input$picture == 1) {
      return(list(
        src = "images/pic1sp.jpg",
        contentType = "image/jpeg",
        alt = "A one species community has low biomass and a lot of bare ground exposed"
      ))
    } else if (input$picture == 4) {
      return(list(
        src = "images/pic4sp.jpg",
        filetype = "image/jpeg",
        alt = "A 4 species community is in the middle - a fair bit of biomass and a little bare ground showing"
      ))
    } else if (input$picture == 16) {
      return(list(
        src = "images/pic16sp.jpg",
        filetype = "image/jpeg",
        alt = "A sixteen species community has a lot of biomass and little to no bare ground showing"
      ))}
  }, deleteFile = FALSE)
    output$DryPlot <- renderPlot({
      
      make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(data = rep(seq(0, 1, length.out = n) * cos(rad), n),byrow = TRUE,ncol = n) + matrix(data = rep(seq(0, 1, length.out = n) * sin(rad), n),byrow = FALSE,ncol = n)
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(image = mat, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
}
      g <- make_gradient(deg = 270, n = 500, cols = brewer.pal(9, "Reds"))

        inpic <- as.numeric(input$picture)
        
        bacsub <- bac[bac$Diversity==inpic&bac$HeatNum%in%input$checkGroup,]
        
ggplot(bac[bac$HeatNum%in%input$checkGroup,],aes(Productivity,vpdgrowing2014.neg10,group=factor(HeatOrd,levels=c("Control","Low","High")),color=factor(HeatOrd,levels=c("Control","Low","High"))))+ annotation_custom(grob = g, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)+annotate("rect", xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.8, fill="white") +geom_point(shape=4,alpha=0.3) +geom_point(data=bacsub,aes(color=factor(HeatOrd,levels=c("Control","Low","High"))))+stat_ellipse(data=bacsub,aes(fill=factor(HeatOrd,levels=c("Control","Low","High"))),geom="polygon",alpha=0.1)+scale_color_manual(name="warming",values=c("royalblue","orange","firebrick"),drop=F)+scale_fill_manual(name="warming",values=c("royalblue","orange","firebrick"),drop=F)+theme_classic()+labs(x="Plant Biomass Production",y="\"Dryness\" (Vapor Pressure Deficit)")+coord_cartesian(xlim=c(0,1100),ylim=c(0,12))+theme(legend.position = "top",axis.title=element_text(size=14,face="bold"))


},width=300)
    
 output$divcomment <- renderText({
if(input$picture==1){"- These low diversity communities have low biomass, as evidenced by the exposed soil in the picture..."
}else if(input$picture==4){"- These 4 species communities have more biomass than low diversity communities but less than the high diversity plots..."
}else if(input$picture==16){"- The highest diversity communities have the highest levels of biomass (no ground can be seen in the picture), which creates its own microclimate below the canopy..."}})
 
    output$warmingcomment <- renderText({
ifelse(input$picture==1&(is.element(2,input$checkGroup)|is.element(3,input$checkGroup)),"...making the community more susceptible to drying caused by warming, which is bad for growth.",ifelse(input$picture==4&(is.element(2,input$checkGroup)|is.element(3,input$checkGroup)),"...This puts their response to warming somewhere between 1 species communities and 16 species communities.",ifelse(input$picture==16&(is.element(2,input$checkGroup)|is.element(3,input$checkGroup)),"...This canopy buffers the negative impacts of warming, while containing the largest growth potential.","- Click an additional warming treatment for more information!"))) }) 
    output$summary <- renderText({
ifelse(input$picture==1&(is.element(2,input$checkGroup)|is.element(3,input$checkGroup)),"Low diversity -> Low biomass -> exposed soil -> drying -> less growth",ifelse(input$picture==4&(is.element(2,input$checkGroup)|is.element(3,input$checkGroup)),"Medium diversity -> Decent plant cover -> Medium response (balance of positives and negatives ",ifelse(input$picture==16&(is.element(2,input$checkGroup)|is.element(3,input$checkGroup)),"High diversity -> High cover -> Limited drying & warm happy plants -> Biggest positive effect of warming"," ")))})



}

# Run the application 
shinyApp(ui = ui, server = server)
