 regression_plot <- function(data,
                            x_var,
                            y_var,
                            Choice = "Plot",
                            Shift = 0,
                            LineCheck = FALSE,
                            Interactive = FALSE) {
  require(ggplot2)
  require(ggpubr)
  require(plotly)
  require(stringr)
  cat("This Function has 4 outputs that it can create:","\n",
      "1) The regression plot with the equation","\n",
      "2) The regression summary with coefficients and standard errors","\n",
      "3) The confidence intervals for the coefficients","\n",
      "4) The metrics to evaluate the regression including RMSE and R2","\n",
      "Change the argument choice to plot, regression output, confidence intervals or metrics to change this.")
  Choice <- tolower(Choice)
  if(startsWith(Choice,"p")){Choice <- "Plot"}
  if(startsWith(Choice,"r")){Choice <- "Regression Output"}
  if(startsWith(Choice,"c")){Choice <- "Confidence Intervals"}
  if(startsWith(Choice,"m")){Choice <- "Metrics"}
  if(startsWith(Choice,"i")){Choice <- "Interactive"}
  
  
  M <- lm(paste(y_var, "~", x_var), data = data)
  COEF <- round(coef(M),2)
  VARIABLES <- str_split(as.character(formula(M)),pattern = "~")
  library(stringr)
  EQUAT <- paste0(VARIABLES[[2]]," = ",COEF[[1]]," + ",COEF[[2]],VARIABLES[[2]])

  X_Lab_Pos <-   (0.90-Shift)* max(data[[x_var]]) 
  Y_Lab_Pos <-   (0.98-Shift)* max(data[[y_var]])

  OffSet <- 0.10* ((max(data[[y_var]]) - (min(data[[y_var]]))))
  
  PLOT <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point() +
    geom_smooth(method = if(LineCheck == FALSE){"lm"}else{"loess"}) +
    annotate("text", 
             x = X_Lab_Pos,
             y = Y_Lab_Pos,
             label = EQUAT) +
    annotate("text",
             x = X_Lab_Pos,
             y = Y_Lab_Pos - OffSet,
             label = paste0("R^2 = ", round(summary(lm(paste(y_var, "~", x_var), data=data))$r.squared,3))) +
    labs(title = paste0(VARIABLES[[2]]," by ", VARIABLES[[3]]),
         y = y_var,
         x = x_var) +
    easy_center_title()
  
  OUTPUT <- summary(M)
  
  DF_1 <- round(OUTPUT$coefficients,2)
  
  REGOUTPUT <- ggtexttable(DF_1)
  
  DF_2 <- round(confint(M,level=0.95),2)
  
  CONIFINT <- ggtexttable(DF_2)
  
  
  DF_3<-data.frame(R2=OUTPUT$r.squared,RMSE=sqrt(mean(OUTPUT$residuals^2)))
  
  DF_3<-round(DF_3,3)
  
  row.names(DF_3)<-"Metrics"
  
  METRICS <- ggtexttable(DF_3)
  
  if(Interactive){return(ggplotly(PLOT))}
  
  
  if(Choice == "Plot"){return(PLOT)}
  if(Choice == "Regression Output"){return(REGOUTPUT)}
  if(Choice == "Confidence Intervals"){return(CONIFINT)}
  if(Choice == "Metrics"){return(METRICS)}
}
regression_plot(mtcars,
                "hp",
                "wt",
                Shift = 0,
                Choice = "p",
                LineCheck = F)
#shifts equation display

regression_plot(DATA,
                "SG.ARG",
                "Average.Score",
                Shift = 0,
                Choice = "p",
                LineCheck = F)
