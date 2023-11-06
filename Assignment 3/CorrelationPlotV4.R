AdvancedCorPlot <- function(DF = mtcars,
                            X = "hp",
                            Y = "wt",
                            POINTFILL = 'black',
                            LINECOLOR = 'blue',
                            METHOD = "pearson",
                            POINTOUTLINE = "cyan",
                            POINTSIZE = 3,
                            POINTSHAPE = 21,
                            MainTitle = paste(if(METHOD == "pearson"){"Pearson" }else{"Spearman"},
                              "Correlation Plot of", X, "and", Y),
                            YTitle = Y,
                            XTitle = X,
                            PermTest = "No",
                            PermAmount = 500,
                            Seed = NA) {
  if(!is.na(Seed)){set.seed(Seed)}
  require(tidyverse)
  require(ggplot2)
  require(ggpubr)
  require(ggeasy)
  require(plotly)
  require(flextable)
  
  METHOD <- tolower(METHOD)
  PermTest <- tolower(PermTest)
  
  if(startsWith(METHOD,"s") & METHOD != "spearman"){message("It appears you want spearman correlation but made a typo.")}
  if(startsWith(METHOD,"s")){METHOD <- "spearman"}
  
  if(startsWith(METHOD,"p") & METHOD != "pearson"){message("It appears you want pearson correlation but made a typo.")}
  if(startsWith(METHOD,"p")){METHOD <- "pearson"}
  
  if(!(METHOD %in% c("spearman","pearson"))){
    stop("Make sure method is either spearman or pearson.")
  }
  
  if(!(PermTest %in% c("no","add","only","table","interactive"))){
    stop("Make sure the permutation test is no, add, table, interactive or only.")
  }

  
  sm_statCorr <- function(...,
                          fit.params = list(),
                          corr_method = METHOD,
                          alternative = 'two.sided',
                          separate_by = ',',
                          label_x = NULL,
                          label_y = NULL,
                          text_size = 4,
                          show_text = TRUE,
                          borders = TRUE,
                          legends = FALSE) {
    params <- list(...)
    fit.params <- modifyList(params, fit.params)
    
    fitPlot <- do.call('geom_smooth',
                       modifyList(list(
                         method = if(METHOD == "pearson"){'lm'}else{"loess"},
                         se = F,
                         alpha = 0.2,
                         weight = 0.8
                       ), fit.params))
    
    
    textPlot <-
      ggpubr::stat_cor(
        p.accuracy = 0.000001,
        method = METHOD,
        alternative = alternative,
        label.sep = separate_by,
        label.x = label_x,
        label.y = label_y,
        size = 4
      )
    
    if (show_text == FALSE) {
      textPlot <- NULL
    }
    
    list(fitPlot,
         textPlot,
         sm_hvgrid(borders = borders, legends = legends))
  }
  
  sm_hvgrid <- function(legends = TRUE,
                        borders = TRUE) {
    if (legends == TRUE) {
      if (borders == FALSE) {
        ggplot2::theme_bw(base_size = 10, base_family = '') +
          cowplot::theme_minimal_grid() +
          ggplot2::theme(
            axis.text =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
            axis.title.y =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
            axis.title.x =  ggplot2::element_text(
              size = ggplot2::rel(.85),
              color = "black",
              vjust = -1
            ),
            axis.text.x =  ggplot2::element_text(vjust = 0, size = ggplot2::rel(1.)),
            panel.grid.major =  ggplot2::element_line(size = 0.4),
            plot.title =  ggplot2::element_text(
              size = ggplot2::rel(.85),
              hjust = 0.5,
              face = 'bold'
            ),
            panel.grid.minor.x =  ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = NA, colour = NA),
            strip.text.x = ggplot2::element_text(colour = "black", size = ggplot2::rel(.85)),
            strip.text.y = ggplot2::element_text(colour = "black", size = ggplot2::rel(.85)),
            legend.text = ggplot2::element_text(size = ggplot2::rel(.78)),
            legend.title = ggplot2::element_text(size = ggplot2::rel(.78))
          )
      } else if (borders == TRUE) {
        ggplot2::theme_bw(base_size = 10, base_family = '') +
          ggplot2::theme(
            axis.text =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
            axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
            axis.title.x =  ggplot2::element_text(
              size = ggplot2::rel(1.2),
              color = "black",
              vjust = -1
            ),
            axis.text.x =  ggplot2::element_text(vjust = 0),
            panel.grid.major =  ggplot2::element_line(size = 0.4),
            plot.title =  ggplot2::element_text(hjust = 0.5, face = 'bold'),
            panel.grid.minor.x =  ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = NA, colour = NA),
            strip.text.x = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.2)),
            strip.text.y = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.2)),
            legend.text = ggplot2::element_text(size = ggplot2::rel(1.1)),
            legend.title = ggplot2::element_text(size = ggplot2::rel(1.1))
          )
      }
    } else if (legends == FALSE) {
      if (borders == FALSE) {
        ggplot2::theme_bw(base_size = 10, base_family = '') +
          cowplot::theme_minimal_grid() +
          ggplot2::theme(
            axis.text =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
            axis.title.y =  ggplot2::element_text(size = ggplot2::rel(.85), color = "black"),
            axis.title.x =  ggplot2::element_text(
              size = ggplot2::rel(.85),
              color = "black",
              vjust = -1
            ),
            axis.text.x =  ggplot2::element_text(vjust = 0),
            panel.grid.major =  ggplot2::element_line(size = 0.4),
            plot.title =  ggplot2::element_text(
              size = ggplot2::rel(.85),
              hjust = 0.5,
              face = 'bold'
            ),
            panel.grid.minor.x =  ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(fill = NA, colour = NA),
            strip.text.x = ggplot2::element_text(colour = "black", size = ggplot2::rel(.85)),
            strip.text.y = ggplot2::element_text(colour = "black", size = ggplot2::rel(.85)),
            legend.position = 'none'
          )
      } else if (borders == TRUE) {
        ggplot2::theme_bw(base_size = 10, base_family = '') +
          ggplot2::theme(
            axis.text =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
            axis.title.y =  ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
            axis.title.x =  ggplot2::element_text(
              size = ggplot2::rel(1.2),
              color = "black",
              vjust = -1
            ),
            axis.text.x =  ggplot2::element_text(vjust = 0),
            panel.grid.major =  ggplot2::element_line(size = 0.4),
            plot.title =  ggplot2::element_text(hjust = 0.5, face = 'bold'),
            panel.grid.minor.x =  ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank(),
            strip.background = element_rect(fill = NA, colour = NA),
            strip.text.x = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.2)),
            strip.text.y = ggplot2::element_text(colour = "black", size = ggplot2::rel(1.2)),
            legend.position = 'none'
          )
      }
    }
  }

PLOT <- suppressMessages(ggplot(data = DF, mapping = aes(x = !!sym(X), y = !!sym(Y))) +
  geom_point(shape = POINTSHAPE, fill = POINTFILL, color = POINTOUTLINE, size = POINTSIZE) + 
    labs(title = MainTitle,
         x = XTitle,
         y = YTitle) +
  sm_statCorr(color = LINECOLOR, corr_method = METHOD,
              linetype = 'dashed'))
EST <- c()
for (i in 1:PermAmount) {
  RANDOMX <- sample(DF[[X]])
  RANDOMY <- sample(DF[[Y]])
  EST[i] <- cor.test(RANDOMX,RANDOMY,
                     method = METHOD)$est
}
DF2 <- data.frame(Estimate = EST)
CORValue <- cor.test(DF[[X]], DF[[Y]], method = METHOD)$est
cor_estimate <- abs(cor.test(DF[[X]], DF[[Y]], method = METHOD)$est)



HIST2 <- ggplot(DF2, aes(x = Estimate, fill = factor(Estimate < -1*cor_estimate | Estimate > cor_estimate))) +
  geom_histogram(color = "black", alpha = 0.6) +
  geom_vline(xintercept  = cor.test(DF[[X]],DF[[Y]],
                                    method = METHOD)$est,
             col = "red",
             lwd = 2) +
  geom_vline(xintercept  = -1*cor.test(DF[[X]],DF[[Y]],
                                       method = METHOD)$est,
             col = "red",
             lwd = 2,
             linetype = "dashed") +
  scale_fill_manual(values = c("white", "blue"))  +
  labs(title = "Permutation Procedure",
       x = "Correlation Values", y = "Frequency") +
  theme_minimal() +
  easy_center_title() +
  theme(legend.position = "none")



if(PermTest == "add"){
  return(ggarrange(PLOT,HIST2))
}

if(PermTest %in% c("only","table")){
  pearson <- cor(DF[[X]], DF[[Y]])
  spearman <- cor(DF[[X]], DF[[Y]], method = "spearman")
  cor.extreme <- length(which(abs(DF2$Estimate) >= abs(pearson)))
  cor.pvalue <- cor.extreme/PermAmount
  spear.extreme <- length(which(abs(DF2$Estimate) >= 
                                  abs(spearman)))
  spear.pvalue <- spear.extreme/PermAmount
  RESULTS <- matrix(c(CORValue, CORValue,
                      cor.pvalue, spear.pvalue,
                      round(binom.test(cor.extreme, PermAmount)$conf.int[1], digits = 3),
                      round(binom.test(spear.extreme, PermAmount)$conf.int[1], digits = 3),
                      round(binom.test(cor.extreme, PermAmount)$conf.int[2], digits = 3),
                      round(binom.test(spear.extreme, PermAmount)$conf.int[2], digits = 3)),
                    nrow = 2)
  RESULTS <- round(data.frame(RESULTS),4)

  rownames(RESULTS) <- c("Pearson's r", "Spearman's rank correlation")
  colnames(RESULTS) <- c("Correlation Value", "Estimated p-value","Lower CI","Upper CI")
  if(METHOD == "pearson"){
    RESULTS <- RESULTS[1,]
  }else{
    RESULTS <- RESULTS[2,]
  }
  if(PermTest == "table"){
    return(ggtexttable(RESULTS))
  }
  cat("Permutation procedure:\n")
  if(METHOD == "pearson"){
  cat(paste("With", PermAmount, "permutations, we are 95% confident that:\n", 
            "the p-value of Pearson's correlation (r) is between", 
            round(binom.test(cor.extreme, PermAmount)$conf.int[1], 
                  digits = 3), "and", round(binom.test(cor.extreme, 
                                                       PermAmount)$conf.int[2], digits = 3), "\n"))}
  if(METHOD == "spearman"){
  cat(paste(" the p-value of Spearman's rank correlation is between", 
            round(binom.test(spear.extreme, PermAmount)$conf.int[1], 
                  digits = 3), "and", round(binom.test(spear.extreme, 
                                                       PermAmount)$conf.int[2], digits = 3), "\n"))
  cat("Note:  If 0.05 is in this range, increase the permutations= argument.\n\n")}
  return(list("To view the correlation table, press back on the plot window." = ggtexttable(RESULTS),
              "Histogram of random permuations currently displayed."= HIST2))
}


if(PermTest != "interactive"){return(PLOT)}else{
  suppressMessages(ggplotly(ggplot(data = DF, mapping = aes(x = !!sym(X), y = !!sym(Y))) +
             geom_point(shape = POINTSHAPE, fill = POINTFILL, color = POINTOUTLINE, size = POINTSIZE) + 
             labs(title = MainTitle,
                  x = XTitle,
                  y = YTitle)+
             sm_statCorr(color = LINECOLOR,
                         linetype = 'dashed',
                         show_text = FALSE)))
}
}

#Example




AdvancedCorPlot(COLLEGEFOOTBALL,
                X = "Yards.Per.Game.Allowed",
                Y = "Win",
                MainTitle = 'Correlation Plot of Total Yards Per Game and Wins',
                METHOD = "Pearson",
                XTitle = "Yards Allowed",
                YTitle = "Total Wins",
                POINTOUTLINE = "darkblue",
                POINTFILL = "DarkGreen",
                LINECOLOR = "red",
                PermTest = "no",
                Seed = 138)




AdvancedCorPlot(COLLEGEFOOTBALL,
                X = "Redzone.Points.Allowed",
                Y = "Loss",
                MainTitle = 'Correlation Plot of Red-Zone Points Allowed and Total Losses',
                METHOD = "pearson",
                XTitle = "Redzone Points (% of Total Points Scored)",
                YTitle = "Total Losses",
                POINTOUTLINE = "darkblue",
                POINTFILL = "Red",
                LINECOLOR = "Blue",
                PermTest = "no",
                Seed = 138)
