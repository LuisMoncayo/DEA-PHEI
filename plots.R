library(fmsb)
library(latex2exp)
library(ggplot2)

variablePlot <- function(data2016, data2019, variable){
  
  if (variable == 'Lecturers'){
    laby = 'Number of lecturers'
  }else if(variable == 'Students'){
    laby = 'Number of students'
  }else if(variable == 'Alumni'){
    laby = 'Alumni'
  }else if(variable == 'AcademicProgr'){
    laby = 'Academic Programs'
  }else if(variable == 'Researchers'){
    laby = 'Number of researchers'
  }else if(variable == 'WoSpapers'){
    laby = 'Research products indexed in WoS'
    data2019$WoSpapers <- data2019$WoSpapers + data2019$WoSworks
    data2016$WoSpapers <- data2016$WoSpapers + data2016$WoSworks
  }else if(variable == 'SCOPUSpapers'){
    laby = 'Research products indexed in SCOPUS'
    data2019$SCOPUSpapers <- data2019$SCOPUSpapers + data2019$SCOPUSworks
    data2016$SCOPUSpapers <- data2016$SCOPUSpapers + data2016$SCOPUSworks
  }else if(variable == 'PatMx'){
    laby = 'Granted patents'
  }else if(variable == 'EditedJour'){
    laby = 'Numberr of edited journals'
  }else if(variable == 'acreUndergradProg'){
    laby = 'Accredited undergradate programs'
  }else if(variable == 'acrePosgradPro'){
    laby = 'Accredited postgradate programs'
  }
  
  variablesNames <- c(variable)
  dataToGraph <- data.frame(data2019$Acronym, data2016[,variablesNames[1]], data2019[,variablesNames[1]])
  
  ptheme <- theme (
    #axis.text            = element_text(size = 9),              # tick labels
    #axis.title           = element_text(size = 9),              # axis labels
    axis.ticks           = element_line(colour = "grey70", size = 0.25),
    panel.background     = element_rect(fill = "white", colour = NA),
    panel.border         = element_rect(fill = NA, colour = "grey70", size = 0.25),
    panel.grid.major     = element_line(colour = "grey85", size = 0.25),
    panel.grid.minor     = element_line(colour = "grey85", size = 0.125),
    #panel.margin         = unit(0 , "lines"),
    legend.justification = c(1, 0), 
    legend.position      = c(0.15, 0.7),
    legend.key           = element_rect(fill = "white", color = NA),
    legend.background    = element_rect(fill = "white", linetype= "blank"),
    #legend.text          = element_text(size = 8),
    plot.margin          = unit(c(0.1, 0.1, 0.1, 0.01), "npc"),   # c(bottom, left, top, right), values can be negative
    axis.text.x = element_text(angle = 90, hjust = 1)
  )
  
  cols    <- c( "c1" = "#0f0f0f", "c2" = "#0f0f0f" )
  shapes  <- c("s1" = 1, "s2" = 4)
  
  p1 <- ggplot(data = dataToGraph, aes(x = data2019.Acronym))
  p1 <- p1 + geom_point(size = 3, aes( y = data2016...variablesNames.1.. , shape = "s1"))
  p1 <- p1 + geom_point(size = 3, aes( y = data2019...variablesNames.1.., shape = "s2")) 
  p1 <- p1 + labs( x = "Public HEI", y =  laby )
  #p1 <- p1 + scale_color_manual(name = "Year", 
  #                              breaks = c("c1", "c2"), 
  #                              values = cols,
  #                              labels = c("2016", "2019"))
  p1 <- p1 + scale_shape_manual(name = "Year", 
                                breaks = c("s1", "s2"),
                                values = shapes,
                                labels = c("2016", "2019"))
  p1 <- p1 +  ptheme
  
  print(p1)
}


fundingPlots <- function(subsidyData, includeUNAM){
  shapes  <- c("s1" = 1, "s2" = 4)
  subsidyData <- subsidyData[order(subsidyData$Subsidy2016),]
  
  if (includeUNAM == 'Yes'){
    subsidyData1 <- subsidyData
  }else{
    subsidyData1 <- subset(subsidyData, Subsidy2019 <= 40000)
  }
  #subsidyData <- subsidyData[order(subsidyData$Subsidy2016),]
  #subsidyData1 <- subsidyData
  #subsidyData1 <- subset(subsidyData, Subsidy2019 <= 40000)
  
  fundinPlot <- ggplot(subsidyData1) +
                geom_segment( aes(x=factor(Acronym, levels = Acronym ), xend=factor(Acronym, levels = Acronym ), y=Subsidy2016, yend=Subsidy2019), color="black") +
                geom_point( size=3, aes(x=factor(Acronym, levels = Acronym ),y=Subsidy2016, shape = "s1") ) +
                geom_point( size=3, aes(x=factor(Acronym, levels = Acronym ), y=Subsidy2019, shape = "s2") ) +
                coord_flip() +
                theme_light() +
                scale_shape_manual(name = "Year", 
                                   breaks = c("s1", "s2"),
                                   values = shapes,
                                   labels = c("2016", "2019")) +
                theme(
                  legend.position = c(.95, .55),
                  legend.justification = c("right", "bottom"),
                  panel.grid.major     =  element_line(colour = "grey9", size = 0.02),
                  panel.grid.minor     =  element_line(colour = "grey9", size = 0.1, linetype = "dotted"),
                ) +
                xlab("Public HEI") +
                ylab(TeX("Public funding $\\left( 1 \\times 10^6  \\right)$"))
  
  print(fundinPlot)
  
  # --- Increments in percentage  
  subsidyData$incrPro <- ((subsidyData$Subsidy2019 - subsidyData$Subsidy2016)/subsidyData$Subsidy2016)*100

  subsidyData <- subsidyData[order(subsidyData$incrPro, decreasing = TRUE ),]
  # Plot
  plotIncr <- ggplot(subsidyData, aes(x=factor(Acronym, levels = Acronym ), y=incrPro)) +
              geom_segment( aes(x=factor(Acronym, levels = Acronym ), xend=factor(Acronym, levels = Acronym ), y=0, yend=incrPro), color="grey") +
              geom_point( color="black", size=3) +
              theme_light() +
              scale_y_continuous(name="Increments in public funding (%)", limits=c(-5,27), breaks = c(-5,0,5,10,15,20,25)) +
              theme(
                panel.grid.major.x = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1)
              ) +
              xlab("Public HEI")
  
  print(plotIncr)
}


#### Plots of the results
cV2016 <- function(allResults){
  rownames(allResults) <- allResults$Acronym
  final_df <- as.data.frame(t(allResults))
  final_df <- final_df[-c(1), ] 
  
  plotEfficiencies <- final_df[c(1,2),]
  
  minimum <- as.numeric(min(sapply(plotEfficiencies, min)))
  maximum <- as.numeric(max(sapply(plotEfficiencies, max)))
  plotEfficiencies <- rbind(rep(maximum, ncol(plotEfficiencies)) , rep(minimum, ncol(plotEfficiencies)) , plotEfficiencies)
  
  plotEfficiencies <- as.data.frame(sapply(plotEfficiencies, as.numeric))
  
  # Custom the radarChart !
  radarchart(plotEfficiencies, axistype=1,
                            #custom the grid
                            cglcol="grey", 
                            cglty=3, 
                            axislabcol= "black", 
                            caxislabels = c(seq( round(minimum,2), round(maximum,2), round((round(maximum,2) - round(minimum,2))/4,2))), 
                            cglwd=0.8,
                            vlcex=0.8,
                            #custom polygon
                            pcol=c('black','black'), 
                            plwd=1.5, 
                            plty=c(2,1),
                            pty = c(1,16)
                            ) 
  
                            legend(x = "bottom",
                                           horiz = TRUE, 
                                           legend = c("VRS 2016", "CRS 2016"), 
                                           bty = "n", 
                                           pch = c(1,16), 
                                           text.col = "black", 
                                           cex=0.9, 
                                           pt.cex=1,
                                           lty = c(2,1),
                                           lwd = 1.1
                                           #y.intersp = 0.05
                                        ) 
}

cV2019 <- function(allResults){
  rownames(allResults) <- allResults$Acronym
  final_df <- as.data.frame(t(allResults))
  final_df <- final_df[-c(1), ] 
  
  plotEfficiencies <- final_df[c(3,4),]
  
  minimum <- as.numeric(min(sapply(plotEfficiencies, min)))
  maximum <- as.numeric(max(sapply(plotEfficiencies, max)))
  plotEfficiencies <- rbind(rep(maximum, ncol(plotEfficiencies)) , rep(minimum, ncol(plotEfficiencies)) , plotEfficiencies)
  
  plotEfficiencies <- as.data.frame(sapply(plotEfficiencies, as.numeric))
  
  # Custom the radarChart !
  radarchart( plotEfficiencies, axistype=1,
              #custom the grid
              cglcol="grey", 
              cglty=3, 
              axislabcol="black", 
              #caxislabels = c(seq( round(minimum,2), round(maximum,2), round((round(maximum,2) - round(minimum,2))/4,2))),
              caxislabels = c(seq( round(minimum,2), round(maximum,2), round((round(maximum+0.01,2) - round(minimum,2))/4,2) )),
              cglwd=0.8,
              vlcex=0.8,
              #custom polygon
              pcol=c('black','black'), 
              plwd=1.5, 
              plty=c(2,1),
              pty = c(1,16)
  )
  # Legend
  legend(x = "bottom",
         horiz = TRUE, 
         legend = c("VRS 2019", "CRS 2019"), 
         bty = "n", 
         pch=c(1,16), 
         text.col = "black", 
         cex=0.9, 
         pt.cex=1,
         lty = c(2,1),
         lwd = 1.1
         #y.intersp = 0.05
  )
}


vV2016_2019 <- function(allResults){
  rownames(allResults) <- allResults$Acronym
  final_df <- as.data.frame(t(allResults))
  final_df <- final_df[-c(1), ] 
  
  plotEfficiencies <- final_df[c(1,3),]
  
  minimum <- as.numeric(min(sapply(plotEfficiencies, min)))
  maximum <- as.numeric(max(sapply(plotEfficiencies, max)))
  plotEfficiencies <- rbind(rep(maximum, ncol(plotEfficiencies)) , rep(minimum, ncol(plotEfficiencies)) , plotEfficiencies)
  
  plotEfficiencies <- as.data.frame(sapply(plotEfficiencies, as.numeric))
  
  # Custom the radarChart !
  radarchart( plotEfficiencies, axistype=1,
              #custom the grid
              cglcol="grey", 
              cglty=3, 
              axislabcol="black", 
              caxislabels = c(seq( round(minimum,2), round(maximum,2), round((round(maximum,2) - round(minimum,2))/4,2))), 
              cglwd=0.8,
              vlcex=0.8,
              #custom polygon
              pcol=c('black','black'), 
              plwd=1.5, 
              plty=c(2,1),
              pty = c(1,16)
  )
  # Legend
  legend(x = "bottom",
         horiz = TRUE, 
         legend = c("VRS 2016", "VRS 2019"), 
         bty = "n", 
         pch=c(1,16), 
         text.col = "black", 
         cex=0.9, 
         pt.cex=1,
         lty = c(2,1),
         lwd = 1.1
         #y.intersp = 0.05
  )
}

cC2016_2019 <- function(allResults){
  rownames(allResults) <- allResults$Acronym
  final_df <- as.data.frame(t(allResults))
  final_df <- final_df[-c(1), ] 
  
  plotEfficiencies <- final_df[c(2,4),]
  
  minimum <- as.numeric(min(sapply(plotEfficiencies, min)))
  maximum <- as.numeric(max(sapply(plotEfficiencies, max)))
  plotEfficiencies <- rbind(rep(maximum, ncol(plotEfficiencies)) , rep(minimum, ncol(plotEfficiencies)) , plotEfficiencies)
  
  plotEfficiencies <- as.data.frame(sapply(plotEfficiencies, as.numeric))
  
  # Custom the radarChart !
  radarchart( plotEfficiencies, axistype=1,
              #custom the grid
              cglcol="grey", 
              cglty=3, 
              axislabcol="black", 
              caxislabels = c(seq( round(minimum,2), round(maximum,2), round((round(maximum,2) - round(minimum,2))/4,2))), 
              cglwd=0.8,
              vlcex=0.8,
              #custom polygon
              pcol=c('black','black'), 
              plwd=1.5, 
              plty=c(2,1),
              pty = c(1,16)
  )
  # Legend
  legend(x = "bottom",
         horiz = TRUE, 
         legend = c("CRS 2016", "CRS 2019"), 
         bty = "n", 
         pch=c(1,16), 
         text.col = "black", 
         cex=0.9, 
         pt.cex=1,
         lty = c(2,1),
         lwd = 1.1
         #y.intersp = 0.05
  )
}

malmquistIndexPlot <- function(allResults){
  dataMalmquistIndex <- data.frame('Acronym' = allResults$Acronym, 'MalmIn' = allResults$MalmquistIndex)
  dataMalmquistIndex$MalmIn <- round(dataMalmquistIndex$MalmIn,2)
  dataMalmquistIndex <- dataMalmquistIndex[order(dataMalmquistIndex$MalmIn),]
  dataMalmquistIndex$Acronym <- factor(dataMalmquistIndex$Acronym, levels = dataMalmquistIndex$Acronym ) 
  
  plot <- ggplot(dataMalmquistIndex, aes(x=Acronym, y=MalmIn, group=1)) +
          geom_point(size=2) + geom_line(linetype = "dashed")+
          geom_text(
            label=dataMalmquistIndex$MalmIn,
            nudge_x = 0, nudge_y = 0.035, 
            #check_overlap = T
          ) + geom_hline(yintercept = 1) +
          xlab("Public HEI") + ylab("Malmquist index - MI(2016,2019)") +
          coord_flip() + theme_light()
  print(plot)
}

technicalChangeIndexPlot <- function(allResults){
  dataMalmquistIndex <- data.frame('Acronym' = allResults$Acronym, 'MalmIn' = allResults$TechnicalChange)
  dataMalmquistIndex$MalmIn <- round(dataMalmquistIndex$MalmIn,2)
  dataMalmquistIndex <- dataMalmquistIndex[order(dataMalmquistIndex$MalmIn),]
  dataMalmquistIndex$Acronym <- factor(dataMalmquistIndex$Acronym, levels = dataMalmquistIndex$Acronym ) 
  
  plot <- ggplot(dataMalmquistIndex, aes(x=Acronym, y=MalmIn, group=1)) +
    geom_point(size=2) + geom_line(linetype = "dashed")+
    geom_text(
      label=dataMalmquistIndex$MalmIn,
      nudge_x = 0, nudge_y = 0.035, 
      #check_overlap = T
    ) + geom_hline(yintercept = 1) +
    xlab("Public HEI") + ylab("Technical change index - TC(2016,2019)") +
    coord_flip() +theme_light()
  print(plot)
}

efficiencyChangeIndexPlot <- function(allResults){
  dataMalmquistIndex <- data.frame('Acronym' = allResults$Acronym, 'MalmIn' = allResults$EfficiencyChange)
  dataMalmquistIndex$MalmIn <- round(dataMalmquistIndex$MalmIn,2)
  dataMalmquistIndex <- dataMalmquistIndex[order(dataMalmquistIndex$MalmIn),]
  dataMalmquistIndex$Acronym <- factor(dataMalmquistIndex$Acronym, levels = dataMalmquistIndex$Acronym ) 
  
  plot <- ggplot(dataMalmquistIndex, aes(x=Acronym, y=MalmIn, group=1)) +
    geom_point(size=2) + geom_line(linetype = "dashed")+
    geom_text(
      label=dataMalmquistIndex$MalmIn,
      nudge_x = 0, nudge_y = 0.035, 
      #check_overlap = T
    ) + geom_hline(yintercept = 1) +
    xlab("Public HEI") + ylab("Efficiency change index - EC(2016,2019)") +
    coord_flip() + theme_light()
  print(plot)
}






