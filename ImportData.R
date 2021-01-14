library(latex2exp)
library(readxl)
library(reshape2)
library(ggplot2)
library(data.table)
library(dplyr)
library(hrbrthemes)



# Financing for years 2016 and 2019 --------------------------------------------------------
subsidyData <- data.frame(read_xlsx('Data/execum.xlsx', col_names = TRUE, range = 'A8:D46'))
colnames(subsidyData) <- c('PHEI','Acronym','Subsidy2019','Subsidy2016')
subsidyData$Subsidy2019 <- as.numeric(gsub("[',]", "", subsidyData$Subsidy2019))/1e6
#--------------------------------------------------------------------------------------------

# Inputs variables for the two years ++++++++++++++++++++++++
#------------------------
# Data for the year 2019 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------
rawinputData2019 <- data.frame(read_xlsx('Data/execum5fea4dd57972b.xlsx', col_names = TRUE, sheet = 'datos 2019', range = 'A8:AL54'))
inputData2019 <- rawinputData2019[c('INSTITUCIONES','...2','...4','...8','...10','...12','...14','...16','...18','...20','...24','...30','...33','...37')]
colnames(inputData2019) <- c('PHEI','Lecturers','Students','Alumni','AcademicProgr','Researchers','WoSpapers','WoSworks','SCOPUSpapers','SCOPUSworks','PatMx','EditedJour','acreUndergradProg','acrePosgradPro')

for(i in 2:ncol(inputData2019)) {
  if(class(inputData2019[,i]) == "character"){
    inputData2019[,i] <- as.numeric(inputData2019[,i])
  }
}
inputData2019[is.na(inputData2019)] <- 0

data2019 <- merge(inputData2019, subsidyData, by = "PHEI")
data2019 <- data2019[,-17]
#------------------------
# Data for the year 2016 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#------------------------
rawinputData2016 <- data.frame(read_xlsx('Data/execum5fea4dd57972b.xlsx', col_names = TRUE, sheet = 'datos 2016', range = 'A8:AL54'))
inputData2016 <- rawinputData2016[c('INSTITUCIONES','...2','...4','...8','...10','...12','...14','...16','...18','...20','...24','...30','...33','...37')]
colnames(inputData2016) <- c('PHEI','Lecturers','Students','Alumni','AcademicProgr','Researchers','WoSpapers','WoSworks','SCOPUSpapers','SCOPUSworks','PatMx','EditedJour','acreUndergradProg','acrePosgradPro')

for(i in 2:ncol(inputData2016)) {
  if(class(inputData2016[,i]) == "character"){
    inputData2016[,i] <- as.numeric(inputData2016[,i])
  }
}
inputData2016[is.na(inputData2016)] <- 0

data2016 <- merge(inputData2016, subsidyData, by = "PHEI")
data2016 <- data2016[,-16]
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#############################################
#Graphs of the variables
##############################################
names(data2016)
variablesNames <- c("EditedJour")
dataToGraph <- data.frame(data2019$Acronym, data2016[,variablesNames[1]], data2019[,variablesNames[1]])

ptheme <- theme (
  #axis.text            = element_text(size = 9),              # tick labels
  #axis.title           = element_text(size = 9),              # axis labels
  axis.ticks           = element_line(colour = "grey70", size = 0.25),
  panel.background     = element_rect(fill = "white", colour = NA),
  panel.border         = element_rect(fill = NA, colour = "grey70", size = 0.25),
  panel.grid.major     = element_line(colour = "grey85", size = 0.25),
  panel.grid.minor     = element_line(colour = "grey85", size = 0.125),
  panel.margin         = unit(0 , "lines"),
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
p1 <- p1 + labs( x = "Public HEI", y = variablesNames[1] )
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

############################
#
# Graphs public funding
# 
###########################

# --- Increments in percentage  
subsidyData$incrPro <- ((subsidyData$Subsidy2019 - subsidyData$Subsidy2016)/subsidyData$Subsidy2016)*100

subsidyData <- subsidyData[order(subsidyData$incrPro, decreasing = TRUE ),]
# Plot
ggplot(subsidyData, aes(x=factor(Acronym, levels = Acronym ), y=incrPro)) +
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

# ---- Public funding in 2016 and 201

subsidyData <- subsidyData[order(subsidyData$Subsidy2016),]
subsidyData1 <- subsidyData
#subsidyData1 <- subset(subsidyData, Subsidy2019 <= 40000)

ggplot(subsidyData1) +
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






