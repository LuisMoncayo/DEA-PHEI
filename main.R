library(Benchmarking)

source("data.R")
source("plots.R")

# Create the data from the downloaded files
subsidyData <- publicFunding()
data2016 <- variables2016(subsidyData)
data2019 <- variables2019(subsidyData)

# Plot the value of the variables
#Select (deleting the #) of the variable you want to plot

#dev.off()
variable = "Lecturers" #Number of lecturers at each public HEI
#variable = "Students" #Number of current students at all levels
#variable = "Alumni" #Number of graduate students at all levels 
#variable = "AcademicProgr" #Number of academic programs at all levels
#variable = "Researchers" #Number of researchers
#variable = "WoSpapers" #Number journal papers and other research work indexed in WoS
#variable = "SCOPUSpapers" #Number journal papers and other research work indexed in SCOPUS
#variable = "PatMx" #Granted patents
#variable = "EditedJour" #Number of journals that are edited by the public HEI
#variable = "acreUndergradProg" #Accredited undergraduate programs
#variable = "acrePosgradPro" #Accredited postgraduate programs

variablePlot(data2016, data2019, variable)

# Include the public HEI that receives the biggest amount of public funds
# "Yes" or "No"
include = "Yes"
fundingPlots(subsidyData, include)


#------------------
#--- DEA 2016
#------------------
var2016 <- data.frame(data2016$Lecturers,data2016$Students,data2016$Alumni,data2016$AcademicProgr,data2016$Researchers,
                      data2016$WoSpapers+data2016$SCOPUSpapers, data2016$SCOPUSpapers+data2016$SCOPUSworks,
                      data2016$PatMx, data2016$EditedJour,data2016$acreUndergradProg,data2016$acrePosgradPro,
                      data2016$Subsidy2016
                      )
colnames(var2016) <- c(paste0("x", 1:11), "y1")

x2016 <- with(var2016, cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11))
y2016 <- with(var2016, cbind(y1))

VRS2016 <- dea(x2016,y2016, SLACK = TRUE, DUAL = TRUE, RTS = "vrs", ORIENTATION="in")
CRS2016 <- dea(x2016,y2016, SLACK = TRUE, DUAL = TRUE, RTS = "crs", ORIENTATION="in")

resultsDEA2016 <- data.frame("Acronym" = data2016$Acronym, "VRS2016" = eff(VRS2016), "CRS2016" = eff(CRS2016))

#------------------
#--- DEA 2019
#------------------
var2019 <- data.frame(data2019$Lecturers,data2019$Students,data2019$Alumni,data2019$AcademicProgr,data2019$Researchers,
                      data2019$WoSpapers+data2019$SCOPUSpapers, data2019$SCOPUSpapers+data2019$SCOPUSworks,
                      data2019$PatMx, data2019$EditedJour,data2019$acreUndergradProg,data2019$acrePosgradPro,
                      data2019$Subsidy2019
)
colnames(var2019) <- c(paste0("x", 1:11), "y1")

x2019 <- with(var2019, cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11))
y2019 <- with(var2019, cbind(y1))

VRS2019 <- dea(x2019,y2019, SLACK=TRUE, DUAL=TRUE, RTS="vrs", ORIENTATION="in")
CRS2019 <- dea(x2019,y2019, SLACK=TRUE, DUAL=TRUE, RTS="crs", ORIENTATION="in")

resultsDEA2019 <- data.frame("Acronym" = data2019$Acronym, "VRS2019" = eff(VRS2019), "CRS2019" = eff(CRS2019))
#resultsDEA2019 <- data.frame("Acronym" = data2019$Acronym, "Efficiency2019" = eff(results2019))

allResults <- merge(resultsDEA2016, resultsDEA2019, by = "Acronym")

#-----------------------
#--- Malmquist Index
#-----------------------
malmquistIndex <- malmq(x2016, y2016, ID0 = NULL, x2019, y2019, ID1 = NULL, RTS = "crs")

outputsMalmquistIndex <- data.frame('Acronym' = resultsDEA2019$Acronym, 
                                    'MalmquistIndex' = malmquistIndex$m, 
                                    'TechnicalChange' = malmquistIndex$tc,
                                    'EfficiencyChange' = malmquistIndex$ec)

allResults = merge(allResults, outputsMalmquistIndex, by = 'Acronym')

#---------------------------------------
#--- Graph the efficiencies and indexes
#---------------------------------------
cV2016(allResults)
cV2019(allResults)
vV2016_2019(allResults)
cC2016_2019(allResults)

#Malmquist index plot
malmquistIndexPlot(allResults)
#Technical change index plot
technicalChangeIndexPlot(allResults)
#Efficiency change index plot
efficiencyChangeIndexPlot(allResults)


