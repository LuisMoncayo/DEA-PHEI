#https://academic.oup.com/imaman

library(Benchmarking)
library(ggplot2)
require(reshape2)
library(writexl)

#dataInputsOutputs <- data.frame(x1P1 = c(6, 7,10,13,10,13,16),
#                                x2P1 = c(10,6, 3, 1, 9, 5, 3),
#                                y1P1 = c(rep(1,7)),
#                                x1P2 = c(2,7,12,16,7,13,12),
#                                x2P2 = c(9,4, 2, 1,8, 5, 2),
#                                y1P2 = c(rep(1,7))
#                                )

dataInputsOutputs <- data.frame(x1P1 = c(4, 6,6,9 ,8,8,5,13),
                                x2P1 = c(11,6,3,1,10,6,5,4),
                                y1P1 = c(rep(1,8)),
                                x1P2 = c(1,2,6,12,6,10,11,13),
                                x2P2 = c(10,6,3,1,9,7,5,4),
                                y1P2 = c(rep(1,8))
)

dataInputsOutputs$x1y1P1 <- dataInputsOutputs$x1P1/dataInputsOutputs$y1P1
dataInputsOutputs$x2y1P1 <- dataInputsOutputs$x2P1/dataInputsOutputs$y1P1
dataInputsOutputs$x1y1P2 <- dataInputsOutputs$x1P2/dataInputsOutputs$y1P2
dataInputsOutputs$x2y1P2 <- dataInputsOutputs$x2P2/dataInputsOutputs$y1P2

#p = ggplot() + 
#  geom_line(data = dataInputsOutputs, aes(x = x1y1P1, y = x2y1P1), color = "blue") +
#  geom_line(data = dataInputsOutputs, aes(x = x1y1P2, y = x2y1P2), color = "red")
#plot(p)

#-------------------------------------------------------------------------------
#---- Compute DEA efficiency
#------------------------------------------------------------------------------
xP1 <- with(dataInputsOutputs, cbind(x1P1,x2P1))
yP1 <- with(dataInputsOutputs, cbind(y1P1))
efficiencyP1 <- dea(xP1,yP1, SLACK=TRUE, DUAL=TRUE,RTS= "vrs", ORIENTATION="in")

xP2 <- with(dataInputsOutputs, cbind(x1P2,x2P2))
yP2 <- with(dataInputsOutputs, cbind(y1P2))
efficiencyP2 <- dea(xP2,yP2, SLACK=TRUE, DUAL=TRUE,RTS= "vrs", ORIENTATION="in")

#-----------------------
#--- Malmquist Index
#-----------------------
malmquistIndex <- malmq(xP1, yP1, ID0 = NULL, xP2, yP2, ID1 = NULL, RTS = "crs")

allResults <- data.frame( 'e00' = malmquistIndex$e00,
                          'e11' = malmquistIndex$e11,
                          'e01' = malmquistIndex$e01,
                          'e10' = malmquistIndex$e10,
                          'MalmquistIndex' = malmquistIndex$m, 
                          'TechnicalChange' = malmquistIndex$tc,
                          'EfficiencyChange' = malmquistIndex$ec)
print(dataInputsOutputs)
print(allResults)

write_xlsx(allResults, "allResults.xls")
