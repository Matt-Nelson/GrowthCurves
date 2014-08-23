library(shiny)
library(shinyIncubator)
girlWeight <- read.csv("GirlWeightStandard.csv")
girlLength <- read.csv("GirlLengthStandard.csv")
boyWeight <- read.csv("BoyWeightStandard.csv")
boyLength <- read.csv("BoyLengthStandard.csv")
combined <- cbind(girlWeight$Day, girlWeight$SD0, girlWeight$SD1 - girlWeight$SD0)
combined <- cbind(combined, girlLength$SD0, girlLength$SD1 - girlLength$SD0)
combined <- cbind(combined, boyWeight$SD0, boyWeight$SD1 - boyWeight$SD0)
combined <- cbind(combined, boyLength$SD0, boyLength$SD1 - boyLength$SD0)
combined <- data.frame(combined)
names(combined) <- c("Day", "meanGirlWeight", "stDevGirlWeight", "meanGirlLength", "stDevGirlLength", "meanBoyWeight", "stDevBoyWeight", "meanBoyLength", "stDevBoyLength")
startData <- combined[(0:28 * 10) + 1, c(1, 2)]
startData[startData[, 1] >= 180, 2] = startData[startData[, 1] >= 180, 2] * (1 + (startData[startData[, 1] >= 180, 1] - 180) / 1000)

shinyUI(pageWithSidebar(
	headerPanel('Fitting growth with Gompertz curves'),
	sidebarPanel(
		selectInput("measureType", label = strong("Select a measurement"),
					choices = list("Weight(kilograms)" = "weight(kg)", 
								   "Weight(pounds)" = "weight(lb)",
								   "Length(centimeters)" = "length(cm)", 
								   "Length(inches)" = "length(in)",
									selected = 1)),
		br(),
		span(textOutput("predictAgeHelp"), style="font-size: 0.875em"),
		numericInput("predictAge", label = strong("Prediction Age"), value=300, min=30, max=1856),
		span(helpText("Enter the child's age when the treatment began."), style="font-size: 0.875em"),
		numericInput("treatmentStartAge", label = strong("Treatment Start Age"), value=180),
		span(textOutput("tableHelp"), style="font-size: 0.875em"),
		matrixInput('growthData', label = strong("Growth Measurements"), startData)
	),
	mainPanel(
		helpText("This application fits entered growth data to a Gompertz curve model. The goal of the application is for you to be able to understand how changes in a child's situation affects their growth prediction. You are allowed to enter a day when you changed some treatment of the child, such as introducing dietary changes, supplementing with a feeding tube, or administration of new medicines. You can also enter a prediction age, which estimates the child's measurement for that age based on a curve using all of the data, a curve using just the data before the treatment, and a curve using just the data after the start of treatment."),
		tableOutput("predictionTable"),
		plotOutput("curvePlot")
	)
))