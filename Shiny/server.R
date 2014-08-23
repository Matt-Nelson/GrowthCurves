library(shiny)
library(grofit)
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

predict90Range <- function(time, params){
	paste(
		format(round(gompertz(time, A = params$ci90.A.model.lo, mu = params$ci90.mu.model.lo, lambda = params$ci90.lambda.model.lo), 2), nsmall = 2),
		"to",
		format(round(gompertz(time, A = params$ci90.A.model.up, mu = params$ci90.mu.model.up, lambda = params$ci90.lambda.model.up), 2), nsmall = 2)
	)
}

predict95Range <- function(time, params){
	paste(
		format(round(gompertz(time, A = params$ci95.A.model.lo, mu = params$ci95.mu.model.lo, lambda = params$ci95.lambda.model.lo), 2), nsmall = 2),
		"to",
		format(round(gompertz(time, A = params$ci95.A.model.up, mu = params$ci95.mu.model.up, lambda = params$ci95.lambda.model.up), 2), nsmall = 2)
	)
}

shinyServer(function(input, output) {
	output$tableHelp <- renderText({
		paste("Enter your measurements. The child's age in days should be entered in the first column, the", input$measureType, "in the second. Press + for more rows, - for fewer.")
	})
	output$predictAgeHelp <- renderText({
		paste("Enter the age (in days) at which you would like to predict the child's ", input$measureType, ".", sep="")
	})
	models <- reactive ({
 		data <- data.frame(input$growthData)
 		names(data) <- c("age(days)", input$measureType)
 		fits <- list()
		fits$full <- gcFitModel(time=data[, 1], data=data[, 2], control=grofit.control(model=c("gompertz")))
		fits$full$summary <- summary.gcFitModel(fits$full)
		fits$before <- gcFitModel(time=data[data[ ,1] < input$treatmentStartAge, 1], data=data[data[ ,1] < input$treatmentStartAge, 2], control=grofit.control(model=c("gompertz")))
		fits$before$summary <- summary.gcFitModel(fits$before)
		fits$after <- gcFitModel(time=data[data[ ,1] >= input$treatmentStartAge, 1], data=data[data[ ,1] >= input$treatmentStartAge, 2], control=grofit.control(model=c("gompertz")))
		fits$after$summary <- summary.gcFitModel(fits$after)
		fits$predictions <- data.frame(rbind(c(
				format(round(gompertz(input$predictAge, A = fits$full$parameters$A[1], mu = fits$full$parameters$mu[1], lambda = fits$full$parameters$lambda[1]), 2), nsmall = 2),
				predict90Range(input$predictAge, summary(fits$full)),
				predict95Range(input$predictAge, summary(fits$full))),
				c(format(round(gompertz(input$predictAge, A = fits$before$parameters$A[1], mu = fits$before$parameters$mu[1], lambda = fits$before$parameters$lambda[1]), 2), nsmall = 2),
				predict90Range(input$predictAge, summary(fits$before)),
				predict95Range(input$predictAge, summary(fits$before))),
				c(format(round(gompertz(input$predictAge, A = fits$after$parameters$A[1], mu = fits$after$parameters$mu[1], lambda = fits$after$parameters$lambda[1]), 2), nsmall = 2),
				predict90Range(input$predictAge, summary(fits$after)),
				predict95Range(input$predictAge, summary(fits$after))
			)))
		names(fits$predictions) <- c("Estimate", "90% interval", "95% interval")
		row.names(fits$predictions) <- c("All Data", "Before Treatment", "After Treatment")
		fits
	})
	output$predictionTable <- renderTable({
		models()$predictions
	})
 	output$curvePlot <- renderPlot({
 		data <- data.frame(input$growthData)
 		plot(data[,2] ~ data[,1], xlab="age(days)", ylab=input$measureType, 
 			 xlim = c(min(data[, 1]), input$predictAge),
 			 ylim = c(min(data[, 2]), max(as.numeric(levels(models()$predictions$Estimate))[models()$predictions$Estimate])))
 		predictPoints <- c(0:input$predictAge)
 		predictPoints <- cbind(predictPoints, gompertz(predictPoints, A = models()$full$parameters$A[1], mu = models()$full$parameters$mu[1], lambda = models()$full$parameters$lambda[1], ))
 		predictPoints <- cbind(predictPoints, gompertz(predictPoints[, 1], A = models()$before$parameters$A[1], mu = models()$before$parameters$mu[1], lambda = models()$before$parameters$lambda[1], ))
 		predictPoints <- cbind(predictPoints, gompertz(predictPoints[, 1], A = models()$after$parameters$A[1], mu = models()$after$parameters$mu[1], lambda = models()$after$parameters$lambda[1], ))
 		lines(predictPoints[, 2] ~ predictPoints[, 1], type="l", col="black", lwd=1.5)
 		lines(predictPoints[, 3] ~ predictPoints[, 1], type="l", col="red", lwd=1.5)
 		lines(predictPoints[, 4] ~ predictPoints[, 1], type="l", col="blue", lwd=1.5)
 		legend(x = "bottomright", legend = c("Curve from All", "Curve from Before", "Curve from After"),
 			   lty = c(1, 1, 1),
 			   col = c("black", "red", "blue"),
 			   lwd = c(1.5, 1.5, 1.5))
 	})
})