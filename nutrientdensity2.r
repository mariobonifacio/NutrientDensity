nutrientdensity2 <- function(){

## This tool summarized the entire SR27
## into individual nutrient density score

mydata <- read.csv("ABBREV.csv")

## I don't know how to make an empty column. Embarrassing

temp <- data.frame(nd=1:8618)
temp[1] <- NA

mydata <- cbind(mydata,temp)

for(i in 1:8618){
test <- mydata[i,]

mult <- 100 / test[1,4]

pctrdi <- c(mult * test$Protein_.g.     / 50
		   ,mult * test$Fiber_TD_.g.    / 25
		   ,mult * test$Calcium_.mg.    / 1000
		   ,mult * test$Iron_.mg.       / 18
		   ,mult * test$Magnesium_.mg.  / 400
		   ,mult * test$Phosphorus_.mg. / 1000
		   ,mult * test$Potassium_.mg.  / 3500
		   ,mult * test$Sodium_.mg.     / 2400
		   ,mult * test$Zinc_.mg.       / 15
		   ,mult * test$Copper_mg.      / 2
		   ,mult * test$Manganese_.mg.  / 2
##		   ,mult * test$Selenium_.æg.   / 70
		   ,mult * test[1,20]             / 70
		   ,mult * test$Vit_C_.mg.      / 60
		   ,mult * test$Thiamin_.mg.    / 1.5
		   ,mult * test$Riboflavin_.mg. / 1.7
		   ,mult * test$Niacin_.mg.     / 20
		   ,mult * test$Panto_Acid_mg.  / 10
		   ,mult * test$Vit_B6_.mg.     / 2
##		   ,mult * test$Folate_Tot_.æg. / 400
		   ,mult * test[1,27]             / 400
##		   ,mult * test$Vit_B12_.æg.    / 6
		   ,mult * test[1,32]             / 6
		   ,mult * test$Vit_A_IU        / 5000
		   ,mult * test$Vit_E_.mg.      / 21
		   ,mult * test$Vit_D_IU        / 400
##		   ,mult * test$Vit_K_.æg.      / 80)
		   ,mult * test[1,44]             / 80)
ndcomponents <- ifelse(pctrdi <= .05, 0, log10(pctrdi/.05))
mydata[i,54] <- sum(ndcomponents,na.rm = TRUE)
}
write.csv(mydata,"abbrev2.csv")

}