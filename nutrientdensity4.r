nutrientdensity4 <- function(){

## This tool summarized the entire SR27
## into individual nutrient density score
## and reports percent of recommended dietary
## intake for each

mydata <- read.csv("ABBREV.csv")

## I don't know how to make an empty column. Embarrassing

temp <- data.frame(nd=1:8618)
temp[1] <- NA

ndsummary <- data.frame()

## This if statement just comments out me trying to set colnames of an empty data.frame)

if(FALSE){colnames(ndsummary) <- c("food"
                        ,"protein"
                        ,"fiber"
                        ,"calcium"
                        ,"iron"
                        ,"magnesium"
                        ,"phosphorous"
                        ,"potassium"
                        ,"sodium"
                        ,"zinc"
                        ,"copper"
                        ,"manganese"
                        ,"selenium"
                        ,"vitc"
                        ,"thiamin"
                        ,"riboflavin"
                        ,"niacin"
                        ,"pantothenicacid"
                        ,"vitb6"
                        ,"folate"
                        ,"vitb12"
                        ,"vita"
                        ,"vite"
                        ,"vitd"
                        ,"vitk"
						,"nd") }
mydata <- cbind(mydata,temp)

for(i in 1:8618){
test <- mydata[i,]

if(grepl(",RAW",as.character(test$Shrt_Desc))){
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

ndsummary[i,"food"] <- test$Shrt_Desc
ndsummary[i,"kalescale"] <- round(100 * sum(ndcomponents,na.rm = TRUE) / 17.4691468003112)
ndsummary[i,"nd"] <- sum(ndcomponents,na.rm = TRUE)
ndsummary[i,"protein"] <- (mult * test$Protein_.g.     / 50)
ndsummary[i,"fiber"] <- mult * test$Fiber_TD_.g.    / 25
ndsummary[i,"calcium"] <- mult * test$Calcium_.mg.    / 1000
ndsummary[i,"iron"] <- mult * test$Iron_.mg.       / 18
ndsummary[i,"magnesium"] <- mult * test$Magnesium_.mg.  / 400
ndsummary[i,"phosphorous"] <- mult * test$Phosphorus_.mg. / 1000
ndsummary[i,"potassium"] <- mult * test$Potassium_.mg.  / 3500
ndsummary[i,"sodium"] <- mult * test$Sodium_.mg.     / 2400
ndsummary[i,"zinc"] <- mult * test$Zinc_.mg.       / 15
ndsummary[i,"copper"] <- mult * test$Copper_mg.      / 2
ndsummary[i,"manganese"] <- mult * test$Manganese_.mg.  / 2
ndsummary[i,"selenium"] <- mult * test[1,20]             / 70
ndsummary[i,"vitc"] <- mult * test$Vit_C_.mg.      / 60
ndsummary[i,"thiamin"] <- mult * test$Thiamin_.mg.    / 1.5
ndsummary[i,"riboflavin"] <- mult * test$Riboflavin_.mg. / 1.7
ndsummary[i,"niacin"] <- mult * test$Niacin_.mg.     / 20
ndsummary[i,"pantothenicacid"] <- mult * test$Panto_Acid_mg.  / 10
ndsummary[i,"vitb6"] <- mult * test$Vit_B6_.mg.     / 2
ndsummary[i,"folate"] <- mult * test[1,27]             / 400
ndsummary[i,"vitb12"] <- mult * test[1,32]             / 6
ndsummary[i,"vita"] <- mult * test$Vit_A_IU        / 5000
ndsummary[i,"vite"] <- mult * test$Vit_E_.mg.      / 21
ndsummary[i,"vitd"] <- mult * test$Vit_D_IU        / 400
ndsummary[i,"vitk"] <- mult * test[1,44]             / 80
}


}

ndsummary <- ndsummary[complete.cases(ndsummary),]

ndsummary <- ndsummary[order(-ndsummary$nd),]



write.csv(ndsummary,"abbrev4.csv")

}