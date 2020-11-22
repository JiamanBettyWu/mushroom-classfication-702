setwd("~/Desktop/ids_702/assignment")

# load data
dt_original = read.csv("../data/mushrooms.csv", stringsAsFactors = TRUE)

# remove columns
dt = dt_original[, !colnames(dt_original) %in% c("veil.type", "odor") ]
# veil.type is removed because every row has the same category

c("veil.color", "stalk.color.below.ring", "stalk.surface.above.ring", "gill.color", "gill.attachment") 

dt$class = factor(dt$class, levels = c("e", "p"), labels = c("0", "1")) # convert class to 0, 1 variable

dt$population = factor(dt$population,
       levels = c("a", "c", "n", "s", "v", "y"),
       labels = c("many", "many", "many", "few", "few", "few"))

dt$ring.number = factor(dt$ring.number,
       levels = c("n", "o", "t"),
       labels = c("less than 2", "less than 2", "two"))
       
dt$stalk.color.above.ring = factor(dt$stalk.color.above.ring,
                        levels = c("b", "c", "e", "g", "n", "o", "p", "w", "y"),
                        labels = c("other","other","other","other","other","other","other", "white or yellow", "white or yellow"))             

dt$stalk.surface.above.ring = factor(dt$stalk.surface.above.ring,
       levels = levels(dt$stalk.surface.above.ring),
       labels = c("unsmooth", "unsmooth", "smooth", "smooth"))

# dt$odor = factor(dt$odor,
      # levels = levels(dt$odor),
      # labels = c("odor", "odor","odor","odor","odor", "none", "odor","odor","odor"))

dt$cap.color = factor(dt$cap.color,
       levels = levels(dt$cap.color),
       labels = c("bright", "bright","bright","dull","dull", "bright", "bright","bright","bright", "bright")) 

dt$cap.surface = factor(dt$cap.surface,
       levels = levels(dt$cap.surface),
       labels = c("f", "y","s","y"))   

dt$cap.shape = factor(dt$cap.shape,
       levels = levels(dt$cap.shape),
       labels = c("bell", "conical","flat","conical", "sunken", "convex")) 

dt$cap.shape = factor(dt$cap.shape,
       levels = levels(dt$cap.shape),
       labels = c("round", "other","flat","other", "round")) 

dt$habitat = factor(dt$habitat,
       levels = levels(dt$habitat),
       labels = c("d", "g","l","m", "artificial", "artificial", "artificial")) 
dt$habitat = relevel(dt$habitat, ref = "artificial") 

dt$spore.print.color = factor(dt$spore.print.color,
       levels = levels(dt$spore.print.color),
       labels = c("bright", "dull","dull","dull", "bright", "bright", "bright", "bright", "bright")) 


dt$gill.color = factor(dt$gill.color,
       levels = levels(dt$gill.color),
       labels = c("bright", "bright","dull","dull", "dull", "dull", "bright", "bright", "bright", "bright", "bright", "bright")) 

dt$veil.color = factor(dt$veil.color,
       levels = levels(dt$veil.color),
       labels = c("nonwhite", "nonwhite", "white", "nonwhite")) 

factor(dt$ring.type,
       levels = levels(dt$ring.type),
       labels = c("nonwhite", "nonwhite", "white", "nonwhite")) 
# ---------------------------------------------------------------- 
# move to logistic.R 

formula = class ~ cap.shape + cap.surface + cap.color + bruises + odor +
  gill.spacing + gill.size + stalk.surface.above.ring + stalk.color.above.ring +
  ring.number  + spore.print.color + population +
  habitat
# formula = class ~ odor

formula = class ~ habitat + population  + spore.print.color + ring.number +
  stalk.color.above.ring + stalk.surface.above.ring + gill.size +
  odor + cap.shape + cap.surface + cap.color + bruises + veil.color

model = glm(formula, data = dt, family = binomial, maxit = 100)
summary(model)
arm::binnedplot(fitted(model),residuals(model,"resp"),xlab="Pred. probabilities",col.int="red",
                ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model) >= 0.5, "1","0")),
                            dt$class,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"]
Conf_mat$byClass[c("Sensitivity","Specificity")]
roc(dt$class, fitted(model),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")

