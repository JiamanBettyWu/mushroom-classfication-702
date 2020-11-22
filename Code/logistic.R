library(caret)
library(pROC)
library(rms)
library(arm)
# FULL MODEL 
formula = class ~ habitat + population  + spore.print.color +
  stalk.color.above.ring + stalk.surface.above.ring + gill.size + gill.color + stalk.shape +
  cap.shape + cap.surface + cap.color + bruises + veil.color


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


# BIC STEPWISE MODEL SELECTION 
NullModel = glm(class ~ 1, data = dt, family = binomial)


Model_forward = step(NullModel, scope = formula(model),direction="both",trace=0, k = log(nrow(dt)))
Model_forward$formula

# BIC 
model_formula = class ~ gill.size + stalk.surface.above.ring + stalk.color.above.ring + 
  cap.surface + population + habitat + cap.color + veil.color + 
  stalk.shape + spore.print.color + gill.color

# more interactions

model_formula = class ~ gill.size + stalk.surface.above.ring + stalk.color.above.ring + 
  cap.surface + population + habitat + cap.color + veil.color + 
  stalk.shape + spore.print.color + gill.color +
  cap.surface * stalk.color.above.ring + gill.size * cap.shape + 
  gill.size*cap.shape


# ANOVA
final = glm(model_formula, data = dt, family = binomial)
summary(final)

arm::binnedplot(fitted(final),residuals(final,"resp"),xlab="Pred. probabilities",col.int="red",
                ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

anova(model, final, test = "Chisq") # >0.05 drop

# final 
library(sjPlot)
tab_model(final)
summary(final)

# ASSESSMENT
# residules v fitted prob
# ???
arm::binnedplot(fitted(final),residuals(final,"resp"),xlab="Pred. probabilities",col.int="red",
                ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(final) >= 0.5, "1","0")),
                            dt$class,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"]
Conf_mat$byClass[c("Sensitivity","Specificity")]
roc(dt$class, fitted(final),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3", main = "ROC: Logistic Regression")
vif(final)

# ?VALIDATION



formula = class ~habitat + population  + spore.print.color +
  stalk.color.above.ring + stalk.surface.above.ring +
  gill.size + gill.color + stalk.shape +
  cap.shape + cap.surface + cap.color + bruises + veil.color


  
formula = class ~habitat + population  + spore.print.color +
  stalk.color.above.ring + stalk.surface.above.ring +
  gill.size + gill.color + stalk.shape +
  cap.shape + cap.surface + cap.color + bruises + veil.color + 
  gill.size * cap.surface +
  gill.size * spore.print.color +
  spore.print.color * stalk.shape +
  bruises*gill.color +
  stalk.shape * stalk.surface.above.ring +
  spore.print.color * stalk.surface.above.ring +
  cap.color * stalk.surface.above.ring +
  cap.color * gill.color



model = glm(formula, data = dt, family = binomial)
summary(model)

arm::binnedplot(fitted(model),residuals(model,"resp"),xlab="Pred. probabilities",col.int="red",
                ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")


# **
# 1

class ~ habitat + population  + spore.print.color +
  stalk.color.above.ring + stalk.surface.above.ring +
  gill.size + gill.color + stalk.shape +
  cap.shape + cap.surface + cap.color + bruises + veil.color

formula = class ~ habitat + population+ spore.print.color + stalk.color.above.ring +
  + gill.size + gill.color + stalk.shape + 
  cap.shape + cap.surface + cap.color  + bruises 
  
model = glm(formula, data = dt, family = binomial)
summary(model)

arm::binnedplot(fitted(model),residuals(model,"resp"),xlab="Pred. probabilities",col.int="red",
                ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model) >= 0.5, "1","0")),
                            dt$class,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"]
Conf_mat$byClass[c("Sensitivity","Specificity")]
roc(dt$class, fitted(model),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3", main = "ROC: Logistic Regression")
vif(model)

# 2. BIC 

NullModel = glm(class ~ 1, data = dt, family = binomial)
Model_forward = step(NullModel, scope = formula(model),direction="both",trace=0, k = log(nrow(dt)))
Model_forward$formula

formula = class ~ gill.size + bruises + stalk.color.above.ring + 
  cap.color + cap.surface + spore.print.color + stalk.shape

model_bic = glm(Model_forward$formula, data = dt, family = binomial)
summary(model_bic)

arm::binnedplot(fitted(model_bic),residuals(model_bic,"resp"),xlab="Pred. probabilities",col.int="red",
                ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_bic) >= 0.5, "1","0")),
                            dt$class,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"]
Conf_mat$byClass[c("Sensitivity","Specificity")]
roc(dt$class, fitted(model_bic),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3", main = "ROC: Logistic Regression")
anova(model, model_bic, test = "Chisq")

# BIC + Interactions

formula = class ~ gill.size + bruises + stalk.color.above.ring + 
  cap.color + cap.surface + spore.print.color + stalk.shape +
  cap.surface * cap.color + 
  gill.size * cap.surface +
  spore.print.color * stalk.shape +
  spore.print.color* cap.surface + 
  gill.size * spore.print.color
  
  

model_int = glm(formula, data = dt, family = binomial)
summary(model_int)

arm::binnedplot(fitted(model_int),residuals(model_int,"resp"),xlab="Pred. probabilities",col.int="red",
                ylab="Avg. residuals",main="Binned residual plot",col.pts="navy")

Conf_mat <- confusionMatrix(as.factor(ifelse(fitted(model_int) >= 0.5, "1","0")),
                            dt$class,positive = "1")
Conf_mat$table
Conf_mat$overall["Accuracy"]
Conf_mat$byClass[c("Sensitivity","Specificity")]
roc(dt$class, fitted(model_int),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3", main = "ROC: Logistic Regression")
anova(model, model_bic, test = "Chisq")
