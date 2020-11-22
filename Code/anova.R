
formA = class ~habitat + population  + spore.print.color +
  stalk.color.above.ring + stalk.surface.above.ring +
  gill.size + gill.color + stalk.shape +
  cap.shape + cap.surface + cap.color + bruises + veil.color + 
  gill.size * cap.surface +
  spore.print.color * stalk.shape +
  spore.print.color* cap.surface + 
  gill.size * spore.print.color + 
  population* bruises  

modelA = glm(formA, data = dt, family = binomial)

formB = class ~habitat + population  + spore.print.color +
  stalk.color.above.ring + stalk.surface.above.ring +
  gill.size + gill.color + stalk.shape +
  cap.shape + cap.surface + cap.color + bruises + veil.color 

modelB = glm(formB, data = dt, family = binomial)

anova(modelA, modelB, test = "Chisq")
