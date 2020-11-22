library(ggplot2)

class ~ habitat + population  + spore.print.color + ring.number +
  stalk.color.above.ring + stalk.surface.above.ring + gill.size +
  odor + cap.shape + cap.surface + cap.color + bruises + veil.color

ggplot(dt) +
  geom_bar(aes(x = habitat, fill = class)) +
  facet_wrap("~population")
table(dt$habitat, dt$population, dt$class)
# contain 0 observations

ggplot(dt) +
  geom_bar(aes(x = habitat, fill = class)) +
  facet_wrap("~spore.print.color")
table(dt$habitat, dt$spore.print.color, dt$class)
# contain 0

ggplot(dt) +
  geom_bar(aes(x = cap.color, fill = class)) +
  facet_wrap("~veil.color")
table(dt$cap.color, dt$veil.color, dt$class)

ggplot(dt) +
  geom_bar(aes(x = odor, fill = class), color = "black") +
  scale_fill_brewer(palette="Blues", labels = c("Edible", "Poisonous")) +
  theme_classic() +
  labs(title = "Odor v Class by Habitat") +
  facet_wrap("~habitat", labeller = as_labeller(c("d" = "woods",
                                                  "g" = "grasses",
                                                  "l" = "leaves",
                                                  "m" = "meadows",
                                                  "artificial" = "artificial")))
table(dt$cap.color, dt$veil.color, dt$class)


ggplot(dt) + 
  geom_bar(aes(x = bruises, fill = class), color = "black") +
  scale_fill_brewer(palette="Blues", labels = c("Edible", "Poisonous")) + 
  scale_x_discrete("Bruises",labels = c("None", "Bruises")) +
  theme_classic() +
  labs(title = "Bruises v Class by Gill Size") +
  facet_wrap("~gill.size", labeller = as_labeller(c("b" = "broad",
                                                    "n" = "narrow")))
table(dt$gill.size, dt$bruises, dt$class)
