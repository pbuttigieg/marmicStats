require(vegan)
data("varespec")
data("varechem")


myCca <- cca(varespec, varechem)
ordiplot(myCca)
