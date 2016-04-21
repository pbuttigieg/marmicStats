require(vegan)
require(pipeR)

pipeline(
  rda(scotchData_pal),
  plot(type = "t")
)

scotchData_pal.rda <- rda(scotchData_pal)
screeplot(scotchData_pal.rda)

noseRda <- pipeline(
  rda(scotchData_nose)
)

palRda <-pipeline(
  rda(scotchData_pal)
)

pipeline(
  procrustes(palRda, noseRda),
  plot
)


pipeline(
  rda(scotchData_pal ~ REGION + AGE,data = scotchData_meta),
  plot(type = "t")
)

myRda <- rda(scotchData_pal ~ REGION + AGE,data = scotchData_meta)
