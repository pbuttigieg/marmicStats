require(vegan)
require(pipeR)

pipeline(
  rda(scotchData_pal),
  plot(type = "t")
)


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
  rda(scotchData_pal ~ REGION,data = scotchData_meta),
  plot(type = "t")
)