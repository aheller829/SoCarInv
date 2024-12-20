## load some libs
library(aqp)

## munsell data comes with a lookup table in xyY colorspace

# Converts munsell to RGB or hexcodes for plotting
names(C)

munsell2rgb(
  C$Hue,
  C$Value,
  C$Chroma,
  alpha = 1,
  maxColorValue = 1,
  return_triplets = FALSE,
  returnLAB = TRUE
)


(C_color <- munsell2rgb(C$Hue, C$Value, C$Chroma))
soilPalette(C_color)

(FSL_color <- munsell2rgb(FSL$Hue, FSL$Value, FSL$Chroma))
soilPalette(FSL_color)

(sandsheet_color <- munsell2rgb(sandsheet$Hue, sandsheet$Value, sandsheet$Chroma))
soilPalette(sandsheet_color)


munsell2rgb(
  sandsheet$Hue,
  sandsheet$Value,
  sandsheet$Chroma,
  alpha = 1,
  maxColorValue = 1,
  return_triplets = FALSE,
  returnLAB = FALSE
)

