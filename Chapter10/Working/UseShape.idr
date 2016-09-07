module UseShape

import Shape

area : Shape -> Double
area shape with (shapeView shape)
  area (triangle base height) | STriangle = base * height / 2
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius
