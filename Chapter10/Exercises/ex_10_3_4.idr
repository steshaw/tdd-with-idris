import DataStore

{- 1 -}

getValues : DataStore (SString .+. val_schema) -> 
            List (SchemaType val_schema)
getValues input with (storeView input)
  getValues input | SNil = []
  getValues (addToStore (key, value) store) | (SAdd rec) 
       = value :: getValues store | rec

{- 2 -}

export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Square Double 
           | Circle Double

export
triangle : Double -> Double -> Shape 
triangle = Triangle

export
rectangle : Double -> Double -> Shape 
rectangle = Rectangle

export
circle : Double -> Shape 
circle = Circle

data ShapeView : Shape -> Type where
     STriangle : ShapeView (triangle base height)
     SRectangle : ShapeView (rectangle width height)
     SCircle : ShapeView (circle radius)

shapeView : (s : Shape) -> ShapeView s
shapeView (Triangle x y) = STriangle
shapeView (Rectangle x y) = SRectangle
shapeView (Circle x) = SCircle

area : Shape -> Double
area s with (shapeView s)
  area (triangle base height) | STriangle = 0.5 * base * height
  area (rectangle width height) | SRectangle = width * height
  area (circle radius) | SCircle = pi * radius * radius

