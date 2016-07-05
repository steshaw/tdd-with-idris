data Shape = ||| A triangle, with its base length and height
             Triangle Double Double
           | ||| A rectangle, with its length and height
              Rectangle Double Double
           | ||| A circle, with its radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

test_picture : Picture
test_picture = Combine (Translate 5 5 rectangle)
               (Combine (Translate 35 5 circle)
               (Translate 15 25 triangle))

picture_area : Picture -> Double
picture_area (Primitive shape) = area shape
picture_area (Combine pic pic1) = picture_area pic + picture_area pic1
picture_area (Rotate x pic) = picture_area pic
picture_area (Translate x y pic) = picture_area pic
