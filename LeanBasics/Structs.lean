structure Point where
  x: Float
  y: Float

def origin : Point := { x := 0.0, y := 0.0 }
#eval origin
#eval origin.x
#eval origin.y

def addPoints (p1: Point) (p2: Point) : Point :=
  {x := p1.x + p2.x, y := p1.y + p2.y}
#eval addPoints { x := 1.5, y := 32 } { x := -8, y := 0.2 }

def distance (p1: Point) (p2: Point) : Float :=
  Float.sqrt (((p2.x - p1.x)^2) + ((p2.y - p1.y)^2))
#eval distance { x := 1.0, y := 2.0 } { x := 5.0, y := -1.0 } -- 5

structure Point3D where
  x: Float
  y: Float
  z: Float

def origin3D : Point3D := { x := 0.0, y := 0.0, z := 0.0 }

-- #check { x := 0.0, y := 0.0 } Invalid because no type notation
#check { x := 0.0, y := 0.0 : Point}

/- updating structures
ex. we want to update the x value of Point
in most languages we would mean that the memory location pointed to by x will be overwritten with a new value
in functional programming langs a fresh Point is allocated with the x field pointting to a new value and all other fields pointing to the original values from the input
-/
def zeroX (p: Point): Point :=
  -- { x := 0.0, y := p.y}
  { p with x := 0 }

def fourAndThree : Point :=
  { x := 4.3, y := 3.4 }
#eval fourAndThree
#eval zeroX fourAndThree
#eval fourAndThree -- struct updates do not modify the original struct


-- constructors
#check Point.mk 1.5 2.8 -- { x := 1.5, y := 2.8 } : Point
#check (Point.mk) -- Point.mk : Float → Float → Point
-- rename constructor
structure PointWithRenamedConstructor where
  point ::
  x : Float
  y : Float
#check (PointWithRenamedConstructor.point)


/- getters
TARGET.f ARG1 ARG2 ...
if isinstance(TARGET, T) we call the function T.f
ex. we can call String.append directly on a string
-/
#eval "one string".append " and another" -- TARGET = "one string", ARG1 = " and another"
#check (Point.x) -- Point.x : Point -> Float
#check (Point.y) -- Point.y : Point -> Float


def Point.modifyBoth (f : Float -> Float) (p: Point): Point :=
  {x := f p.x, y := f p.y}

#eval fourAndThree.modifyBoth Float.floor

structure RectangularPrism where
  height: Float
  width: Float
  depth: Float

def RectangularPrism.volume (rp: RectangularPrism): Float :=
  rp.height * rp.width * rp.depth

structure Segment where
  starting_point: Point
  end_point: Point

def Segment.length (s: Segment): Float :=
  Float.sqrt ((s.end_point.x - s.starting_point.x)^2 + (s.end_point.y - s.starting_point.y)^2)

def origin2D : Point :=
  {x := 0, y := 0}

def randomPoint : Point :=
  {x := 1, y := 2}

def randomSegment : Segment :=
  {starting_point := origin2D, end_point := randomPoint : Segment}

#eval randomSegment.length
