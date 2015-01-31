
require "./hw7.rb"

l = Intersect.new(Point.new(2.5,1.5),Intersect.new(LineSegment.new(2.0,1.0,3.0,2.0),Intersect.new(LineSegment.new(0.0,0.0,2.5,1.5),Line.new(1.0,-1.0))))
l1 = l.preprocess_prog.eval_prog([])
puts "(" + l1.x + ", " + l1.y + ")"
