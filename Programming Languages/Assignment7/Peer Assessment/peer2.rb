# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code) 

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np  # receive double dispatch
    np # could also have NoPoints.new here instead of in each sub class
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end
  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  def inbetween(v,end1,end2)
    ep=GeometryExpression::Epsilon
    (end1-ep <= v and v <= end2 +ep) or 
    (end2-ep <= v and v <= end1 +ep)
  end
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    Point.new(@x+dx,@y+dy)
  end
  def intersect other # start double-dispatch e1 is self and e2 is other.
    other.intersectPoint self 
  end
  def intersectPoint p # p is e1, self is e2 (a point) - receive double dispatch
    if real_close_point(p.x,p.y,@x,@y) then p else NoPoints.new end
  end
  def intersectLine line  # line is e1, self is e2 (a point) - receive double dispatch
    if real_close(@y,line.m*@x+line.b) then self else NoPoints.new end
  end
  def intersectVerticalLine vline  # vline is e1, self is e2 (a point) - receive double dispatch
    if real_close(vline.x,@x) then self else NoPoints.new end
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  #from super - def intersectLineSegment seg  # seg is e1, self is e2 (a Point) - receive double dispatch
  #  line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2)) # line_result is a point or noPoints
  #  line_result.intersectWithSegmentAsLineResult seg
  #end
  def intersectWithSegmentAsLineResult seg  # seg is e1, self is the intersect point  - receive dynamic dispatch
    #return the point if it is on the line segment, otherwise noPoint
    if inbetween(@x,seg.x1,seg.x2) and inbetween(@y,seg.y1,seg.y2) then self else NoPoints.new end
  end
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end
  ###
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-prossessing to do here
  end
  def shift(dx,dy)
    Line.new(@m,@b+dy-(@m*dx))
  end
  def intersect other # start double-dispatch e1 is self and e2 is other.
    other.intersectLine self 
  end
  def intersectPoint p # p is e1, self is e2 (a line)  - receive double dispatch
    if real_close(p.y,@m*p.x+@b) then p else NoPoints.new end
  end
  def intersectLine line # line is e1, self is e2 (a line)  - receive double dispatch
    if real_close(line.m,@m) then (if real_close(line.b,@b) then line else NoPoints.new end)
    else x=(@b-line.b)/(line.m-@m)
          y=(line.m*x+line.b)
          Point.new(x,y)  
    end 
  end
  def intersectVerticalLine vline  # vline is e1, self is e2 (a line)  - receive double dispatch
    Point.new(vline.x,@m*vline.x+@b)
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  #from super - def intersectLineSegment seg  # seg is e1, self is e2 (a Line) - receive double dispatch
  #  line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2)) # line_result is a point, a line or noPoints
      # lines cross at a point, coincide on the same line, or don't cross at all (parallel)
  #  line_result.intersectWithSegmentAsLineResult seg
  #end
  def intersectWithSegmentAsLineResult seg  # seg is e1, self is e2 (a line)  - receive dynamic dispatch
    seg # the segment e1 is on the line e2 because they coincide
  end

end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end
  ###
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-prossessing to do here
  end
  def shift(dx,dy)
    VerticalLine.new(x+dx)
  end
  def intersect other # start double-dispatch e1 is self and e2 is other.
    other.intersectVerticalLine self 
  end
  def intersectPoint p # p is e1, self is e2 (a vertical line)  - receive double dispatch
    if real_close(p.x,@x) then p else NoPoints.new end
  end
  def intersectLine line # line is e1, self is e2 (a vertical line)  - receive double dispatch
      Point.new(@x,line.m * @x +line.b)  
  end
  def intersectVerticalLine vline  # vline is e1, self is e2 (a vertical line)  - receive double dispatch
    if real_close(@x,vline.x) then vline else NoPoints.new end
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  #from super - def intersectLineSegment seg  # seg is e1, self is e2 (a VerticalLine) - receive double dispatch
  #  line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2)) # line_result is a point, a line, or noPoints
      # lineseg and vertline cross at a point, coincide on the same vertline, or don't cross at all (parallel)
  #  line_result.intersectWithSegmentAsLineResult seg
  #end
  def intersectWithSegmentAsLineResult seg  # seg is e1, self is e2 (a vertical line)  - receive dynamic dispatch
    seg   # seg is on this vertical line
  end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end
    ###
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog  # line segments get preprossed
    #puts ("in preprocess_prog")
    if real_close_point(x1,y1,x2,y2) then Point.new(x1,y1) # seg so tiny really a point.
    elsif real_close(x1,x2) 
        then if y1 < y2 then self else LineSegment.new(x2,y2,x1,y1) end # nearly vertical so put lower point first
        elsif x1 < x2 then self else LineSegment.new(x2,y2,x1,y1) end # put left point first
  end
  def shift(dx,dy)
    LineSegment.new(x1+dx,y1+dy,x2+dx,y2+dy)
  end
  def intersect other # start double-dispatch e1 is self and e2 is other.
    other.intersectLineSegment self 
  end
  def intersectPoint p # p is e1, self is e2 (a line segment)  - receive double dispatch
    p.intersectLineSegment self
  end
  def intersectLine line # line is e1, self is e2 (a line segment)  - receive double dispatch
    line.intersectLineSegment self
  end 
  def intersectVerticalLine vline  # vline is e1, self is e2 (a line segment)  - receive double dispatch
    vline.intersectLineSegment self
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  #from super - def intersectLineSegment seg  # seg is e1, self is e2 (a LineSegment) - receive double dispatch
  #  line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2)) # line_result is a point, a lineseg, or noPoints
      # lineseg and vertline cross at a point, coincide on the same vertline, or don't cross at all (parallel)
  #  line_result.intersectWithSegmentAsLineResult seg
  #end
  def intersectWithSegmentAsLineResult seg  # seg is e1, self is e2 (seg2 a line segment)  - receive double dispatch
    seg2=self
    x1start=seg.x1;   y1start=seg.y1;   x1end=seg.x2;   y1end=seg.y2   # seg
    x2start=seg2.x1;  y2start=seg2.y1;  x2end=seg2.x2;  y2end=seg2.y2  # seg2
#THIS PIECE OF CODE SUCKS>>>>>
    if real_close(x1start,x1end)
        #(* the segments are on a vertical line *)
        #(* let segment a start at or below start of segment b *)
      if y1start < y2start
        #(seg,seg2)
          aXstart=seg.x1;  aYstart=seg.y1;  aXend=seg.x2;   aYend=seg.y2;
          bXstart=seg2.x1; bYstart=seg2.y1; bXend=seg2.x2;  bYend=seg2.y2;
        else  #(seg2:seg) 
          bXstart=seg.x1;  bYstart=seg.y1;  bXend=seg.x2;   bYend=seg.y2;
          aXstart=seg2.x1; aYstart=seg2.y1; aXend=seg2.x2;  aYend=seg2.y2;
        end
      if real_close(aYend,bYstart) then Point.new(aXend,aYend) #(* just touching *)
        elsif aYend < bYstart      then NoPoints.new #(* disjoint *)
          elsif aYend > bYend      then LineSegment.new(bXstart,bYstart,bXend,bYend) #(* b inside a *)
            else LineSegment.new(bXstart,bYstart,aXend,aYend) #(* overlapping *)
            end
    else #(* the segments are on a (non-vertical) line *)
        #(* let segment a start at or to the left of start of segment b *)
      if x1start < x2start
        #(seg,seg2)
          aXstart=seg.x1;  aYstart=seg.y1;  aXend=seg.x2;   aYend=seg.y2;
          bXstart=seg2.x1; bYstart=seg2.y1; bXend=seg2.x2;  bYend=seg2.y2;
        else  #(seg2:seg) 
          bXstart=seg.x1;  bYstart=seg.y1;  bXend=seg.x2;   bYend=seg.y2;
          aXstart=seg2.x1; aYstart=seg2.y1; aXend=seg2.x2;  aYend=seg2.y2;
        end  
      if real_close(aXend,bXstart) then Point.new(aXend,aYend) #(* just touching *)
        elsif aXend < bXstart      then NoPoints.new  #(* disjoint *)
          elsif aXend > bXend      then LineSegment.new(bXstart,bYstart,bXend,bYend) #(* b inside a *)
            else LineSegment.new(bXstart,bYstart,aXend,aYend) #(* overlapping *)
            end
          end
    end  
  end


# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end
  ###
  def eval_prog env # eval both e1 and e2 to values, then start double dispatch for interset with e1 using e2 as other
    @e1.eval_prog(env).intersect @e2.eval_prog(env)
  end
  def preprocess_prog  # both exp in intersect get preprossed if linesegs
     Intersect.new(@e1.preprocess_prog,@e2.preprocess_prog)
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end
  ###
  def eval_prog env   # add a new item to the env array which starts out as [] = empty array
    #puts "in eval_prog of let"
    newenv=[[ @s , @e1.eval_prog(env) ]] + env
    #puts "new.env  #{newenv.to_s}"
    #puts "e2 is #{@e2.to_s}"
    @e2.eval_prog(newenv)
  end
  def preprocess_prog  # exp in Let get preprossed if linesegs
     Let.new(@s,@e1.preprocess_prog,@e2.preprocess_prog)
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def eval_prog env # remember: do not change this method
    #puts "in eval_prog of Var"
    #puts "env is  #{env.to_s} and s is #{@s}"
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
    ###
  def preprocess_prog  # nothing to do with Var string here
     self
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end
    ###
  def eval_prog env 
    @e.shift(@dx,@dy)
  end
  def preprocess_prog  # exp in Let get preprossed if linesegs 
     Shift.new(@dx,@dy,@e.preprocess_prog)
  end
end
