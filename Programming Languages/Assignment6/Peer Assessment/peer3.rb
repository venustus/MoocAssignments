# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
	All_My_Pieces = Piece::All_Pieces.concat([
		   rotations([[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]]),  # 1add
		   [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]], # long2 (only needs two)
		   [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
		   rotations([[0, 0], [1, 0], [0, 1]])]) # 3add
		   
	Cheat_Piece = [[[0, 0]]]
  # your enhancements here
   def self.next_piece (board)
	   if !board.ischeat?
	   then
		MyPiece.new(All_My_Pieces.sample, board)
	   else
		MyPiece.new(Cheat_Piece, board)
	   end
   end
end

class MyBoard < Board
  # your enhancements here
  def next_piece
	@current_block = MyPiece.next_piece(self)
	@current_pos = nil
	@ischeat = false
  end
  
  def rotate180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 1)
	  @current_block.move(0, 0, 1)
    end
    draw
  end
  
  def ischeat?
	@ischeat
  end
  
  def cheat
    if !game_over? and @game.is_running? and @score >= 100 and !@ischeat
		@ischeat = true	
		@score -= 100
    end
  end
  
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  def key_bindings 
	super
	@root.bind('u', proc {@board.rotate180})
    @root.bind('c', proc {@board.cheat}) 
  end
end


