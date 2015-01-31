# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [rotations([[0, 0], [1, 0], [0, 1]]),                  # Enhancement - Three
                                rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]),  # Enhancement - P
                                [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],          # Enhancement - Very long
                                  [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]]]

  Cheat_Piece = [[[0, 0]]];

  # Override
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece, board)
  end
end

class MyBoard < Board

  #Override
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheat_mode = false
  end

  # Override
  def next_piece
    if @cheat_mode
      @current_block = MyPiece.cheat_piece(self)
      @cheat_mode = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  # Override
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size - 1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
        @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def rotate_180
    rotate_clockwise
    rotate_clockwise
  end

  def cheat
    if score >= 100 and !@cheat_mode
      @score -= 100
      @cheat_mode = true
    end
  end
end

class MyTetris < Tetris

  # Override
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end
end
