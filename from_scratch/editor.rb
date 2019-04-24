#!/usr/bin/env ruby
# This is a simple forth editor written in Ruby, following along with
# the DAS "from scratch" text editor tutorial.

require "io/console"

class Editor

  def initialize(filename="")
    if filename.length > 0
      if File.exists? filename
        lines = File.readlines(filename).map do |line|
          line.sub(/\n$/, "")
        end
      else
        lines = ['']        
      end
    else
      filename = "untitled.txt"      
      lines = ['']
    end
    @filename = filename
    @buffer = Buffer.new(lines)
    @cursor = Cursor.new
    @history = []
  end

  def run
    IO.console.raw do
      loop do
        render
        handle_input
      end
    end

  rescue
    50.times { puts }
    raise
  end

  def render
    ANSI.clear_screen
    ANSI.move_cursor(0, 0)
    @buffer.render
    ANSI.move_cursor(@cursor.row, @cursor.col)
  end

  def handle_input
    char = $stdin.getc
    case char
    when "\C-q" then exit(0)
    # operating on the cursor makes undo easier
    when "\C-p" then @cursor = @cursor.up(@buffer)
    when "\C-n" then @cursor = @cursor.down(@buffer)
    when "\C-b" then @cursor = @cursor.left(@buffer)
    when "\C-f" then @cursor = @cursor.right(@buffer)
    when "\C-s" then @buffer.write(@filename)
    when "\C-u" then restore_snapshot
    when "\C-e" then
      @cursor = @cursor.move_to_col(@buffer.line_length(@cursor.row))
    when "\C-a" then @cursor = @cursor.move_to_col(0)
    when "\C-r" then forth
    when 127.chr
      if @cursor.col > 0
        save_snapshot
        @buffer = @buffer.delete(@cursor.row, @cursor.col-1)
        @cursor = @cursor.left(@buffer)
      end
    when "\r"
      # in raw mode, enter sends \r
      save_snapshot
      @buffer = @buffer.split_line(@cursor.row, @cursor.col)
      @cursor = @cursor.down(@buffer).move_to_col(0)
    else
      save_snapshot
      # keep buffer immutable so we can do undo
      @buffer = @buffer.insert(char, @cursor.row, @cursor.col)
      # lot of the implemenation of basic actions is ensuring
      # cursor consistency
      @cursor = @cursor.right(@buffer)
    end
  end

  def save_snapshot
    @history <<[@buffer, @cursor]
  end

  def restore_snapshot
    if @history.length > 0
      @buffer, @cursor = @history.pop
    end
  end

  def forth
    ANSI.clear_screen
    puts "gforth {@filename}\r\n"
    puts "------\r\n"
    output = `gforth #{@filename}`.split("\n")
    Buffer.new(output).render
    puts "-----\r\n"
    puts "Press <enter> to exit...\r\n"
    
    loop do
      char = $stdin.getc
      case char
      when "\r" then break
      end
    end
  end
end

class Buffer
  def initialize(lines)
    @lines = lines
  end

  def render
    @lines.each do |line|
      $stdout.write(line + "\r\n")
    end
  end

  def insert(char, row, col)
    lines = @lines.map(&:dup)
    lines.fetch(row).insert(col, char)
    Buffer.new(lines)
  end

  def delete(row, col)
    lines = @lines.map(&:dup)
    lines.fetch(row).slice!(col)
    Buffer.new(lines)
  end

  def split_line(row, col)
    lines = @lines.map(&:dup)
    line = lines.fetch(row)
    lines[row..row] = [line[0...col], line[col..-1]]
    Buffer.new(lines)
  end

  def line_count
    @lines.count
  end

  def line_length(row)
    @lines.fetch(row).length
  end

  def write(filename)
    File.write(filename, @lines.join("\n"))
  end
end

class Cursor
  attr_reader :row, :col
  def initialize(row=0, col=0)
    @row = row
    @col = col
  end

  def left(buffer)
    Cursor.new(@row, @col-1).clamp(buffer)
  end

  def right(buffer)
    Cursor.new(@row, @col+1).clamp(buffer)
  end

  def up(buffer)
    Cursor.new(@row-1, @col).clamp(buffer)
  end

  def down(buffer)
    Cursor.new(@row+1, @col).clamp(buffer)
  end

  def clamp(buffer)
    row = @row.clamp(0, buffer.line_count - 1)
    col = @col.clamp(0, buffer.line_length(row))
    Cursor.new(row, col)
  end

  def move_to_col(col)
    Cursor.new(row, col)
  end
end

class ANSI
  def self.clear_screen
    $stdout.write("\e[2J")
  end

  def self.move_cursor(row, col)
    $stdout.write("\e[#{row+1};#{col+1}H")
  end
end

filename = ""
if ARGV.length > 0
  filename = ARGV[0]
end
Editor.new(filename).run
