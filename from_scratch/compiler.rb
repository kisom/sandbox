#!/usr/bin/env ruby

# lexer(tokeniser)->parser->code generator

class Tokenizer
  TOKEN_TYPES = [
    [:def, /\bdef\b/],
    [:end, /\bend\b/],
    [:identifier, /\b[a-zA-Z]+\b/],
    [:integer, /\b[0-9]+\b/],
    [:oparen, /\(/],
    [:cparen, /\)/],
  ]

  def initialize(code)
    @code = code
  end

  def tokenize
    tokens = []
    until @code.empty?
      token = tokenize_one
      tokens << token
      @code = @code.strip
    end
    return  tokens
  end

  def tokenize_one
    TOKEN_TYPES.each do |type, re|
      re = /\A(#{re})/
      if @code =~ re
        value = $1
        @code = @code[value.length..-1]
        return Token.new(type, value)
      end
    end
  end
end

Token = Struct.new(:type, :value)

# 11m30s
class Parser
  def initialize(tokens)
    @tokens = tokens
  end

  def parse
  end
end


tokens = Tokenizer.new(File.read("test.src")).tokenize
puts tokens.map(&:inspect).join("\n")
tree = Parser.new(tokens).parse
p tree
