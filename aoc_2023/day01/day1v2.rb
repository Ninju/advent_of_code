#!/usr/bin/env ruby

REG = /(one|two|three|four|five|six|seven|eight|nine|[0-9])/
REVREG = /(eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|[0-9])/

DIGITMAP = {
  "one" => "1",
  "two" => "2",
  "three" => "3",
  "four" => "4",
  "five" => "5",
  "six" => "6",
  "seven" => "7",
  "eight" => "8",
  "nine" => "9",
}

REVDIGITMAP = {
  "eno" => "1",
  "owt" => "2",
  "eerht" => "3",
  "ruof" => "4",
  "evif" => "5",
  "xis" => "6",
  "neves" => "7",
  "thgie" => "8",
  "enin" => "9",
}

def last_digit(str)
  last_match = str.reverse.scan(REVREG).flatten.first
  REVDIGITMAP[last_match] || last_match
end

def first_digit(str)
  first_match = str.scan(REG).flatten.first
  DIGITMAP[first_match] || first_match
end


def main(filename)
  (File.readlines(filename).map(&:chomp).map do |line|

       frst = first_digit(line)
       lst = last_digit(line)

       (frst + lst).to_i
   end).sum
end

p main("input_2023_day1.txt")
