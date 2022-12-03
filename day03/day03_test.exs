ExUnit.start()

defmodule Day03.Test do
  use ExUnit.Case, async: true

  test "priorities" do
    assert Day03.priority_of(?p) == 16
    assert Day03.priority_of(?A) == 27
    assert Day03.priority_of(?P) == 42
    assert Day03.priority_of(?L) == 38
  end

  test "common items, in pairs" do
    assert Day03.common_items(['vJrwpWtwJgWr', 'hcsFMMfFFhFp']) == MapSet.new([?p])
    assert Day03.common_items(['jqHRNqRjqzjGDLGL', 'rsFMfFZSrLrFZsSL']) == MapSet.new([?L])
  end

  test "common items, in threes" do
    input = ['vJrwpWtwJgWrhcsFMMfFFhFp', 'jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL', 'PmmdzqPrVvPwwTWBwg']
    assert Day03.common_items(input) == MapSet.new([?r])

    input = ['wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn', 'ttgJtRGJQctTZtZT', 'CrZsJsPPZsGzwwsLwLmpwMDw']
    assert Day03.common_items(input) == MapSet.new([?Z])
  end

  test "splitting into compartments" do
    assert Day03.split('vJrwpWtwJgWrhcsFMMfFFhFp') == [
             'vJrwpWtwJgWr',
             'hcsFMMfFFhFp'
           ]
  end

  test "Example part 1" do
    assert example_input() |> Day03.rucksacks_from_input() |> Day03.solve_part1() == 157
  end

  test "Puzzle part 1" do
    assert puzzle_input() |> Day03.rucksacks_from_input() |> Day03.solve_part1() == 8493
  end

  test "Example part 2" do
    assert example_input() |> Day03.rucksacks_from_input() |> Day03.solve_part2() == 70
  end

  test "Puzzle part 2" do
    assert puzzle_input() |> Day03.rucksacks_from_input() |> Day03.solve_part2() == 2552
  end

  def puzzle_input() do
    File.read!("input")
  end

  def example_input() do
    "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"
  end
end
