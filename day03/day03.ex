defmodule Day03 do
  def priority_of(c) when c >= ?a and c <= ?z do
    c - ?a + 1
  end

  def priority_of(c) when c >= ?A and c <= ?Z do
    c - ?A + 27
  end

  def common_items(group) do
    Enum.map(group, &MapSet.new/1)
    |> Enum.reduce(&MapSet.intersection/2)
  end

  def split(r) do
    Enum.split(r, div(length(r), 2)) |> Tuple.to_list()
  end

  def solve_part1(rucksacks) do
    rucksacks
    |> Enum.map(&split/1)
    |> Enum.map(&common_items/1)
    |> Enum.concat()
    |> Enum.map(&priority_of/1)
    |> Enum.sum()
  end

  def group_rucksacks(rucksacks) do
    Enum.chunk_every(rucksacks, 3)
  end

  def solve_part2(rucksacks) do
    rucksacks
    |> group_rucksacks()
    |> Enum.map(&common_items/1)
    |> Enum.concat()
    |> Enum.map(&priority_of/1)
    |> Enum.sum()
  end

  def rucksacks_from_input(text) do
    String.split(text) |> Enum.map(&String.to_charlist/1)
  end
end
