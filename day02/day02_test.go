package day02

import (
	"testing"
)

func TestScoreRockPaperScissors(t *testing.T) {
	// Todo: refactor this to a loop (table tst)
	score := ScoreRockPaperScissors(Rock{}, Paper{})
	expected := 8

	if score != expected {
		t.Errorf("Expected %d, got %d", expected, score)
	}

	score = ScoreRockPaperScissors(Paper{}, Rock{})
	expected = 1

	if score != expected {
		t.Errorf("Expected %d, got %d", expected, score)
	}

	score = ScoreRockPaperScissors(Scissors{}, Scissors{})
	expected = 6

	if score != expected {
		t.Errorf("Expected %d, got %d", expected, score)
	}
}

func TestPuzzle1(t *testing.T) {
	rounds, err := ReadInput1("input")
	if err != nil {
		t.Fatal("Failed to load input")
	}

	score := PlayAllRounds(rounds)

	expected := 15422
	if score != expected {
		t.Errorf("Expected %d, got %d", expected, score)
	}
}

func TestPuzzle2(t *testing.T) {
	games, err := ReadInput2("input")
	if err != nil {
		t.Fatal("Failed to load input")
	}

	score := PlayAllRoundsPart2(games)

	expected := 15442
	if score != expected {
		t.Errorf("Expected %d, got %d", expected, score)
	}
}
