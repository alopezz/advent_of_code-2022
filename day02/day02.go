package day02

import (
	"os"
	"strings"
)

const (
	winScore  = 6
	drawScore = 3
	loseScore = 0
)

type HandShape interface {
	Score() int
	Weakness() HandShape // The shape that would beat me
	Strength() HandShape // The shape that would lose against me
}

type Rock struct{}
type Paper struct{}
type Scissors struct{}

func (Rock) Score() int {
	return 1
}

func (Rock) Weakness() HandShape {
	return Paper{}
}

func (Rock) Strength() HandShape {
	return Scissors{}
}

func (Paper) Score() int {
	return 2
}

func (Paper) Weakness() HandShape {
	return Scissors{}
}

func (Paper) Strength() HandShape {
	return Rock{}
}

func (Scissors) Score() int {
	return 3
}

func (Scissors) Weakness() HandShape {
	return Rock{}
}

func (Scissors) Strength() HandShape {
	return Paper{}
}

func makeHandShape(s string) HandShape {
	switch s {
	case "X":
		return Rock{}
	case "Y":
		return Paper{}
	case "Z":
		return Scissors{}
	case "A":
		return Rock{}
	case "B":
		return Paper{}
	case "C":
		return Scissors{}

	default:
		return nil
	}
}

type Round struct {
	me  HandShape
	opp HandShape
}

func ScoreRockPaperScissors(opponent HandShape, me HandShape) int {
	score := me.Score()
	switch {
	case opponent == me:
		score += drawScore
	case opponent == me.Strength():
		score += winScore
	case opponent == me.Weakness():
		score += loseScore
	}
	return score
}

func ScoreRockPaperScissorsPart2(opponent string, result string) int {
	var myHand HandShape
	opponentHand := makeHandShape(opponent)

	switch result {
	case "lose":
		myHand = opponentHand.Strength()
	case "draw":
		myHand = opponentHand
	case "win":
		myHand = opponentHand.Weakness()
	}

	return ScoreRockPaperScissors(opponentHand, myHand)
}

func convertStringResult(s string) string {
	switch s {
	case "X":
		return "lose"
	case "Y":
		return "draw"
	default:
		return "win"
	}
}

func PlayAllRounds(rounds []Round) int {
	score := 0
	for _, round := range rounds {
		score += ScoreRockPaperScissors(round.opp, round.me)
	}
	return score
}

func PlayAllRoundsPart2(rounds [][]string) int {
	score := 0
	for _, round := range rounds {
		round := RoundForPart2(round)
		score += ScoreRockPaperScissorsPart2(round[0], round[1])
	}
	return score
}

func RoundFromString(line string) Round {
	entry := strings.Split(line, " ")
	return Round{
		me:  makeHandShape(entry[1]),
		opp: makeHandShape(entry[0]),
	}
}

func RoundForPart2(entry []string) []string {
	entry[0] = entry[0]
	entry[1] = convertStringResult(entry[1])
	return entry
}

func ReadLines(filename string) ([]string, error) {
	contents, err := os.ReadFile(filename)
	if err != nil {
		return []string{}, err
	}

	return strings.Split(string(contents), "\n"), nil
}

func ReadInput1(filename string) ([]Round, error) {
	rounds := []Round{}
	lines, err := ReadLines(filename)
	if err != nil {
		return rounds, err
	}

	for _, line := range lines {
		rounds = append(rounds, RoundFromString(line))
	}
	return rounds, nil
}

func ReadInput2(filename string) ([][]string, error) {
	rounds := [][]string{}
	lines, err := ReadLines(filename)
	if err != nil {
		return rounds, err
	}

	for _, line := range lines {
		rounds = append(rounds, strings.Split(line, " "))
	}
	return rounds, nil
}
