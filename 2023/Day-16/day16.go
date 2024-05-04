package main

import (
	"fmt"
	"os"
	"strings"
	"sync"
)

type Vec2D struct {
	x, y int
}

type Beam struct {
	loc     Vec2D
	heading Vec2D
	doneCh  chan bool
}

func (b *Beam) step() {
	b.loc.x += b.heading.x
	b.loc.y += b.heading.y
}

func (b *Beam) key() struct {
	loc     Vec2D
	heading Vec2D
} {
	return struct {
		loc     Vec2D
		heading Vec2D
	}{b.loc, b.heading}
}

type ConcurrentSet struct {
	m *sync.Map
}

func NewConcurrentSet() *ConcurrentSet {
	return &ConcurrentSet{&sync.Map{}}
}

func (s *ConcurrentSet) Add(v interface{}) {
	s.m.Store(v, true)
}

func (s *ConcurrentSet) Contains(v interface{}) bool {
	_, ok := s.m.Load(v)
	return ok
}

func (s *ConcurrentSet) Len() int64 {
	var count int64 = 0
	s.m.Range(func(_, _ interface{}) bool {
		count++
		return true
	})
	return count
}

func get_n_energized(loc, heading Vec2D, grid []string, n_rows, n_cols int) int64 {
	var energized_set = NewConcurrentSet()
	var unique_beams = NewConcurrentSet()

	spawnCh := make(chan Beam)

	run_beam := func(beam Beam) {
		wait_child_beams := func(children []Beam) {
			wait_channels := []chan bool{}
			for _, child := range children {
				child_key := child.key()
				if !unique_beams.Contains(child_key) {
					unique_beams.Add(child_key)
					wait_channels = append(wait_channels, child.doneCh)
					spawnCh <- child
				}
			}
			for _, ch := range wait_channels {
				<-ch
			}
		}

		for {
			beam.step()
			if beam.loc.x < 0 || beam.loc.x >= n_cols || beam.loc.y < 0 || beam.loc.y >= n_rows {
				// beam is out of bounds
				break
			}

			energized_set.Add(beam.loc)

			if grid_char := grid[beam.loc.y][beam.loc.x]; grid_char == '|' && beam.heading.x != 0 {
				b1 := Beam{beam.loc, Vec2D{0, -1}, make(chan bool)}
				b2 := Beam{beam.loc, Vec2D{0, 1}, make(chan bool)}
				wait_child_beams([]Beam{b1, b2})
				break
			} else if grid_char == '-' && beam.heading.y != 0 {
				b1 := Beam{beam.loc, Vec2D{-1, 0}, make(chan bool)}
				b2 := Beam{beam.loc, Vec2D{1, 0}, make(chan bool)}
				wait_child_beams([]Beam{b1, b2})
				break
			} else if grid_char == '/' {
				b := Beam{beam.loc, Vec2D{-beam.heading.y, -beam.heading.x}, make(chan bool)}
				wait_child_beams([]Beam{b})
				break
			} else if grid_char == '\\' {
				b := Beam{beam.loc, Vec2D{beam.heading.y, beam.heading.x}, make(chan bool)}
				wait_child_beams([]Beam{b})
				break
			}
		}

		beam.doneCh <- true
	}

	// goroutine to spawn beams
	go func() {
		for beam := range spawnCh {
			go run_beam(beam)
		}
	}()

	start_beam := Beam{loc, heading, make(chan bool)}
	spawnCh <- start_beam
	<-start_beam.doneCh

	return energized_set.Len()
}

func get_n_energized_ch(loc, heading Vec2D, grid []string, n_rows, n_cols int, reduceCh chan<- int64) {
	reduceCh <- get_n_energized(loc, heading, grid, n_rows, n_cols)
}

func main() {
	var filename string = os.Args[1]
	data, err := os.ReadFile(filename)
	if err != nil {
		panic(err)
	}

	grid := strings.Split(strings.Trim(string(data), "\n"), "\n")
	n_rows, n_cols := len(grid), len(grid[0])
	fmt.Printf("Part 1: %d\n", get_n_energized(Vec2D{-1, 0}, Vec2D{1, 0}, grid, n_rows, n_cols))

	var max_energized int64 = 0
	reduceCh := make(chan int64)
	for y := range n_rows {
		go get_n_energized_ch(Vec2D{-1, y}, Vec2D{1, 0}, grid, n_rows, n_cols, reduceCh)
		go get_n_energized_ch(Vec2D{n_cols, y}, Vec2D{-1, 0}, grid, n_rows, n_cols, reduceCh)
	}
	for x := range n_cols {
		go get_n_energized_ch(Vec2D{x, -1}, Vec2D{0, 1}, grid, n_rows, n_cols, reduceCh)
		go get_n_energized_ch(Vec2D{x, n_rows}, Vec2D{0, -1}, grid, n_rows, n_cols, reduceCh)
	}

	for range 2*n_rows + 2*n_cols {
		max_energized = max(max_energized, <-reduceCh)
	}
	fmt.Printf("Part 2: %d\n", max_energized)
}
