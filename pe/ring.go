package main

type Message struct {
	RCount int // how many times around the ring have we been
	FCount int // how many times has the message been forwarded
}
