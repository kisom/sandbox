package main

import (
	"fmt"
	"time"
)

type Message struct {
	TTL    int
	Parent chan Message
}

func process(from, next chan Message, id int) {
	fmt.Printf("PID %d: from=%p, next=%p\n", id, from, next)
	for {
		message, ok := <-from
		if !ok {
			return
		}

		fmt.Printf("process %d: message: %+v\n", message)
		message.TTL--
		if message.TTL == 0 {
			message.Parent <- message
		}

		next <- message
	}
}

func genProcess(count int) chan Message {
	fmt.Printf("spawn: %p, count=%d\n", from, count)
	next := make(chan Message, 0)
	count--
	go process(from, next, count)
	if count == 0 {
		return from
	}

	return genProcess(next, count)
}

func main() {
	fmt.Println("startup")
	entrypoint := make(chan Message, 0)
	ring := genProcess(entrypoint, 3)
	fmt.Printf("%p\t%p\n", ring, entrypoint)
	time.Sleep(1)
	rcv := make(chan Message, 0)
	message := Message{3, rcv}
	message = <-rcv
	fmt.Println(message)
}
