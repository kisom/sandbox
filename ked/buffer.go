package main

import (
	"os"
	"bufio"
	"fmt"
)

type Buffer struct {
	contents []string
	filename string
	cursor Cursor
	dirty bool
}

func NewBuffer(filename string) (*Buffer, error) {
	buffer := &Buffer{
		contents: []string{""},
		filename: filename,
		cursor: Cursor{row: 0, col: 0},
	}

	err := buffer.Load()
	return buffer, err
}

func (buffer *Buffer) Load() error {
	if buffer.filename == "" {
		return nil
	}
	
	file, err := os.Open(buffer.filename)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return err
	}
	defer file.Close()

	buffer.contents = []string{}
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		buffer.contents = append(buffer.contents, line)
		fmt.Printf("'%s'\n", line)
	}

	return nil
}

func (buffer *Buffer) Close() error {
	return nil
}
