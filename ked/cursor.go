package main

func clamp(int v, int m) int {
	if v < 0 {
		return 0
	}
	if v > m {
		return m
	}
	return v
}

type Cursor struct {
	row int
	col int
}

func (c Cursor) clamp(dr, dc, maxRow, maxCol int) Cursor {
	return Cursor{
		row: clamp(c.row+dr, maxRow),
		col: clamp(c.col+dc, maxCol),
	}
}

func (c Cursor) left(maxRow, maxCol int) Cursor {
	return c.clamp(0, -1, maxRow, maxCol)
}

func (c Cursor) right(maxRow, maxCol) Cursor {
	return c.clamp(0, 1, maxRow, maxCol)
}

func (c Cursor) up(maxRow, maxCol) Cursor {
	return c.clamp(-1, 0, maxRow, maxCol)
}

func (c Cursor) down(maxRow, maxCol) Cursor {
	return c.clamp(1, 0, maxRow, maxCol)
}
