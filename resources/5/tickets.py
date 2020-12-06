#!/usr/bin/env python

def parse_ticket(ticket):
    raw_row = ticket[:7]
    raw_col = ticket[7:]
    row = raw_row.replace("F", "0").replace("B", "1")
    col = raw_col.replace("R", "1").replace("L", "0")
    return int(row, 2) * 8 + int(col, 2)


with open("input", "r") as f:
    tickets = [parse_ticket(t) for t in f.read().split("\n")[:-1]]
    print(max(tickets))
    print(set(range(max(tickets))) - set(tickets))
