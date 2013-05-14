-- Vocabulary
-- We have two sorts, Number and Exchange:
-- Number(x) | Exchange(x)
-- ~(Number(x) & Exchange(x)) --

-- Numbers can be InService or OutOfService but it looks like they don't have to:
-- InService(x) => Number(x) --
-- OutOfService(x) => Number(x) --

-- Types for predicates and decisions:
-- GetExchange(x,y) => Number(x) & Exchange(y) --
-- Toll(x,y) => Number(x) & Number(y) --
-- TollFree(x,y) => Number(x) & Number(y) --

-- Policy
GetExchange(src, e1) & GetExchange(dest, e1) => TollFree(src, dest)
GetExchange(src, e2) & GetExchange(dest, e3) => Toll(src, dest) | (e2 = e3)
--GetExchange(src,e2) & GetExchange(dest,e3) & NE(e2,e3) => Toll(src,dest)

-- OutOfService(src) & Number(dest) => Refuse(src, dest) --
-- OutOfService(dest) & Number(src) => Refuse(src, dest) --
-- (src = dest) => Refuse(src,dest) --

-- Query
Toll(src,dest) => exists e1. exists e2. GetExchange(src,e1) & GetExchange(dest,e2) & NE(e1,e2)
-- ~(NE(x,y) & x = y) --
-- Exchange(x) & Exchange(y) => NE(x,y) | x = y

TollFree(src,dest) => exists e1. GetExchange(src, e1) & GetExchange(dest,e1)

exists x. exists y. Toll(x,y) & TollFree(x,y)