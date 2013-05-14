-- Vocabulary
-- Sorts:
~(Author(x) & ReadPaper(x) & Paper(x))

-- Predicates
Conflicted(x,y) => Author(x) & Paper(y)
Assigned(x,y) => Author(x) & Paper(y)


-- Policy
ReadPaper(a) & Paper(r) & Author(s) => Permit(s,a,r) | Conflicted(s,r)
Assigned(s,r) & ReadPaper(a) & Paper(r) => Permit(s,a,r)
Conflicted(s,r) & ReadPaper(a) & Paper(r) => Deny(s,a,r)

-- Query
Permit(s,a,r) => (ReadPaper(a) & Paper(r) & Author(s) & NotConflicted(s,r)) | (Assigned(s,r) & ReadPaper(a) & Paper(r))

Conflicted(s,r) => exists a . (ReadPaper(a) & Paper(r) & Author(s) & NotPermit(s,a,r))

~(Conflicted(s,r) & NotConflicted(s,r))
~(Permit(s,a,r) & NotPermit(s,a,r))

--Author(s) & Paper(r) => Conflicted(s,r) | NotConflicted(s,r)

exists x.exists y.exists z.Permit(x,y,z) & Conflicted(x,z)