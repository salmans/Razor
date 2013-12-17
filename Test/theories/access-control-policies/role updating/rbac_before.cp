-- Policy:
-- exists r.Role(r) & HadRole(u,r) & HadPerm(u,p) => Permitted(u,p)

HadRole(u, r) & HadPerm(r,p) => Permitted(u,p)