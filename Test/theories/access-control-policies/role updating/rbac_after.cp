-- Policy:
-- exists r.Role(r) & HasRole(u,r) & HasPerm(u,p) => Permit(u,p)

HasRole(u, r) & HasPerm(r,p) => Permit(u,p)