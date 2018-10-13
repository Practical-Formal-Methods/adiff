SELECT r.run_id,
r.verifier_name,
p.origin,
r.code_hash,
(SELECT COUNT(*) FROM runs AS t WHERE r.code_hash = t.code_hash AND result='unsat')  as unsats,
(SELECT COUNT(*) FROM runs AS t WHERE r.code_hash = t.code_hash AND result='sat')  as sats
FROM runs r NATURAL JOIN programs p
