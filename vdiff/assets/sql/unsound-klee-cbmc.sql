SELECT r.run_id,
r.verifier_name,
p.origin,
r.code_hash,
(SELECT COUNT(*) FROM runs AS t WHERE r.code_hash = t.code_hash AND result='unsat')  as unsats,
(SELECT COUNT(*) FROM runs AS t WHERE r.code_hash = t.code_hash AND result='sat')  as sats
FROM runs r NATURAL JOIN programs p
WHERE result = 'unsat'
      AND verifier_name <> 'cbmc' AND verifier_name <> 'klee' 
      AND EXISTS (
       SELECT *
       FROM runs AS q
       WHERE r.code_hash = q.code_hash
            AND result='sat'
            AND (q.verifier_name = 'cbmc' OR  q.verifier_name = 'klee'))
