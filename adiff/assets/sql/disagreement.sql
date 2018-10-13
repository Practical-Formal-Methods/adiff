SELECT DISTINCT
  0,
	'-',
	p.origin,
	r.code_hash,
	(SELECT COUNT(*) FROM runs AS t WHERE r.code_hash = t.code_hash AND result='unsat')  as unsats,
	(SELECT COUNT(*) FROM runs AS t WHERE r.code_hash = t.code_hash AND result='sat')  as sats
FROM runs r NATURAL JOIN programs p
WHERE unsats <> 0 AND sats <> 0
ORDER BY abs(unsats - sats) - 0.5
