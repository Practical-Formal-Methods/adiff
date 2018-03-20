SELECT run_id,
	verifier_name,
	origin,
	(SELECT COUNT(*) FROM runs AS r WHERE r.code_hash = code_hash AND result='unsat')  as accepting
	(SELECT COUNT(*) FROM runs AS r WHERE r.code_hash = code_hash AND result='sat')  as rejecting	
FROM runs NATURAL JOIN programs
WHERE result = "unsat"
