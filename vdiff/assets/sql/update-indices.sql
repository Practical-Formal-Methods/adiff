-- create index on code_hash+result to improve queries for "unsoundness"/incompleteness
CREATE INDEX IF NOT EXISTS run_code_hash ON runs(code_hash,result)

