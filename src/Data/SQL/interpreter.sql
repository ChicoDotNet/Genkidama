-- Interpreter pattern in SQLite: the grammar is represented as token rows and
-- the interpreter is a recursive relation that evaluates Expr := Number ('+' Number)*.

CREATE TABLE expression_tokens (
    position INTEGER PRIMARY KEY,
    token_kind TEXT NOT NULL CHECK (token_kind IN ('number', 'plus')),
    lexeme TEXT NOT NULL
);

INSERT INTO expression_tokens (position, token_kind, lexeme) VALUES
    (1, 'number', '2'),
    (2, 'plus', '+'),
    (3, 'number', '3'),
    (4, 'plus', '+'),
    (5, 'number', '4');

WITH RECURSIVE interpret(next_position, value) AS (
    SELECT 2, CAST(lexeme AS INTEGER)
    FROM expression_tokens
    WHERE position = 1 AND token_kind = 'number'

    UNION ALL

    SELECT number.position + 1,
           interpret.value + CAST(number.lexeme AS INTEGER)
    FROM interpret
    JOIN expression_tokens AS operator
      ON operator.position = interpret.next_position
     AND operator.token_kind = 'plus'
    JOIN expression_tokens AS number
      ON number.position = operator.position + 1
     AND number.token_kind = 'number'
)
SELECT 'value=' || value
FROM interpret
ORDER BY next_position DESC
LIMIT 1;
