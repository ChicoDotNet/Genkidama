WITH RECURSIVE
request(amount) AS (
    VALUES (250)
),
handlers(position, name, max_amount) AS (
    VALUES
        (1, 'faq', 50),
        (2, 'billing', 500),
        (3, 'escalation', NULL)
),
walk(position, visited, handled) AS (
    SELECT 1, '', NULL
    UNION ALL
    SELECT
        walk.position + 1,
        CASE
            WHEN walk.visited = '' THEN handlers.name
            ELSE walk.visited || '>' || handlers.name
        END,
        CASE
            WHEN handlers.max_amount IS NULL OR request.amount <= handlers.max_amount
                THEN handlers.name
            ELSE NULL
        END
    FROM walk
    JOIN handlers ON handlers.position = walk.position
    CROSS JOIN request
    WHERE walk.handled IS NULL
)
SELECT
    'visited=' || walk.visited ||
    ';handled=' || walk.handled ||
    ';result=refund(' || request.amount || ')'
FROM walk
CROSS JOIN request
WHERE walk.handled IS NOT NULL
ORDER BY walk.position
LIMIT 1;
