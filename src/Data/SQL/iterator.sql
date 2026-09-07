-- Iterator: traverse an ordered relation through explicit cursor state without exposing storage details.
WITH RECURSIVE
collection(position, value) AS (
  VALUES (1, 10), (2, 20), (3, 30)
),
iterator(position, value) AS (
  SELECT position, value FROM collection WHERE position = 1
  UNION ALL
  SELECT next.position, next.value
  FROM iterator current
  JOIN collection next ON next.position = current.position + 1
)
SELECT 'iterator=' || group_concat(value, ',') FROM iterator;
