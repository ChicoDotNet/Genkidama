PRAGMA foreign_keys = ON;

CREATE TABLE account (
    id INTEGER PRIMARY KEY,
    balance INTEGER NOT NULL
);

CREATE TABLE command_queue (
    sequence_no INTEGER PRIMARY KEY,
    operation TEXT NOT NULL CHECK (operation IN ('deposit', 'withdraw')),
    amount INTEGER NOT NULL CHECK (amount > 0)
);

INSERT INTO account (id, balance) VALUES (1, 100);
INSERT INTO command_queue (sequence_no, operation, amount) VALUES
    (1, 'deposit', 50),
    (2, 'withdraw', 20);

UPDATE account
SET balance = balance + COALESCE((
    SELECT SUM(CASE operation WHEN 'deposit' THEN amount WHEN 'withdraw' THEN -amount END)
    FROM command_queue
), 0)
WHERE id = 1;

SELECT 'balance=' || balance || ';commands=' || (SELECT COUNT(*) FROM command_queue)
FROM account
WHERE id = 1;
