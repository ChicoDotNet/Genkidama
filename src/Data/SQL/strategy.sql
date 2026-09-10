PRAGMA foreign_keys = ON;

-- Strategy expressed as interchangeable pricing policy rows. The context selects
-- one policy without changing the evaluation query.
CREATE TABLE pricing_strategy (
    name TEXT PRIMARY KEY,
    percent INTEGER NOT NULL CHECK (percent BETWEEN 0 AND 100),
    minimum_price INTEGER NOT NULL CHECK (minimum_price >= 0)
);

INSERT INTO pricing_strategy(name, percent, minimum_price) VALUES
    ('regular', 100, 0),
    ('vip', 80, 0),
    ('campaign', 75, 100);

CREATE TABLE pricing_context (
    slot TEXT PRIMARY KEY,
    strategy_name TEXT NOT NULL REFERENCES pricing_strategy(name)
);

INSERT INTO pricing_context(slot, strategy_name) VALUES ('checkout', 'regular');

CREATE TABLE pricing_request (
    slot TEXT PRIMARY KEY REFERENCES pricing_context(slot),
    base_price INTEGER NOT NULL CHECK (base_price >= 0)
);

INSERT INTO pricing_request(slot, base_price) VALUES ('checkout', 100);

CREATE VIEW pricing_result AS
SELECT
    request.slot,
    context.strategy_name,
    request.base_price,
    CASE
        WHEN request.base_price < strategy.minimum_price THEN request.base_price
        ELSE request.base_price * strategy.percent / 100
    END AS final_price
FROM pricing_request AS request
JOIN pricing_context AS context ON context.slot = request.slot
JOIN pricing_strategy AS strategy ON strategy.name = context.strategy_name;
