<?php

interface FactoryMethodDatabase
{
    public function connect(): void;
    public function query(): void;
}

final class FactoryMethodPostgres implements FactoryMethodDatabase
{
    public function connect(): void { echo "PostgreSQL connect\n"; }
    public function query(): void { echo "PostgreSQL query\n"; }
}

final class FactoryMethodMySql implements FactoryMethodDatabase
{
    public function connect(): void { echo "MySQL connect\n"; }
    public function query(): void { echo "MySQL query\n"; }
}

abstract class FactoryMethodCreator
{
    abstract protected function createDatabase(): FactoryMethodDatabase;

    final public function useDatabase(): void
    {
        $database = $this->createDatabase();
        $database->connect();
        $database->query();
    }
}

final class FactoryMethodPostgresCreator extends FactoryMethodCreator
{
    protected function createDatabase(): FactoryMethodDatabase { return new FactoryMethodPostgres(); }
}

final class FactoryMethodMySqlCreator extends FactoryMethodCreator
{
    protected function createDatabase(): FactoryMethodDatabase { return new FactoryMethodMySql(); }
}

(new FactoryMethodPostgresCreator())->useDatabase();
(new FactoryMethodMySqlCreator())->useDatabase();
