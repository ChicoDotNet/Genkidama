// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

interface IFactoryMethodDatabase {
    function name() external pure returns (string memory);
}

contract FactoryMethodPostgres is IFactoryMethodDatabase {
    function name() external pure returns (string memory) { return "PostgreSQL"; }
}

contract FactoryMethodMySql is IFactoryMethodDatabase {
    function name() external pure returns (string memory) { return "MySQL"; }
}

abstract contract FactoryMethodCreator {
    function createDatabase() internal virtual returns (IFactoryMethodDatabase);

    function useDatabase() external returns (string memory) {
        IFactoryMethodDatabase database = createDatabase();
        return database.name();
    }
}

contract FactoryMethodPostgresCreator is FactoryMethodCreator {
    function createDatabase() internal override returns (IFactoryMethodDatabase) {
        return new FactoryMethodPostgres();
    }
}

contract FactoryMethodMySqlCreator is FactoryMethodCreator {
    function createDatabase() internal override returns (IFactoryMethodDatabase) {
        return new FactoryMethodMySql();
    }
}
