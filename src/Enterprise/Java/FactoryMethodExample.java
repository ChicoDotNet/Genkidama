interface FactoryMethodDatabase {
    void connect();
    void query();
}

final class FactoryMethodPostgres implements FactoryMethodDatabase {
    public void connect() { System.out.println("PostgreSQL connect"); }
    public void query() { System.out.println("PostgreSQL query"); }
}

final class FactoryMethodMySql implements FactoryMethodDatabase {
    public void connect() { System.out.println("MySQL connect"); }
    public void query() { System.out.println("MySQL query"); }
}

abstract class FactoryMethodCreator {
    protected abstract FactoryMethodDatabase createDatabase();

    public final void useDatabase() {
        FactoryMethodDatabase database = createDatabase();
        database.connect();
        database.query();
    }
}

final class FactoryMethodPostgresCreator extends FactoryMethodCreator {
    protected FactoryMethodDatabase createDatabase() { return new FactoryMethodPostgres(); }
}

final class FactoryMethodMySqlCreator extends FactoryMethodCreator {
    protected FactoryMethodDatabase createDatabase() { return new FactoryMethodMySql(); }
}

public final class FactoryMethodExample {
    private FactoryMethodExample() { }

    public static void main(String[] args) {
        new FactoryMethodPostgresCreator().useDatabase();
        new FactoryMethodMySqlCreator().useDatabase();
    }
}
