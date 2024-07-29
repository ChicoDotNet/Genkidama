using System;

// Abstract Product
public interface Button {
    void Render();
}

public interface Checkbox {
    void Render();
}

// Concrete Product
public class DarkButton : Button {
    public void Render() {
        Console.WriteLine("Dark Button");
    }
}

public class LightButton : Button {
    public void Render() {
        Console.WriteLine("Light Button");
    }
}

public class DarkCheckbox : Checkbox {
    public void Render() {
        Console.WriteLine("Dark Checkbox");
    }
}

public class LightCheckbox : Checkbox {
    public void Render() {
        Console.WriteLine("Light Checkbox");
    }
}

// Abstract Factory
public interface UIFactory {
    Button CreateButton();
    Checkbox CreateCheckbox();
}

// Concrete Factory
public class DarkFactory : UIFactory {
    public Button CreateButton() {
        return new DarkButton();
    }

    public Checkbox CreateCheckbox() {
        return new DarkCheckbox();
    }
}

public class LightFactory : UIFactory {
    public Button CreateButton() {
        return new LightButton();
    }

    public Checkbox CreateCheckbox() {
        return new LightCheckbox();
    }
}

public class Example1 {
    public static void CreateUIComponents(UIFactory factory) {
        Button button = factory.CreateButton();
        Checkbox checkbox = factory.CreateCheckbox();
        button.Render();
        checkbox.Render();
    }

    public static void Main(string[] args) {
        CreateUIComponents(new DarkFactory());
        CreateUIComponents(new LightFactory());
    }
}
