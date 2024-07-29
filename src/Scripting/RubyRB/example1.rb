# Abstract Factory
class UIFactory
    def create_button
      raise NotImplementedError, 'create_button() must be implemented'
    end
  
    def create_checkbox
      raise NotImplementedError, 'create_checkbox() must be implemented'
    end
  end
  
  # Concrete Factory
  class DarkFactory < UIFactory
    def create_button
      DarkButton.new
    end
  
    def create_checkbox
      DarkCheckbox.new
    end
  end
  
  class LightFactory < UIFactory
    def create_button
      LightButton.new
    end
  
    def create_checkbox
      LightCheckbox.new
    end
  end
  
  # Products
  class DarkButton
    def render
      puts 'Dark Button'
    end
  end
  
  class LightButton
    def render
      puts 'Light Button'
    end
  end
  
  class DarkCheckbox
    def render
      puts 'Dark Checkbox'
    end
  end
  
  class LightCheckbox
    def render
      puts 'Light Checkbox'
    end
  end
  
  def create_ui_components(factory)
    button = factory.create_button
    checkbox = factory.create_checkbox
    button.render
    checkbox.render
  end
  
  # Usage
  create_ui_components(DarkFactory.new)
  create_ui_components(LightFactory.new)
  