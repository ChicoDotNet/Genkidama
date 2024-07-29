class Button a where
  renderButton :: a -> IO ()

data DarkButton = DarkButton
data LightButton = LightButton

instance Button DarkButton where
  renderButton _ = putStrLn "Dark Button"

instance Button LightButton where
  renderButton _ = putStrLn "Light Button"

class Checkbox a where
  renderCheckbox :: a -> IO ()

data DarkCheckbox = DarkCheckbox
data LightCheckbox = LightCheckbox

instance Checkbox DarkCheckbox where
  renderCheckbox _ = putStrLn "Dark Checkbox"

instance Checkbox LightCheckbox where
  renderCheckbox _ = putStrLn "Light Checkbox"

data UIFactory = UIFactory
  { createButton :: IO (),
    createCheckbox :: IO ()
  }

darkFactory :: UIFactory
darkFactory = UIFactory
  { createButton = renderButton DarkButton,
    createCheckbox = renderCheckbox DarkCheckbox
  }

lightFactory :: UIFactory
lightFactory = UIFactory
  { createButton = renderButton LightButton,
    createCheckbox = renderCheckbox LightCheckbox
  }

createUIComponents :: UIFactory -> IO ()
createUIComponents factory = do
  createButton factory
  createCheckbox factory

main :: IO ()
main = do
  createUIComponents darkFactory
  createUIComponents lightFactory
