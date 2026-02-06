
### Hermetic Haskell: A Pristine Programming Language

In a hermetic version of Haskell, ("Hermetic Haskell") all live functions would have to be moved out of standard libraries, making imports inert. Live functions could then be moved into a single `World` dependency with fields corresponding to the standard libraries.

```haskell

-- A world record that mirrors familiar library groupings.
-- Each field is a record of the effectful operations that library normally provides.

data World = World
  { systemIO   :: SystemIO
  , systemDir  :: SystemDirectory
  , systemTime :: SystemTime
  , systemEnv  :: SystemEnvironment
  , netHTTP    :: NetworkHTTP
  , netSocket  :: NetworkSocket
  , random     :: Random
  }

-- System.IO (live operations that touch the real console/files)
data SystemIO = SystemIO
  { putStrLn  :: String -> IO ()
  , getLine   :: IO String
  , readFile  :: FilePath -> IO String
  , writeFile :: FilePath -> String -> IO ()
  , appendFile :: FilePath -> String -> IO ()
  }

```

The program logic would then be moved into a hermetic function into which the real world could be injected.

```haskell
-- The program is parameterized over the world
program :: World -> IO ()
program w = do
  putStrLn (systemIO w) "Hello, World!"
```


The caller can route the output to any device, including a mock for testing.

```haskell
import Data.IORef

testProgram :: IO Bool
testProgram = do
    -- 1. Create a mutable reference to capture output
    outputRef <- newIORef []

    -- 2. Define the mock behavior
    -- Instead of writing to stdout, we append the message to our reference.
    let mockPutStrLn msg = modifyIORef outputRef (\logs -> logs ++ [msg])
    let mockConsole = Console { putStrLn = mockPutStrLn }

    -- 3. Inject the mock into the program
    main mockConsole

    -- 4. Verify the captured side-effects
    captured <- readIORef outputRef
    return (captured == ["Hello, World!"])
```

For compatibility with the host runtime, build system, and tooling, Hermetic Haskell programs might include an implicit `main` that does nothing but inject `program` with a live `world` value.

```haskell
-- Implicit live `main` that binds the program to the real world
main :: IO ()
main = program world
```
