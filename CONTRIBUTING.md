# `// contributing`

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
                                                   // weapon-server // haskell
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

   "The Sprawl was a single conurbation. These city limits had long since
    merged, and the neon forest of the business district reflected in the
    most immaculate of the great slabs that lined the hills."

                                                             — Count Zero
```

## `// philosophy`

Production Haskell exists at the intersection of mathematical beauty and
economic reality. We write in a language that could express category theory
but choose to express business logic instead. Not because we can't do the
former, but because making money with functional programming is the ultimate
proof of concept.

We are not the same as the Haskell you learned in university. We're what
happens when you take those ideas and make them work for money.

## `// core principle // optimize for disambiguation`

In modern codebases where agents generate significant amounts of code,
traditional economics invert:

- code is written once by agents in seconds
- code is read hundreds of times by humans and agents
- code is debugged when you're under pressure by tired humans
- code is modified by agents who lack the original context

**Every ambiguity compounds exponentially.**

```haskell
-- this costs an agent 0.1 seconds to write, a human 10 minutes to debug
process e = if p e > 0 then go e else stop

-- this costs an agent 0.2 seconds to write, saves hours of confusion
processIncomingRequest :: HttpRequest -> IO ResponseResult
processIncomingRequest httpRequest =
  if requestTimeout httpRequest > 0
    then processValidRequest httpRequest
    else returnTimeoutError
```

## `// language extensions`

### `// green light // use freely`

```haskell
{-# LANGUAGE BangPatterns #-}          -- strictness is good
{-# LANGUAGE OverloadedStrings #-}     -- text everywhere
{-# LANGUAGE RecordWildCards #-}       -- tasteful destructuring
{-# LANGUAGE NamedFieldPuns #-}        -- clear intent
{-# LANGUAGE DeriveGeneric #-}         -- boring is good
{-# LANGUAGE DerivingStrategies #-}    -- be explicit
{-# LANGUAGE StrictData #-}            -- default strict
{-# LANGUAGE NumericUnderscores #-}    -- 1_000_000 is clearer
```

### `// yellow light // use with purpose`

```haskell
{-# LANGUAGE TypeFamilies #-}          -- ok for libraries
{-# LANGUAGE GADTs #-}                 -- when the juice is worth the squeeze
{-# LANGUAGE RankNTypes #-}            -- sometimes necessary
{-# LANGUAGE FlexibleContexts #-}      -- when the alternative is worse
{-# LANGUAGE TemplateHaskell #-}       -- for aeson/lens, but measure build impact
```

### `// red light // justify your existence`

```haskell
{-# LANGUAGE DataKinds #-}             -- type-level programming rarely pays off
{-# LANGUAGE TypeOperators #-}         -- compile times and error messages suffer
{-# LANGUAGE UndecidableInstances #-}  -- usually means wrong problem
{-# LANGUAGE ImplicitParams #-}        -- debugging nightmare
{-# LANGUAGE OverlappingInstances #-}  -- semantic timebomb
```

## `// naming // the three-character rule`

If an identifier is 3 characters or less, it's probably too short:

```haskell
-- bad: abbreviated names multiply confusion
cfg <- loadCfg
conn <- mkConn cfg
res <- proc req

-- good: full words tell the story
configuration <- loadServerConfiguration
connection <- createDatabaseConnection configuration
response <- processClientRequest request
```

### `// standard exceptions // use sparingly`

Only in local scope where type makes it unambiguous:

- `xs, ys` — lists in pure functions
- `m, n` — indices in array algorithms
- `k, v` — key/value in map operations
- `f, g` — functions in higher-order contexts

## `// control flow // keep it flat`

Deep nesting is a maintenance liability. Every level of indentation is a
place where merge conflicts multiply, off-by-one space errors break
compilation, and code reviews devolve into whitespace debates.

```haskell
-- bad: philosophically pure but practically painful
processRequest request =
  case validateRequest request of
    Nothing -> handleInvalid
    Just validReq ->
      case findRoute routes validReq of
        Nothing -> handleNoRoute
        Just route ->
          case lookupHandler route of
            Nothing -> handleMissingHandler
            Just handler ->
              executeHandler handler validReq

-- good: small do-block for sequencing, where-clause with guards
handleWebRequest :: Request -> AppM Response
handleWebRequest request = do
  startTime <- getCurrentTime
  validated <- validateOrReject request
  enriched <- enrichRequest validated
  result <- processRequest enriched
  recordMetrics startTime result
  return result
  where
    validateOrReject req
      | not (validMethod req) = throwError InvalidMethod
      | not (validHeaders req) = throwError InvalidHeaders
      | not (validBody req) = throwError InvalidBody
      | otherwise = pure req

    processRequest req
      | isHealthCheck req = return healthCheckResponse
      | needsAuth req && not (hasValidAuth req) = throwError Unauthorized
      | otherwise = routeToHandler req
```

## `// newtype wrapping // pragmatic boundaries`

### `// always wrap`

```haskell
-- domain boundaries: prevents mixing up parameters
newtype SessionId = SessionId UUID
newtype RequestId = RequestId Int64
newtype RouteId = RouteId Text

-- units and semantics: when the type carries meaning
newtype Milliseconds = Milliseconds Int64
newtype ByteCount = ByteCount Word64

-- validation boundaries: when construction can fail
newtype Email = Email { unEmail :: Text }
mkEmail :: Text -> Either ValidationError Email
```

### `// don't wrap`

```haskell
-- internal module details
type LoopCounter = Int
type CacheSize = Int

-- well-typed contexts where confusion is unlikely
data ThreadPool = ThreadPool
  { poolThreadCount :: !Int
  , poolQueueDepth :: !Int
  , poolMaxIdleTime :: !NominalDiffTime
  }
```

The rule: start with type aliases, upgrade to newtypes when you find bugs
mixing things up. With `-O2`, GHC eliminates newtype overhead anyway.

## `// compiler warnings // your automated colleague`

Always use strict warnings:

```yaml
ghc-options:
  - -Wall
  - -Werror
  - -Wincomplete-patterns
  - -Wincomplete-record-updates
  - -Wmissing-signatures
  - -Wname-shadowing
  - -Wunused-matches
  - -Wunused-imports
```

## `// stm // composable concurrency`

STM shines in production because transactions compose and retry elegantly:

```haskell
-- composable operations for connection pools
allocateFromPool :: ConnectionPool -> Int -> STM (Maybe [Connection])
allocateFromPool pool requestedCount = do
  available <- readTVar (poolAvailable pool)
  if length available >= requestedCount
    then do
      let (allocated, remaining) = splitAt requestedCount available
      writeTVar (poolAvailable pool) remaining
      modifyTVar (poolInUse pool) (allocated ++)
      return (Just allocated)
    else return Nothing

-- combine multiple operations atomically
transferConnections :: ConnectionPool -> ConnectionPool -> Int -> STM Bool
transferConnections fromPool toPool connectionCount = do
  maybeConnections <- allocateFromPool fromPool connectionCount
  case maybeConnections of
    Nothing -> return False
    Just connections -> do
      modifyTVar (poolAvailable toPool) (connections ++)
      return True
```

## `// structured logging`

```haskell
-- structured, parseable, grepable
handleHttpRequest :: Request -> IO Response
handleHttpRequest request = do
  logInfo $ "[http] [request] [received] [id :: " <> requestId request <>
            "] [method :: " <> requestMethod request <>
            "] [path :: " <> requestPath request <> "]"

  result <- routeAndHandle request

  logInfo $ "[http] [request] [complete] [id :: " <> requestId request <>
            "] [status :: " <> showStatus result <>
            "] [duration_ms :: " <> showDuration result <> "]"

  return result
```

## `// testing philosophy`

### `// property tests for invariants`

```haskell
-- unit tests: thorough but mechanical (agent-friendly)
describe "parseHttpHeaders" $ do
  it "parses valid headers" $ do
    let input = "Content-Type: application/json\r\nContent-Length: 42\r\n"
    parseHttpHeaders input `shouldBe` Right expectedHeaders

-- property tests: invariants and edge cases (human insight)
prop_headerRoundTrip :: ValidHeaders -> Bool
prop_headerRoundTrip headers =
  parseHttpHeaders (renderHeaders headers) == Right headers

prop_connectionPoolInvariants :: PoolState -> Bool
prop_connectionPoolInvariants pool =
  let available = poolAvailableConnections pool
      leased = poolLeasedConnections pool
  in Set.null (available `Set.intersection` leased)
```

## `// typographical conventions`

We use the straylight typographical conventions for documentation:

### `// unicode delimiters`

- `━` heavy line — file-level framing (80 chars)
- `═` double line — major sections
- `─` light line — subsections
- `—` em-dash — attribution only

### `// code block headers`

```haskell
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                   // haskell // module title
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

### `// comment capitalization`

- `// workaday // lowercase` — working notes, observations
- `// author // voice` — documentation that warrants heading but lives inline
- `// proper // grammar` — markdown and module descriptions

### `// todo convention`

```haskell
-- TODO[handle]: minor debt, will address

-- TODO[handle]: !! urgent — this is embarrassing !!
```

### `// discouraged`

- ascii art when unicode alternatives exist
- emojis (banned, pain of death)
- `--` where `—` is meant
- `camelCase` in nix identifiers

## `// the vibe test`

Good production Haskell passes these checks:

- could you debug it during an incident without ghci?
- could a colleague (human or AI) extend it without breaking invariants?
- do the types prevent tomorrow's bug?
- is every abbreviation worth the confusion it creates?
- does it compile fast enough for flow state?
- will it still make sense after multiple contributors have touched it?

The Haskell community optimized for elegance. We optimize for clarity at
scale. Beauty in production code comes from disambiguation, not cleverness.

```
────────────────────────────────────────────────────────────────────────────────

   "Case had always taken it for granted that the real bosses, the kingpins in
    a given industry, would be both more and less than people."

                                                                 — Neuromancer
────────────────────────────────────────────────────────────────────────────────
```
