# Bug Report: Pattern-constrained string generation causes 100% discard rate

## Summary

When an OpenAPI schema defines a `pattern` constraint on a string parameter (e.g., `"pattern": "^ses.*"`), haskemathesis generates random alphanumeric strings and filters them against the pattern. This approach has an astronomically low probability of generating matching strings, causing 100% test discard rates for any endpoint with pattern-constrained path parameters.

## Environment

- **haskemathesis version**: (current main branch)
- **GHC version**: 9.10.3
- **OS**: Linux

## Reproduction

### Minimal OpenAPI spec

```json
{
  "openapi": "3.0.0",
  "info": { "title": "Test", "version": "1.0.0" },
  "paths": {
    "/session/{sessionID}": {
      "get": {
        "operationId": "session.get",
        "parameters": [
          {
            "in": "path",
            "name": "sessionID",
            "required": true,
            "schema": {
              "type": "string",
              "minLength": 1,
              "pattern": "^ses.*"
            }
          }
        ],
        "responses": {
          "200": {
            "description": "Success",
            "content": {
              "application/json": {
                "schema": { "type": "object" }
              }
            }
          }
        }
      }
    }
  }
}
```

### Test output

```
session.get: FAIL (0.85s)
    gave up after 1000 discards, passed 0 tests.
```

Even with 1000 discards allowed, zero tests pass because no generated strings match the pattern.

## Root Cause Analysis

The issue is in `src/Haskemathesis/Gen/Primitive.hs`, lines 84-91:

```haskell
genString :: Schema -> Gen Value
genString schema =
    genConstOrEnum schema $
        case schemaPattern schema of
            Just pat ->
                let minL = fromMaybe 0 (schemaMinLength schema)
                    maxL = fromMaybe (minL + 8) (schemaMaxLength schema)
                    range = Range.linear minL maxL
                 in Gen.filter
                        (matchesPattern pat)
                        (String <$> Gen.text range Gen.alphaNum)
            Nothing -> ...
```

When a pattern is present, the generator:
1. Generates random alphanumeric strings using `Gen.text range Gen.alphaNum`
2. Filters using `Gen.filter (matchesPattern pat)` to keep only matching strings

### Why this fails

For the pattern `^ses.*`:
- The string must start with exactly "s", "e", "s"
- `Gen.alphaNum` generates from 62 characters (a-z, A-Z, 0-9)
- Probability of generating "ses" as the first 3 characters: `(1/62)^3 = 1/238,328`
- With minLength=1 and default maxLength=9, most generated strings are too short or don't start with "ses"

**Effective probability of a match: ~0.0004%**

This makes `Gen.filter` effectively useless - it will discard essentially all generated values.

## Suggested Fixes

### Option 1: Use a regex-aware string generator (Recommended)

Use a library like [`regex-genex`](https://hackage.haskell.org/package/regex-genex) or implement simple regex-to-generator conversion for common patterns:

```haskell
import Text.Regex.Genex (genMatches)

genString :: Schema -> Gen Value
genString schema =
    genConstOrEnum schema $
        case schemaPattern schema of
            Just pat -> do
                -- Generate strings that match the pattern directly
                txt <- genFromPattern pat minL maxL
                pure (String txt)
            Nothing -> ...

-- Simple implementation for common patterns
genFromPattern :: Text -> Int -> Int -> Gen Text
genFromPattern pat minL maxL
    | "^" `T.isPrefixOf` pat = do
        -- Extract literal prefix and generate suffix
        let prefix = extractLiteralPrefix (T.drop 1 pat)
            suffixLen = maxL - T.length prefix
        suffix <- Gen.text (Range.linear 0 suffixLen) Gen.alphaNum
        pure (prefix <> suffix)
    | otherwise = ...

extractLiteralPrefix :: Text -> Text
extractLiteralPrefix pat = T.takeWhile isLiteral pat
  where
    isLiteral c = c `notElem` (".*+?[]{}()|\\^$" :: String)
```

### Option 2: Hybrid approach with increased generation

Generate pattern-matching strings more intelligently by:
1. Parsing simple anchored patterns like `^prefix.*`
2. Generating the literal prefix directly
3. Appending random suffix

```haskell
genString schema =
    genConstOrEnum schema $
        case schemaPattern schema of
            Just pat
                | Just prefix <- parseAnchoredPrefix pat ->
                    -- Generate: prefix + random suffix
                    let prefixLen = T.length prefix
                        suffixRange = Range.linear 0 (maxL - prefixLen)
                    in do
                        suffix <- Gen.text suffixRange Gen.alphaNum
                        pure (String (prefix <> suffix))
                | otherwise ->
                    -- Fall back to filter for complex patterns
                    Gen.filter (matchesPattern pat) baseGen
            Nothing -> baseGen

parseAnchoredPrefix :: Text -> Maybe Text
parseAnchoredPrefix pat
    | Just rest <- T.stripPrefix "^" pat =
        let (literal, remainder) = T.span isLiteral rest
        in if ".*" `T.isPrefixOf` remainder || T.null remainder
           then Just literal
           else Nothing
    | otherwise = Nothing
  where
    isLiteral c = c `notElem` (".*+?[]{}()|\\^$" :: String)
```

### Option 3: Warn and skip pattern-constrained parameters

If pattern generation is too complex, at minimum warn the user:

```haskell
genString schema =
    genConstOrEnum schema $
        case schemaPattern schema of
            Just pat -> do
                -- Log warning about low success rate
                trace ("Warning: pattern " <> show pat <> " may cause high discard rate") $
                    Gen.filter (matchesPattern pat) baseGen
            Nothing -> baseGen
```

## Impact

This bug effectively makes haskemathesis unable to test any API with pattern-constrained path parameters. Common patterns affected:

- Session IDs: `^ses.*`, `^session_.*`
- UUIDs: `^[0-9a-f]{8}-...`
- Prefixed IDs: `^usr_.*`, `^org_.*`, `^msg_.*`
- Slugs: `^[a-z0-9-]+$`

## Workaround

Currently, users must remove `pattern` constraints from their OpenAPI spec to run haskemathesis tests, which defeats the purpose of schema validation.

## Additional Context

We discovered this while implementing OpenAPI conformance tests for a server with session endpoints. All session-related tests showed 100% discard rates:

```
session.get:         gave up after 100 discards, passed 0 tests.
session.delete:      gave up after 100 discards, passed 0 tests.
session.update:      gave up after 100 discards, passed 0 tests.
session.children:    gave up after 100 discards, passed 0 tests.
session.fork:        gave up after 100 discards, passed 0 tests.
session.prompt:      gave up after 100 discards, passed 0 tests.
...
```

Debug tracing confirmed the executor was never called - all discards happened in the generator before any HTTP request was made.

## References

- Similar issue in Python's Schemathesis: They use [`hypothesis-regex`](https://github.com/Zac-HD/hypothesis-regex) for pattern generation
- [`regex-genex`](https://hackage.haskell.org/package/regex-genex) - Haskell library for generating strings from regexes
- [`regex-applicative`](https://hackage.haskell.org/package/regex-applicative) - Could potentially be used for simple pattern parsing
