{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Formatter.Status
  ( FormatterStatus (..),
    statusFor,
    statusForConfig,
    baseFormatters,
    formattersFor,
  )
where

import Config.Config qualified as Config
import Config.Types qualified as CT
import Data.Aeson (ToJSON (..), object, (.=))
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (findExecutable)

data FormatterInfo = FormatterInfo
  { fiName :: Text,
    fiExtensions :: [Text],
    fiEnabled :: FilePath -> IO Bool
  }

data FormatterStatus = FormatterStatus
  { fsName :: Text,
    fsExtensions :: [Text],
    fsEnabled :: Bool
  }
  deriving (Show, Eq, Generic)

instance ToJSON FormatterStatus where
  toJSON status =
    object
      [ "name" .= fsName status,
        "extensions" .= fsExtensions status,
        "enabled" .= fsEnabled status
      ]

statusFor :: FilePath -> IO [FormatterStatus]
statusFor dir = do
  cfg <- Config.get dir
  statusForConfig dir cfg

statusForConfig :: FilePath -> CT.Config -> IO [FormatterStatus]
statusForConfig dir cfg = mapM (toStatus dir) (formattersFor cfg)

toStatus :: FilePath -> FormatterInfo -> IO FormatterStatus
toStatus dir info = do
  enabled <- fiEnabled info dir
  pure $
    FormatterStatus
      { fsName = fiName info,
        fsExtensions = fiExtensions info,
        fsEnabled = enabled
      }

formattersFor :: CT.Config -> [FormatterInfo]
formattersFor cfg = case CT.cfgFormatter cfg of
  Just CT.FormatterDisabled -> []
  Just (CT.FormatterConfig entries) -> Map.elems (applyEntries entries baseMap)
  Nothing -> baseFormatters
  where
    baseMap = Map.fromList (map (\info -> (fiName info, info)) baseFormatters)
    applyEntries entries base = foldl' applyEntry base (Map.toList entries)
    applyEntry acc (name, entry)
      | CT.feDisabled entry == Just True = Map.delete name acc
      | otherwise = case Map.lookup name acc of
          Just info ->
            let updated =
                  info
                    { fiExtensions = fromMaybe (fiExtensions info) (CT.feExtensions entry),
                      fiEnabled = if hasCommand entry then const (pure True) else fiEnabled info
                    }
             in Map.insert name updated acc
          Nothing -> case CT.feCommand entry of
            Just cmd
              | not (null cmd) ->
                  let info =
                        FormatterInfo
                          { fiName = name,
                            fiExtensions = fromMaybe [] (CT.feExtensions entry),
                            fiEnabled = const (pure True)
                          }
                   in Map.insert name info acc
            Just _cmd -> acc
            Nothing -> acc
    hasCommand entry = case CT.feCommand entry of
      Just cmd -> not (null cmd)
      Nothing -> False

-- | Base formatters list - defined at top level for sharing
-- Uses NOINLINE to ensure the list is shared as a CAF
baseFormatters :: [FormatterInfo]
baseFormatters =
  [ FormatterInfo "gofmt" goExtensions (hasExecutable "gofmt"),
    FormatterInfo "mix" mixExtensions (hasExecutable "mix"),
    FormatterInfo "prettier" prettierExtensions (hasExecutable "prettier"),
    FormatterInfo "oxfmt" jsExtensions (hasExecutable "oxfmt"),
    FormatterInfo "biome" prettierExtensions (hasExecutable "biome"),
    FormatterInfo "zig" zigExtensions (hasExecutable "zig"),
    FormatterInfo "clang-format" clangExtensions (hasExecutable "clang-format"),
    FormatterInfo "ktlint" kotlinExtensions (hasExecutable "ktlint"),
    FormatterInfo "ruff" pythonExtensions (hasExecutable "ruff")
  ]
{-# NOINLINE baseFormatters #-}

-- Extension lists as top-level CAFs for sharing
goExtensions :: [Text]
goExtensions = [".go"]
{-# NOINLINE goExtensions #-}

mixExtensions :: [Text]
mixExtensions = [".ex", ".exs", ".eex", ".heex", ".leex", ".neex", ".sface"]
{-# NOINLINE mixExtensions #-}

jsExtensions :: [Text]
jsExtensions = [".js", ".jsx", ".mjs", ".cjs", ".ts", ".tsx", ".mts", ".cts"]
{-# NOINLINE jsExtensions #-}

zigExtensions :: [Text]
zigExtensions = [".zig", ".zon"]
{-# NOINLINE zigExtensions #-}

kotlinExtensions :: [Text]
kotlinExtensions = [".kt", ".kts"]
{-# NOINLINE kotlinExtensions #-}

pythonExtensions :: [Text]
pythonExtensions = [".py", ".pyi"]
{-# NOINLINE pythonExtensions #-}

prettierExtensions :: [Text]
prettierExtensions =
  [ ".js",
    ".jsx",
    ".mjs",
    ".cjs",
    ".ts",
    ".tsx",
    ".mts",
    ".cts",
    ".html",
    ".htm",
    ".css",
    ".scss",
    ".sass",
    ".less",
    ".vue",
    ".svelte",
    ".json",
    ".jsonc",
    ".yaml",
    ".yml",
    ".toml",
    ".xml",
    ".md",
    ".mdx",
    ".graphql",
    ".gql"
  ]
{-# NOINLINE prettierExtensions #-}

clangExtensions :: [Text]
clangExtensions =
  [ ".c",
    ".cc",
    ".cpp",
    ".cxx",
    ".c++",
    ".h",
    ".hh",
    ".hpp",
    ".hxx",
    ".h++",
    ".ino",
    ".C",
    ".H"
  ]
{-# NOINLINE clangExtensions #-}

-- | Check if an executable exists
hasExecutable :: String -> FilePath -> IO Bool
hasExecutable exe _ = isJust <$> findExecutable exe
