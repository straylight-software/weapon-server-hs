-- Gruvbox Theme
-- Based on https://github.com/morhetz/gruvbox
-- A retro groove color scheme for Vim
let Theme = ../Types/Theme.dhall

let rgb = Theme.rgb

let bg0 = rgb 0.157 0.157 0.157

let bg1 = rgb 0.235 0.220 0.212

let bg2 = rgb 0.314 0.290 0.275

let bg3 = rgb 0.392 0.361 0.341

let fg0 = rgb 0.984 0.945 0.843

let fg1 = rgb 0.922 0.859 0.698

let fg2 = rgb 0.839 0.769 0.631

let fg3 = rgb 0.741 0.675 0.553

let red = rgb 0.800 0.141 0.114

let green = rgb 0.596 0.592 0.102

let yellow = rgb 0.843 0.600 0.129

let blue = rgb 0.271 0.522 0.533

let purple = rgb 0.694 0.384 0.525

let aqua = rgb 0.408 0.616 0.416

let orange = rgb 0.839 0.365 0.055

let gray = rgb 0.573 0.514 0.455

let brightRed = rgb 0.984 0.286 0.204

let brightGreen = rgb 0.722 0.733 0.149

let brightYellow = rgb 0.980 0.741 0.184

let brightBlue = rgb 0.514 0.647 0.596

let brightPurple = rgb 0.827 0.525 0.608

let brightAqua = rgb 0.557 0.753 0.486

let brightOrange = rgb 0.996 0.545 0.196

in    { -- Semantic colors
        primary = brightAqua
      , secondary = brightBlue
      , accent = brightOrange
      , error = brightRed
      , warning = brightYellow
      , success = brightGreen
      , info = brightBlue
      , -- Text colors
        text = fg1
      , textMuted = fg3
      , selectedListItemText = Some fg0
      , -- Background colors
        background = bg0
      , backgroundPanel = bg1
      , backgroundElement = bg2
      , backgroundMenu = Some bg1
      , -- Border colors
        border = bg3
      , borderActive = brightAqua
      , borderSubtle = bg2
      , -- Diff colors
        diffAdded = brightGreen
      , diffRemoved = brightRed
      , diffContext = fg3
      , diffHunkHeader = brightBlue
      , diffHighlightAdded = green
      , diffHighlightRemoved = red
      , diffAddedBg = rgb 0.2 0.3 0.2
      , diffRemovedBg = rgb 0.3 0.2 0.2
      , diffContextBg = bg1
      , diffLineNumber = fg3
      , diffAddedLineNumberBg = rgb 0.15 0.25 0.15
      , diffRemovedLineNumberBg = rgb 0.25 0.15 0.15
      , -- Markdown colors
        markdownText = fg1
      , markdownHeading = brightOrange
      , markdownLink = brightBlue
      , markdownLinkText = brightAqua
      , markdownCode = brightGreen
      , markdownBlockQuote = gray
      , markdownEmph = brightYellow
      , markdownStrong = brightOrange
      , markdownHorizontalRule = bg3
      , markdownListItem = fg2
      , markdownListEnumeration = brightAqua
      , markdownImage = brightPurple
      , markdownImageText = brightPurple
      , markdownCodeBlock = fg1
      , -- Syntax highlighting colors
        syntaxComment = gray
      , syntaxKeyword = brightRed
      , syntaxFunction = brightGreen
      , syntaxVariable = fg1
      , syntaxString = brightGreen
      , syntaxNumber = brightPurple
      , syntaxType = brightYellow
      , syntaxOperator = fg1
      , syntaxPunctuation = fg2
      , -- Additional
        thinkingOpacity = 0.5
      }
    : Theme.Theme.Type
