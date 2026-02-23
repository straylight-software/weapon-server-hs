-- Theme.dhall
-- Theme color definitions (50+ color fields)

-- RGBA color representation (0-1 range for each component)
let Color = { r : Double, g : Double, b : Double, a : Double }

let rgb =
      \(r : Double) -> \(g : Double) -> \(b : Double) -> { r, g, b, a = 1.0 }

let rgba =
      \(r : Double) ->
      \(g : Double) ->
      \(b : Double) ->
      \(a : Double) ->
        { r, g, b, a }

let ThemeColors =
      { Type =
          { -- Semantic colors
            primary : Color
          , secondary : Color
          , accent : Color
          , error : Color
          , warning : Color
          , success : Color
          , info : Color
          , -- Text colors
            text : Color
          , textMuted : Color
          , selectedListItemText : Optional Color
          , -- Background colors
            background : Color
          , backgroundPanel : Color
          , backgroundElement : Color
          , backgroundMenu : Optional Color
          , -- Border colors
            border : Color
          , borderActive : Color
          , borderSubtle : Color
          , -- Diff colors
            diffAdded : Color
          , diffRemoved : Color
          , diffContext : Color
          , diffHunkHeader : Color
          , diffHighlightAdded : Color
          , diffHighlightRemoved : Color
          , diffAddedBg : Color
          , diffRemovedBg : Color
          , diffContextBg : Color
          , diffLineNumber : Color
          , diffAddedLineNumberBg : Color
          , diffRemovedLineNumberBg : Color
          , -- Markdown colors
            markdownText : Color
          , markdownHeading : Color
          , markdownLink : Color
          , markdownLinkText : Color
          , markdownCode : Color
          , markdownBlockQuote : Color
          , markdownEmph : Color
          , markdownStrong : Color
          , markdownHorizontalRule : Color
          , markdownListItem : Color
          , markdownListEnumeration : Color
          , markdownImage : Color
          , markdownImageText : Color
          , markdownCodeBlock : Color
          , -- Syntax highlighting colors
            syntaxComment : Color
          , syntaxKeyword : Color
          , syntaxFunction : Color
          , syntaxVariable : Color
          , syntaxString : Color
          , syntaxNumber : Color
          , syntaxType : Color
          , syntaxOperator : Color
          , syntaxPunctuation : Color
          }
      }

let Theme =
      { Type =
          { -- Semantic colors
            primary : Color
          , secondary : Color
          , accent : Color
          , error : Color
          , warning : Color
          , success : Color
          , info : Color
          , -- Text colors
            text : Color
          , textMuted : Color
          , selectedListItemText : Optional Color
          , -- Background colors
            background : Color
          , backgroundPanel : Color
          , backgroundElement : Color
          , backgroundMenu : Optional Color
          , -- Border colors
            border : Color
          , borderActive : Color
          , borderSubtle : Color
          , -- Diff colors
            diffAdded : Color
          , diffRemoved : Color
          , diffContext : Color
          , diffHunkHeader : Color
          , diffHighlightAdded : Color
          , diffHighlightRemoved : Color
          , diffAddedBg : Color
          , diffRemovedBg : Color
          , diffContextBg : Color
          , diffLineNumber : Color
          , diffAddedLineNumberBg : Color
          , diffRemovedLineNumberBg : Color
          , -- Markdown colors
            markdownText : Color
          , markdownHeading : Color
          , markdownLink : Color
          , markdownLinkText : Color
          , markdownCode : Color
          , markdownBlockQuote : Color
          , markdownEmph : Color
          , markdownStrong : Color
          , markdownHorizontalRule : Color
          , markdownListItem : Color
          , markdownListEnumeration : Color
          , markdownImage : Color
          , markdownImageText : Color
          , markdownCodeBlock : Color
          , -- Syntax highlighting colors
            syntaxComment : Color
          , syntaxKeyword : Color
          , syntaxFunction : Color
          , syntaxVariable : Color
          , syntaxString : Color
          , syntaxNumber : Color
          , syntaxType : Color
          , syntaxOperator : Color
          , syntaxPunctuation : Color
          , -- Additional
            thinkingOpacity : Double
          }
      }

in  { Color, rgb, rgba, ThemeColors, Theme }
