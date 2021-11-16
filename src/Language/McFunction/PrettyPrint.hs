module Language.McFunction.PrettyPrint where

import Calcite.Prelude hiding (Get)

import Language.McFunction.Types

prettyPrint ::(?namespace :: Text) => CompiledModule -> (FilePath, Text)
prettyPrint (path, commands) = (path, unlines $ map prettyCommand commands)

prettyCommand :: (?namespace :: Text) => Command -> Text
prettyCommand = \case
    DefaultGamemode gm      -> unwords ["defaultgamemode", prettyGamemode gm]
    Difficulty dif          -> unwords ["difficulty", prettyDifficulty dif]
    Enchant sel ench mlevel -> unwords $ ["enchant", prettySelector sel, prettyNamespaced ench] <> toList (show <$> mlevel)
    Execute execArg         -> unwords ["execute", prettyExecArg execArg]
    Forceload farg          -> unwords ["forceload", prettyForceloadArg farg]
    Function fname          -> unwords ["function", prettyNamespaced fname]
    Gamemode gm msel        -> unwords $ ["gamemode", prettyGamemode gm] <> toList (prettySelector <$> msel)
    Gamerule gamerule arg   -> unwords ["gamerule", gamerule, prettyGameruleArg arg]
    Give sel item mcount    -> unwords $ ["give", prettySelector sel, prettyNamespaced item] <> toList (show <$> mcount)
    Help                    -> "help"
    Kick sel mmessage       -> unwords $ ["kick", prettySelector sel] <> toList mmessage
    Kill sel                -> unwords ["kill", prettySelector sel]
    List                    -> "list"
    ListUUIDs               -> "listuuids"
    Locate t                -> unwords ["locate", t]
    LocateBiome nn          -> unwords ["locatebiome", prettyNamespaced nn]
    Me msg                  -> unwords ["me", msg]
    Msg sel msg             -> unwords ["msg", prettySelector sel, msg]
    Publish                 -> "publish"
    Reload                  -> "reload"
    Say msg                 -> unwords ["say", msg]
    Schedule sarg fname     -> unwords ["schedule", prettyScheduleArg sarg, prettyNamespaced fname]
    Scoreboard sbarg        -> unwords ["scoreboard", prettyScoreboardArg sbarg]
    Seed                    -> "seed"
    Summon nn msarg         -> unwords $ ["summon", prettyNamespaced nn] <> toList (prettySummonArg <$> msarg)
    _ -> undefined

prettyGamemode :: Gamemode -> Text
prettyGamemode = \case 
    Survival  -> "survival"
    Creative  -> "creative"
    Adventure -> "adventure"
    Spectator -> "spectator"

prettyDifficulty :: Difficulty -> Text
prettyDifficulty = \case  
    Peaceful    -> "peaceful"
    Easy        -> "easy"
    Normal      -> "normal"
    Hard        -> "hard"

prettySelector :: (?namespace :: Text) => Selector -> Text
prettySelector = \case
    LiteralPlayer name -> name
    NamespacedPlayer nn -> prettyNamespaced nn
    Entity args         -> "@e[" <> intercalate "," (map prettySelectorArg args) <> "]"
    AllPlayers args     -> "@a[" <> intercalate "," (map prettySelectorArg args) <> "]"
    NearestPlayer args  -> "@p[" <> intercalate "," (map prettySelectorArg args) <> "]"
    RandomPlayer args   -> "@r[" <> intercalate "," (map prettySelectorArg args) <> "]"
    Self args           -> "@s[" <> intercalate "," (map prettySelectorArg args) <> "]"

prettySelectorArg :: (?namespace :: Text) => SelectorArg -> Text
prettySelectorArg = \case
    SType nn        -> "type=" <> prettyNamespaced nn
    SPredicate nn   -> "predicate=" <> prettyNamespaced nn
    SScores scores  -> "scores={" <> intercalate "," (map (\(obj, s) -> obj <> "=" <> prettyRange s) scores) <> "}"
    STag tag        -> "tag=" <> tag

prettyNamespaced :: (?namespace :: Text) => NamespacedName -> Text
prettyNamespaced = \case
    Own n -> ?namespace <> ":" <> n
    Foreign foreignNS n -> foreignNS <> ":" <> n

prettyExecArg :: (?namespace :: Text) => ExecuteArg  -> Text
prettyExecArg = \case
    EAnchored eaa ea    -> unwords ["anchored", prettyAnchoredArg eaa, prettyExecArg ea]
    EAs selector ea     -> unwords ["as", prettySelector selector, prettyExecArg ea]
    EAt selector ea     -> unwords ["at", prettySelector selector, prettyExecArg ea]
    EIf eia ea          -> unwords ["if", prettyEIfArg eia, prettyExecArg ea]
    EIn nn ea           -> unwords ["at", prettyNamespaced nn, prettyExecArg ea]
    ERun com            -> unwords ["run", prettyCommand com]

prettyAnchoredArg :: EAnchoredArg -> Text
prettyAnchoredArg = \case
  Eyes -> "eyes"
  Feet -> "feet"

prettyEIfArg :: (?namespace :: Text) => EIfArg -> Text
prettyEIfArg = \case
    IEntity sel         -> unwords ["entity", prettySelector sel]
    IPredicate nn       -> unwords ["predicate", prettyNamespaced nn]
    IScore se obj isa   -> unwords ["score", prettySelector se, obj, prettyIfScoreArg isa]

prettyIfScoreArg :: (?namespace :: Text) => IfScoreArg -> Text
prettyIfScoreArg = \case
    ILT sel obj -> unwords ["<", prettySelector sel, obj]
    ILE sel obj -> unwords ["<=", prettySelector sel, obj]
    IEQ sel obj -> unwords ["=", prettySelector sel, obj]
    IGT sel obj -> unwords [">", prettySelector sel, obj]
    IGE sel obj -> unwords [">=", prettySelector sel, obj]
    IMatches ra -> unwords ["matches", prettyRange ra]


prettyRange :: Range -> Text
prettyRange = \case
    n :.. i -> show n <> ".." <> show i
    RLE n -> ".." <> show n
    RGE n -> show n <> ".."
    REQ n -> show n


prettyForceloadArg :: ForceloadArg -> Text
prettyForceloadArg = \case 
    FAdd x y -> unwords ["add", show x, show y]

prettyGameruleArg :: GameruleArg -> Text
prettyGameruleArg = \case
    GInt n -> show n
    GBool True -> "true"
    GBool False -> "false"


prettyScheduleArg :: ScheduleArg -> Text
prettyScheduleArg = \case
    SClear -> "clear"
    SFunction -> "function"

prettyScoreboardArg :: (?namespace :: Text) => ScoreboardArg -> Text
prettyScoreboardArg = \case
    Players psa     -> unwords ["players", prettyPlayerScoreboardArg psa]
    Objectives osa  -> unwords ["objectives", prettyObjectivesScoreboardArg osa]

prettyPlayerScoreboardArg :: (?namespace :: Text) => PlayerScoreboardArg -> Text
prettyPlayerScoreboardArg = \case
    Add se txt n    -> unwords ["add", txt, show n]
    Enable se txt   -> unwords ["enable", prettySelector se, txt]
    Get se txt      -> unwords ["get", prettySelector se, txt] 
    SList m_se      -> unwords $ ["list"] <> toList (prettySelector <$> m_se)
    Operation se txt so se' txt' -> unwords ["operation", prettySelector se, txt, prettySOperation so, prettySelector se', txt']
    Remove se txt n -> unwords ["remove", prettySelector se, txt, show n]
    Reset se m_txt  -> unwords $ ["reset", prettySelector se] <> toList m_txt
    Set se txt n    -> unwords ["set", prettySelector se, txt, show n]
    
prettyObjectivesScoreboardArg :: ObjectiveScoreboardArg -> Text
prettyObjectivesScoreboardArg = \case
    OAdd txt txt' m_txt     -> unwords $ ["add", txt, txt'] <> toList m_txt
    ORemove txt             -> unwords ["remove", txt]
    OModify oma             -> unwords ["modify", prettyOModifyArg oma]
    OList                   -> "list"
    OSetDisplay txt txt'    -> unwords ["setdisplay", txt, txt']

prettySOperation :: SOperation -> Text
prettySOperation = \case
    SMod -> "%="
    SMul -> "*="
    SAdd -> "+="
    SSub -> "-="
    SDiv -> "/="
    SMin -> "<"
    SAssign -> "="
    SMax -> ">"
    SSwap -> "><"


prettyOModifyArg :: OModifyArg -> Text
prettyOModifyArg = \case
    DisplayName txt -> unwords ["displayname", txt]
    RenderType omrt -> unwords $ ["rendertype", prettyOModifyRenderType omrt]

prettyOModifyRenderType :: OModifyRenderType -> Text
prettyOModifyRenderType = \case
    RHearts -> "hearts"
    RIntegers -> "integers"


prettySummonArg :: SummonArg -> Text
prettySummonArg (SummonArg pos m_nbt) = unwords $ [prettyPosition pos] <> toList (prettyNBT <$> m_nbt)

prettyPosition :: Position -> Text
prettyPosition = \case 
  Abs x y z -> unwords (map show [x, y, z])
  Rel x y z -> unwords (map (("~"<>) . show) [x, y, z]) 

prettyNBT :: NBT -> Text
prettyNBT = error "NBT is NYI"


