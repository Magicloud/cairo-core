{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Exception
import qualified Data.ByteString.Lazy as LBS
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Tree.NTree.TypeDefs
import           Distribution.Simple             hiding ( Module(..) )
import           Distribution.Simple.PreProcess
import           Distribution.Types.BuildInfo
import qualified Language.Haskell.Extension      as C
import           Language.Haskell.Exts.Comments
import qualified Language.Haskell.Exts.Extension as H
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax    hiding (XTag, XAttr)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           System.Directory
import           System.FilePath
import           Text.XML.HXT.DOM.ShowXml
import           Text.XML.HXT.DOM.TypeDefs
import           Text.XML.HXT.Parser.HtmlParsec
import           Text.XML.HXT.XPath

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPreProcessors = [ ("chs", \bi lbi clbi ->
    PreProcessor False $ \(iD, iF) (oD, oF) verbosity -> do
      catch (do
        (runPreProcessor $ ppC2hs bi lbi clbi) (iD, iF) (oD, oF) verbosity
        rmLINE (oD </> oF)
        bindingDoc (oD </> oF)
        c2hsWrapper bi (oD </> oF)) (\(e :: SomeException) -> do
          print e
          copyFile (oD </> oF) "/tmp/fuck"
          removeFile (oD </> oF))
      render bi oD (oD </> oF))
                          , ("hs", \bi _ _ ->
    PreProcessor False $ \(iD, iF) (oD, oF) _ -> do
      copyFile (iD </> iF) (oD </> oF)
      bindingDoc (oD </> oF)
      render bi oD (oD </> oF))]}

rmLINE :: FilePath -> IO ()
rmLINE fp = do
  readFile fp >>=
    writeFile (fp ++ ".fuck") . unlines . filter (\line -> not $ "{-# LINE " `isPrefixOf` line && "\" #-}" `isSuffixOf` line) . lines
  renameFile (fp ++ ".fuck") fp

bindingDoc :: FilePath -> IO ()
bindingDoc fp = readFile fp >>=
  mapM (\line' -> if "-- λ http" `isPrefixOf` line'
    then do
      doc <- mkDoc $ drop 5 line'
      return $ "{- |" ++ doc ++ "-}"
    else case seqPos "λ http" line' of
      Nothing -> return line'
      Just i -> (++) (take i line') <$> mkDoc (drop (i + 2) line')) . lines >>=
  writeFile fp . unlines

seqPos :: Eq a => [a] -> [a] -> Maybe Int
seqPos x xs =
  let xs' = filter (isPrefixOf x) $ tails xs
  in if not $ null xs'
    then Just $ length xs - length (head xs')
    else Nothing

mkDoc :: String -> IO String
mkDoc line = map (\case
  '\160' -> ' '
  x -> x) <$> case '#' `elemIndex` line of
  Just i -> do
    let url = take i line
        archor = drop (i + 1) line
    defShow . head . getXPath ("//a[@name='" ++ archor ++ "']/..") <$> cache url
  Nothing -> case ' ' `elemIndex` line of
    Just i -> do
      let url = take i line
          xpath = drop (i + 1) line
      xshow . getXPath xpath <$> cache url
    Nothing -> error line

strip :: String -> String
strip qeds
  | head qeds == '"'
  , last qeds == '"'
  = tail $ init qeds
  | otherwise = qeds

haddockEscape :: String -> String
haddockEscape = concatMap (\c ->
  if c `elem` "/"
    then ['\\', c]
    else [c])

defShow :: NTree XNode -> String
defShow (NTree (XTag div' _) contents)
  | div' == mkName "div"
  = intercalate "\n" $ map defShow contents
defShow (NTree (XTag p' [NTree (XAttr class') [NTree (XText "since") []]]) [NTree (XText since) []])
  | p' == mkName "p"
  , class' == mkName "class"
  , "Since: " `isPrefixOf` since
  = "@since " ++ drop 7 since
  | otherwise = show since
defShow (NTree (XTag h' []) inners)
  | h' `elem` map (mkName . (:) 'h' . show) [1 .. 9 :: Int]
  = replicate (read $ drop 1 $ strip $ show h') '=' ++ " " ++ concatMap defShow inners
defShow (NTree (XTag p' _) inners)
  | p' == mkName "p"
  = map (\case
      '\n' -> ' '
      x -> x) $ concatMap defShow inners
defShow (NTree (XText t) inners) = if t == "\n"
    then concatMap defShow inners
    else t ++ concatMap defShow inners
defShow (NTree (XTag a' [NTree (XAttr name') _]) [])
  | a' == mkName "a"
  , name' == mkName "name"
  = "" -- Ignore HTML anchor.
defShow (NTree (XTag pre' _) code)
  | pre' == mkName "pre"
  = intercalate "\n" ["@", concatMap defShow code, "@"]
defShow t@(NTree (XTag table' _) _)
  | table' == mkName "table"
  , not $ null $ getXPath "/table[@class='informaltable' and colgroup/col/@class='struct_members_name' and colgroup/col/@class='struct_members_description' and colgroup/col/@class='struct_members_annotations']" t
  = intercalate "\n" $ map (\tr ->
    let name = concatMap defShow $ getXPath "/tr/td[@class='struct_member_name']/child::node()" tr
        desc = concatMap defShow $ getXPath "/tr/td[@class='struct_member_description']/child::node()" tr
        anno = concatMap defShow $ getXPath "/tr/td[@class='struct_member_annotations']/child::node()" tr
    in "[" ++ name ++ "]: " ++ desc ++ "\n\n    " ++ anno) $ getXPath "/table/tbody/tr" t
defShow t@(NTree (XTag table' _) _)
  | table' == mkName "table"
  , not $ null $ getXPath "/table[@class='informaltable' and colgroup/col/@class='enum_members_name' and colgroup/col/@class='enum_members_description' and colgroup/col/@class='enum_members_annotations']" t
  = intercalate "\n" $ map (\tr ->
    let name = concatMap defShow $ getXPath "/tr/td[@class='enum_member_name']/child::node()" tr
        desc = concatMap defShow $ getXPath "/tr/td[@class='enum_member_description']/child::node()" tr
        anno = concatMap defShow $ getXPath "/tr/td[@class='enum_member_annotations']/child::node()" tr
    in "[" ++ name ++ "]: " ++ desc ++ "\n\n    " ++ anno) $ getXPath "/table/tbody/tr" t
defShow t@(NTree (XTag table' _) _)
  | table' == mkName "table"
  , not $ null $ getXPath "/table[@class='informaltable' and colgroup/col/@class='parameters_name' and colgroup/col/@class='parameters_description' and colgroup/col/@class='parameters_annotations']" t
  = intercalate "\n" $ map (\tr ->
    let name = concatMap defShow $ getXPath "/tr/td[@class='parameter_name']/child::node()" tr
        desc = concatMap defShow $ getXPath "/tr/td[@class='parameter_description']/child::node()" tr
        anno = concatMap defShow $ getXPath "/tr/td[@class='parameter_annotations']/child::node()" tr
    in "[" ++ name ++ "]: " ++ desc ++ "\n\n    " ++ anno) $ getXPath "/table/tbody/tr" t
defShow t@(NTree (XTag table' _) _)
  | table' == mkName "table"
  , not $ null $ getXPath "/table[@class='listing_frame']/tbody/tr/td[@class='listing_code']/pre[@class='programlisting']" t
  = let code = xshow $ getXPath "string(/table[@class='listing_frame']/tbody/tr/td[@class='listing_code']/pre[@class='programlisting'])" t
    in "@\n" ++ code ++ "\n@"
defShow (NTree (XTag a' attrs) inners)
  | a' == mkName "a"
  = let [NTree _ [NTree (XText url') _]] = filter (attrMatch "href") attrs
        url = haddockEscape $ if "http" `isPrefixOf` url'
          then url'
          else "https://www.cairographics.org/manual/" ++ url'
    in "[" ++ concatMap defShow inners ++ "](" ++ url ++ ")"
defShow (NTree (XTag hr' _) _)
  | hr' == mkName "hr"
  = "\n"
defShow (NTree (XTag br' _) _)
  | br' == mkName "br"
  = "\n"
defShow (NTree (XTag ul' _) lis)
  | ul' == mkName "ul"
  = intercalate "\n" $ map ulShow lis
defShow (NTree (XTag ol' _) lis)
  | ol' == mkName "ol"
  = intercalate "\n" $ map olShow $ zip lis [1..]
defShow (NTree (XTag code' _) inners)
  | code' == mkName "code"
  = concatMap defShow inners
defShow (NTree (XTag em' _) inners)
  | em' == mkName "em"
  = "/" ++ concatMap defShow inners ++ "/"
defShow (NTree (XTag span' _) inners)
  | span' == mkName "span"
  = concatMap defShow inners
defShow (NTree (XTag b' _) inners)
  | b' == mkName "b"
  = concatMap defShow inners
defShow x = error $ show x

ulShow :: NTree XNode -> String
ulShow (NTree (XTag li' _) xs)
  | li' == mkName "li"
  = "- " ++ concatMap defShow xs
ulShow x = defShow x

olShow :: (NTree XNode, Int) -> String
olShow (NTree (XTag li' _) xs, i)
  | li' == mkName "li"
  = show i ++ ". " ++ concatMap defShow xs
olShow (x, _) = defShow x

attrMatch :: String -> NTree XNode -> Bool
attrMatch attrName (NTree (XAttr attr') _)
  | attr' == mkName attrName
  = True
attrMatch _ _ = False

cache :: String -> IO (NTree XNode)
cache url = do
  tmpD <- getTemporaryDirectory
  let pageName = tmpD </> map (\x -> if isAlphaNum x
                  then x
                  else '_') url
  doesFileExist pageName >>= \case
    True -> return ()
    False -> do
      req <- parseRequest url
      newTlsManager >>= httpLbs req >>= LBS.writeFile pageName . responseBody
  last . parseHtmlContent <$>
    readFile pageName

render :: BuildInfo -> FilePath -> FilePath -> IO ()
render bi dir inFile = do
  doesE <- doesFileExist (dir </> "Graphics/Cairo/Render.hs")
  let renderFile = if doesE
                    then dir </> "Graphics/Cairo/Render.hs"
                    else "src/Graphics/Cairo/Render.hs"
  (modRender, _) <- parseModuleFile renderFile (defaultExtensions bi)
  r <- genRender modRender <$> parseModuleFile inFile (defaultExtensions bi)
  writeFile (dir </> "Graphics/Cairo/Render.hs") $ prettyPrint r

c2hsWrapper :: BuildInfo -> FilePath -> IO ()
c2hsWrapper bi fp = do
  (m, c) <- parseModuleFile fp (defaultExtensions bi)
  let mc = associateHaddock (m, c)
  if not $ skipModule c
    then writeFile fp $ interleavePrint $ refineExports $ liftIO mc c
    else return ()

interleavePrint :: Module (SrcSpanInfo, [Comment]) -> String
interleavePrint (Module _ b c d e) = intercalate "\n"
  [ intercalate "\n" $ map prettyPrint c
  , case b of
      Nothing -> ""
      Just mh@(ModuleHead _ (ModuleName (_, mc) _) _ _) ->
        intercalate "\n"
          [ commentsPrint mc
          , prettyPrint mh ]
  , intercalate "\n" $ map prettyPrint d
  , intercalate "\n" $ map (\decl -> intercalate "\n"
    [ commentsPrint $ declComments decl
    , prettyPrint decl ]) e ]
interleavePrint x = error $ show x

commentsPrint :: [Comment] -> String
commentsPrint [] = ""
commentsPrint cs = "{-" ++ intercalate "\n" (map (\ (Comment _ _ s) -> s) cs) ++ "-}"

getLanguagePragmas :: String -> [H.Extension]
getLanguagePragmas content = case getTopPragmas content of
  ParseFailed _ _ -> []
  ParseOk pragmas -> concatMap (\case
    LanguagePragma _ lps -> map (\(Ident _ n) -> H.EnableExtension $ read n) lps
    _ -> []) pragmas

refineExports :: Module (SrcSpanInfo, [Comment]) -> Module (SrcSpanInfo, [Comment])
refineExports (Module a (Just (ModuleHead b f g Nothing)) c d e) = refineExports' a b c d e f g noL
refineExports (Module a (Just (ModuleHead b f g (Just (ExportSpecList h [])))) c d e) = refineExports' a b c d e f g h
refineExports x = x

refineExports' :: (SrcSpanInfo, [Comment])
                  -> (SrcSpanInfo, [Comment])
                  -> [ModulePragma (SrcSpanInfo, [Comment])]
                  -> [ImportDecl (SrcSpanInfo, [Comment])]
                  -> [Decl (SrcSpanInfo, [Comment])]
                  -> ModuleName (SrcSpanInfo, [Comment])
                  -> Maybe (WarningText (SrcSpanInfo, [Comment]))
                  -> (SrcSpanInfo, [Comment])
                  -> Module (SrcSpanInfo, [Comment])
refineExports' a b c d e f@(ModuleName _ mn) g h = Module a (Just $ ModuleHead b f g $ Just $ ExportSpecList h exports) c d e
  where
    exports = catMaybes $ concatMap (\case
      FunBind _ matches -> map (\(Match _ (Ident _ n) _ _ _) -> if notExport n
        then Nothing
        else Just $ exportFunction mn n) matches
      TypeDecl _ dh _ -> [Just $ exportType mn $ nameOfDeclHead dh]
      DataDecl _ _ _ dh _ _ -> [Just $ exportData mn $ nameOfDeclHead dh]
      PatBind _ (PVar _ (Ident _ n)) _ _ -> [Just $ exportPattern mn n]
      _ -> [Nothing]) e

exportType :: String -> String -> ExportSpec (SrcSpanInfo, [Comment])
exportType mn n = EAbs noL (NoNamespace noL) $ Qual noL (ModuleName noL mn) $ Ident noL n
exportData :: String -> String -> ExportSpec (SrcSpanInfo, [Comment])
exportData mn n = EThingWith noL (EWildcard noL 0) (Qual noL (ModuleName noL mn) $ Ident noL n) []
exportPattern :: String -> String -> ExportSpec (SrcSpanInfo, [Comment])
exportPattern mn n = EAbs noL (NoNamespace noL) $ Qual noL (ModuleName noL mn) $ Ident noL n
exportFunction :: String -> String -> ExportSpec (SrcSpanInfo, [Comment])
exportFunction mn n = EVar noL $ Qual noL (ModuleName noL mn) $ Ident noL n

notExport :: String -> Bool
notExport n = last n `elem` ['\'', '_']

nameOfDeclHead :: Show l => DeclHead l -> String
nameOfDeclHead (DHead _ (Ident _ n)) = n
nameOfDeclHead (DHApp _ x _) = nameOfDeclHead x
nameOfDeclHead x = error $ show x

genRender :: Module SrcSpanInfo
             -> (Module SrcSpanInfo, [Comment]) -> Module SrcSpanInfo
genRender modRender (Module _ (Just (ModuleHead _ (ModuleName _ mn) _ _)) _ _ e, _)
  | mn `elem` ["Graphics.Cairo.Render", "Graphics.Cairo.Types"] = modRender
  | otherwise =
    let renders = concatMap (\case
                    TypeSig _ a t@(TyFun _ (TyCon _ (UnQual _ (Ident _ "Context"))) b) ->
                      TypeSig noSrcSpan a (liftOut b) : map (funBind (count t) mn) a
                    TypeSig _ a t@(TyFun _ (TyParen _ (TyCon _ (UnQual _ (Ident _ "Context")))) b) ->
                      TypeSig noSrcSpan a (liftOut b) : map (funBind (count t) mn) a
                    TypeSig _ a (TyForall _ b c t@(TyFun _ (TyCon _ (UnQual _ (Ident _ "Context"))) d)) ->
                      TypeSig noSrcSpan a (TyForall noSrcSpan b c $ liftOut d) : map (funBind (count t) mn) a
                    TypeSig _ a (TyForall _ b c t@(TyFun _ (TyParen _ (TyCon _ (UnQual _ (Ident _ "Context")))) d)) ->
                      TypeSig noSrcSpan a (TyForall noSrcSpan b c $ liftOut d) : map (funBind (count t) mn) a
                    _ -> []) e
    in if null renders
      then modRender
      else updateImport mn $ mergeDecl modRender renders
genRender _ x = error $ show x

mergeDecl :: Show l => Module l -> [Decl l] -> Module l
mergeDecl (Module a b c d e) decls = Module a b c d (deleteFirstsBy sameName e decls ++ decls)
mergeDecl x _ = error $ show x

sameName :: Decl l -> Decl l -> Bool
sameName (TypeSig _ [Ident _ n1] _) (TypeSig _ [Ident _ n2] _) = n1 == n2
sameName (FunBind _ [Match _ (Ident _ n1) _ _ _]) (FunBind _ [Match _ (Ident _ n2) _ _ _]) = n1 == n2
sameName _ _ = False

count :: Type l -> Integer
count = sum . unfoldr (\case
  TyFun _ _ n -> Just (1, n)
  _ -> Nothing)

liftOut :: Type SrcSpanInfo -> Type SrcSpanInfo
liftOut (TyFun _ a t) = TyFun noSrcSpan a $ liftOut t
liftOut (TyApp _ (TyVar _ (Ident _ "m")) a) =
  TyApp noSrcSpan (TyApp noSrcSpan (TyCon noSrcSpan $ UnQual noSrcSpan $ Ident noSrcSpan "Render")
                                   (TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "m")))) a
liftOut x = error $ show x

updateImport :: String -> Module SrcSpanInfo -> Module SrcSpanInfo
updateImport mn m@(Module a b c d e) =
  let newImport = ImportDecl noSrcSpan (ModuleName noSrcSpan mn) True False False Nothing Nothing Nothing
  in case any (newImport ==) d of
    True -> m
    False -> Module a b c (newImport : d) e
updateImport _ x = error $ show x

funBind :: Integer -> String -> Name SrcSpanInfo -> Decl SrcSpanInfo
funBind paramNum modName funName = FunBind noSrcSpan
  [Match noSrcSpan funName
          (map (\i -> PVar noSrcSpan (Ident noSrcSpan ("v" ++ show i))) [2..paramNum])
          (UnGuardedRhs noSrcSpan $ fromParseResult $ parseExp $ intercalate "\n"
            [ "Render $ do"
            , "  context <- ask"
            , concat ["  ", modName, ".", prettyPrint funName, " context ", intercalate " " $ map ((++) "v" . show) [2..paramNum]]])
          Nothing]

liftIO :: Module (SrcSpanInfo, [Comment]) -> [Comment] -> Module (SrcSpanInfo, [Comment])
liftIO (Module a' b' c' d' e') comments =
  let name2LiftIO = detectFunctions2LiftIO e' comments
  in if null name2LiftIO
      then Module a' b' c' d' e'
      else Module a' b' c' (importMonadIO : d') $ concatMap (\case
        TypeSig a ns b -> map (\n -> if n `isIn` name2LiftIO
          then liftIOTypeSig $ TypeSig a [n] b
          else TypeSig a [n] b) ns
        g@(PatBind a f@(PVar _ b) (UnGuardedRhs c d) e) -> [if b `isIn` name2LiftIO
          then PatBind a f (UnGuardedRhs c $ InfixApp noL
                                                      (Var noL (UnQual noL (Ident noL "liftIO")))
                                                      (QVarOp noL (UnQual noL (Symbol noL ".")))
                                                      d) e
          else g]
        FunBind a bs -> map (\b@(Match c d e (UnGuardedRhs f g) h) -> if d `isIn` name2LiftIO
          then FunBind a [Match c d e (UnGuardedRhs f $ InfixApp noL
                                                          (Var noL (UnQual noL (Ident noL "liftIO")))
                                                          (QVarOp noL (UnQual noL (Symbol noL "$")))
                                                          g) h]
          else FunBind a [b]) bs
        x -> [x]) e'
liftIO x _ = error $ show x

noL :: (SrcSpanInfo, [Comment])
noL = (noSrcSpan, [])

isIn :: Name l -> [Name l] -> Bool
isIn (Ident _ x) xs = case find (\case
  Ident _ n -> n == x
  Symbol _ n -> n == x) xs of
  Nothing -> False
  _ -> True
isIn (Symbol _ x) xs = case find (\case
  Ident _ n -> n == x
  Symbol _ n -> n == x) xs of
  Nothing -> False
  _ -> True

parseModuleFile :: String
                   -> [Extension] -> IO (Module SrcSpanInfo, [Comment])
parseModuleFile fp exts = do
  c <- readFile fp
  case parseModuleWithComments (defaultParseMode
    { parseFilename = fp
    , extensions = getLanguagePragmas c ++ map why exts }) c of
    ParseFailed l s -> fail (s ++ ": " ++ fp ++ "\n" ++ show l)
    ParseOk r -> return r

why :: C.Extension -> H.Extension
why = read . show

skipModule :: [Comment] -> Bool
skipModule = any (\(Comment ml _ comment) ->
  not ml && comment == " λ SKIP MODULE")

detectFunctions2LiftIO :: [Decl l] -> [Comment] -> [Name l]
detectFunctions2LiftIO decls comments =
  concatMap (\(TypeSig _ ns _) -> ns) $ filter (\case
    TypeSig _ ns (TyForall _ _ _ t) -> checkType t && all (\(Ident _ n) -> not $ checkAnn n comments) ns
    TypeSig _ ns (TyFun _ _ t) -> checkType t && all (\(Ident _ n) -> not $ checkAnn n comments) ns
    _ -> False) decls

checkType :: Type l -> Bool
checkType (TyFun _ _ t) = checkType t
checkType (TyApp _ (TyCon _ (UnQual _ (Ident _ "IO"))) _) = True
checkType _ = False

checkAnn :: String -> [Comment] -> Bool
checkAnn n = any (\(Comment ml _ comment) ->
  not ml && comment == (" λ SKIP " ++ n))

liftIOTypeSig :: Decl (SrcSpanInfo, [Comment]) -> Decl (SrcSpanInfo, [Comment])
liftIOTypeSig (TypeSig a b z@TyFun{}) = TypeSig a b
  (TyForall noL Nothing
    (Just $ CxTuple noL
      [ ClassA noL
               (UnQual noL (Ident noL "MonadIO"))
               [TyVar noL (Ident noL "m")] ])
    (replaceIO z))
liftIOTypeSig (TypeSig a b (TyForall c d (Just (CxSingle e f)) z)) =
  TypeSig a b (TyForall c d
    (Just $ CxTuple e [f, contextMonadIO]) (replaceIO z))
liftIOTypeSig (TypeSig a b (TyForall c d (Just (CxTuple e f)) z)) =
  TypeSig a b
    (TyForall c d (Just $ CxTuple e $ contextMonadIO : f) (replaceIO z))
liftIOTypeSig x = error $ show x

replaceIO :: Show l => Type l -> Type l
replaceIO (TyFun a b c) = TyFun a b $ replaceIO c
replaceIO (TyApp a (TyCon b (UnQual c (Ident d "IO"))) e) =
  TyApp a (TyCon b (UnQual c (Ident d "m"))) e
replaceIO x = error $ show x

importMonadIO :: ImportDecl (SrcSpanInfo, [Comment])
importMonadIO = ImportDecl noL
  (ModuleName noL "Control.Monad.IO.Class")
  False False False Nothing Nothing Nothing

contextMonadIO :: Asst (SrcSpanInfo, [Comment])
contextMonadIO = ClassA noL
  (UnQual noL (Ident noL "MonadIO"))
  [TyVar noL (Ident noL "m")]

declComments :: Decl (a, p) -> p
declComments (TypeDecl (_, c) _ _) = c
declComments (TypeFamDecl (_, c) _ _ _) = c
declComments (ClosedTypeFamDecl (_, c) _ _ _ _) = c
declComments (DataDecl (_, c) _ _ _ _ _) = c
declComments (GDataDecl (_, c) _ _ _ _ _ _) = c
declComments (DataFamDecl (_, c) _ _ _) = c
declComments (TypeInsDecl (_, c) _ _) = c
declComments (DataInsDecl (_, c) _ _ _ _) = c
declComments (GDataInsDecl (_, c) _ _ _ _ _) = c
declComments (ClassDecl (_, c) _ _ _ _) = c
declComments (InstDecl (_, c) _ _ _) = c
declComments (DerivDecl (_, c) _ _ _) = c
declComments (InfixDecl (_, c) _ _ _) = c
declComments (DefaultDecl (_, c) _) = c
declComments (SpliceDecl (_, c) _) = c
declComments (TypeSig (_, c) _ _) = c
declComments (PatSynSig (_, c) _ _ _ _ _) = c
declComments (FunBind (_, c) _) = c
declComments (PatBind (_, c) _ _ _) = c
declComments (PatSyn (_, c) _ _ _) = c
declComments (ForImp (_, c) _ _ _ _ _) = c
declComments (ForExp (_, c) _ _ _ _) = c
declComments (RulePragmaDecl (_, c) _) = c
declComments (DeprPragmaDecl (_, c) _) = c
declComments (WarnPragmaDecl (_, c) _) = c
declComments (InlineSig (_, c) _ _ _) = c
declComments (InlineConlikeSig (_, c) _ _) = c
declComments (SpecSig (_, c) _ _ _) = c
declComments (SpecInlineSig (_, c) _ _ _ _) = c
declComments (InstSig (_, c) _) = c
declComments (AnnPragma (_, c) _) = c
declComments (MinimalPragma (_, c) _) = c
declComments (RoleAnnotDecl (_, c) _ _) = c
declComments (CompletePragma (_, c) _ _) = c
