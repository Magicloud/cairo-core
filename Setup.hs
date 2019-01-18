{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase      #-}
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Distribution.Simple             hiding ( Module(..) )
import           Distribution.Simple.PreProcess
import           Distribution.Types.BuildInfo
import qualified Language.Haskell.Extension      as C
import           Language.Haskell.Exts.Comments
import qualified Language.Haskell.Exts.Extension as H
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           System.Directory
import           System.FilePath

main :: IO ()
main = do
  modRender' <- newEmptyMVar
  defaultMainWithHooks simpleUserHooks
    { hookedPreProcessors = [ ("chs", \bi lbi clbi ->
      PreProcessor False $ \(iD, iF) (oD, oF) verbosity -> do
        (runPreProcessor $ ppC2hs bi lbi clbi) (iD, iF) (oD, oF) verbosity
        renameFile (oD </> oF) (oD </> (oF ++ ".c2hs"))
        liftIOwrapper bi (oD </> (oF ++ ".c2hs")) (oD </> oF)
        putStrLn ("Render: " ++ iF)
        render bi modRender' oD (oD </> oF))
                            , ("hs", \bi _ _ ->
      PreProcessor False $ \(iD, iF) (oD, oF) _ -> do
        copyFile (iD </> iF) (oD </> oF)
        putStrLn ("Render: " ++ iF)
        render bi modRender' oD (oD </> oF))]}

render :: BuildInfo
          -> MVar (Module SrcSpanInfo) -> FilePath -> FilePath -> IO ()
render bi mvar dir inFile = do
  isE <- isEmptyMVar mvar
  when isE $ do
    doesE <- doesFileExist (dir </> "Graphics/Cairo/Render.hs")
    let renderFile = if doesE
                      then dir </> "Graphics/Cairo/Render.hs"
                      else "src/Graphics/Cairo/Render.hs"
    parseModuleFile renderFile (defaultExtensions bi) >>=
      void . tryPutMVar mvar . fst
  modifyMVar_ mvar $ \modRender -> do
    r <- genRender modRender <$> parseModuleFile inFile (defaultExtensions bi)
    writeFile (dir </> "Graphics/Cairo/Render.hs") $ prettyPrint r
    return r

liftIOwrapper :: BuildInfo -> FilePath -> FilePath -> IO ()
liftIOwrapper bi iF oF = do
  m <- parseModuleFile iF (defaultExtensions bi)
  let lifted = liftIO m
  writeFile oF $ prettyPrint $ refineExports lifted

getLanguagePragmas :: String -> [H.Extension]
getLanguagePragmas content = case getTopPragmas content of
  ParseFailed _ _ -> []
  ParseOk pragmas -> concatMap (\case
    LanguagePragma _ lps -> map (\(Ident _ n) -> H.EnableExtension $ read n) lps
    _ -> []) pragmas

refineExports :: Module SrcSpanInfo -> Module SrcSpanInfo
refineExports (Module a (Just (ModuleHead b f g Nothing)) c d e) = refineExports' a b c d e f g noSrcSpan
refineExports (Module a (Just (ModuleHead b f g (Just (ExportSpecList h [])))) c d e) = refineExports' a b c d e f g h
refineExports x = x

refineExports' :: SrcSpanInfo
                  -> SrcSpanInfo
                  -> [ModulePragma SrcSpanInfo]
                  -> [ImportDecl SrcSpanInfo]
                  -> [Decl SrcSpanInfo]
                  -> ModuleName SrcSpanInfo
                  -> Maybe (WarningText SrcSpanInfo)
                  -> SrcSpanInfo
                  -> Module SrcSpanInfo
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

exportType :: String -> String -> ExportSpec SrcSpanInfo
exportType mn n = EAbs noSrcSpan (NoNamespace noSrcSpan) $ Qual noSrcSpan (ModuleName noSrcSpan mn) $ Ident noSrcSpan n

exportData :: String -> String -> ExportSpec SrcSpanInfo
exportData mn n = EThingWith noSrcSpan (EWildcard noSrcSpan 0) (Qual noSrcSpan (ModuleName noSrcSpan mn) $ Ident noSrcSpan n) []

exportPattern :: String -> String -> ExportSpec SrcSpanInfo
exportPattern mn n = EAbs noSrcSpan (NoNamespace noSrcSpan) $ Qual noSrcSpan (ModuleName noSrcSpan mn) $ Ident noSrcSpan n

exportFunction :: String -> String -> ExportSpec SrcSpanInfo
exportFunction mn n = EVar noSrcSpan $ Qual noSrcSpan (ModuleName noSrcSpan mn) $ Ident noSrcSpan n

notExport :: [Char] -> Bool
notExport n = last n `elem` ['\'', '_']

nameOfDeclHead :: DeclHead SrcSpanInfo -> String
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

mergeDecl :: Module SrcSpanInfo -> [Decl SrcSpanInfo] -> Module SrcSpanInfo
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

liftIO :: (Module SrcSpanInfo, [Comment]) -> Module SrcSpanInfo
liftIO (Module a' b' c' d' e', comments) =
  let name2LiftIO = detectFunctions2LiftIO e' comments
  in if skipModule comments || null name2LiftIO
      then Module a' b' c' d' e'
      else Module a' b' c' (importMonadIO : d') $ concatMap (\case
        TypeSig a ns b -> map (\n -> if n `isIn` name2LiftIO
          then liftIOTypeSig $ TypeSig a [n] b
          else TypeSig a [n] b) ns
        g@(PatBind a f@(PVar _ b) (UnGuardedRhs c d) e) -> [if b `isIn` name2LiftIO
          then PatBind a f (UnGuardedRhs c $ InfixApp noSrcSpan
                                                      (Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "liftIO")))
                                                      (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan ".")))
                                                      d) e
          else g]
        FunBind a bs -> map (\b@(Match c d e (UnGuardedRhs f g) h) -> if d `isIn` name2LiftIO
          then FunBind a [Match c d e (UnGuardedRhs f $ InfixApp noSrcSpan
                                                          (Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "liftIO")))
                                                          (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "$")))
                                                          g) h]
          else FunBind a [b]) bs
        x -> [x]) e'
liftIO x = error $ show x

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

detectFunctions2LiftIO :: [Decl SrcSpanInfo] -> [Comment] -> [Name SrcSpanInfo]
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

liftIOTypeSig :: Decl SrcSpanInfo -> Decl SrcSpanInfo
liftIOTypeSig (TypeSig a b z@TyFun{}) = TypeSig a b
  (TyForall noSrcSpan Nothing
    (Just $ CxTuple noSrcSpan
      [ ClassA noSrcSpan
               (UnQual noSrcSpan (Ident noSrcSpan "MonadIO"))
               [TyVar noSrcSpan (Ident noSrcSpan "m")] ])
    (replaceIO z))
liftIOTypeSig (TypeSig a b (TyForall c d (Just (CxSingle e f)) z)) =
  TypeSig a b (TyForall c d
    (Just $ CxTuple e [f, contextMonadIO]) (replaceIO z))
liftIOTypeSig (TypeSig a b (TyForall c d (Just (CxTuple e f)) z)) =
  TypeSig a b
    (TyForall c d (Just $ CxTuple e $ contextMonadIO : f) (replaceIO z))
liftIOTypeSig x = error $ show x

replaceIO :: Type SrcSpanInfo -> Type SrcSpanInfo
replaceIO (TyFun a b c) = TyFun a b $ replaceIO c
replaceIO (TyApp a (TyCon b (UnQual c (Ident d "IO"))) e) =
  TyApp a (TyCon b (UnQual c (Ident d "m"))) e
replaceIO x = error $ show x

importMonadIO :: ImportDecl SrcSpanInfo
importMonadIO = ImportDecl noSrcSpan
  (ModuleName noSrcSpan "Control.Monad.IO.Class")
  False False False Nothing Nothing Nothing

contextMonadIO :: Asst SrcSpanInfo
contextMonadIO = ClassA noSrcSpan
  (UnQual noSrcSpan (Ident noSrcSpan "MonadIO"))
  [TyVar noSrcSpan (Ident noSrcSpan "m")]
