{-# LANGUAGE LambdaCase #-}
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Distribution.Simple             hiding ( Module(..) )
import           Distribution.Simple.PreProcess
import           Distribution.Types.BuildInfo
import           Distribution.Types.LocalBuildInfo
import qualified Language.Haskell.Extension      as C
import           Language.Haskell.Exts.Comments
import qualified Language.Haskell.Exts.Extension as H
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           System.Directory
import           System.FilePath

main = do
  modRender' <- newEmptyMVar
  defaultMainWithHooks simpleUserHooks
    { hookedPreProcessors = [ ("chs", \bi lbi clbi ->
      PreProcessor False $ \(iD, iF) (oD, oF) verbosity -> do
        parseModuleFile "src/Graphics/Cairo/Render.hs" (defaultExtensions bi) >>= void . tryPutMVar modRender' . fst
        (runPreProcessor $ ppC2hs bi lbi clbi) (iD, iF) (oD, oF) verbosity
        renameFile (oD </> oF) (oD </> (oF ++ ".c2hs"))
        m <- parseModuleFile (oD </> (oF ++ ".c2hs")) (defaultExtensions bi)
        let lifted = liftIO m
        writeFile (oD </> oF) $ prettyPrint $ refineExports lifted
        modifyMVar_ modRender' $ \modRender -> do
          let r = genRender modRender (lifted, [])
          writeFile (oD </> "Graphics/Cairo/Render.hs") $ prettyPrint r
          return r)
                            , ("hs", \bi _ _ ->
      PreProcessor False $ \(iD, iF) (oD, oF) _ -> do
        parseModuleFile "src/Graphics/Cairo/Render.hs" (defaultExtensions bi) >>= void . tryPutMVar modRender' . fst
        modifyMVar_ modRender' $ \modRender -> do
          r <- genRender modRender <$> parseModuleFile (iD </> iF) (defaultExtensions bi)
          writeFile (oD </> "Graphics/Cairo/Render.hs") $ prettyPrint r
          return r) ] }

refineExports (Module a (Just (ModuleHead b f g Nothing)) c d e) = refineExports' a b c d e f g noSrcSpan
refineExports (Module a (Just (ModuleHead b f g (Just (ExportSpecList h [])))) c d e) = refineExports' a b c d e f g h
refineExports x = x
refineExports' a b c d e f@(ModuleName _ mn) g h = Module a (Just $ ModuleHead b f g $ Just $ ExportSpecList h exports) c d e
  where
    exports = catMaybes $ concatMap (\case
      FunBind _ matches -> map (\(Match _ (Ident _ n) _ _ _) -> if (last n) `elem` ['\'', '_']
        then Nothing
        else Just $ EVar noSrcSpan $ Qual noSrcSpan (ModuleName noSrcSpan mn) $ Ident noSrcSpan n) matches
      TypeDecl _ dh _ -> [Just $ EAbs noSrcSpan (NoNamespace noSrcSpan) $ Qual noSrcSpan (ModuleName noSrcSpan mn) $ Ident noSrcSpan $ nameOfDeclHead dh]
      DataDecl _ _ _ dh _ _ -> [Just $ EThingWith noSrcSpan (EWildcard noSrcSpan 0) (Qual noSrcSpan (ModuleName noSrcSpan mn) $ Ident noSrcSpan $ nameOfDeclHead dh) []]
      PatBind _ (PVar _ (Ident _ n)) _ _ -> [Just $ EAbs noSrcSpan (NoNamespace noSrcSpan) $ Qual noSrcSpan (ModuleName noSrcSpan mn) $ Ident noSrcSpan n]
      _ -> [Nothing]) e

nameOfDeclHead (DHead _ (Ident _ n)) = n
nameOfDeclHead (DHApp _ x _) = nameOfDeclHead x

genRender modRender (Module _ (Just (ModuleHead _ (ModuleName _ mn) _ _)) _ _ e, comments)
  | mn `elem` ["Graphics.Cairo.Render", "Graphics.Cairo.Types"] = modRender
  | otherwise =
    foldl (\modRender decl -> case decl of
      n@(TypeSig _ a t@(TyFun _ (TyCon _ (UnQual _ (Ident _ "Context"))) b)) ->
        addRender (names [n]) (count t) mn $
          (\(Module a' b' c' d' e') -> Module a' b' c' d' ((TypeSig noSrcSpan a $ addType b) : e')) $
            updateImport mn modRender
      n@(TypeSig _ a (TyForall _ b c t@(TyFun _ (TyCon _ (UnQual _ (Ident _ "Context"))) d))) ->
        addRender (names [n]) (count t) mn $
          (\(Module a' b' c' d' e') -> Module a' b' c' d' ((TypeSig noSrcSpan a $ TyForall noSrcSpan b c $ addType d) : e')) $
            updateImport mn modRender
      n@(TypeSig _ a t@(TyFun _ (TyParen _ (TyCon _ (UnQual _ (Ident _ "Context")))) b)) ->
        addRender (names [n]) (count t) mn $
          (\(Module a' b' c' d' e') -> Module a' b' c' d' ((TypeSig noSrcSpan a $ addType b) : e')) $
            updateImport mn modRender
      n@(TypeSig _ a (TyForall _ b c t@(TyFun _ (TyParen _ (TyCon _ (UnQual _ (Ident _ "Context")))) d))) ->
        addRender (names [n]) (count t) mn $
          (\(Module a' b' c' d' e') -> Module a' b' c' d' ((TypeSig noSrcSpan a $ TyForall noSrcSpan b c $ addType d) : e')) $
            updateImport mn modRender
      _ -> modRender) modRender e

addType (TyFun _ a t) = TyFun noSrcSpan a $ addType t
addType (TyApp _ (TyCon _ (UnQual _ (Ident _ "m"))) a) =
  TyApp noSrcSpan (TyApp noSrcSpan (TyCon noSrcSpan $ UnQual noSrcSpan $ Ident noSrcSpan "Render") (TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "m")))) a

updateImport mn m@(Module a b c d e) =
  let newImport = ImportDecl noSrcSpan (ModuleName noSrcSpan mn) True False False Nothing Nothing Nothing
  in case any (newImport ==) d of
    True -> m
    False -> Module a b c (newImport : d) e

addRender ns c mn (Module a b cc d e) = Module a b cc d $ e ++
  map (\n -> FunBind noSrcSpan [Match noSrcSpan (Ident noSrcSpan n)
               (map (\i -> PVar noSrcSpan (Ident noSrcSpan ("v" ++ show i))) [2..c])
               (UnGuardedRhs noSrcSpan $ fromParseResult $ parseExp $ intercalate "\n"
                 [ "Render $ do"
                 , "  context <- ask"
                 , concat ["  ", mn, ".", n, " context ", intercalate " " $ map ((++) "v" . show) [2..c]]])
              Nothing]) ns

count = sum . unfoldr (\case
  TyFun _ _ n -> Just (1, n)
  _ -> Nothing)

liftIO (Module a b c d e, comments) =
  let (typeSig2LiftIO, rest) = detectFunctions2LiftIO e comments in
  if skipModule comments || null typeSig2LiftIO
    then Module a b c d e
    else Module a b c (importMonadIO : d)
      $ map liftIOTypeSig typeSig2LiftIO ++ map (liftIOFunction (names typeSig2LiftIO)) rest

parseModuleFile fp exts = parseModuleWithComments
  (defaultParseMode { parseFilename = fp
                    , extensions    = H.EnableExtension H.GADTs : map why exts })
  <$> readFile fp >>= \case
    ParseFailed l s -> fail (s ++ ": " ++ fp ++ "\n" ++ show l)
    ParseOk r -> return r

why :: C.Extension -> H.Extension
why = read . show

skipModule = any (\(Comment ml _ comment) ->
  not ml && comment == " λ SKIP MODULE")

detectFunctions2LiftIO decls comments = flip partition decls $ \case
  TypeSig _ ns (TyForall _ _ _ t) -> checkType t && all (\(Ident _ n) -> not $ checkAnn n comments) ns
  TypeSig _ ns (TyFun _ _ t) -> checkType t && all (\(Ident _ n) -> not $ checkAnn n comments) ns
  _ -> False
  -- TypeSig _ _ (TyList _ _) -> (decl, False)
  -- TypeSig _ _ (TyCon _ (UnQual _ (Ident _ _))) ->
  --   (decl, False)
  -- TypeSig _ _ (TyParen _ (TyCon _ (UnQual _ (Ident _ _))))
  --   -> (decl, False)

names = concatMap (\(TypeSig _ ns _) -> map (\(Ident _ n) -> n) ns)

liftIOFunction ns decl
  | PatBind a f@(PVar _ (Ident _ b)) (UnGuardedRhs c d) e <- decl
  , b `elem` ns =
    PatBind a f (UnGuardedRhs c $ InfixApp noSrcSpan
      (Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "liftIO")))
      (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan ".")))
      d) e
  | FunBind a bs <- decl = FunBind a $ map (liftIOFunction' ns) bs
  | otherwise = decl

liftIOFunction' ns decl
  | Match a b@(Ident _ g) c (UnGuardedRhs f d) e <- decl
  , g `elem` ns =
    Match a b c (UnGuardedRhs f $ InfixApp noSrcSpan
      (Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "liftIO")))
      (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "$")))
      d) e
  | otherwise = decl

checkType (TyFun _ _ t) = checkType t
checkType (TyApp _ (TyCon _ (UnQual _ (Ident _ "IO"))) _) = True
-- checkType (TyCon _ (UnQual _ (Ident _ _))) = False
-- checkType (TyVar _ (Ident  _ _          )) = False
checkType _ = False
checkAnn n = any (\(Comment ml _ comment) ->
  not ml && comment == (" λ SKIP " ++ n))

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

replaceIO (TyFun a b c) = TyFun a b $ replaceIO c
replaceIO (TyApp a (TyCon b (UnQual c (Ident d "IO"))) e) =
  TyApp a (TyCon b (UnQual c (Ident d "m"))) e

importMonadIO = ImportDecl noSrcSpan
  (ModuleName noSrcSpan "Control.Monad.IO.Class")
  False False False Nothing Nothing Nothing

contextMonadIO = ClassA noSrcSpan
  (UnQual noSrcSpan (Ident noSrcSpan "MonadIO"))
  [TyVar noSrcSpan (Ident noSrcSpan "m")]
