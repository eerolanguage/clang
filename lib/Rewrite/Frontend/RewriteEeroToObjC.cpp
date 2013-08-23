#include <deque>
#include <set>
#include "clang/Rewrite/Frontend/ASTConsumers.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/AST/ParentMap.h"
#include "clang/Lex/Lexer.h"
#include "llvm/Support/raw_ostream.h"

using namespace std;
using namespace clang;

namespace {

class TranslatorPrinterHelper;

//-----------------------------------------------------------------------------------------------
// TranslatorVisitor
//-----------------------------------------------------------------------------------------------
class TranslatorVisitor : public RecursiveASTVisitor<TranslatorVisitor> {

  //---------------------------------------------------------------------------------------------
  public:
    TranslatorVisitor(Rewriter& R) :
      TheRewriter(R), SM(0), Policy(0), TheSystemPrinterHelper(0), BlockStmtMap(0) {}

    virtual ~TranslatorVisitor() { delete BlockStmtMap; }
  
    void Initialize();
    void Finalize();

    // Inherited from RecursiveASTVisitor
    //
    bool VisitDecl(Decl* D);
    bool VisitStmt(Stmt* S);

    bool shouldVisitImplicitCode() const {
      return false;
    }

    void RewriteForwardClassDecl(DeclGroupRef D);
    void RewriteForwardProtocolDecl(DeclGroupRef D);

  //---------------------------------------------------------------------------------------------
  protected:
  
    void RewriteImportsAndIncludes();

    void RewriteFunctionDecl(FunctionDecl* Function);

    void RewriteInterfaceDecl(ObjCInterfaceDecl* ClassDecl);
    void RewriteProtocolDecl(ObjCProtocolDecl* PDecl);
    void RewriteMethodDeclaration(ObjCMethodDecl* Method);
    void RewriteImplementationDecl(ObjCImplementationDecl* ImpDecl);

    void RewriteInstanceVariables(ObjCInterfaceDecl::ivar_iterator Begin,
                                  ObjCInterfaceDecl::ivar_iterator End,
                                  SourceLocation LBraceLoc,
                                  SourceLocation RBraceLoc);

    void RewriteProperty(ObjCPropertyDecl* P, SourceLocation& PreviousAtLoc);

    void RewriteCategoryDecl(ObjCCategoryDecl* CatDecl);
    void RewriteCategoryImplDecl(ObjCCategoryImplDecl* CatDecl);

    void RewriteTopLevelDecl(Decl* D);
  
    void RewriteStatement(Stmt* S);
    void RewriteCompoundStatement(CompoundStmt* S);
    void RewriteIfStatement(IfStmt* S);
    void RewriteForStatement(ForStmt* S);
    void RewriteForCollectionStatement(ObjCForCollectionStmt* S);
    void RewriteCXXForRangeStmtStatement(CXXForRangeStmt* S);
    void RewriteForCArrayStmtStatement(CXXForRangeStmt* S);
    void RewriteForEnumerateCounter(SourceLocation Forloc, CompoundStmt* Body, string& Str);
    void RewriteWhileStatement(WhileStmt* S);
    void RewriteDoStatement(DoStmt* S);
    void RewriteSwitchStatement(SwitchStmt* S);
    void RewriteCaseStatement(CaseStmt* S);
    void RewriteDefaultStatement(DefaultStmt* S);
    void RewriteBreakStatement(BreakStmt* S);
    void RewriteAutoreleasePoolStmt(ObjCAutoreleasePoolStmt* S);
    void RewriteSynchronizedStmt(ObjCAtSynchronizedStmt* S);
    void RewriteTryStmt(ObjCAtTryStmt* S);
    void RewriteThrowStmt(ObjCAtThrowStmt* S);
    enum StatementStringMode {
        ADD_TRAILING_SEMICOLON_IF_NOT_PRESENT,
        REMOVE_TRAILING_SEMICOLON,
        DO_NOT_MODIFY_TRAILING_SEMICOLON
    };

    string GetStatementString(Stmt* S,
                              StatementStringMode mode = ADD_TRAILING_SEMICOLON_IF_NOT_PRESENT);

    void StripTrailingSemicolon(string& str);

    int GetTokenLength(SourceLocation Loc);

    SourceRange GetRange(const SourceLocation& LocStart,
                         const SourceLocation& LocEnd);

    SourceRange GetRange(SourceRange LocRange);
    SourceRange GetRange(Stmt* S);

    void MoveLocToNextLine(SourceLocation& Loc);
    void MoveLocToEndOfLine(SourceLocation& Loc);
    void MoveLocToBeginningOfLine(SourceLocation& Loc);

    bool CheckForChar(const SourceLocation LocStart,
                      const SourceLocation LocEnd,
                      const char c);

    void AddAtToStartAndEndKeywordsIfNeeded(const SourceLocation LocStart,
                                            const SourceLocation LocEnd);

    void AddAtToAccessSpecIfNeeded(const SourceLocation& LocStart,
                                   const SourceLocation& LocEnd);

    void AddAtToRequiredOptionalIfNeeded(const SourceLocation& LocStart,
                                         const SourceLocation& LocEnd);

    void AddAtToKeywordsInReverse(const char* Keywords[],
                                  size_t NumOfKeywords,
                                  const SourceLocation& LocStart,
                                  const SourceLocation& LocEnd);

    void AddAtToSynthesizeDynamicKeywordsIfNeeded(const SourceLocation LocStart,
                                                  const SourceLocation LocEnd);

    void RemovePrefixDeclarations();

    void DeferredInsertText(SourceLocation, string);
    void DeferredInsertTextAfterToken(SourceLocation, string);
    void DeferredInsertTextAtEndOfLine(SourceLocation, string);

  //---------------------------------------------------------------------------------------------
  private:
    Rewriter& TheRewriter;
    LangOptions LangOpts;
    SourceManager* SM;
    PrintingPolicy* Policy;
    TranslatorPrinterHelper* TheSystemPrinterHelper;

    // Set of strings to insert after all other rewriting is done
    //
    struct StringInsertion {
      string Str;
      SourceLocation Loc;
      bool InsertAfterToken;
    };
    deque<StringInsertion> StringInsertions;

    // Used to handle generation of methods with optional parameters. Helps
    // avoid overwriting.
    //
    SourceLocation PreviousMethodLoc;

    // Track block bodies for special handling, since Locs are messed up.
    //
    ParentMap* BlockStmtMap;
};

//-----------------------------------------------------------------------------------------------
// RewriteEeroToObjC
//-----------------------------------------------------------------------------------------------
class RewriteEeroToObjC : public ASTConsumer {

  //---------------------------------------------------------------------------------------------
  protected:

    Rewriter Rewrite;
    DiagnosticsEngine& Diags;
    const LangOptions& LangOpts;
    SourceManager* SM;

    FileID MainFileID;
    string InFileName;
    raw_ostream* OutFile;

    bool SilenceRewriteMacroWarning;
    unsigned RewriteFailedDiag;

    TranslatorVisitor Visitor;

  //---------------------------------------------------------------------------------------------
  public:

    RewriteEeroToObjC(string inFile, raw_ostream* OS,
                      DiagnosticsEngine& D, const LangOptions& LOpts,
                      bool silenceMacroWarn);

    ~RewriteEeroToObjC() {}

    virtual void Initialize(ASTContext& context);

    virtual void HandleTranslationUnit(ASTContext& C);

    virtual bool HandleTopLevelDecl(DeclGroupRef D);
};

//------------------------------------------------------------------------------------------------
static void ProcessMacroArguments(const SourceManager& SM,
                                  string& MacroStr,
                                  const SourceLocation& Loc,
                                  SourceLocation& ExpansionLoc) {

  // Functions like SM.isMacroArgExpansion() don't always seem to work properly,
  // so simply look for parens after a macro expansion. However, we ignore opening
  // parens after a newline, which isn't perfect, but maybe good enough (for now,
  // at least).
  //
  const char* charBuf = SM.getCharacterData(ExpansionLoc);
  size_t i = MacroStr.size();

  while (charBuf[i] == ' ' || charBuf[i] == '\t') {
    ++i;
  }

  if (charBuf[i] == '(') {
    int parenCount = 0;
    do {
      if (charBuf[i] == '(') {
        ++parenCount;
      } else if (charBuf[i] == ')') {
        --parenCount;
      }
      MacroStr += charBuf[i];
      ExpansionLoc = SM.getExpansionLoc(Loc).getLocWithOffset(i);
      ++i;
    } while (parenCount > 0);
  }
}

//------------------------------------------------------------------------------------------------
// TranslatorPrinterHelper
//------------------------------------------------------------------------------------------------

class TranslatorPrinterHelper : public PrinterHelper {
  public:
    TranslatorPrinterHelper(PrintingPolicy& P, SourceManager& S)
        : Policy(P), SM(S) {}
    virtual ~TranslatorPrinterHelper() {}

  protected:
    virtual bool handledStmt(Stmt* E, raw_ostream& OS) {

      if (DeclStmt* DS = dyn_cast_or_null<DeclStmt>(E)) {
        return HandleDeclStmt(DS, OS);
      }

      if (PseudoObjectExpr* P =
              dyn_cast_or_null<PseudoObjectExpr>(E)) {
        return HandlePseudoObjectExpr(P, OS);
      }

      if (E->getLocStart().isMacroID() && E->getLocEnd().isMacroID()) {
        return HandleMacroExpr(E, OS);
      }

      if (ObjCArrayLiteral* AL = dyn_cast_or_null<ObjCArrayLiteral>(E)) {
        return HandleObjCArrayLiteral(AL, OS);
      }

      if (ObjCDictionaryLiteral* DL = dyn_cast_or_null<ObjCDictionaryLiteral>(E)) {
        return HandleObjCDictionaryLiteral(DL, OS);
      }
      
      if (BlockExpr* BE = dyn_cast_or_null<BlockExpr>(E)) {
        return HandleBlockExpr(BE, OS);
      }
      
      // There seems to be a bug in StmtPrinter.cpp -- fixing here for now
      //
      if (ObjCPropertyRefExpr* PR = dyn_cast_or_null<ObjCPropertyRefExpr>(E)) {
        return HandlePropertyRefExpr(PR, OS);
      }
      
      return false;
    }

  bool HandleDeclStmt(DeclStmt* DS, raw_ostream& OS) {
    
    DeclGroupRef::iterator I = DS->getDeclGroup().begin();
    DeclGroupRef::iterator E = DS->getDeclGroup().end();
   
    bool oneOrMoreFound = false;
   
    do {
      if (VarDecl* VD = dyn_cast_or_null<VarDecl>(*I)) {
        string TypeStr = VD->getType().getAsString(Policy);
        string NameStr = " " + VD->getName().str();

        size_t pos = TypeStr.find("^");
        if (pos != string::npos) {
          pos = TypeStr.find(")", pos);
          TypeStr.insert(pos, NameStr);
          NameStr.clear();
        }
      
        if (oneOrMoreFound) {
          OS << "; ";
        }

        OS << TypeStr;
        OS << NameStr;
        
        if (Stmt* InitStmt = VD->getInit()) {
          OS << " = ";
          if (!handledStmt(InitStmt, OS)) {
            InitStmt->printPretty(OS, this, Policy);
            OS << ";";
          }
        }
        oneOrMoreFound = true;
      }
    } while (!DS->isSingleDecl() && (++I != E));
    
    if (oneOrMoreFound) {
      return true;
    }
    return false;
  }

  bool HandlePseudoObjectExpr(PseudoObjectExpr* E, raw_ostream& OS) {

    // If this is an ObjC object subscript expression with an
    // NSRange key, print the generated message expression.
    //
    if (ObjCSubscriptRefExpr* S =
            dyn_cast_or_null<ObjCSubscriptRefExpr>(E->getSyntacticForm())) {

      QualType keyType = S->getKeyExpr()->getType().getUnqualifiedType();

      if (keyType.getCanonicalType().getAsString(Policy) == "struct _NSRange") {

        if (ObjCMessageExpr* M =
                dyn_cast_or_null<ObjCMessageExpr>(E->getResultExpr())) {

          M->printPretty(OS, 0, Policy);
          return true;
        }
      }
    }
    return false;
  }

  bool HandleMacroExpr(Stmt* E, raw_ostream& OS) {

    // If this is a single (contiguous) macro, print the macro name itself
    // instead of what it is defined as
    //
    const SourceLocation& LocStart = E->getLocStart();
    const SourceLocation& LocEnd = E->getLocEnd();

    if (Lexer::isAtStartOfMacroExpansion(LocStart, SM, Policy.LangOpts) &&
        Lexer::isAtEndOfMacroExpansion(LocEnd, SM, Policy.LangOpts)) {

      const SourceLocation& ExpansionLocStart = SM.getExpansionLoc(LocStart);
      SourceLocation ExpansionLocEnd = SM.getExpansionLoc(LocEnd);

      if (ExpansionLocEnd == ExpansionLocStart) {
        SmallVector<char, 64> buffer;
        string str = Lexer::getSpelling(ExpansionLocStart, buffer, SM, Policy.LangOpts);

        // Handle macros with arguments
        //
        ProcessMacroArguments(SM, str, LocEnd, ExpansionLocEnd);

        OS << str;
        return true;
      }
    }
    return false;
  }

  bool HandleObjCArrayLiteral(ObjCArrayLiteral* E, raw_ostream& OS) {
    if (E->getNumElements() == 0) {
      OS << "[NSMutableArray array]";
      return true;
    }
    return false;
  }

  bool HandleObjCDictionaryLiteral(ObjCDictionaryLiteral* E, raw_ostream& OS) {
    if (E->getNumElements() == 0) {
      OS << "[NSMutableDictionary dictionary]";
      return true;
    }
    return false;
  }

  bool HandleBlockExpr(BlockExpr* E, raw_ostream& OS) {
    BlockDecl *BD = E->getBlockDecl();
    OS << "^";

    const FunctionType *AFT = E->getFunctionType();

    if (isa<FunctionNoProtoType>(AFT)) {
      OS << "()";
    } else if (!BD->param_empty() || cast<FunctionProtoType>(AFT)->isVariadic()) {
      OS << '(';
      std::string ParamStr;
      for (BlockDecl::param_iterator AI = BD->param_begin(),
           E = BD->param_end(); AI != E; ++AI) {
        if (AI != BD->param_begin()) OS << ", ";
        ParamStr = (*AI)->getNameAsString();
        (*AI)->getType().getAsStringInternal(ParamStr, Policy);
        OS << ParamStr;
      }

      const FunctionProtoType *FT = cast<FunctionProtoType>(AFT);
      if (FT->isVariadic()) {
        if (!BD->param_empty()) OS << ", ";
        OS << "...";
      }
      OS << ") ";
    }
    
    // Print out the block's body as well
    //
    string stringBuf;
    llvm::raw_string_ostream stringStream(stringBuf);

    E->getBody()->printPretty(stringStream, 0, Policy);
    string str = stringStream.str();
    str.erase(str.find_last_not_of(" \t\n") + 1); // strip trailing newline
    OS << str;
    
    return true;
  }

  bool HandlePropertyRefExpr(ObjCPropertyRefExpr* E, raw_ostream& OS) {

    // There seems to be a bug in StmtPrinter.cpp -- fixing here for now

    if (E->isSuperReceiver())
      OS << "super";
    else if (E->isObjectReceiver() && E->getBase())
      E->getBase()->printPretty(OS, 0, Policy);
    else if (E->isClassReceiver())
      OS << E->getClassReceiver()->getName();

    OS << ".";

    if (E->isImplicitProperty())
      OS << E->getImplicitPropertyGetter()->getSelector().getAsString();
    else
      OS << E->getExplicitProperty()->getName();
    
    return true;
  }

  private:
    PrintingPolicy& Policy;
    SourceManager& SM;
};

}  // namespace clang


//------------------------------------------------------------------------------------------------
//
RewriteEeroToObjC::RewriteEeroToObjC(string inFile, raw_ostream* OS,
                                     DiagnosticsEngine& D, const LangOptions& LOpts,
                                     bool silenceMacroWarn)
  : Diags(D), LangOpts(LOpts), InFileName(inFile), OutFile(OS),
    SilenceRewriteMacroWarning(silenceMacroWarn), Visitor(Rewrite) {

  RewriteFailedDiag = Diags.getCustomDiagID(DiagnosticsEngine::Warning,
                      "rewriting sub-expression within a macro (may not be correct)");
}

//------------------------------------------------------------------------------------------------
//
ASTConsumer* clang::CreateEeroToObjCRewriter(const string& InFile,
    raw_ostream* OS,
    DiagnosticsEngine& Diags,
    const LangOptions& LOpts,
    bool SilenceRewriteMacroWarning) {
  return new RewriteEeroToObjC(InFile, OS, Diags, LOpts, SilenceRewriteMacroWarning);
}

//------------------------------------------------------------------------------------------------
//
void RewriteEeroToObjC::HandleTranslationUnit(ASTContext& C) {

  if (Diags.hasErrorOccurred()) {
    return;
  }

  Visitor.TraverseDecl(C.getTranslationUnitDecl());

  Visitor.Finalize();

  // Get the buffer corresponding to MainFileID.  If we haven't changed it, then
  // we are done.
  //
  const RewriteBuffer* RewriteBuf = Rewrite.getRewriteBufferFor(MainFileID);
  if (RewriteBuf) {
    *OutFile << string(RewriteBuf->begin(), RewriteBuf->end());
  } else {
    llvm::errs() << "\n*** No changes ***\n";
  }

  OutFile->flush();
}

//------------------------------------------------------------------------------------------------
//
bool RewriteEeroToObjC::HandleTopLevelDecl(DeclGroupRef D) {

  for (DeclGroupRef::iterator I = D.begin(), E = D.end(); I != E; ++I) {

    if (!SM->isWrittenInMainFile((*I)->getLocation())) {
      return true;
    }

    if (ObjCInterfaceDecl *Class = dyn_cast<ObjCInterfaceDecl>(*I)) {
      if (!Class->isThisDeclarationADefinition()) {
        Visitor.RewriteForwardClassDecl(D);
        break;
      }
    }
    if (ObjCProtocolDecl *Proto = dyn_cast<ObjCProtocolDecl>(*I)) {
      if (!Proto->isThisDeclarationADefinition()) {
        Visitor.RewriteForwardProtocolDecl(D);
        break;
      }
    }
  }

  return true;
}

//------------------------------------------------------------------------------------------------
//
void RewriteEeroToObjC::Initialize(ASTContext& context) {

  SM = &(context.getSourceManager());
  MainFileID = SM->getMainFileID();

  Rewrite.setSourceMgr(*SM, context.getLangOpts());
  
  Visitor.Initialize();
}

//------------------------------------------------------------------------------------------------
// TranslatorVisitor
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//
bool TranslatorVisitor::VisitDecl(Decl* D) {

  // Skip all headers. We only want to translate the primary source file.
  //
  if (!SM->isWrittenInMainFile(D->getLocation())) {
    return true;
  }
  
  if (ObjCInterfaceDecl* IFD = dyn_cast_or_null<ObjCInterfaceDecl>(D)) {
    if (IFD->isThisDeclarationADefinition()) {
      RewriteInterfaceDecl(IFD);
    }
  } else if (ObjCProtocolDecl* PD = dyn_cast_or_null<ObjCProtocolDecl>(D)) {
    if (PD->isThisDeclarationADefinition()) {
      RewriteProtocolDecl(PD);
    }
  } else if (ObjCImplementationDecl* IMPD = dyn_cast_or_null<ObjCImplementationDecl>(D)) {
    RewriteImplementationDecl(IMPD);

  } else if (ObjCCategoryDecl* CD = dyn_cast_or_null<ObjCCategoryDecl>(D)) {
    RewriteCategoryDecl(CD);

  } else if (ObjCCategoryImplDecl* CID = dyn_cast_or_null<ObjCCategoryImplDecl>(D)) {
    RewriteCategoryImplDecl(CID);

  } else if (FunctionDecl* FD = dyn_cast_or_null<FunctionDecl>(D)) {
    RewriteFunctionDecl(FD);

  } else if (D->getDeclContext()->isTranslationUnit()) {
    RewriteTopLevelDecl(D);
  }

  return true;
}

//------------------------------------------------------------------------------------------------
//
bool TranslatorVisitor::VisitStmt(Stmt* S) {

  // Skip all headers. We only want to translate the primary source file.
  //
  if (!SM->isWrittenInMainFile(S->getLocStart())) {
    return true;
  }

  // We're only interested in compound statements in this call. We don't want to
  // get multiple visits for a single statement (it apparently gets called for each
  // expression).
  //
  if (CompoundStmt* CS = dyn_cast_or_null<CompoundStmt>(S)) {
    RewriteCompoundStatement(CS);

  // Blocks need special handling, since the locations appear to get munged. Clang's
  // AST printer exhibits the problem as well.
  //
  } else if (BlockExpr* BE = dyn_cast_or_null<BlockExpr>(S)) {
    if (BlockStmtMap) {
      BlockStmtMap->addStmt(BE->getBody());
    } else {
      BlockStmtMap = new ParentMap(BE->getBody());
    }
  }

  return true;
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteImportsAndIncludes() {

  const FileID MainFileID = SM->getMainFileID();
  const StringRef MainBuf = SM->getBufferData(MainFileID);
  const SourceLocation& LocStart = SM->getLocForStartOfFile(MainFileID);

  vector<size_t> changelist;

  const char* macros[] = { "#import", "#include" };
  const size_t macrosCount = sizeof(macros)/sizeof(const char*);
  
  for (size_t i = 0; i < macrosCount; i++) {
    size_t pos = 0;
    while ((pos = MainBuf.find(macros[i], pos)) != StringRef::npos) {

      pos = MainBuf.find_first_of("<\"'", pos);

      if (pos != StringRef::npos && MainBuf[pos] == '\'') {
        changelist.push_back(pos);
        pos = MainBuf.find('\'', ++pos);
        changelist.push_back(pos);
      }
    }
  }

  while (!changelist.empty()) {
    TheRewriter.ReplaceText(LocStart.getLocWithOffset(changelist.back()), 1, "\"");
    changelist.pop_back();
  }  
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteFunctionDecl(FunctionDecl* Function) {

  string str = Function->getResultType().getAsString(*Policy);
  str += ' ';
  str += Function->getName();
  str += "(";

  for (FunctionDecl::param_iterator B = Function->param_begin(),
                                    E = Function->param_end(),
                                    I = B;
       I != E;
       ++I) {

    if (I != B) {
      str += ", ";
    }
    str += (*I)->getType().getAsString(*Policy);
    str += ' ';
    str += (*I)->getName();
  }

  if (Function->isVariadic()) {
    str += ", ...";
  }

  str += ')';

  const SourceLocation& LocStart = Function->getLocStart();
  SourceLocation LocEnd;

  if (Function->isThisDeclarationADefinition()) {
    LocEnd = Function->getBody()->getLocStart();
    str += " {";
    DeferredInsertTextAfterToken(Function->getLocEnd(), "\n}");
  } else {
    LocEnd = Function->getLocEnd();
    str += ";";
  }

  TheRewriter.ReplaceText(GetRange(LocStart, LocEnd), str);
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteInterfaceDecl(ObjCInterfaceDecl* ClassDecl) {

  const SourceLocation LocStart = ClassDecl->getAtStartLoc();
  const SourceLocation LocEnd = ClassDecl->getLocEnd();

  AddAtToStartAndEndKeywordsIfNeeded(LocStart, LocEnd);

  SourceLocation CurrentLoc;

  // Rewrite superclass name, since it might have an implicit prefix
  //
  ObjCInterfaceDecl* SuperClassDecl = ClassDecl->getSuperClass();
  if (SuperClassDecl) {
    SourceLocation loc = ClassDecl->getSuperClassLoc();
    string str;
    if (loc.isInvalid()) { // implicit NSObject superclass
      loc = ClassDecl->getLocation();
      str += ClassDecl->getName();
      str += " : ";
    }
    str += SuperClassDecl->getName();
    const int len = GetTokenLength(loc);
    TheRewriter.ReplaceText(loc, len, str);
    CurrentLoc = loc.getLocWithOffset(len + 1);

  } else { // no superclass -- this is a user-defined root class, so remove ":void"
    const SourceLocation& NameLoc = ClassDecl->getLocation();
    const SourceLocation& Start = Lexer::getLocForEndOfToken(NameLoc, 0, *SM, LangOpts);
    SourceLocation End;
    if (ClassDecl->getReferencedProtocols().size() > 0) {
      const StringRef bufferStr(SM->getCharacterData(NameLoc));
      const size_t endPos = bufferStr.find("<");
      if (endPos != StringRef::npos) {
        End = NameLoc.getLocWithOffset(endPos - 1);
      }
    } else {
      End = NameLoc;
      MoveLocToEndOfLine(End);
    }
    if (Start.isValid() && End.isValid()) {
      TheRewriter.RemoveText(GetRange(Start, End));
      CurrentLoc = End.getLocWithOffset(1);
    }
  }

  // Rewrite protocol names as well, since they might have implicit prefixes
  //
  const ObjCList<ObjCProtocolDecl>& Protocols = ClassDecl->getReferencedProtocols();
  ObjCInterfaceDecl::protocol_loc_iterator LI = ClassDecl->protocol_loc_begin();
  for (ObjCList<ObjCProtocolDecl>::iterator I = Protocols.begin(),
       E = Protocols.end(); I != E; ++I) {
    const SourceLocation loc = (*LI);
    const int len = GetTokenLength(loc);
    TheRewriter.ReplaceText(loc, len, (*I)->getName());
    CurrentLoc = loc.getLocWithOffset(len);
    LI++;
  }

  RewriteInstanceVariables(ClassDecl->ivar_begin(),
                           ClassDecl->ivar_end(),
                           CurrentLoc,
                           SourceLocation());

  SourceLocation AtPropertyLocation;
  for (ObjCInterfaceDecl::prop_iterator I = ClassDecl->prop_begin(),
       E = ClassDecl->prop_end(); I != E; ++I) {
    RewriteProperty(*I, AtPropertyLocation);
  }

  for (ObjCInterfaceDecl::instmeth_iterator
       I = ClassDecl->instmeth_begin(), E = ClassDecl->instmeth_end();
       I != E; ++I) {
    RewriteMethodDeclaration(*I);
  }

  for (ObjCInterfaceDecl::classmeth_iterator
       I = ClassDecl->classmeth_begin(), E = ClassDecl->classmeth_end();
       I != E; ++I) {
    RewriteMethodDeclaration(*I);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteProtocolDecl(ObjCProtocolDecl* PDecl) {

  const SourceLocation& LocStart = PDecl->getAtStartLoc();
  const SourceLocation& LocEnd = PDecl->getLocEnd();

  AddAtToStartAndEndKeywordsIfNeeded(LocStart, LocEnd);

  SourceLocation CurrentLoc = LocStart;

  // Rewrite protocol names, since they might have implicit prefixes
  //
  const ObjCList<ObjCProtocolDecl>& Protocols = PDecl->getReferencedProtocols();
  ObjCProtocolDecl::protocol_loc_iterator LI = PDecl->protocol_loc_begin();
  for (ObjCList<ObjCProtocolDecl>::iterator I = Protocols.begin(),
       E = Protocols.end(); I != E; ++I) {
    const SourceLocation loc = (*LI);
    const int len = GetTokenLength(loc);
    TheRewriter.ReplaceText(loc, len, (*I)->getName());
    CurrentLoc = loc.getLocWithOffset(len);
    LI++;
  }

  MoveLocToNextLine(CurrentLoc);
  SourceLocation AtPropertyTrackingLoc;

  for (ObjCProtocolDecl::prop_iterator I = PDecl->prop_begin(),
       E = PDecl->prop_end(); I != E; ++I) {
    RewriteProperty(*I, AtPropertyTrackingLoc);
    if (CurrentLoc < AtPropertyTrackingLoc) {
      AddAtToRequiredOptionalIfNeeded(CurrentLoc, AtPropertyTrackingLoc);
    }
    CurrentLoc = Lexer::getLocForEndOfToken(I->getLocEnd(), 0, *SM, LangOpts);
  }

  for (ObjCProtocolDecl::instmeth_iterator I = PDecl->instmeth_begin(),
                                           E = PDecl->instmeth_end();
       I != E;
       ++I) {

    if (!I->isImplicit()) {
      AddAtToRequiredOptionalIfNeeded(CurrentLoc, I->getLocStart());
      RewriteMethodDeclaration(*I);
      CurrentLoc = Lexer::getLocForEndOfToken(I->getLocEnd(), 0, *SM, LangOpts);
    }
  }

  for (ObjCProtocolDecl::classmeth_iterator
       I = PDecl->classmeth_begin(), E = PDecl->classmeth_end();
       I != E; ++I) {

    if (!I->isImplicit()) {
      AddAtToRequiredOptionalIfNeeded(CurrentLoc, I->getLocStart());
      RewriteMethodDeclaration(*I);
      CurrentLoc = Lexer::getLocForEndOfToken(I->getLocEnd(), 0, *SM, LangOpts);
    }
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteMethodDeclaration(ObjCMethodDecl* Method) {

  // When method is a synthesized one, such as a getter/setter there is
  // nothing to rewrite.
  //
  if (Method->isImplicit()) {
    return;
  }

  SourceLocation LocStart = Method->getLocStart();
  SourceLocation LocEnd = Method->getDeclaratorEndLoc();

  // Determine whether it's not a compiler-generated method (resulting from
  // optional parameters).
  //
  const bool isNotCompilerGenerated =
      (PreviousMethodLoc.isInvalid() || LocStart != PreviousMethodLoc);
  PreviousMethodLoc = LocStart;

  string resultStr;

  if (Method->isInstanceMethod()) {
    resultStr += "-";
  } else {
    resultStr += "+";
  }

  if (!Method->getResultType().isNull()) {
    resultStr += "(";
    resultStr += Method->getResultType().getAsString(*Policy);
    resultStr += ")";
  }

  string name = Method->getSelector().getAsString();
  string::size_type pos, lastPos = 0;
  for (ObjCMethodDecl::param_iterator PI = Method->param_begin(),
       E = Method->param_end(); PI != E; ++PI) {

    pos = name.find_first_of(':', lastPos);

    resultStr += " ";
    resultStr += name.substr(lastPos, pos - lastPos);
    resultStr += ":(";

    string typeName = (*PI)->getType().getAsString(*Policy);
    if (typeName == "instancetype") {
      resultStr += "id";
    } else {
      resultStr += typeName;
    }

    resultStr += ")";
    resultStr += (*PI)->getNameAsString();

    lastPos = pos + 1;
  }

  if (Method->param_begin() == Method->param_end()) {
    resultStr += name;
  }

  if (Method->isVariadic()) {
    resultStr += ", ...";
  }

  if (!Method->isThisDeclarationADefinition()) {
    resultStr += ";";
  } else {
    resultStr += " {";
    if (isNotCompilerGenerated) {
      DeferredInsertTextAtEndOfLine(Method->getLocEnd(), "\n}");
    }
    // Handle default return values
    //
    CompoundStmt* compoundBody = Method->getCompoundBody();
    if (compoundBody && (compoundBody->size() > 1)) {
      if (ReturnStmt* lastReturnStmt =
              dyn_cast_or_null<ReturnStmt>(compoundBody->body_back())) {
        Stmt* firstStmt = *(compoundBody->body_begin());
        if (firstStmt &&
            (lastReturnStmt->getLocStart() < firstStmt->getLocStart())) {
          resultStr += " //";
          string returnStr = "\n";
          returnStr += GetStatementString(lastReturnStmt);
          DeferredInsertTextAtEndOfLine(compoundBody->getRBracLoc(), returnStr);
        }
      }
    }
  }

  // Handle generated methods with optional params -- don't overwrite
  //
  if (isNotCompilerGenerated) {
    TheRewriter.ReplaceText(GetRange(LocStart, LocEnd), resultStr);
  } else {
    if (Method->isThisDeclarationADefinition()) {
      resultStr += "\n  ";
      resultStr += GetStatementString(Method->getBody());
      resultStr += "\n  }\n";
    }
    resultStr += "\n";
    DeferredInsertText(LocStart, resultStr);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteImplementationDecl(ObjCImplementationDecl* ImpDecl) {

  const SourceLocation LocStart = ImpDecl->getAtStartLoc();
  const SourceLocation LocEnd = ImpDecl->getLocEnd();

  AddAtToStartAndEndKeywordsIfNeeded(LocStart, LocEnd);

  AddAtToSynthesizeDynamicKeywordsIfNeeded(LocStart, LocEnd);

  RewriteInstanceVariables(ImpDecl->ivar_begin(),
                           ImpDecl->ivar_end(),
                           ImpDecl->getIvarLBraceLoc(),
                           ImpDecl->getIvarRBraceLoc());

  for (ObjCImplementationDecl::instmeth_iterator I = ImpDecl->instmeth_begin(),
       E = ImpDecl->instmeth_end();
       I != E; ++I) {
    ObjCMethodDecl* OMD = *I;
    RewriteMethodDeclaration(OMD);
  }

  for (ObjCImplementationDecl::classmeth_iterator I = ImpDecl->classmeth_begin(),
       E = ImpDecl->classmeth_end();
       I != E; ++I) {
    ObjCMethodDecl* OMD = *I;
    RewriteMethodDeclaration(OMD);
  }
}


//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteInstanceVariables(ObjCInterfaceDecl::ivar_iterator I,
                                                 ObjCInterfaceDecl::ivar_iterator E,
                                                 SourceLocation LBraceLoc,
                                                 SourceLocation RBraceLoc) {
  bool addCurlyBraces = false;
  SourceLocation SearchLoc = LBraceLoc;

  for (bool first = true; I != E; I++) {

    if (!I->getSynthesize()) { // ignore synthesized ivars

      if (first) { // look for relevant curly braces
        if (RBraceLoc.isInvalid()) {
          addCurlyBraces = !CheckForChar(LBraceLoc, I->getLocStart(), '{');
        } else {
          const char* buf = SM->getCharacterData(LBraceLoc);
          addCurlyBraces = (buf[0] != '{');
        }
        first = false;
      }

      // Look for access control specifier preceding this var
      //
      AddAtToAccessSpecIfNeeded(SearchLoc, I->getLocStart());

      string ivarStr = I->getType().getAsString();
      ivarStr += " ";
      ivarStr += I->getNameAsString();
      ivarStr += ";";
      TheRewriter.ReplaceText(GetRange(I->getSourceRange()), ivarStr);

      const SourceLocation loc = I->getLocEnd();
      const int len = GetTokenLength(loc);
      SearchLoc = loc.getLocWithOffset(len);
    }
  }

  if (addCurlyBraces) {
    DeferredInsertTextAfterToken(LBraceLoc, " {");

    // Instead of using the RBraceLoc, just skip to the next line after the
    // last ivar declaration.
    //
    MoveLocToNextLine(SearchLoc);
    DeferredInsertText(SearchLoc, "}");
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteProperty(ObjCPropertyDecl* PDecl,
                                        SourceLocation& PreviousAtLoc) {

  struct Attribute {
    ObjCPropertyDecl::PropertyAttributeKind kind;
    const char* keyword;
  };

  static const Attribute Attributes[] = {
    {ObjCPropertyDecl::OBJC_PR_readonly,          "readonly"},
    {ObjCPropertyDecl::OBJC_PR_assign,            "assign"},
    {ObjCPropertyDecl::OBJC_PR_readwrite,         "readwrite"},
    {ObjCPropertyDecl::OBJC_PR_retain,            "retain"},
    {ObjCPropertyDecl::OBJC_PR_copy,              "copy"},
    {ObjCPropertyDecl::OBJC_PR_nonatomic,         "nonatomic"},
    {ObjCPropertyDecl::OBJC_PR_atomic,            "atomic"},
    {ObjCPropertyDecl::OBJC_PR_weak,              "weak"},
    {ObjCPropertyDecl::OBJC_PR_strong,            "strong"},
    {ObjCPropertyDecl::OBJC_PR_unsafe_unretained, "unsafe_unretained"},
  };

  string str = "  @property ";

  if (PDecl->getPropertyAttributes() != ObjCPropertyDecl::OBJC_PR_noattr) {

    str += '(';

    bool first = true;

    for (size_t i = 0; i < sizeof(Attributes) / sizeof(Attribute); i++) {
      if (PDecl->getPropertyAttributes() & Attributes[i].kind) {
        if (first) {
          first = false;
        } else {
          str += ", ";
        }
        str += Attributes[i].keyword;
      }
    }

    if (PDecl->getPropertyAttributes() & ObjCPropertyDecl::OBJC_PR_getter) {
      if (first) {
        first = false;
      } else {
        str += ", ";
      }
      str += "getter = ";
      str += PDecl->getGetterName().getAsString();
    }

    if (PDecl->getPropertyAttributes() & ObjCPropertyDecl::OBJC_PR_setter) {
      if (first) {
        first = false;
      } else {
        str += ", ";
      }
      str += "setter = ";
      str += PDecl->getSetterName().getAsString();
    }

    str += ") ";
  }

  str += PDecl->getType().getAsString(*Policy);
  str += ' ';
  str += PDecl->getNameAsString();
  str += ';';

  const SourceLocation& AtLoc = PDecl->getAtLoc();
  const SourceLocation& LocEnd = PDecl->getLocEnd();
  SourceLocation Loc;

  if (PreviousAtLoc.isInvalid() || AtLoc != PreviousAtLoc) {
    Loc = AtLoc;
    PreviousAtLoc = AtLoc;
    str = '\n' + str;
  } else {
    Loc = PDecl->getLocation();
    MoveLocToBeginningOfLine(Loc); // we don't have the type loc
  }

  TheRewriter.ReplaceText(GetRange(Loc, LocEnd), str);

  const SourceLocation& LBraceLoc = PDecl->getLParenLoc();
  if (LBraceLoc.isValid()) {
    SourceLocation RBracLoc = LBraceLoc;
    MoveLocToEndOfLine(RBracLoc);
    TheRewriter.ReplaceText(GetRange(LBraceLoc, RBracLoc), "  ");    
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteCategoryDecl(ObjCCategoryDecl* CatDecl) {

  const SourceLocation LocStart = CatDecl->getAtStartLoc();
  const SourceLocation LocEnd = CatDecl->getLocEnd();

  AddAtToStartAndEndKeywordsIfNeeded(LocStart, LocEnd);

  // Rewrite class name, since it might have an implicit prefix
  //
  const SourceLocation& ClassNameLocStart =
      Lexer::getLocForEndOfToken(CatDecl->getLocStart(), 0, *SM, LangOpts);

  const SourceLocation& ClassNameLocEnd =
      CatDecl->getCategoryNameLoc().getLocWithOffset(-1);

  const SourceRange& ClassNameRange =
      GetRange(ClassNameLocStart, ClassNameLocEnd);

  string str = " ";
  str += CatDecl->getClassInterface()->getName();
  str += " (";
  
  TheRewriter.ReplaceText(ClassNameRange, str);

  SourceLocation AtPropertyLocation;
  for (ObjCInterfaceDecl::prop_iterator I = CatDecl->prop_begin(),
       E = CatDecl->prop_end(); I != E; ++I) {
    RewriteProperty(*I, AtPropertyLocation);
  }

  for (ObjCInterfaceDecl::instmeth_iterator
       I = CatDecl->instmeth_begin(), E = CatDecl->instmeth_end();
       I != E; ++I) {
    RewriteMethodDeclaration(*I);
  }

  for (ObjCInterfaceDecl::classmeth_iterator
       I = CatDecl->classmeth_begin(), E = CatDecl->classmeth_end();
       I != E; ++I) {
    RewriteMethodDeclaration(*I);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteCategoryImplDecl(ObjCCategoryImplDecl* CatDecl) {

  const SourceLocation LocStart = CatDecl->getAtStartLoc();
  const SourceLocation LocEnd = CatDecl->getLocEnd();

  AddAtToStartAndEndKeywordsIfNeeded(LocStart, LocEnd);

  // Rewrite class name, since it might have an implicit prefix
  //
  const SourceLocation& ClassNameLocStart =
      Lexer::getLocForEndOfToken(CatDecl->getLocStart(), 0, *SM, LangOpts);

  const SourceLocation& ClassNameLocEnd =
      CatDecl->getCategoryNameLoc().getLocWithOffset(-1);

  const SourceRange& ClassNameRange =
      GetRange(ClassNameLocStart, ClassNameLocEnd);

  string str = " ";
  str += CatDecl->getClassInterface()->getName();
  str += " (";
  
  TheRewriter.ReplaceText(ClassNameRange, str);

  SourceLocation AtPropertyLocation;
  for (ObjCCategoryImplDecl::prop_iterator I = CatDecl->prop_begin(),
       E = CatDecl->prop_end(); I != E; ++I) {
    RewriteProperty(*I, AtPropertyLocation);
  }

  for (ObjCCategoryImplDecl::instmeth_iterator
       I = CatDecl->instmeth_begin(), E = CatDecl->instmeth_end();
       I != E; ++I) {
    RewriteMethodDeclaration(*I);
  }

  for (ObjCCategoryImplDecl::classmeth_iterator
       I = CatDecl->classmeth_begin(), E = CatDecl->classmeth_end();
       I != E; ++I) {
    RewriteMethodDeclaration(*I);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteForwardClassDecl(DeclGroupRef D) {

  string str = "@class ";
  SourceRange Range;

  for (DeclGroupRef::iterator I = D.begin(), E = D.end(); I != E; ++I) {
    ObjCInterfaceDecl *ForwardDecl = cast<ObjCInterfaceDecl>(*I);
    Range = ForwardDecl->getSourceRange();
    if (I != D.begin()) {
      str += ", ";
    }
    str += ForwardDecl->getNameAsString();
  }

  if (Range.isValid()) {
    str += ";";
    TheRewriter.ReplaceText(Range, str);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteForwardProtocolDecl(DeclGroupRef D) {

  string str = "@protocol ";
  SourceRange Range;

  for (DeclGroupRef::iterator I = D.begin(), E = D.end(); I != E; ++I) {
    ObjCProtocolDecl *ForwardDecl = cast<ObjCProtocolDecl>(*I);
    Range = ForwardDecl->getSourceRange();
    if (I != D.begin()) {
      str += ", ";
    }
    str += ForwardDecl->getNameAsString();
  }

  if (Range.isValid()) {
    str += ";";
    TheRewriter.ReplaceText(Range, str);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteTopLevelDecl(Decl* D) {

  string stringBuf;
  llvm::raw_string_ostream stringStream(stringBuf);
  D->print(stringStream, *Policy);

  string str;

  if (isa<VarDecl>(D) && !isa<ParmVarDecl>(D)) {
    VarDecl* VD = dyn_cast_or_null<VarDecl>(D);
    str += VD->getType().getAsString(*Policy);
    str += " ";
    str += VD->getName();
    
    if (Stmt* InitStmt = VD->getInit()) {
      str += " = ";
      str += GetStatementString(InitStmt, REMOVE_TRAILING_SEMICOLON);
    }
  } else {
    str = stringStream.str();
  }
  // Make sure we don't lose embedded tag definitions when rewriting
  //
  TagDecl* tag = dyn_cast_or_null<TagDecl>(D);

  if (tag && !tag->isFreeStanding()) {
    string tagStr = stringStream.str();
    tagStr = " " + tagStr.substr(tagStr.find('{'));
    DeferredInsertText(tag->getLocEnd(), tagStr);

  } else if (!isa<ParmVarDecl>(D)) { // ignore params that show up here
    str += ';';

    // For handling multiple declarators as well as possible
    //
    static SourceLocation PrevLocStart;
    static SourceLocation PrevLocEnd;

    SourceRange DeclRange;
    if (D->getLocStart() != PrevLocStart) { // single declarator
      DeclRange = GetRange(D->getLocStart(), D->getLocEnd());
    } else {
      SourceLocation End = Lexer::getLocForEndOfToken(PrevLocEnd, 0, *SM, LangOpts);
      DeclRange = GetRange(End, D->getLocEnd());
      str.insert(0, " ");
    }

    PrevLocStart = D->getLocStart();
    PrevLocEnd = D->getLocEnd();

    TheRewriter.ReplaceText(DeclRange, str);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteStatement(Stmt* S) {

  if (CompoundStmt* CS = dyn_cast_or_null<CompoundStmt>(S)) {
    RewriteCompoundStatement(CS);

  } else if (IfStmt* IS = dyn_cast_or_null<IfStmt>(S)) {
    RewriteIfStatement(IS);

  } else if (ForStmt* FS = dyn_cast_or_null<ForStmt>(S)) {
    RewriteForStatement(FS);

  } else if (ObjCForCollectionStmt* FCS = dyn_cast_or_null<ObjCForCollectionStmt>(S)) {
    RewriteForCollectionStatement(FCS);

  } else if (CXXForRangeStmt* FRS = dyn_cast_or_null<CXXForRangeStmt>(S)) {
    RewriteCXXForRangeStmtStatement(FRS);

  } else if (WhileStmt* WS = dyn_cast_or_null<WhileStmt>(S)) {
    RewriteWhileStatement(WS);

  } else if (DoStmt* DWS = dyn_cast_or_null<DoStmt>(S)) {
    RewriteDoStatement(DWS);

  } else if (SwitchStmt* SS = dyn_cast_or_null<SwitchStmt>(S)) {
    RewriteSwitchStatement(SS);

  } else if (CaseStmt* SCS = dyn_cast_or_null<CaseStmt>(S)) {
    RewriteCaseStatement(SCS);

  } else if (DefaultStmt* SDS = dyn_cast_or_null<DefaultStmt>(S)) {
    RewriteDefaultStatement(SDS);

  } else if (BreakStmt* SBS = dyn_cast_or_null<BreakStmt>(S)) {
    RewriteBreakStatement(SBS);

  } else if (ObjCAutoreleasePoolStmt* APS =
        dyn_cast_or_null<ObjCAutoreleasePoolStmt>(S)) {
    RewriteAutoreleasePoolStmt(APS);

  } else if (ObjCAtSynchronizedStmt* SNS =
        dyn_cast_or_null<ObjCAtSynchronizedStmt>(S)) {
    RewriteSynchronizedStmt(SNS);

  } else if (ObjCAtTryStmt* TS = dyn_cast_or_null<ObjCAtTryStmt>(S)) {
    RewriteTryStmt(TS);

  } else if (ObjCAtThrowStmt* THS = dyn_cast_or_null<ObjCAtThrowStmt>(S)) {
    RewriteThrowStmt(THS);

  } else {
    string Str = GetStatementString(S);
    if (!Str.empty() && S->getLocStart().isValid() && S->getLocEnd().isValid()) {
      TheRewriter.ReplaceText(GetRange(S), GetStatementString(S));
    }
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteCompoundStatement(CompoundStmt* S) {

  for (CompoundStmt::const_body_iterator BI = S->body_begin(),
       E = S->body_end(); BI != E; ++BI) {
    if (!BlockStmtMap || !BlockStmtMap->hasParent(*BI)) { // don't rewrite block statements here
      // Also skip any non-compound statements with invalid locations
      if (isa<CompoundStmt>(*BI) || (*BI)->getLocStart().isValid()) {
        RewriteStatement(*BI);
      }
    }
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteIfStatement(IfStmt* S) {

  string condStr = "(";
  condStr += GetStatementString(S->getCond(), REMOVE_TRAILING_SEMICOLON);
  condStr += ") {";

  TheRewriter.ReplaceText(GetRange(S->getCond()), condStr);

  if (Stmt* ElseStmt = S->getElse()) {
    DeferredInsertText(S->getElseLoc(), "} ");

    if (IfStmt* ElseIfStmt = dyn_cast_or_null<IfStmt>(ElseStmt)) {
      RewriteIfStatement(ElseIfStmt);
    } else {
      DeferredInsertText(S->getElseLoc().getLocWithOffset(sizeof("else") - 1), " {");
      DeferredInsertTextAtEndOfLine(ElseStmt->getLocEnd(), "\n}");
    }
  } else {
    DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteForStatement(ForStmt* S) {

  string initStr = "for (";

  Stmt* initStmt = S->getInit();
  SourceLocation InitEnd = Lexer::getLocForEndOfToken(initStmt->getLocEnd(), 0, *SM, LangOpts);
  StatementStringMode SemiMode =
      (*SM->getCharacterData(InitEnd) == ';') ?
          REMOVE_TRAILING_SEMICOLON : ADD_TRAILING_SEMICOLON_IF_NOT_PRESENT;

  initStr += GetStatementString(initStmt, SemiMode);

  Stmt* condStmt = S->getCond();
  SourceLocation CondEnd = Lexer::getLocForEndOfToken(condStmt->getLocEnd(), 0, *SM, LangOpts);
  SemiMode = (*SM->getCharacterData(CondEnd) == ';') ?
                 REMOVE_TRAILING_SEMICOLON : ADD_TRAILING_SEMICOLON_IF_NOT_PRESENT;

  string condStr = GetStatementString(condStmt, SemiMode);

  Stmt* incStmt = S->getInc();
  string incStr = GetStatementString(incStmt, REMOVE_TRAILING_SEMICOLON);
  incStr += ')';

  incStr += " {";

  // If applicable, handle generated index counter, which is similar to Python's "for-in-enumerate"
  RewriteForEnumerateCounter(S->getForLoc(), cast<CompoundStmt>(S->getBody()), incStr);

  TheRewriter.ReplaceText(GetRange(S->getForLoc(), initStmt->getLocEnd()), initStr);
  TheRewriter.ReplaceText(GetRange(condStmt), condStr);
  TheRewriter.ReplaceText(GetRange(incStmt->getLocStart(), S->getRParenLoc()), incStr);

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteForCollectionStatement(ObjCForCollectionStmt* S) {

  string str = "for (";

  if (DeclStmt *DS = dyn_cast<DeclStmt>(S->getElement())) {
    str += GetStatementString(DS, REMOVE_TRAILING_SEMICOLON);
  } else {
    str += GetStatementString(S->getElement(), REMOVE_TRAILING_SEMICOLON);
  }
  str += " in ";
  str += GetStatementString(S->getCollection(), REMOVE_TRAILING_SEMICOLON);
  str += ") {";

  // If applicable, handle generated index counter, which is similar to Python's "for-in-enumerate"
  RewriteForEnumerateCounter(S->getForLoc(), cast<CompoundStmt>(S->getBody()), str);

  TheRewriter.ReplaceText(GetRange(S->getForLoc(), S->getRParenLoc()), str);

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteCXXForRangeStmtStatement(CXXForRangeStmt* S) {

  // Handle C array for-in loops when not translating to Objective-C++
  //
  if (!LangOpts.CPlusPlus && S->getRangeInit()->getType()->isArrayType()) {
    RewriteForCArrayStmtStatement(S);

  } else { // Objective-C++
    string str = "for (";

    PrintingPolicy SubPolicy(*Policy);
    SubPolicy.SuppressInitializers = true;
    string stringBuf;
    llvm::raw_string_ostream stringStream(stringBuf);
    S->getLoopVariable()->print(stringStream, SubPolicy);
    str += stringStream.str();

    str += " : ";
    str += GetStatementString(S->getRangeInit(), REMOVE_TRAILING_SEMICOLON);
    str += ") {";

    // If applicable, handle generated index counter, which is similar to Python's "for-in-enumerate"
    RewriteForEnumerateCounter(S->getForLoc(), cast<CompoundStmt>(S->getBody()), str);

    TheRewriter.ReplaceText(GetRange(S->getForLoc(), S->getRParenLoc()), str);

    DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteForCArrayStmtStatement(CXXForRangeStmt* S) {
  
  string initStr = "for (";

  Stmt* initStmt = S->getBeginEndStmt();
  initStr += GetStatementString(initStmt, REMOVE_TRAILING_SEMICOLON);

  // Replace the generated var name (which relies on a C++ reference) with the original
  // range variable name
  //
  string unusedRangeVar = cast<NamedDecl>(S->getRangeStmt()->getSingleDecl())->getName();
  string rangeVar = GetStatementString(S->getRangeInit(), REMOVE_TRAILING_SEMICOLON);
  size_t pos = 0;
  while((pos = initStr.find(unusedRangeVar, pos)) != string::npos) {
   initStr.replace(pos, unusedRangeVar.length(), rangeVar);
   pos += rangeVar.length();
  }

  // Replace hard-coded length with something better
  //
  pos = initStr.find('+');
  if (pos != string::npos) {
    initStr.erase(pos + 1);
    initStr += " sizeof(";
    initStr += rangeVar;
    initStr += ")/sizeof(";
    initStr += S->getLoopVariable()->getType().getAsString();
    initStr += ");";
  }

  Stmt* condStmt = S->getCond();
  string condStr = GetStatementString(condStmt, ADD_TRAILING_SEMICOLON_IF_NOT_PRESENT);

  Stmt* incStmt = S->getInc();
  string incStr = GetStatementString(incStmt, REMOVE_TRAILING_SEMICOLON);
  incStr += ") {";

  // Add the loop var statment to the beginning of the loop body
  //
  CompoundStmt* body = cast<CompoundStmt>(S->getBody());
  Stmt* firstStmt = *(body->body_begin());
  DeferredInsertText(firstStmt->getLocStart(), GetStatementString(S->getLoopVarStmt()) + '\n');

  TheRewriter.ReplaceText(GetRange(S->getForLoc(), S->getColonLoc().getLocWithOffset(-1)), initStr);
  TheRewriter.ReplaceText(GetRange(S->getColonLoc(), S->getColonLoc()), condStr);
  TheRewriter.ReplaceText(GetRange(S->getRangeInit()->getLocStart(), S->getRParenLoc()), incStr);

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteForEnumerateCounter(SourceLocation ForLoc,
                                                   CompoundStmt* Body,
                                                   string& Str) {
  if (Body) {
    Stmt* firstStmt = *(Body->body_begin());
    if (firstStmt->getLocStart().isInvalid()) {
      // Add counter statement to for string
      Str += " ";
      Str += GetStatementString(firstStmt);
      // Replace original "for" with "{"
      const FileID MainFileID = SM->getMainFileID();
      const SourceLocation& LocStart = SM->getLocForStartOfFile(MainFileID);
      const char* startBuf = SM->getCharacterData(LocStart);
      const char* endBuf = SM->getCharacterData(ForLoc);
      std::string bufferStr(startBuf, endBuf - startBuf);
      const size_t pos = bufferStr.rfind("for");
      TheRewriter.ReplaceText(GetRange(LocStart.getLocWithOffset(pos),
                                       LocStart.getLocWithOffset(pos+strlen("for"))),
                                       "{");
    }
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteWhileStatement(WhileStmt* S) {

  string condStr = "(";
  condStr += GetStatementString(S->getCond(), REMOVE_TRAILING_SEMICOLON);
  condStr += ") {";

  TheRewriter.ReplaceText(GetRange(S->getCond()), condStr);

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteDoStatement(DoStmt* S) {

  const SourceLocation& DoLoc = S->getDoLoc();
  const SourceLocation& DoLocEnd = Lexer::getLocForEndOfToken(DoLoc, 0, *SM, LangOpts);
  TheRewriter.ReplaceText(GetRange(DoLoc, DoLocEnd), "do {");

  DeferredInsertText(S->getWhileLoc(), "} ");

  string condStr = " (";
  condStr += GetStatementString(S->getCond(), REMOVE_TRAILING_SEMICOLON);
  condStr += ");";

  const SourceLocation& LParenLoc =
      Lexer::getLocForEndOfToken(S->getWhileLoc(), 0, *SM, LangOpts);

  TheRewriter.ReplaceText(GetRange(LParenLoc, S->getRParenLoc()), condStr);
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteSwitchStatement(SwitchStmt* S) {

  string condStr = GetStatementString(S->getCond(), REMOVE_TRAILING_SEMICOLON);

  if (condStr[0] != '(') {
    condStr = '(' + condStr;
    condStr += ')';
  }

  condStr += " {";
  TheRewriter.ReplaceText(GetRange(S->getCond()), condStr);

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteCaseStatement(CaseStmt* S) {

  const SourceRange range = GetRange(S->getCaseLoc(), S->getColonLoc());
  string CaseStr = "case ";
  CaseStr += GetStatementString(S->getLHS(), DO_NOT_MODIFY_TRAILING_SEMICOLON);

  if (Expr* RHS = S->getRHS()) {
    CaseStr += " ... ";
    CaseStr += GetStatementString(RHS, DO_NOT_MODIFY_TRAILING_SEMICOLON);
  }

  CaseStr += ": ";

  if (Stmt* SubStmt = S->getSubStmt()) {
    if (CaseStmt* SCS = dyn_cast_or_null<CaseStmt>(SubStmt)) {
      TheRewriter.ReplaceText(range, CaseStr);
      RewriteCaseStatement(SCS);
      DeferredInsertText(SCS->getLocStart(), "\n");
    } else {
      CaseStr += '{';
      TheRewriter.ReplaceText(range, CaseStr);
      DeferredInsertTextAtEndOfLine(SubStmt->getLocEnd(), "\n}");
    }
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteDefaultStatement(DefaultStmt* S) {

  const SourceRange range = GetRange(S->getDefaultLoc(), S->getColonLoc());
  string DefaultStr = "default: {";

  TheRewriter.ReplaceText(range, DefaultStr);

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteBreakStatement(BreakStmt* S) {

  const char* startBuf = SM->getCharacterData(S->getBreakLoc());
  std::string bufferStr(startBuf, sizeof("break") - 1);

  if (bufferStr == "break") { // regular break statement
    TheRewriter.ReplaceText(GetRange(S), GetStatementString(S));

  } else { // inserted break
    DeferredInsertTextAtEndOfLine(S->getBreakLoc(), "\nbreak;");
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteAutoreleasePoolStmt(ObjCAutoreleasePoolStmt* S) {
 
  const SourceRange& range =
      GetRange(S->getAtLoc(), S->getSubStmt()->getLocStart());
  
  TheRewriter.ReplaceText(range, "@autoreleasepool {");

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteSynchronizedStmt(ObjCAtSynchronizedStmt* S) {

  const SourceRange& LeftRange =
      GetRange(S->getAtSynchronizedLoc(),
               S->getSynchExpr()->getLocStart().getLocWithOffset(-1));
  
  TheRewriter.ReplaceText(LeftRange, "@synchronized");
  
  string exprStr = "(";
  exprStr += GetStatementString(S->getSynchExpr(), REMOVE_TRAILING_SEMICOLON);
  exprStr += ") {";

  TheRewriter.ReplaceText(S->getSynchExpr()->getSourceRange(), exprStr);

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteTryStmt(ObjCAtTryStmt* S) {
 
  const SourceRange& TryRange =
      GetRange(S->getAtTryLoc(), S->getTryBody()->getLocStart());
  
  TheRewriter.ReplaceText(TryRange, "@try {");

  for (unsigned i = 0; i < S->getNumCatchStmts(); i++) {
    if (ObjCAtCatchStmt* CatchStmt = S->getCatchStmt(i)) {
      VarDecl* ParamDecl = CatchStmt->getCatchParamDecl();
      if (ParamDecl) {
        const SourceRange& CatchRange = GetRange(CatchStmt->getAtCatchLoc(),
                                        ParamDecl->getLocStart().getLocWithOffset(-1));

        TheRewriter.ReplaceText(CatchRange, "} @catch");
        
        string declStr = "(";
        declStr += ParamDecl->getType().getAsString(*Policy);
        declStr += ' ';
        declStr += ParamDecl->getName();
        declStr += ") {";
        
        TheRewriter.ReplaceText(ParamDecl->getSourceRange(), declStr);
      } else {
        const SourceRange& CatchRange = GetRange(CatchStmt->getAtCatchLoc(),
                                                 CatchStmt->getRParenLoc());
        TheRewriter.ReplaceText(CatchRange, "} @catch (...) {");
      }
    }
  }
  
  if (ObjCAtFinallyStmt* FinallyStmt = S->getFinallyStmt()) {
    const SourceRange& FinallyRange = GetRange(FinallyStmt->getAtFinallyLoc(),
                                               FinallyStmt->getFinallyBody()->getLocStart());
    TheRewriter.ReplaceText(FinallyRange, "} @finally {");
  }

  DeferredInsertTextAtEndOfLine(S->getLocEnd(), "\n}");
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RewriteThrowStmt(ObjCAtThrowStmt* S) {

  const SourceRange& LeftRange =
      GetRange(S->getThrowLoc(),
               S->getThrowExpr()->getLocStart().getLocWithOffset(-1));
  
  TheRewriter.ReplaceText(LeftRange, "@throw");
  
  RewriteStatement(S->getThrowExpr());
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::Initialize() {

  SM = &(TheRewriter.getSourceMgr());
  LangOpts = TheRewriter.getLangOpts();
  Policy = new PrintingPolicy(LangOpts);

  TheSystemPrinterHelper = new TranslatorPrinterHelper(*Policy, *SM);
  SystemPrinterHelper::set(TheSystemPrinterHelper);
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::Finalize() {

  RewriteImportsAndIncludes();

  RemovePrefixDeclarations();

  while (!StringInsertions.empty()) {
    StringInsertion insertion = StringInsertions.front();

    if (insertion.InsertAfterToken) {
      TheRewriter.InsertTextAfterToken(insertion.Loc, insertion.Str);
    } else {
      TheRewriter.InsertText(insertion.Loc, insertion.Str, false, true);
    }

    StringInsertions.pop_front();
  }
  
  SystemPrinterHelper::set(0);
  delete TheSystemPrinterHelper;
}

//------------------------------------------------------------------------------------------------
//
string TranslatorVisitor::GetStatementString(Stmt* S, StatementStringMode mode) {

  string stringBuf;
  llvm::raw_string_ostream stringStream(stringBuf);

  S->printPretty(stringStream, 0, *Policy);
  string str = stringStream.str();

  // Get rid of the extra newline the pretty printer adds
  //
  str.erase(str.find_last_not_of(" \t\n") + 1);

  // Handle trailing ';'s

  if (mode != DO_NOT_MODIFY_TRAILING_SEMICOLON) {
    StripTrailingSemicolon(str);
  }

  if (mode == ADD_TRAILING_SEMICOLON_IF_NOT_PRESENT) {
    str += ';';
  }

  return str;
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::StripTrailingSemicolon(string& str) {
  str.erase(str.find_last_not_of(" \t;") + 1);
}

//------------------------------------------------------------------------------------------------
//
int TranslatorVisitor::GetTokenLength(SourceLocation Loc) {
  return Lexer::MeasureTokenLength(Loc, *SM, LangOpts);
}


//------------------------------------------------------------------------------------------------
//
SourceRange TranslatorVisitor::GetRange(const SourceLocation& LocStart,
                                        const SourceLocation& LocEnd) {
  SourceLocation AdjustedLocStart;

  if (LocStart.isMacroID()) {
    AdjustedLocStart = SM->getExpansionLoc(LocStart);
  } else {
    AdjustedLocStart = LocStart;
  }

  SourceLocation AdjustedLocEnd;

  if (LocEnd.isMacroID()) {
    AdjustedLocEnd = SM->getExpansionLoc(LocEnd);

    // Skip past any function-like macro arguments
    //
    SmallVector<char, 64> buffer;
    string str = Lexer::getSpelling(AdjustedLocEnd, buffer, *SM, LangOpts);
    ProcessMacroArguments(*SM, str, LocEnd, AdjustedLocEnd);

  } else {
    AdjustedLocEnd = LocEnd;
  }

  return SourceRange(AdjustedLocStart, AdjustedLocEnd);
}

//------------------------------------------------------------------------------------------------
//
SourceRange TranslatorVisitor::GetRange(SourceRange LocRange) {
  return GetRange(LocRange.getBegin(), LocRange.getEnd());
}

//------------------------------------------------------------------------------------------------
//
SourceRange TranslatorVisitor::GetRange(Stmt* S) {
  return GetRange(S->getSourceRange());
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::MoveLocToNextLine(SourceLocation& Loc) {
  const unsigned LocLineNumber = SM->getSpellingLineNumber(Loc);
  while (SM->getSpellingLineNumber(Loc) == LocLineNumber) {
    Loc = Loc.getLocWithOffset(1);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::MoveLocToEndOfLine(SourceLocation& Loc) {
  const unsigned LocLineNumber = SM->getSpellingLineNumber(Loc);
  SourceLocation NewLoc = Loc;
  while (SM->getSpellingLineNumber(NewLoc) == LocLineNumber) {
    Loc = NewLoc;
    NewLoc = NewLoc.getLocWithOffset(1);
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::MoveLocToBeginningOfLine(SourceLocation& Loc) {
  const unsigned LocLineNumber = SM->getSpellingLineNumber(Loc);
  SourceLocation NewLoc = Loc;
  while (SM->getSpellingLineNumber(NewLoc) == LocLineNumber) {
    Loc = NewLoc;
    NewLoc = NewLoc.getLocWithOffset(-1);
  }
}

//------------------------------------------------------------------------------------------------
//
bool TranslatorVisitor::CheckForChar(const SourceLocation LocStart,
                                     const SourceLocation LocEnd,
                                     const char c) {

  // String buffer for searche
  const char* startBuf = SM->getCharacterData(LocStart);
  const char* endBuf = SM->getCharacterData(LocEnd);
  std::string bufferStr(startBuf, endBuf - startBuf);

  const size_t pos = bufferStr.rfind(c);
  if (pos != std::string::npos) {
    return true;
  } else {
    return false;
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::AddAtToStartAndEndKeywordsIfNeeded(const SourceLocation LocStart,
                                                           const SourceLocation LocEnd) {
  // Add '@'s to beginning and end keywords, if needed
  //
  const char* startBuf = SM->getCharacterData(LocStart);
  if (startBuf[0] != '@') {
    DeferredInsertText(LocStart, "@");
  }
  const char* endBuf = SM->getCharacterData(LocEnd.getLocWithOffset(-1));
  if (endBuf[0] != '@') {
    DeferredInsertText(LocEnd, "@");
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::AddAtToSynthesizeDynamicKeywordsIfNeeded(const SourceLocation LocStart,
                                                                 const SourceLocation LocEnd) {
  // Add '@'s and semicolons, if needed.
  //
  const char* startBuf = SM->getCharacterData(LocStart);
  const char* endBuf = SM->getCharacterData(LocEnd.getLocWithOffset(-1));

  const StringRef BufferStr(startBuf, endBuf - startBuf);

  const char* keywords[] = { "synthesize", "dynamic" };
  const size_t keywordsCount = sizeof(keywords)/sizeof(const char*);
  
  for (size_t i = 0; i < keywordsCount; i++) {
    size_t pos = 0;
    while ((pos = BufferStr.find(keywords[i], pos)) != StringRef::npos) {
      if (BufferStr[pos-1] != '@') {
        DeferredInsertText(LocStart.getLocWithOffset(pos), "@");
      }
      while ((pos = BufferStr.find('\n', pos)) != StringRef::npos) {
        size_t lastCharOnLinePos = BufferStr.find_last_not_of(" \t\n", pos);
        if (BufferStr[lastCharOnLinePos] != ',') {
          pos = lastCharOnLinePos;
          break;
        }
        pos = BufferStr.find_first_not_of(" \t\n", ++pos);
      }
      if (pos != StringRef::npos && BufferStr[pos] != ';') {
        DeferredInsertTextAfterToken(LocStart.getLocWithOffset(pos), ";");
      }
    }
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::RemovePrefixDeclarations() {

  const FileID MainFileID = SM->getMainFileID();
  const StringRef MainBuf = SM->getBufferData(MainFileID);
  const SourceLocation& LocStart = SM->getLocForStartOfFile(MainFileID);

  size_t pos = 0;
  while ((pos = MainBuf.find("using prefix", pos)) != StringRef::npos) {
    DeferredInsertText(LocStart.getLocWithOffset(pos), "// ");
    ++pos;
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::AddAtToAccessSpecIfNeeded(const SourceLocation& LocStart,
                                                  const SourceLocation& LocEnd) {
  const char* keywords[] = { "private", "protected", "public", "package" };
  const size_t count = sizeof(keywords) / sizeof(const char*);

  AddAtToKeywordsInReverse(keywords, count, LocStart, LocEnd);
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::AddAtToRequiredOptionalIfNeeded(const SourceLocation& LocStart,
                                                        const SourceLocation& LocEnd) {
  const char* keywords[] = { "required", "optional" };
  const size_t count = sizeof(keywords) / sizeof(const char*);

  AddAtToKeywordsInReverse(keywords, count, LocStart, LocEnd);
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::AddAtToKeywordsInReverse(const char* Keywords[],
                                                 size_t NumOfKeywords,
                                                 const SourceLocation& LocStart,
                                                 const SourceLocation& LocEnd) {

  const char* startBuf = SM->getCharacterData(LocStart);
  const char* endBuf = SM->getCharacterData(LocEnd);
  const StringRef bufferStr(startBuf, endBuf - startBuf);
  
  // Look for keywords to add '@s' if needed
  //
  for (size_t i = 0; i < NumOfKeywords; i++) {
    const size_t pos = bufferStr.rfind(Keywords[i]);
    if (pos != StringRef::npos) {
      if (pos == 0 || bufferStr[pos - 1] != '@') {
        DeferredInsertText(LocStart.getLocWithOffset(pos), "@");
      }
    }
  }
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::DeferredInsertText(SourceLocation Loc, string Str) {
  StringInsertion insertion = { Str, Loc, false };
  StringInsertions.push_back(insertion);
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::DeferredInsertTextAfterToken(SourceLocation Loc, string Str) {
  StringInsertion insertion = { Str, Loc, true };
  StringInsertions.push_back(insertion);
}

//------------------------------------------------------------------------------------------------
//
void TranslatorVisitor::DeferredInsertTextAtEndOfLine(SourceLocation Loc, string Str) {
  SourceLocation NewLoc = Loc;
  MoveLocToEndOfLine(NewLoc);
  DeferredInsertText(NewLoc, Str);
}



